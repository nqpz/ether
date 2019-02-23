import "functions/stencil"
import "functions/hsl"

import "lib/github.com/athas/matte/colour"
import "lib/github.com/athas/vector/vspace"
import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/cpprandom/shuffle"

module vec2 = mk_vspace_2d f32

type ethon = {dir: vec2.vector,
              spin: f32}
type pixel = argb_colour.colour

module rng = xorshift128plus
module dist = uniform_real_distribution f32 rng
module norm_dist = normal_distribution f32 rng
module shuffle = mk_shuffle rng

let charge_ethon (ul: ethon) (um: ethon) (ur: ethon) (ml: ethon) (mm: ethon) (mr: ethon) (ll: ethon) (lm: ethon) (lr: ethon): ethon =
  let dir = vec2.(ul.dir + um.dir + ur.dir + ml.dir + mr.dir + ll.dir + lm.dir + lr.dir)
  let dir' = if vec2.norm dir > 0.0
             then vec2.normalise dir
             else {x=0.0, y=0.0}
  in {dir=dir', spin=mm.spin}

let turn_ethon (e: ethon): ethon =
  let angle = f32.atan2 e.dir.y e.dir.x
  let angle' = angle + e.spin
  in {dir={x=f32.cos angle', y=f32.sin angle'},
      spin=e.spin}

let step' ether =
  stencil_wraparound charge_ethon ether |>
  map (map turn_ethon)

let ethon_angle (e: ethon): f32 =
  f32.atan2 e.dir.y e.dir.x

let render_ethon (e: ethon): pixel =
  let hue = ethon_angle e / (2.0 * f32.pi)
  let (r, g, b) = hsl_to_rgb hue 0.5 0.5
  in argb_colour.from_rgba r g b 1.0

let rngs seed n =
  let rng = rng.rng_from_seed [seed]
  in rng.split_rng n rng

let random_ethon rng: (rng.rng, ethon) =
  let (rng, a) = dist.rand (0.0, 2.0 * f32.pi) rng
  let (rng, spin) = norm_dist.rand {mean=0, stddev=0.05} rng
  in (rng,
      {dir={x=f32.cos a, y=f32.sin a},
       spin=spin})

let randomise_angle rng (e: ethon): (rng.rng, ethon) =
  let a = f32.atan2 e.dir.y e.dir.x
  let (rng, t) = norm_dist.rand {mean=0, stddev=2} rng
  let a' = a + t
  in (rng,
      {dir={x=f32.cos a', y=f32.sin a'},
       spin=e.spin})

let invert_angle (e: ethon): ethon =
  let a = f32.atan2 e.dir.y e.dir.x
  let a' = a + f32.pi
  in {dir={x=f32.cos a', y=f32.sin a'},
      spin=e.spin}

let colour_from (efrom: ethon) (e: ethon): ethon =
  {dir={x=efrom.dir.y, y=efrom.dir.x},
   spin=e.spin}

type click_kind = #randomise | #invert

let click_at [h][w] (ether: [h][w]ethon)
      (x: i32) (y: i32) (click_kind: click_kind) (diam: f32) (rng: rng.rng): (rng.rng, [h][w]ethon) =
  let rad = diam / 2.0
  let rad' = t32 rad
  let ether_flat = copy (flatten ether)
  let i xd yd = if f32.sqrt(r32 xd**2 + r32 yd**2) < rad
                then let x' = x + xd
                     let y' = y + yd
                     in if x' >= 0 && x' < w && y' >= 0 && y' < h
                        then y' * w + x'
                        else -1
                else -1
  let is = flatten (map (\xd -> map (\yd -> i xd yd) (-rad'..<rad')) (-rad'..<rad'))
  let rngs0 = rng.split_rng (rad' * 2 * rad' * 2) rng
  let vs0 = (map (\i -> if i == -1
                        then {dir={x=0, y=0}, spin=0}
                        else unsafe ether_flat[i]) is)
  let (rngs, vs) =
    match click_kind
    case #randomise -> map2 randomise_angle rngs0 vs0 |> unzip
    case #invert -> (rngs0, map invert_angle vs0)
  in (rng.join_rng rngs,
      unflatten h w (scatter ether_flat is vs))

entry colour_at [h][w] (ether: [h][w]ethon)
      (xfrom: i32) (yfrom: i32) (xto: i32) (yto: i32) (diam: f32): [h][w]ethon =
  let rad = diam / 2.0
  let rad' = t32 rad
  let ether_flat = copy (flatten ether)
  let i xd yd = if f32.sqrt(r32 xd**2 + r32 yd**2) < rad
                then let x' = xto + xd
                     let y' = yto + yd
                     in if x' >= 0 && x' < w && y' >= 0 && y' < h
                        then y' * w + x'
                        else -1
                else -1
  let f xfd yfd = if f32.sqrt(r32 xfd**2 + r32 yfd**2) < rad
                  then let x' = xfrom + xfd
                       let y' = yfrom + yfd
                       in if x' >= 0 && x' < w && y' >= 0 && y' < h
                          then y' * w + x'
                          else -1
                  else -1
  let is = flatten (map (\xd -> map (\yd -> i xd yd) (-rad'..<rad')) (-rad'..<rad'))
  let vs0 = (map (\i -> if i == -1
                        then {dir={x=0, y=0}, spin=0}
                        else unsafe ether_flat[i]) is)
  let fs = flatten (map (\xd -> map (\yd -> f xd yd) (-rad'..<rad')) (-rad'..<rad'))
  let fs0 = (map (\f -> if f == -1
                        then {dir={x=0, y=0}, spin=0}
                        else unsafe ether_flat[f]) fs)
  let vs = map2 colour_from fs0 vs0
  in unflatten h w (scatter ether_flat is vs)

let shuffle_ethons [h][w] (ether: [h][w]ethon) (rng: rng.rng): (rng.rng, [h][w]ethon) =
  let rngs = rng.split_rng (w*h) rng |> unflatten h w
  let (rngs, ether) = map2 shuffle.shuffle' rngs ether |> unzip
  in (rng.join_rng (flatten rngs), ether)

import "lib/github.com/diku-dk/sorts/radix_sort"

let order_ethons [h][w] (ether: [h][w]ethon): [h][w]ethon =
  let order = radix_sort_by_key ethon_angle f32.num_bits f32.get_bit
  in map order ether

let randomise_spin rng (e: ethon): (rng.rng, ethon) =
  let (rng, spin) = norm_dist.rand {mean=0, stddev=0.05} rng
  in (rng, {dir=e.dir, spin=spin})

let randomise_spins [h][w] (ether: [h][w]ethon) (rng: rng.rng): (rng.rng, [h][w]ethon) =
  let rngs = rng.split_rng (w*h) rng |> unflatten h w
  let (rngs, ether) = map2 (map2 randomise_spin) rngs ether |> map unzip |> unzip
  in (rng.join_rng (flatten rngs), ether)

import "lib/github.com/diku-dk/lys/lys"

type text_content = (f32, i32)
module lys: lys with text_content = text_content = {
  type text_content = text_content

  type state = {ethons: [][]ethon,
                brush: i32,
                rng: rng.rng,
                dragging: {active: bool, x: i32, y: i32}
               }

  let init (h: i32) (w: i32): state =
    let seed = h * w
    let (rngs, ethons) = rng.split_rng (w*h) (rng.rng_from_seed [seed])
                         |> map random_ethon
                         |> unzip
    in {ethons=unflatten h w ethons,
        brush=200,
        rng = rng.join_rng rngs,
        dragging = {active=false, x=0, y=0}}

  let resize h w (s: state) = init h w with brush = s.brush
                                       with rng = s.rng

  let key (e: key_event) (key: i32) (s: state) =
    match e
    case #keydown ->
      if      key == 'r'
      then let (rng, ethons) = shuffle_ethons s.ethons s.rng
           in s with rng = rng with ethons = ethons
      else if key == 's'
      then let (rng, ethons) = randomise_spins s.ethons s.rng
           in s with rng = rng with ethons = ethons
      else if key == 'o'
      then let ethons = order_ethons s.ethons
           in s with ethons = ethons
      else s
    case #keyup ->
      s

  let mouse (mouse: i32) (x: i32) (y: i32) (s: state) =
    if mouse == 0b001 || mouse == 0b100
    then let kind = if mouse == 0b001 then #randomise else #invert
         let (rng, ethons) = click_at s.ethons x y kind (r32 s.brush) s.rng
         in s with rng = rng with ethons = ethons
    else if mouse == 0b101
    then let ethons = if s.dragging.active then
                        colour_at s.ethons s.dragging.x s.dragging.y x y (r32 s.brush)
                      else s.ethons
         in s with ethons = ethons with dragging = {active=true, x, y}
    else s with dragging = {active=false, x, y}

  let wheel _ y (s: state) = s with brush = i32.max 0 (s.brush + y)

  let step _ s: state =
    s with ethons=iterate 10 step' s.ethons
      with rng = (rng.rand s.rng).1

  let render (s: state) =
    map (map render_ethon) s.ethons

  let text_format = "FPS: %.2f\nBrush: %d"

  let text_content (render_duration: f32) (s: state): text_content =
    (1000/render_duration, s.brush)

  let text_colour = const argb.white
}
