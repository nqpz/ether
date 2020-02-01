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

module rnge = xorshift128plus
module dist = uniform_real_distribution f32 rnge
module norm_dist = normal_distribution f32 rnge
module shuffle = mk_shuffle rnge
type rng = rnge.rng

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
  let rng = rnge.rng_from_seed [seed]
  in rnge.split_rng n rng

let random_ethon rng: (rng, ethon) =
  let (rng, a) = dist.rand (0.0, 2.0 * f32.pi) rng
  let (rng, spin) = norm_dist.rand {mean=0, stddev=0.05} rng
  in (rng,
      {dir={x=f32.cos a, y=f32.sin a},
       spin=spin})

let randomise_angle rng (e: ethon): (rng, ethon) =
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
      (x: i32) (y: i32) (click_kind: click_kind) (diam: f32) (rng: rng): (rng, [h][w]ethon) =
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
  let rads = (-rad'..<rad')
  let is_length = (rad' * 2 * rad' * 2)
  let is = flatten (map (\xd -> map (\yd -> i xd yd) rads) rads) :> [is_length]i32
  let rngs0 = rnge.split_rng is_length rng
  let vs0 = (map (\i -> if i == -1
                        then {dir={x=0, y=0}, spin=0}
                        else unsafe ether_flat[i]) is)
  let (rngs, vs) =
    match click_kind
    case #randomise -> map2 randomise_angle rngs0 vs0 |> unzip
    case #invert -> (rngs0, map invert_angle vs0)
  in (rnge.join_rng rngs,
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
  let rads = (-rad'..<rad')
  let (is, fs) = unzip (flatten (map (\xd -> map (\yd -> (i xd yd, f xd yd)) rads) rads))
  let vs0 = (map (\i -> if i == -1
                        then {dir={x=0, y=0}, spin=0}
                        else unsafe ether_flat[i]) is)
  let fs0 = (map (\f -> if f == -1
                        then {dir={x=0, y=0}, spin=0}
                        else unsafe ether_flat[f]) fs)
  let vs = map2 colour_from fs0 vs0
  in unflatten h w (scatter ether_flat is vs)

let shuffle_ethons [h][w] (ether: [h][w]ethon) (rng: rng): (rng, [h][w]ethon) =
  let rngs = rnge.split_rng (h*w) rng |> unflatten h w
  let (rngs, ether) = map2 shuffle.shuffle' rngs ether |> unzip
  in (rnge.join_rng (flatten rngs), ether)

import "lib/github.com/diku-dk/sorts/radix_sort"

let order_ethons [h][w] (ether: [h][w]ethon): [h][w]ethon =
  let order = radix_sort_by_key ethon_angle f32.num_bits f32.get_bit
  in map order ether

let randomise_spin rng (e: ethon): (rng, ethon) =
  let (rng, spin) = norm_dist.rand {mean=0, stddev=0.05} rng
  in (rng, {dir=e.dir, spin=spin})

let randomise_spins [h][w] (ether: [h][w]ethon) (rng: rng): (rng, [h][w]ethon) =
  let rngs = rnge.split_rng (h*w) rng |> unflatten h w
  let (rngs, ether) = map2 (map2 randomise_spin) rngs ether |> map unzip |> unzip
  in (rnge.join_rng (flatten rngs), ether)

import "lib/github.com/diku-dk/lys/lys"

type text_content = (i32, i32)
module lys: lys with text_content = text_content = {
  type text_content = text_content

  type~ state = {ethons: [][]ethon,
                 brush: i32,
                 rng: rng,
                 dragging: {active: bool, x: i32, y: i32}
                }

  let init (seed: u32) (h: i32) (w: i32): state =
    let (rngs, ethons) = rnge.split_rng (h*w) (rnge.rng_from_seed [i32.u32 seed])
                         |> map random_ethon
                         |> unzip
    in {ethons=unflatten h w ethons,
        brush=200,
        rng = rnge.join_rng rngs,
        dragging = {active=false, x=0, y=0}}

  let resize h w (s: state) =
    let (rngs, ethons) = rnge.split_rng (h*w) s.rng
                         |> map random_ethon
                         |> unzip
    in s with ethons = unflatten h w ethons
         with rng = rnge.join_rng rngs

  let keydown (key: i32) (s: state) =
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

  let keyup (_key: i32) (s: state) = s

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

  let grab_mouse = false

  let step (s: state) =
    s with ethons=iterate 10 step' s.ethons
      with rng = (rnge.rand s.rng).0

  let move (x: i32, y: i32) (dx,dy) = (x+dx, y+dy)

  let event (e: event) (s: state) =
    match e
    case #step _td -> step s
    case #wheel {dx=_, dy} ->
      s with brush = i32.max 0 (s.brush + dy)
    case #mouse {buttons, x, y} -> mouse buttons x y s
    case #keydown {key} ->
      keydown key s
    case #keyup {key} ->
      keyup key s

  let render (s: state) =
    map (map render_ethon) s.ethons

  let text_format () = "FPS: %d\nBrush: %d"

  let text_content (fps: f32) (s: state): text_content =
    (t32 fps, s.brush)

  let text_colour = const argb.white
}
