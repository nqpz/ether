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

entry step [w] [h]
      (ether: [w][h]ethon):
      [w][h]ethon =
  iterate 10 step' ether

let ethon_angle (e: ethon): f32 =
  f32.atan2 e.dir.y e.dir.x

let render_ethon (e: ethon): pixel =
  let hue = ethon_angle e / (2.0 * f32.pi)
  let (r, g, b) = hsl_to_rgb hue 0.5 0.5
  in argb_colour.from_rgba r g b 1.0

entry render [w] [h]
      (ether: [w][h]ethon):
      [w][h]pixel =
  map (map render_ethon) ether

let rngs seed n =
  let rng = rng.rng_from_seed [seed]
  in rng.split_rng n rng

let random_ethon rng: ethon =
  let (rng, a) = dist.rand (0.0, 2.0 * f32.pi) rng
  let (_rng, spin) = norm_dist.rand {mean=0, stddev=0.05} rng
  in {dir={x=f32.cos a, y=f32.sin a},
      spin=spin}

entry initial_ether (w: i32) (h: i32) (seed: i32): [w][h]ethon =
  copy (unflatten w h (map random_ethon (rngs seed (w * h))))

let randomise_angle rng (e: ethon): ethon =
  let a = f32.atan2 e.dir.y e.dir.x
  let (_rng, t) = norm_dist.rand {mean=0, stddev=2} rng
  let a' = a + t
  in {dir={x=f32.cos a', y=f32.sin a'},
      spin=e.spin}

let invert_angle _rng (e: ethon): ethon =
  let a = f32.atan2 e.dir.y e.dir.x
  let a' = a + f32.pi
  in {dir={x=f32.cos a', y=f32.sin a'},
      spin=e.spin}

let colour_from (efrom: ethon) (e: ethon): ethon =
  {dir={x=efrom.dir.y, y=efrom.dir.x},
   spin=e.spin}

entry click_at [w] [h] (ether: [w][h]ethon)
      (x: i32) (y: i32) (click_kind: i32) (diam: f32) (seed: i32): [w][h]ethon =
  let rad = diam / 2.0
  let rad' = t32 rad
  let ether_flat = copy (flatten ether)
  let i xd yd = if f32.sqrt(r32 xd**2 + r32 yd**2) < rad
                then let x' = x + xd
                     let y' = y + yd
                     in if x' >= 0 && x' < w && y' >= 0 && y' < h
                        then x' * h + y'
                        else -1
                else -1
  let is = flatten (map (\xd -> map (\yd -> i xd yd) (-rad'..<rad')) (-rad'..<rad'))
  let rngs0 = rngs seed (rad' * 2 * rad' * 2)
  let vs0 = (map (\i -> if i == -1
                        then {dir={x=0, y=0}, spin=0}
                        else unsafe ether_flat[i]) is)
  let vs = if click_kind == 1
           then map2 randomise_angle rngs0 vs0
           else map2 invert_angle rngs0 vs0
  in unflatten w h (scatter ether_flat is vs)

entry colour_at [w] [h] (ether: [w][h]ethon)
      (xfrom: i32) (yfrom: i32) (xto: i32) (yto: i32) (diam: f32): [w][h]ethon =
  let rad = diam / 2.0
  let rad' = t32 rad
  let ether_flat = copy (flatten ether)
  let i xd yd = if f32.sqrt(r32 xd**2 + r32 yd**2) < rad
                then let x' = xto + xd
                     let y' = yto + yd
                     in if x' >= 0 && x' < w && y' >= 0 && y' < h
                        then x' * h + y'
                        else -1
                else -1
  let f xfd yfd = if f32.sqrt(r32 xfd**2 + r32 yfd**2) < rad
                  then let x' = xfrom + xfd
                       let y' = yfrom + yfd
                       in if x' >= 0 && x' < w && y' >= 0 && y' < h
                          then x' * h + y'
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
  in unflatten w h (scatter ether_flat is vs)

entry shuffle_ethons [w] [h] (ether: [w][h]ethon) (seed: i32): [w][h]ethon =
  let rngs = rngs seed (w*h) |> unflatten w h
  in map2 shuffle.shuffle' rngs ether |> map (.2)

import "lib/github.com/diku-dk/sorts/radix_sort"

entry order_ethons [w] [h] (ether: [w][h]ethon): [w][h]ethon =
  let order = radix_sort_by_key ethon_angle f32.num_bits f32.get_bit
  in map order ether

let randomise_spin rng (e: ethon): ethon =
  let (_rng, spin) = norm_dist.rand {mean=0, stddev=0.05} rng
  in {dir=e.dir,
      spin=spin}

entry randomise_spins [w] [h] (ether: [w][h]ethon) (seed: i32): [w][h]ethon =
  let rngs = rngs seed (w*h) |> unflatten w h
  in map2 (map2 randomise_spin) rngs ether
