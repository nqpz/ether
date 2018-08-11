import "functions/stencil"
import "functions/hsl"

import "lib/github.com/athas/matte/colour"
import "lib/github.com/athas/vector/vspace"
import "lib/github.com/diku-dk/cpprandom/random"


module vec2 = mk_vspace_2d f32

type ethon = vec2.vector
type pixel = argb_colour.colour

module dist = uniform_real_distribution f32 minstd_rand
module norm_dist = normal_distribution f32 minstd_rand

let outer: ethon = {x= -1.0, y=0.0}

let charge_ethon ul um ur ml mr ll lm lr =
  let dir = vec2.(ul + um + ur + ml + mr + ll + lm + lr)
  in if vec2.norm dir > 0.0
     then vec2.normalise dir
     else {x=0.0, y=0.0}

entry step [w] [h]
      (ether: [w][h]ethon):
      [w][h]ethon =
  stencil charge_ethon ether outer

let render_ethon (dir: ethon): pixel =
  let angle = f32.atan2 dir.y dir.x
  let hue = angle / (2.0 * f32.pi)
  let (r, g, b) = hsl_to_rgb hue 0.5 0.5
  in argb_colour.from_rgba r g b 1.0

entry render [w] [h]
      (ether: [w][h]ethon):
      [w][h]pixel =
  map (map render_ethon) ether

let rngs seed n =
  let rng = minstd_rand.rng_from_seed [seed]
  in minstd_rand.split_rng n rng

let random_ethon rng =
  let (_rng, a) = dist.rand (0.0, 2.0 * f32.pi) rng
  in {x=f32.cos a, y=f32.sin a}

entry initial_ether (w: i32) (h: i32) (seed: i32): [w][h]ethon =
  copy (unflatten w h (map random_ethon (rngs seed (w * h))))

let randomise_angle rng ({x, y}: ethon): ethon =
  let a = f32.atan2 y x
  let (_rng, t) = norm_dist.rand {mean=0, stddev=1} rng
  let a' = a + t
  in {x=f32.cos a', y=f32.sin a'}

let invert_angle _rng ({x, y}: ethon): ethon =
  let a = f32.atan2 y x
  let a' = a + f32.pi
  in {x=f32.cos a', y=f32.sin a'}

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
                        then outer
                        else unsafe ether_flat[i]) is)
  let vs = if click_kind == 1
           then map2 randomise_angle rngs0 vs0
           else map2 invert_angle rngs0 vs0
  in unflatten w h (scatter ether_flat is vs)
