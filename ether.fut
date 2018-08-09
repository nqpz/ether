import "/futlib/vector"

import "functions/stencil"
import "functions/hsl"

import "lib/github.com/athas/matte/colour"
import "lib/github.com/diku-dk/cpprandom/random"


module vec2 = mk_vec2 f32

type ethon = vec2.vec
type pixel = argb_colour.colour


let charge_ethon ul um ur ml mr ll lm lr =
  let dir = ul vec2.+ um vec2.+ ur vec2.+ ml vec2.+ mr vec2.+ ll vec2.+ lm vec2.+ lr
  in vec2.normalise dir

let outer: ethon = {x= -1.0, y=0.0}

entry step [w] [h]
      (ether: [w][h]ethon):
      [w][h]ethon =
  stencil charge_ethon ether outer

let render_ethon (dir: ethon): pixel =
  let base_dir = {x=1.0, y=0.0}
  let angle = f32.acos (vec2.dot base_dir dir)
  let hue = angle / (2.0 * f32.pi)
  let (r, g, b) = hsl_to_rgb hue 0.5 0.5
  in argb_colour.from_rgba r g b 1.0

entry render [w] [h]
      (ether: [w][h]ethon):
      [w][h]pixel =
  map (map render_ethon) ether

module dist = uniform_real_distribution f32 minstd_rand

let random_ethon rng =
  let (_rng, a) = dist.rand (0.0, 2.0 * f32.pi) rng
  in {x=f32.cos a, y=f32.sin a}

let rngs seed n =
  let rng = minstd_rand.rng_from_seed [seed]
  in minstd_rand.split_rng n rng

entry initial_ether (w: i32) (h: i32) (seed: i32): [w][h]ethon =
  copy (unflatten w h (map random_ethon (rngs seed (w * h))))

entry click_at [w] [h] (ether: [w][h]ethon) (x: i32) (y: i32) (seed: i32): [w][h]ethon =
  let diam = 100
  let rad = diam / 2
  let is = flatten (map (\xd -> map (\yd -> (x + xd) * h + y + yd) (-rad..<rad)) (-rad..<rad))
  in unflatten w h (scatter (copy (flatten ether)) is (map random_ethon (rngs seed (diam * diam))))
