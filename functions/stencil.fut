let stencil 'a [w] [h]
    (f: a -> a -> a -> a -> a -> a -> a -> a -> a -> a)
    (array: [w][h]a)
    (empty: a): [w][h]a =
  let point x y =
    if x == 0 || y == 0 || x == w - 1 || y == h - 1
    then empty
    else f (unsafe array[x - 1, y - 1])
           (unsafe array[x, y - 1])
           (unsafe array[x + 1, y - 1])
           (unsafe array[x - 1, y])
           (unsafe array[x, y])
           (unsafe array[x + 1, y])
           (unsafe array[x - 1, y + 1])
           (unsafe array[x, y + 1])
           (unsafe array[x + 1, y + 1])
  in map (\x -> map (\y -> point x y) (0..<h)) (0..<w)

let stencil_wraparound 'a [w] [h]
    (f: a -> a -> a -> a -> a -> a -> a -> a -> a -> a)
    (array: [w][h]a): [w][h]a =
  let point x y =
    let xm1 = if x == 0
              then w - 1
              else x - 1
    let ym1 = if y == 0
              then h - 1
              else y - 1
    let xp1 = if x == w - 1
              then 0
              else x + 1
    let yp1 = if y == h - 1
              then 0
              else y + 1
    in f (unsafe array[xm1, ym1])
         (unsafe array[x, ym1])
         (unsafe array[xp1, ym1])
         (unsafe array[xm1, y])
         (unsafe array[x, y])
         (unsafe array[xp1, y])
         (unsafe array[xm1, yp1])
         (unsafe array[x, yp1])
        (unsafe array[xp1, yp1])
  in map (\x -> map (\y -> point x y) (0..<h)) (0..<w)
