let stencil 'a [w] [h]
    (f: a -> a -> a -> a -> a -> a -> a -> a -> a)
    (array: [w][h]a)
    (empty: a): [w][h]a =
  let point x y =
    if x == 0 || y == 0 || x == w - 1 || y == h - 1
    then empty
    else f (unsafe array[x - 1, y - 1])
           (unsafe array[x, y - 1])
           (unsafe array[x + 1, y - 1])
           (unsafe array[x - 1, y])
           (unsafe array[x + 1, y])
           (unsafe array[x - 1, y + 1])
           (unsafe array[x, y + 1])
           (unsafe array[x + 1, y + 1])
  in map (\x -> map (\y -> point x y) (0..<h)) (0..<w)
