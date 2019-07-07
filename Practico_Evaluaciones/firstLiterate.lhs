The value size i s an integer (Int) , defined to be the sum of
twelve and t h i r t e e n .

> size :: Int
> size = 12+13

The function to square an integer.

> square :: Int -> Int
> square n = n*n

The function to double an integer.

> double :: Int -> Int
> double n = 2*n

An example using double square and size.

> example :: Int
> example = double (size - square (2+2))