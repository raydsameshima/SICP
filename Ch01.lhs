Ch01.lhs

> module Ch01 where

> -- accurman :: Int -> Int -> Int
> ack _ 0 = 0
> ack 0 y = 2*y
> ack _ 1 = 2
> ack x y = ack (x-1) (ack x (y-1))

  *Ackermann> ack 1 10
  1024
  *Ackermann> ack 2 4
  65536
  *Ackermann> ack 3 3
  65536

> change amount = change' amount 5
>   where
>     change' a k
>       | a == 0          = 1
>       | a < 0 || k == 0 = 0
>       | otherwise       = change' a (k-1) + change' (a- fD k) k
>     fD 1 = 1
>     fD 2 = 5
>     fD 3 = 10
>     fD 4 = 25
>     fD 5 = 50
  
  *Cahnge> change 100
  292

> f11 n
>   | n < 3 = n
>   | otherwise = f11 (n-1) + 2*f11 (n-2) + 3*f11 (n-3)
  
  *Ch01> f11 10
  1892
  *Ch01> map f11 [0..10]
  [0,1,2,4,11,25,59,142,335,796,1892]
