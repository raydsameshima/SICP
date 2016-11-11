Ackermann.lhs

> module Ackermann where

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
