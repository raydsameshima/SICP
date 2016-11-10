Accurman.lhs

> module Accurman where

> -- accurman :: Int -> Int -> Int
> accurman _ 0 = 0
> accurman 0 y = 2*y
> accurman _ 1 = 2
> accurman x y = accurman (x-1) (accurman x (y-1))
