fractal 
http://www.shido.info/lisp/scheme8.html

> data Pt 
>   = Pt { px :: Double
>         , py :: Double
>         } 
>         deriving (Eq, Show)

> rappend
>   :: [a] -> [a] -> [a]
> rappend []     bs = bs 
> rappend (a:as) bs = rappend as (a:bs)

> divide
>   :: Pt -> Pt -> Double -> Pt
> divide pts qts r = Pt x y
>   where
>     x = r*(px pts) + (1-r)*(px qts)
>     y = r*(py pts) + (1-r)*(py qts)

> pts2string
>   :: Pt -> String
> pts2string p = show (px p) ++ " " ++ show (py p)

> printCurve
>   :: [Pt] -> FilePath -> IO ()
> printCurve ps fout = writeFile fout s
>   where
>     s = unlines $ map pts2string ps

> aLeaf
>   :: (Pt -> Pt -> Pt) -> Pt -> Pt -> [Pt] 
> aLeaf proc p q
>   = scanl proc p $ repeat q

> toPairs :: [a] -> [(a,a)]
> toPairs [] = []
> toPairs [_] = []
> toPairs (a:bs@(b:_)) = (a,b) : toPairs bs

> pairWiseApply :: [a] -> (a -> a -> a) -> [[a]]
> pairWiseApply []           f = []
> pairWiseApply [_]          f = []
> pairWiseApply (a:bs@(b:_)) f = (scanl f a (repeat b)) : pairWiseApply bs f

> cCurve
>   :: Pt -> Pt -> (Pt,Pt)
> cCurve p1 p2 = (p1, Pt x y)
>   where
>     Pt x3 y3 = divide p1 p2 0.5
>     x = x3 + y3      - (py p2)
>     y = y3 + (px p2) - x3

(define (fractal proc n points fout)
  (let loop((i 0) (points points))
       (if (= n i)
    (print-curve points fout)
    (loop
     (inc i)
     (let iter ((points points) (acc '()))
       (if (null? (cdr points)) 
           (reverse (cons (car points) acc))
           (iter
      (cdr points)
      (rappend (proc (first points) (second points)) acc)))))))
     'done)
   
> fractal 
>   :: (Pt -> Pt -> (Pt,Pt)) -> Int -> Pt -> Pt -> [Pt]
> fractal = undefined

;;; c curve
   (define (c-curve p1 p2) 
     (let ((p3 (devide p1 p2 0.5)))
       (list
        p1
        (point (+ (_x p3) (- (_y p3) (_y p2)))
        (+ (_y p3) (- (_x p2) (_x p3)))))))
   
   ;;; dragon curve
   (define dragon-curve 
     (let ((n 0))
       (lambda (p1 p2)
         (let ((op (if (even? n) + -))
        (p3 (devide  p1 p2 0.5)))
    (set! n (inc n))
    (list
     p1
     (point (op (_x p3) (- (_y p3) (_y p2)))
      (op (_y p3) (- (_x p2) (_x p3)))))))))
         
   
   ;;; koch curve
   (define (koch p1 p2)
     (let ((p3 (devide p1 p2 2/3))
    (p4 (devide p1 p2 1/3))
    (p5 (devide p1 p2 0.5))
    (c  (/ (sqrt 3) 2)))
       (list
        p1
        p3
        (point (- (_x p5) (* c (- (_y p4) (_y p3))))
        (+ (_y p5) (* c (- (_x p4) (_x p3)))))
        p4)))
       
