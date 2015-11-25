module Fractal where

type Point = (Double, Double)
type MyColor = Int

{-
 -
Equation 1:
   z[n+1] = z[n]²+c
c -> Constant being tested to be part of the mandelbrot set or not

both `z` and `c` are points (x,y)

A point `c` is said to be part of the Mandelbrot set if its magnitude never
surpasses 2 after being recursivelly tested by the Equation 1. As this is
an infinite set, we define a recursion limit for testing.
We start with z[0] = (0,0)

As the number of pixels in a screen is limited, we can test them all and see 
which ones are part of the Mandelbrot set and which are not

The general idea is:
    Get the size of the screen where the drawing will be done
    For each pixel, convert its cordinates to the (-2,-2) (2,2) plane
    Get the converted point and generate a list of N points using the Equation 1
    Verify if the point is part of the mandelbrot set
    

-}

{-
 - w - is the number of pixels in the width (how many columns)
 - h - is the number of pixels in the height (how many lines)
 - (x, y) - the point we want to convert
 - ((wmin, hmin), (wmax, hmax)) - the min and max points of our new plane
 -
 - So, if we have a screen that is 2014x768 and we want to convert the
 - coordinates of point (368, 179) to the plane ((-2, -2), (2, 2)) we would do
 -
 - convertPoint 1024 768 (368, 179) ((-2, -2), (2, 2))
 -
 - This will return a point (x, y)
 -}
convertPoint :: Double -> Double -> (Point, Point) -> Point -> Point
convertPoint w h ((wMin, wMax), (hMin, hMax)) (x,y) = do
    let wMag = wMax - wMin
    let hMag = hMax - hMin
    
    let wRatio = wMag / w
    let hRatio = hMag / h

    let x' = wMin + (wRatio * x)
    let y' = hMin + (hRatio * y)
    (x', y')

{-
 - This will take a list of all points to be checked if they are part of the Mandelbrot set
 - or not and return a list of Integers. The returned list represents how many iterations were
 - necessary to find out that a certain point was not part of the Mandelbrot set. For points that
 - are part of the set, the value will be -1.
 -
 - threshold - The number of iterations for the recursive function
 -}
mandelbrot :: Double -> Double -> Int -> [(Point, MyColor)]
mandelbrot w h threshold = do
    let points = getPoints w h
    --let cPoints = map (convertPoint w h ((-2.0, 2.0), (-2.0, 2.0))) points
    let cPoints = map (convertPoint w h ((-2.0, 2.0), (-2.0, 2.0))) points
    let colors = map (inMandelbrot threshold) cPoints
    zip points colors

inMandelbrot :: Int -> Point -> Int
inMandelbrot threshold p = do
    --let points = take threshold (iterate (calcMandelbrot (0, 0)) p)
    let points = take threshold (iterate (calcMandelbrot p) (0, 0))
    length $ takeWhile (<= 4) (map dist points)

dist :: Point -> Double
dist (x, y) = x*x + y*y

{--
 - z² + c ->  (u, v)² + (x, y)
 - z² = u² + 2uv + v²
 - u², v², x -> real parts
 - 2uv, y -> imaginary part
 -}
calcMandelbrot :: Point -> Point -> Point
calcMandelbrot (x, y) (u, v) = (u*u - v*v + x, 2*u*v + y)
--calcMandelbrot (u, v) (x,y) = (u*u - v*v + x, 2*u*v + y)
--calcMandelbrot (u, v) (x,y) = (x*x - y*y + u, 2*x*y + v)


getPoints :: Double -> Double -> [Point]
getPoints w h = [(x, y) | x <- [0..(w - 1)], y <- [0..(h - 1)]]

-- plot [Points MyColor]

{--

main = do 
    putStrLn "Hello"
    putStrLn "x = "
    let x = mandelbrot 1024 768 20
    print [y |y <- x, snd(y) > 0]
--}


-- Generate a list of all points on the screen
-- Pass list to fractal function
-- Fractal function tests each point and returns a list of (Point, MyColor)
-- -- Convert the point to the expected plane
-- -- Calculate N points now
-- -- Validate if they are in the set or not
-- -- Return how many iterations it took to find out that the point is not in the set, or -1 in case it is
-- Pass return from Fractal function to plot function
-- Plot function takes a list of (Point, MyColor)
