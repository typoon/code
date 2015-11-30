module Fractal where

type Point = (Double, Double)
type MyColor = Int

{-
 - The Mandelbrot set is defined by equation:
 -    z[n+1] = z[n]²+c
 - 
 - Where:
 -     z[n] -> We start with point (0,0) and iterate
 -     c -> Point being tested to be part of the Mandelbrot set or not
 - 
 - both `z` and `c` are points (x,yi)
 - 
 - A point `c` is said to be part of the Mandelbrot set if its magnitude never
 - surpasses 2 after being recursivelly tested by the above mentioned equation.
 - As this is an infinite set, we define a recursion limit for testing.
 - 
 - As the number of pixels in a screen is limited, we can test them all and see 
 - which ones are part of the Mandelbrot set and which are not
 - 
 - The general idea is:
 - 
 - * Get the size of the screen where the drawing will be done
 - * For each pixel, convert its cordinates to the (-2,-2) (2,2) plane
 - * Get the converted point and generate a list of N points using the above 
 -   mentioned equation
 - * Verify if the point is part of the mandelbrot set
 - 
 -}

{-
 - Converts a point from one coordinate system to another
 -
 - w - is the number of pixels in the width (how many columns)
 - h - is the number of pixels in the height (how many lines)
 - ((wMin, wMax), (hMin, hMax)) - the min and max points of our new plane
 - (x, y) - the point we want to convert
 -
 - So, if we have a screen that is 1024x768 and we want to convert the
 - coordinates of point (368, 179) to the plane ((-2, 2), (-2, 2)) we would do
 -
 - convertPoint 1024 768 ((-2, -2), (2, 2)) (369, 179)
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

inMandelbrot :: Int -> Point -> Int
inMandelbrot threshold p = do
    let points = take threshold (iterate (calcMandelbrot p) (0, 0))
    -- The check here is <= 4, because we are calculating the squared distance
    -- in order to avoid having to calculate the square root
    length $ takeWhile (<= 4) (map distSquared points)

distSquared :: Point -> Double
distSquared (x, y) = x*x + y*y

{--
 - z -> (u, vi)
 - c -> (x, yi)
 -
 - z² = u² + 2uvi + (vi)²
 - z² = u² + 2uvi - v²
 -
 - z + c = u² + 2uvi - v² + x + yi
 -
 - u², -v², x -> real parts
 - 2uvi, yi -> imaginary part
 -
 - The function calculates the above equation and returns a tuple where the
 - first element is the real part and the second is the imaginary one
 -}
calcMandelbrot :: Point -> Point -> Point
calcMandelbrot (x, y) (u, v) = (u*u - v*v + x, 2*u*v + y)


{--
 - Given the width w and height h, returns a list of all possible points that
 - exist in a plane
 -}
getPoints :: Double -> Double -> [Point]
getPoints w h = [(x, y) | x <- [0..(w - 1)], y <- [0..(h - 1)]]

{-
 - This will take a list of all points to be checked if they are part of the 
 - Mandelbrot set or not and return a list of Integers. The returned list 
 - represents how many iterations were necessary to find out that a certain 
 - point was not part of the Mandelbrot set. For points that
 - are part of the set, the value will be the same as threshold
 -
 - threshold - The number of iterations for the recursive function
 - w - The width of the window where the graph will be ploted in pixels
 - h - The height of the window where the graph will be ploted in pixels
 -
 - Returns a list of (Point, MyColor) 
 -  The point represents a pixel on the screen
 -  The color is a number that can be used as an index into a pallete. This
 -  number represents how many iterations were successfully completed when
 -  testing the point to be part of the Mandelbrot set or not. If this value
 -  is the same as the threshold, then the point is part of the set
 -}
mandelbrot :: Int -> Double -> Double -> [(Point, MyColor)]
mandelbrot threshold w h = do
    let points = getPoints w h
    let cPoints = map (convertPoint w h ((-2.0, 2.0), (-2.0, 2.0))) points
    let colors = map (inMandelbrot threshold) cPoints
    zip points colors
