import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Fractal

threshold = 20
width = 1024
height = 768

main :: IO ()
main = do
    initGUI
    window <- windowNew
    set window [windowTitle := "Fractal",
                windowDefaultWidth := 1024,
                windowDefaultHeight := 768,
                containerBorderWidth := 0]

    frame <- frameNew
    containerAdd window frame
    canvas <- drawingAreaNew
    containerAdd frame canvas
    widgetModifyBg canvas StateNormal (Color 0xffff 0xffff 0xffff)

    widgetShowAll window
    drawWindow <- widgetGetDrawWindow canvas
    onExpose canvas (\x -> do renderWithDrawable drawWindow myDraw
                              return True)

    mainGUI

myDraw :: Render ()
myDraw = do
    let points = mandelbrot threshold Main.width Main.height
    mapM_ drawPixel points
    stroke

drawPixel :: (Fractal.Point, Fractal.MyColor) -> Render ()
drawPixel (point,c) = do
    let px = fst(point)
    let py = snd(point)
    setLineWidth 1
    setColor c
    Graphics.Rendering.Cairo.rectangle px py 1 1
    stroke

setColor c
    | c ==  0 = setSourceRGB 0.1 0.1 0.1
    | c ==  1 = setSourceRGB 0.8 0.1 0.4
    | c ==  2 = setSourceRGB 0.3 0.7 0.4
    | c ==  3 = setSourceRGB 0.1 0.1 0.9
    | c ==  4 = setSourceRGB 0.5 0.5 0.2
    | c ==  5 = setSourceRGB 0.4 0.2 0.1
    | c ==  6 = setSourceRGB 0.8 0.4 0.2
    | c ==  7 = setSourceRGB 0.2 0.4 0.8
    | c ==  8 = setSourceRGB 0.1 0.2 0.4
    | c ==  9 = setSourceRGB 0.1 0.3 0.5
    | c == 10 = setSourceRGB 0.7 0.4 0.2
    | c == 11 = setSourceRGB 0.4 0.4 1.0
    | c == 12 = setSourceRGB 1.0 1.0 0.2
    | c == 13 = setSourceRGB 1.0 0.6 0.3
    | c == 14 = setSourceRGB 0.8 0.6 0.0
    | c == 15 = setSourceRGB 0.0 0.0 0.4
    | c == 16 = setSourceRGB 0.0 0.4 0.0
    | c == 17 = setSourceRGB 0.4 0.0 0.0
    | c == 18 = setSourceRGB 0.4 0.0 0.7
    | c == 19 = setSourceRGB 0.7 0.0 0.4
    | c == 20 = setSourceRGB 0.0 0.0 0.0
    | otherwise = setSourceRGB 1 1 1
