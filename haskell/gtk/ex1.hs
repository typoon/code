import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Fractal

threshold = 20

main :: IO ()
main = do
    initGUI
    window <- windowNew
    set window [windowTitle := "Fractal",
                windowDefaultWidth := 300,
                windowDefaultHeight := 300,
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
    let points = mandelbrot 200 200 threshold

    mapM_ drawPixel points

    --Graphics.Rendering.Cairo.rectangle 0 0 1 1
    stroke

drawPixel :: (Fractal.Point, Fractal.MyColor) -> Render ()
drawPixel (point,c) = do
    let px = fst(point)
    let py = snd(point)
    setLineWidth 1
    setColor c
    --setSourceRGB 0 0 (realToFrac c)
    Graphics.Rendering.Cairo.rectangle (realToFrac px) (realToFrac py) 1 1
    stroke

setColor c
    | c < threshold = setSourceRGB 0 0 0
    | otherwise = setSourceRGB 1 1 1
