import Data.List

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Interface.Pure.Game

import qualified Path as P

boardSize = 900.0
tileSize = 20.0
tileCount = floor $ boardSize / tileSize

toBoard :: P.Point -> Point
toBoard point = (rescale . fst $ point, rescale . snd $ point)
    where rescale x = ((fromIntegral x) * tileSize) - (boardSize / 2.0)

toTile :: Point -> P.Point
toTile point = (rescale . fst $ point, rescale . snd $ point)
    where rescale x = round $ (x / tileSize) + (boardSize / 2.0)

main = display (InWindow "Gloss" (1000,1000) (0,0))
            white (Pictures [rectangleWire 900 900, gameboard] )

tile :: Point -> Point -> Color -> Picture
tile min max bgcolor = Pictures [fill, outline] 
    where path = [min, (fst min, snd max), max, (fst max, snd min), min]
          outline = color black $ line path
          fill = color bgcolor $ polygon path

gameboard :: Picture
gameboard = pictures [pictures $ tileize rose path, pictures $ tileize white grid]
    where path = nub $ P.plot (0,0) (0, tileCount) (tileCount, tileCount) 8
          grid = [ (x,y) | x <- [0,1..tileCount], y <- [0,1..tileCount] ] \\ path
          tileize fillcolor = map (\(x,y) -> tile (toBoard (x,y)) (toBoard (x+1, y+1)) fillcolor)
          
