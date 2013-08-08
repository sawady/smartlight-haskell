module Examples.TicTacToe.TicTacToe where

import Graphics.UI.SDL as SDL
import qualified Data.HashMap.Strict as Map
import Data.Maybe(isJust, fromJust)
import SmartLight

data Cell = X | O deriving (Eq)

type Board = Map.HashMap (Int,Int) Cell

emptyBoard :: Board
emptyBoard = Map.empty

gameCoords :: [(Int,Int)]
gameCoords = [ (x,y) | x <- [1..3], y <- [1..3] ]

row :: Board -> Int -> [Maybe Cell]
row b n = map (\y -> Map.lookup (n,y) b) [1..3] 

rows :: Board -> [[Maybe Cell]]
rows b = map (row b) [1..3]

column :: Int -> Board -> [Maybe Cell]
column n b = map (\x -> Map.lookup (x,n) b) [1..3]

columns :: Board -> [[Maybe Cell]]
columns b = map (`column` b) [1..3]

crosses :: Board -> [[Maybe Cell]]
crosses b =
        [ [Map.lookup (1,1) b, Map.lookup (2,2) b, Map.lookup (3,3) b]
        , [Map.lookup (1,3) b, Map.lookup (2,2) b, Map.lookup (3,1) b]
        ]

winGame :: Cell -> Board -> Bool
winGame s b = checkRows || checkColumns || chechCross
  where
     checkCellGroup :: (Board -> [[Maybe Cell]]) -> Bool
     checkCellGroup g = any (\xs -> all isJust xs && all (== s) (map fromJust xs)) (g b)

     checkRows    = checkCellGroup rows
     checkColumns = checkCellGroup columns
     chechCross   = checkCellGroup crosses
     
addToBoard :: (Int, Int) ->  Cell -> Board -> Board
addToBoard = Map.insert

------------------------------------------------------

toImageName :: Maybe Cell -> String
toImageName Nothing  = "empty"
toImageName (Just X) = "cross"
toImageName (Just O) = "circle"

boardToImageNames :: Board -> [[String]]
boardToImageNames b = 
    (map . map) toImageName (rows b)

boardToImages :: Board -> Map.HashMap String Image -> [Image]
boardToImages b imgs = 
    (concatMap . map) (fromJust . (`Map.lookup` imgs)) (boardToImageNames b)

ticTacToeInit :: IO [Image]
ticTacToeInit = do
    counters <- loadImage "counters"
    
    let emptyImage  = newPartialImage counters (Rect 0 0 138 140)
    let crossImage  = newPartialImage counters (Rect 140 0 138 140)
    let circleImage = newPartialImage counters (Rect 280 0 138 140)
    
    return [emptyImage, crossImage, circleImage]

--ticTacToeLogic   = undefined
--

ticTacToeRender :: Board -> Map.HashMap String Image -> Surface -> IO ()
ticTacToeRender b s surf =
    mapM_ (\((x,y), img) -> drawImage x y img surf) (zip gameCoords (boardToImages b s))
      
--
--ticTacToeLoop :: GameLoop
--ticTacToeLoop = newGameLoop $ defaultGameLoop {
--    _onInit       = ticTacToeInit,
--    _onGameLogic  = ticTacToeLogic,
--    _onRender     = ticTacToeRender
--}

--main :: IO ()
--main  = executeGame (WindowData 640 480 32 "SDL") ticTacToeLoop