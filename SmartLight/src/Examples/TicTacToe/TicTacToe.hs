module Examples.TicTacToe.TicTacToe where

import Graphics.UI.SDL as SDL

import qualified Data.HashMap.Strict as Map
import Data.Maybe(isJust, fromJust)
import SmartLight
import Data.List

data Cell = X | O deriving (Eq, Show)

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

emptyImageName :: String
emptyImageName = "empty"
crossImageName :: String
crossImageName = "cross"
circleImageName :: String
circleImageName = "circle"

imageNames :: [String]
imageNames = [emptyImageName, crossImageName, circleImageName]

toImageName :: Maybe Cell -> String
toImageName Nothing  = emptyImageName
toImageName (Just X) = crossImageName
toImageName (Just O) = circleImageName

boardToImageNames :: Board -> [String]
boardToImageNames b = 
    (concatMap . map) toImageName (rows b)

boardToImages :: Board -> GameEntities -> [Image]
boardToImages b imgs = 
    map (getCurrentImage . fromJust . (`Map.lookup` imgs))
        (boardToImageNames b)

ticTacToeInit :: Game -> IO Game
ticTacToeInit g = do
    
    counters <- loadImage "counters" "src/Examples/TicTacToe/resources/"
    
    let onCounter offset = singleImageEntity $
                            newPartialImage counters (Rect offset 0 138 140)
    
    let cellImages = zip imageNames (map onCounter [0,140,280])
    
    return (foldl' (Prelude.flip $ uncurry addEntity) g cellImages)

boardRender :: Board -> GameEntities -> Surface -> IO ()
boardRender b ents surf = do
    let toDraw = zip gameCoords $ boardToImages b ents
    let drawImages ((x, y), img) = drawImage ((x - 1) * 138) ((y - 1) * 140) img surf
    
    mapM_ drawImages toDraw
  
ticTacToeRender :: Game -> IO ()
ticTacToeRender game = 
    boardRender emptyBoard (_entities game)
                           (_screenSurface . _screen $ game)
      

ticTacToeLoop :: GameLoop
ticTacToeLoop = newGameLoop $ defaultGameLoop {
      _onInit       = ticTacToeInit
--   , _onGameLogic  = ticTacToeLogic
    , _onRender     = ticTacToeRender
    }

main :: IO ()
main  = executeGame (WindowData 640 480 32 "SDL") ticTacToeLoop