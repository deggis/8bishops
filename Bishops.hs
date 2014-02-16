import Data.Maybe
import Data.List
import Control.Arrow
import Data.Ord
import qualified Data.Set as S
import Debug.Trace
import System.Environment

data Color = Black
           | White
    deriving (Eq, Ord)

data Piece = Piece Position Color
     deriving (Eq, Ord)

rows = 4
cols = 5

-- easyStartingBoard = Board $ [(cols,1), Black (1,1), White (cols, 2)]
startingBoard = Board . sort $ map (\y->Piece (cols, y) White ) [1..rows] ++ map (\y->Piece (1, y) Black) [1..rows]

type Steps = Int
type Position = (Int, Int)

instance Show Color where 
  show White = "W"
  show Black = "B"

instance Show Piece where
  show (Piece _ c) = show c

class HasPosition a where
  position :: a -> Position 

instance HasPosition Piece where
  position (Piece p _) = p

data Board = Board [Piece]
     deriving (Eq, Ord)

getColor :: Board -> Color -> [Piece]
getColor (Board pieces) c = filter (\(Piece _ c') -> c == c') pieces

enemy :: Piece -> Color
enemy (Piece _ Black) = White
enemy (Piece _ White) = Black

pieces (Board p) = p

-- B . . . W
-- B . . . W
-- B . . . W
-- B . . . W
instance Show Board where
  show (Board pieces) =
    let positions = map (position &&& show) pieces
        line row = intersperse ' ' . concat . map (fromMaybe "." . flip lookup positions) $ [ (x,y) | x<-[1..cols], let y=row ]
    in unlines . map line $ [1..rows]

-- | Evaluate how close board is to final position
evaluateBoard :: Board -> Int
evaluateBoard (Board pieces) =
    let cost (Piece (x, _) White) = x - 1
        cost (Piece (x, _) Black) = (cols - x)
    in sum . map cost $ pieces

isIn :: Position -> Bool
isIn (x,y) = x >= 1 && x <= cols && y >= 1 && y <= rows

diagonalPositions (orig_x, orig_y) =
   [ (x,y) | dx<-[-cols..cols],
             dy<-[-rows..rows],
             let x = orig_x + dx,
             let y = orig_y + dy,
             -- Diagonal movement
             abs dx == abs dy,
             -- Destination must be different
             abs dx > 0,
             -- Destination must be on board
             isIn (x,y) ]

-- | Return all valid positions from given position
validMoves :: Board -> Piece -> [Position]
validMoves board@(Board pieces) piece =
    let positions = map position pieces
        (orig_x, orig_y) = position piece
        enemyPositions   = map position . getColor board . enemy $ piece
        isFree others p@(x,y) = not $ elem p others
        notUnderAttack p = all (isFree enemyPositions) (diagonalPositions p)
    in [ p | p@(x,y) <- diagonalPositions (orig_x, orig_y),
                 let dx = x - orig_x,
                 let dy = y - orig_y,
                 -- Path to destination
                 let path = zip (range (orig_x+signum dx) (orig_x+dx)) (range (orig_y+signum dy) (orig_y+dy)),
                 -- Path is clear
                 all (isFree positions) path,
                 -- Destination is free from attacks
                 notUnderAttack p ]
  where range from to | from > to = reverse [to..from]
                      | otherwise = [from..to]

movePiece :: Board -> Position -> Position -> Board
movePiece (Board pieces) from to = Board . sort . map locate $ pieces
  where locate p@(Piece pos c) | pos == from = (Piece to c)
                               | otherwise   = p

mutate :: Board -> [Board]
mutate b@(Board pieces) =
    let pieceMoves from = zip (repeat from) $ validMoves b from
        allMoves   = concatMap pieceMoves pieces
    in map (\(from, to) -> movePiece b (position from) to) allMoves

type Step = Board

advance :: Int -> S.Set Board -> [[Board]] -> IO (S.Set Board, [[Board]])
advance 0 allBoards solutionPaths = do
        putStrLn $ concat ["  ", show (S.size allBoards), " boards"]
        let best = head . sortBy (comparing (evaluateBoard . head)) $ solutionPaths
            fitness = evaluateBoard . head $ best
        printPath best
        putStrLn ("Best: " ++ show fitness)
        putStrLn "Done"
        return (allBoards, solutionPaths)
advance steps allBoards solutionPaths = do
        putStrLn $ concat ["  ", show (S.size allBoards), " boards"]
        let (newAllBoards, newSolutionPaths) = go allBoards [] solutionPaths
        advance (steps-1) newAllBoards newSolutionPaths
  where go all newSolutions [] = (all, newSolutions)
        go all newSolutions (x:xs) =
           let (lastSituation:prev) = x
               newBoards = filter (\e -> not $ S.member e all) $ mutate lastSituation
               all' = foldr S.insert all newBoards
               newPaths = map (\b -> (b:lastSituation:prev)) newBoards
           in go all' (newPaths++newSolutions) xs

printPath :: [Board] -> IO ()
printPath = mapM_ (\b -> print b >> putStrLn " ") . reverse

main = do
  [no] <- getArgs
  advance (read no) (S.singleton startingBoard) [[startingBoard]]
