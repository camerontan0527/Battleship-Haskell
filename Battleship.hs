-- Code based on framework designed by Steven X. Han from
-- the Australian National University, who has granted
-- permission for the usage of their work by the cohort
-- enrolled in the course COMP1100 in 2017 Semester 2 for
-- education purposes only. No commercial usage is allowed
-- without the explicit permission from the original author.
--
-- Assignment completed by:
-- Name    : Renhao Tan
-- UID     : u6211458
-- Tutor   : Lizzie Parkinson
-- Lab Time: Thursday 11 am - 1 pm

module Battleship where

import Data.List
import Prelude hiding (Left, Right)

type Board = Matrix Cell
type Ships = Matrix Bool

-- showBoard prepares a nice string for printing onto Terminal.
showBoard :: Board -> String
showBoard cells = "+" ++ replicate 21 '-' ++ "+\n"
                    ++ intercalate "\n" (map rowsToGrid cells)
                    ++ "\n+" ++ replicate 21 '-' ++ "+"
    where
        rowsToGrid :: Row Cell -> String
        rowsToGrid row = "| " ++ intercalate " " (map cStateToCell row) ++ " |"

        cStateToCell :: Cell -> String
        cStateToCell c = case c of
            Unchecked -> " "
            Hit        -> "o"
            Miss       -> "x"

data Cell = Unchecked | Hit | Miss
    deriving (Show, Read)
type Matrix a  = [Row a]
type Row a     = [a]

data Condition = Won | Lost | Playing
    deriving (Show)

data Direction  = Up | Down | Left | Right
    deriving (Show, Read)
type Coordinate = (XCoord, YCoord)
type XCoord     = Coord
type YCoord     = Coord
type Coord      = Integer

data ShipType = Carrier | Battleship | Submarine | Cruiser | Destroyer
    deriving (Show, Read, Eq)

data State = State {board        :: Board,
                    ships        :: Ships,
                    condition    :: Condition,
                    numMoves     :: Integer}
                    deriving (Show)

type ShipsOnGrid = [ShipType]
data GenShips = GenShips {gsShips       :: Ships,
                          existingShips :: ShipsOnGrid,
                          finished      :: Bool}
                          deriving (Show)

-- updateList replaces an element by index in a given list.
updateList :: [a] -> Int -> a -> [a]
updateList list n x = take (n) list ++ [x] ++ drop (n + 1) list

validPlacement :: GenShips -> Coordinate -> Direction -> ShipType -> Bool
validPlacement gs c d s =
                    not (s `elem` (existingShips gs))
                && (all coordInBound onCoords)
                && (not $ any (\x -> isShipAtCoord x (gsShips gs)) $
                         filter coordInBound $ nub $ concatMap getNeighbours onCoords)
    where
        onCoords = getCoords c d (shipLength s)

shipLength :: ShipType -> Integer
shipLength s = case s of
    Carrier    -> 5
    Battleship -> 4
    Cruiser    -> 3
    Submarine  -> 3
    Destroyer  -> 2

-- getCoords returns the list of Coordinate which would be
-- occupied by the ship.
getCoords :: Coordinate -> Direction -> Integer -> [Coordinate]
getCoords (i, j) dir l = case dir of
    Down  -> map (\x -> (i, x)) [j .. (j + l - 1)]
    Right -> map (\x -> (x, j)) [i .. (i + l - 1)]
    Up    -> map (\x -> (i, x)) [j, (j - 1) .. (j - l + 1)]
    Left  -> map (\x -> (x, j)) [i, (i - 1) .. (i - l + 1)]

-- getNeighbours returns a 9-element list containing
-- coordinates around the given coordinate and itself.
getNeighbours :: Coordinate -> [Coordinate]
getNeighbours (x, y) = [(i, j) | i <- [(x - 1) .. (x + 1)], j <- [(y - 1) .. (y + 1)]]

coordInBound :: Coordinate -> Bool
coordInBound (x, y)
    | (x >= 0 && x <= 9) && (y >= 0 && y <= 9) = True
    | otherwise                                = False

-- isShipAtCoord determines if there is a ship already placed
-- at the coord.
isShipAtCoord :: Coordinate -> Ships -> Bool
isShipAtCoord (x, y) grid = grid !! (fromIntegral y) !! (fromIntegral x)

placeShip :: GenShips -> Coordinate -> Direction -> ShipType -> GenShips
placeShip gs (x, y) dir st
    | length (existingShips gs) == 5  = GenShips (gsShips gs) (existingShips gs) True
    | validPlacement gs (x, y) dir st = GenShips (gsShips gs) (updateList (existingShips gs) (length (existingShips gs)) st) (finished gs)
    | otherwise                       = gs

transitionState :: State -> Coordinate -> State
transitionState state (x, y)
    | judgeCondition (condition state) = state
    | numMoves state == 20 = State (board state) (ships state) Lost 20
    | (x < 0 || x > 9) || (y < 0 || y > 9) = state
    | judgeChecked (((board state) !! (fromIntegral y)) !! (fromIntegral x)) = oneMoreMove state
    | ((ships state) !! (fromIntegral y)) !! (fromIntegral x) && length (countHit (board state) 0) == 16 = State (updateList (board state) (fromIntegral y) (updateList ((board state) !! (fromIntegral y)) (fromIntegral x) Hit)) (ships state) Won (numMoves state)
    | ((ships state) !! (fromIntegral y)) !! (fromIntegral x) = State (updateList (board state) (fromIntegral y) (updateList ((board state) !! (fromIntegral y)) (fromIntegral x) Hit)) (ships state) (condition state) (numMoves state)
    | not (((ships state) !! (fromIntegral y)) !! (fromIntegral x)) = oneMoreMove (State (updateList (board state) (fromIntegral y) (updateList ((board state) !! (fromIntegral y)) (fromIntegral x) Miss)) (ships state) (condition state) (numMoves state))

judgeCondition :: Condition -> Bool
judgeCondition con = case con of
    Won     -> True
    Lost    -> True
    Playing -> False

countHit :: Board -> Int -> [Cell]
countHit b n
    | n == 9 = filter judgeHit (b !! n)
    | n >= 0 && n < 9 = filter judgeHit (b !! n) ++ countHit b (n + 1)

judgeChecked :: Cell -> Bool
judgeChecked c = case c of
    Unchecked -> False
    Hit -> True
    Miss -> True

judgeHit :: Cell -> Bool
judgeHit c = case c of
    Hit -> True
    Miss -> False
    Unchecked -> False

oneMoreMove :: State -> State
oneMoreMove (State b s c m) = State b s c (m + 1)