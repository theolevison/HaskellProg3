{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2020
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenge15 (WordSearchGrid,Placement,Posn,Orientation(..), solveWordSearch, createWordSearch,
    LamMacroExpr(..),LamExpr(..),prettyPrint, parseLamMacro,
    cpsTransform,innerRedn1,outerRedn1,compareInnerOuter) where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
-- We import System.Random - make sure that your installation has it installed - use stack ghci and stack ghc
import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import System.IO
import System.Random

import Data.Maybe (catMaybes, fromMaybe)
import Debug.Trace ( trace )
import Data.Ord ( Down(Down) )


--import Hugs.Observe
--import Debug.Hood.Observe




-- types for Part I
type WordSearchGrid = [[ Char ]]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read)

-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
               LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

-- END OF CODE YOU MUST NOT MODIFY

-- ADD YOUR OWN CODE HERE

main :: IO ()
main = do 
    putStrLn "I have crippling depression"
    testFunc ["HASKELL","STRING","STACK","MAIN","METHOD"] 0.1

-- Challenge 1 --

type LettersAndWord = [(Char, Char, String)]

solveWordSearch :: [ String ] -> WordSearchGrid -> [ (String,Maybe Placement) ]
solveWordSearch [] _ = []
solveWordSearch _ [] = []
solveWordSearch words grid = addFailures (check (findSecondLetterAndDirection (findBeginning (getFirstSecondLetters words) grid) grid) grid) words

--convert grid to array, to get O(1) indexing

getFirstSecondLetters :: [ String ] -> LettersAndWord
getFirstSecondLetters validList = [ (head x, head $ tail x, x) | x <- validList]

--grid!!y!!x
findBeginning :: LettersAndWord -> WordSearchGrid -> [(LettersAndWord, Posn)] --finds places that could be the start of each word
findBeginning words grid = [ z | z@(words,_) <- [([law | law@(first, _, _) <- words, grid!!y!!x == first],(x,y)) | x <- [0..max], y <- [0..max]], not $ null words]
    where
        max = length grid -1

findSecondLetterAndDirection :: [(LettersAndWord, Posn)] -> WordSearchGrid -> [(String, Placement)]
findSecondLetterAndDirection [] _ = [] --check second letter against adjacents, when matches are found generate placements
findSecondLetterAndDirection ((words,posn):xs) grid = [(word, generatePlacement posn posn2) | (_, second, word) <- words, (letter,posn2) <- generateAdjacents posn grid, letter == second] ++ findSecondLetterAndDirection xs grid
    where
        adjacents = generateAdjacents posn grid

generatePlacement :: Posn -> Posn -> Placement --positions of first two letters into placement
generatePlacement posn@(x1,y1) (x2,y2)
    | x == 0 && y == -1 = (posn,Up)
    | x == -1 && y == -1 = (posn,UpBack)
    | x == -1 && y == 0 = (posn,Back)
    | x == -1 && y == 1 = (posn,DownBack)
    | x == 0 && y == 1 = (posn,Challenge15.Down)
    | x == 1 && y == 1 = (posn,DownForward)
    | x == 1 && y == 0 = (posn,Forward)
    | x == 1 && y == -1 = (posn,UpForward)
    | otherwise = error ("Invalid: " ++ show x ++ show y)
    where
        x = x2 - x1
        y = y2 - y1

generateAdjacents :: Posn -> WordSearchGrid -> [(Char, Posn)] --generates positions around the first letter
generateAdjacents (x,y) grid = [ x | (Just x) <- [maybeIndex (x+x2,y+y2) grid| x2 <- [-1..1], y2 <- [-1..1], not (x2 == 0 && y2 == 0)]]

maybeIndex :: Posn -> WordSearchGrid -> Maybe (Char, Posn)
maybeIndex (x,y) grid
    | y < 0 || y > length grid -1 = Nothing
    | x < 0 || x > length grid -1 = Nothing
    | otherwise = Just (grid!!y!!x,(x,y))

addFailures :: [(String, Maybe Placement)] -> [String] -> [(String, Maybe Placement)] --if some words arent valid, it adds them into the final words with Nothing
addFailures validList [] = validList
addFailures validList w@(word:words)
    | word `elem` listOfValidWords = addFailures validList words
    | otherwise = (word,Nothing) : addFailures validList words
    where
        listOfValidWords = [ x | (x,y) <- validList]

check :: [(String, Placement)] -> WordSearchGrid -> [(String, Maybe Placement)] --checks the words that could match
check wordsToCheck grid = [ x | (Just x) <- [ if checkWord word posn (orientationToXY orientation) grid then Just (word, Just placement) else Nothing | (word,placement@(posn,orientation)) <- wordsToCheck ]]

orientationToXY :: Orientation -> (Int,Int)
orientationToXY orientation = case orientation of
    Up -> (0,-1)
    UpBack -> (-1,-1)
    Back -> (-1,0)
    DownBack -> (-1,1)
    Challenge15.Down -> (0,1)
    DownForward -> (1,1)
    Forward -> (1,0)
    UpForward -> (1,-1)


checkWord :: String -> Posn -> (Int,Int) -> WordSearchGrid -> Bool --checks if a word fits this pos and orientation
checkWord [] _ _ _ = True
checkWord (letter:word) (x,y) (x2,y2) grid
    | y >= length grid || x >= length grid || x < 0 || y < 0 = False --out of bounds
    | grid!!y!!x == letter = checkWord word (x+x2,y+y2) (x2,y2) grid
    | otherwise = False


-- Two examples for you to try out, the first of which is in the instructions

exGrid1'1 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ] 
exWords1'1 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]

exGrid1'2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
exWords1'2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]


-- Challenge 2 --
type TempWordSearchGrid = [[ Maybe Char ]]

createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch words maxDensity = do return [ catMaybes (fullGrid!!y) | y <- [0..max]] 
    where
        tempGrid = generateEmptyGrid 0 (fromIntegral $ sum $ map length words) maxDensity --grid that is not full, can be Nothing or Just Char
        wordGrid = insertWords (sortOn (Data.Ord.Down . length) words) tempGrid gen--grid with valid words filled in, awaiting random letters
        fullGrid = fillInGrid [(x,y) | x <- [0..max], y <- [0..max]] gen chars wordGrid
        gen = mkStdGen max
        max = length tempGrid -1
        chars = foldr (\x acc -> if x `elem` acc then acc else x : acc) "" (foldr1 (++) words) --turns words into a list of chars with no duplicates

testFunc :: [ String ] -> Double -> IO ()
testFunc words maxDensity = do printGrid [[ fromMaybe '+' x | x <- fullGrid!!y ] | y <- [0..max]] -- printGrid [[ fromMaybe '+' x | x <- wordGrid!!y ] | y <- [0..max]] --printGrid [ catMaybes (fullGrid!!y) | y <- [0..max]]
    where 
        tempGrid = generateEmptyGrid 0 (fromIntegral $ sum $ map length words) maxDensity --grid that is not full, can be Nothing or Just Char
        wordGrid = insertWords (sortOn (Data.Ord.Down . length) words) tempGrid gen--grid with valid words filled in, awaiting random letters
        fullGrid = fillInGrid [(x,y) | x <- [0..max], y <- [0..max]] gen chars wordGrid
        gen = mkStdGen max
        max = length tempGrid - 1
        chars = foldr (\x acc -> if x `elem` acc then acc else x : acc) "" (foldr1 (++) words) --turns words into a list of chars with no duplicates


--will not make a change if there is already a letter in place. Ensure this is only used when validity has already been checked
insertIntoGrid :: Posn -> Char -> TempWordSearchGrid -> TempWordSearchGrid
insertIntoGrid (x,y) letter grid = take y grid ++ [take x yLine ++ 
    case last $ take (x+1) yLine of 
        Nothing -> [Just letter] 
        Just z -> [Just z]
    ++ drop (x+1) yLine] ++ drop (y+1) grid
    where
        yLine = grid!!y

--let grid = [[Just 'a', Just 'b', Just 'c'],[Just '1', Just '2', Just '3'],[Just 'x', Just 'y', Just 'z']]
--let grid = [[Nothing, Nothing, Nothing],[Nothing, Nothing, Nothing],[Nothing, Nothing, Nothing]]
--let words = [ "HASKELL","STRING","STACK","MAIN","METHOD"]

fillInWord :: String -> Posn -> Posn -> TempWordSearchGrid -> TempWordSearchGrid
fillInWord [] _ _ grid = grid
fillInWord (letter:word) posn@(x,y) (x2,y2) grid = fillInWord word (x+x2,y+y2) (x2,y2) (insertIntoGrid posn letter grid)
--fillInWord (letter:word) (x,y) (x2,y2) grid = take x grid : letter : take
    --    take 1 grid ++ (((take 3 $ last $ take 2 grid) ++ "9" ++ (reverse $ take 4 $ reverse $ last $ take 2 grid)) : [])

checkWordFits :: String -> Posn -> Posn -> TempWordSearchGrid -> Bool --checks if a word fits this pos and orientation
checkWordFits [] _ _ _ = True --travel back and insert letters?
checkWordFits (letter:word) (x,y) (x2,y2) grid
    | y >= length grid || x >= length grid || x < 0 || y < 0 = False --out of bounds
    | otherwise = case grid!!y!!x of
        Nothing -> checkWordFits word (x+x2,y+y2) (x2,y2) grid
        Just z -> (z == letter) && checkWordFits word (x+x2,y+y2) (x2,y2) grid


--pass in sortOn length words for words so that the larger words get entered first for more efficiency
insertWords :: [ String ] -> TempWordSearchGrid -> StdGen -> TempWordSearchGrid
insertWords [] grid gen = grid --generate starting positions and orientations for these words to be tried at
--insertWords orig@(word:words) grid gen | trace ("insertWords " ++ show word ++ " | " ++ show words) False = undefined
insertWords orig@(word:words) grid gen
    | fst posn2 == 0 && snd posn2 == 0 = insertWords orig grid newGen
    | checkWordFits word posn1 posn2 grid = insertWords words (fillInWord word posn1 posn2 grid) newGen
    | otherwise = insertWords orig grid newGen
    where 
        (posn1, posn2, newGen) = generatePosnOrientation gen (length grid -1)


--generatePosnOrientation'  = observe "generatePosnOrientation: " generatePosnOrientation 
generatePosnOrientation :: StdGen -> Int -> (Posn,Posn,StdGen)
--generatePosnOrientation gen length | trace ("generatePosnOrientation " ++ show (randomR (-1,length-length+1) (snd (randomR (-1,length-length+1) (snd (randomR (0,length) (snd (randomR (0,length) gen)))))))) False = undefined
generatePosnOrientation gen length =
    let (x1, newGen) = randomR (0,length) gen
        (y1, newGen') = randomR (0,length) newGen
        (x2, newGen'') = randomR (-1,1) newGen'
        (y2, newGen''') = randomR (-1,1) newGen''
    in  ((x1,y1),(x2,y2),newGen''') 

fillInGrid :: [Posn] -> StdGen -> String -> TempWordSearchGrid -> TempWordSearchGrid
fillInGrid [] _ _ grid = grid
fillInGrid (posn:listOfPosn) gen chars grid = fillInGrid listOfPosn newGen chars (insertIntoGrid posn (chars!!x) grid)
    where 
        (x, newGen) = randomR (0,length chars - 1) gen

--assuming no shared valid letters, calculate if it works with shared letters
generateEmptyGrid :: Double -> Double -> Double -> TempWordSearchGrid
generateEmptyGrid n chars maxDensity
    | chars < maxDensity * n * n = replicate (round n) $ replicate (round n) Nothing --valid n
    | otherwise = generateEmptyGrid (n+1) chars maxDensity

--- Convenience functions supplied for testing purposes
createAndSolve :: [ String ] -> Double -> IO [ (String, Maybe Placement) ]
createAndSolve words maxDensity =   do g <- createWordSearch words maxDensity
                                       let soln = solveWordSearch words g
                                       printGrid g
                                       return soln

printGrid :: WordSearchGrid -> IO ()
printGrid [] = return ()
printGrid (w:ws) = do putStrLn w
                      printGrid ws



-- Challenge 3 --

prettyPrint :: LamMacroExpr -> String
prettyPrint _ = ""

-- examples in the instructions
ex3'1 = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))
ex3'2 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))
ex3'3 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))
ex3'4 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2))) 


-- Challenge 4 --

parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro _ = Nothing 


-- Challenge 5

cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform _ = LamDef [] (LamVar 0)

-- Examples in the instructions
exId =  LamAbs 1 (LamVar 1)
ex5'1 = (LamApp (LamVar 1) (LamVar 2))
ex5'2 = (LamDef [ ("F", exId) ] (LamVar 2) )
ex5'3 = (LamDef [ ("F", exId) ] (LamMacro "F") )
ex5'4 = (LamDef [ ("F", exId) ] (LamApp (LamMacro "F") (LamMacro "F")))


-- Challenge 6

innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 _ = Nothing

outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 _ = Nothing

compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter _ _ = (Nothing,Nothing,Nothing,Nothing) 

-- Examples in the instructions

-- (\x1 -> x1 x2)
ex6'1 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))

--  def F = \x1 -> x1 in F  
ex6'2 = LamDef [ ("F",exId) ] (LamMacro "F")

--  (\x1 -> x1) (\x2 -> x2)   
ex6'3 = LamDef [] ( LamApp exId (LamAbs 2 (LamVar 2)))

--  (\x1 -> x1 x1)(\x1 -> x1 x1)  
wExp = (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))
ex6'4 = LamDef [] (LamApp wExp wExp)

--  def ID = \x1 -> x1 in def FST = (\x1 -> λx2 -> x1) in FST x3 (ID x4) 
ex6'5 = LamDef [ ("ID",exId) , ("FST",LamAbs 1 (LamAbs 2 (LamVar 1))) ] ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (LamMacro "ID") (LamVar 4)))

--  def FST = (\x1 -> λx2 -> x1) in FST x3 ((\x1 ->x1) x4))   
ex6'6 = LamDef [ ("FST", LamAbs 1 (LamAbs 2 (LamVar 1)) ) ]  ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (exId) (LamVar 4)))

-- def ID = \x1 -> x1 in def SND = (\x1 -> λx2 -> x2) in SND ((\x1 -> x1) (\x2 -> x2)) ID
ex6'7 = LamDef [ ("ID",exId) , ("SND",LamAbs 1 (LamAbs 2 (LamVar 2))) ]  (LamApp (LamApp (LamMacro "SND") (LamApp wExp wExp) ) (LamMacro "ID") ) 

