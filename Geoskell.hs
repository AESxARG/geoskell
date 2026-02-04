module Main where

import Control.Concurrent (threadDelay)
import System.IO          (hFlush, stdout)
import Text.Printf        (printf)

data Agent = Alpha | Beta deriving (Show, Eq)

data GameState = GameState 
    { turn        :: Int 
    , instigator  :: Agent 
    , alphaVec    :: (Float, Float)
    , betaVec     :: (Float, Float)
    , syncHistory :: [Float]
    , peakMotif   :: Float
    }

isMengerish :: Int -> Float -> Float -> Float -> Float -> Bool
isMengerish 0 _ _ _ _ = True
isMengerish d x y z t
    | countMiddles > 1 = False
    | otherwise        = isMengerish (d-1) (nx x) (nx y) (nx z) t
  where
    isMid v      = floor (v * 3 + sin t * 0.05) == 1
    countMiddles = length $ filter isMid [x,y,z]
    nx v         = (v * 3) - fromIntegral (floor (v * 3))

sharedMengerish :: GameState -> Float -> Float -> Float -> String
sharedMengerish st x y z = 
    let (ap, az) = alphaVec st
        (bp, bz) = betaVec st
        mAlpha   = isMengerish 2 (x * az) (y * az) z ap
        mBeta    = isMengerish 2 (x * bz) (y * bz) z bp
    in case (mAlpha, mBeta) of
        (True, True) -> "██"
        (True, _)    -> "░░"
        (_, True)    -> "▒▒"
        _            -> "  "

drawSyncWave :: [Float] -> String
drawSyncWave = map toChar . reverse . take 60
  where
    levels = "  ▂▃▄▅▆▇█"
    toChar v = levels !! floor (v * fromIntegral (length levels - 1))

updateState :: GameState -> GameState
updateState st = st 
    { turn        = nTurn
    , instigator  = nInst
    , alphaVec    = nAlpha
    , betaVec     = nBeta
    , syncHistory = take 60 (curMotif : syncHistory st)
    , peakMotif   = max (peakMotif st) curMotif
    }
  where
    nTurn   = turn st + 1
    nInst   | (nTurn `div` 75) `mod` 2 == 0 = Alpha
            | otherwise                     = Beta
    (ap,az) = alphaVec st
    (bp,bz) = betaVec st
    dist    = sqrt ((ap-bp)**2 + (az-bz)**2)
    curMotif = max 0.0 (min 1.0 (1.0 - (dist * 1.5)))
    nAlpha  | nInst == Alpha = (ap + 0.05, 1.0 + sin (ap * 0.5) * 0.2)
            | otherwise      = (ap, az)
    nBeta   | nInst == Beta  = (bp + 0.05, 1.0 + cos (bp * 0.5) * 0.2)
            | otherwise      = (bp, bz)

renderFrame :: GameState -> IO ()
renderFrame st = do
    let xRange = [-1.1, -0.05 .. 1.1]
        yRange = [-0.7, -0.09 .. 0.7]
        res    = if null (syncHistory st) then 0 else head (syncHistory st)
        status = if res > 0.7 then "CONVERGING" else "DIVERGING"
        fractal = unlines [concat [sharedMengerish st x y 0.5 | x <- xRange] | y <- yRange]
    
    putStr "\ESC[2J\ESC[H"
    putStr fractal
    putStrLn "\n--- [ HISTORY ] ---"
    putStrLn (drawSyncWave $ syncHistory st)
    putStrLn "\n--- [ INTERFACE ] ---"
    printf "STATE: %s\nINSTIGATOR: %s\nMOTIF MATCH: %0.2f\n" status (show $ instigator st) res
    printf "ALPHA: [%.2f, %.2f]\nBETA:  [%.2f, %.2f]\n" (fst $ alphaVec st) (snd $ alphaVec st) (fst $ betaVec st) (snd $ betaVec st)
    hFlush stdout

loop :: GameState -> IO ()
loop st = renderFrame st >> threadDelay 33333 >> loop (updateState st)

main :: IO ()
main = loop $ GameState 0 Alpha (0.0, 1.0) (0.5, 0.9) [] 0.0