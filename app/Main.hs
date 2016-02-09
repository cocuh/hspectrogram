module Main where
import Control.Concurrent.Async
import Control.Concurrent hiding (yield)
import Control.Monad
import System.Posix.Signals
import Pipes
import Pipes.Concurrent as PC
import qualified Pipes.Group as PG
import qualified Pipes.Prelude as P
import Sound.Pulse.Simple
import Data.WAVE
import qualified Data.List.Extra as E
import qualified Numeric.FFT as F
import Data.Complex
import Data.Monoid
import Data.Int
import Data.Maybe
import qualified UI.HSCurses.CursesHelper as CH
import qualified UI.HSCurses.Curses as C

takeStep :: Int -> [a] -> [a]
takeStep _ [] = []
takeStep n (x:xs)
  | n >= 0 = x : takeStep n (drop (n-1) xs)
  | otherwise = takeStep (-n) (reverse xs)

rjust :: Int -> a -> [a] -> [a]
rjust size filler inp
  | length inp >= size = inp
  | otherwise = inp ++ replicate (size - length inp) filler



fft :: Int -> Pipe [WAVESample] [Double] IO()
fft chunkSize = forever $ do
    let step = 7
    let fftNum = 128
    a <- await
    let vec = rjust fftNum 0 $takeStep step $ takeStep 2 a
    let spec = map magnitude $ F.dft(map fromIntegral $ vec)
    let ratio = 3e-9
    yield $ map (\x -> x * ratio) spec


convSpectrumMat :: [Double] -> Int -> [(Int, String)]
convSpectrumMat input offsetY =
    serializer 0
    where
        arr = rjust 1 0 $ map (\x -> floor (x * 2)) input
        height = 30
        serializer y
            | height < y = []
            | otherwise  = [(height - y + offsetY, map (\i -> if i>y then '#' else ' ') arr)]++serializer (y+1)

spectrogram :: Int -> Int -> Consumer [Double] IO ()
spectrogram chunkNo chunkNum = forever $ do
    (height, width) <-lift $ C.scrSize
    a <- await
    let arr = rjust 1 0 a
    let offsetX = 0
    let offsetY = 5
    let mat = convSpectrumMat arr offsetY
    let cprintLn y str = C.mvWAddStr C.stdScr y offsetX str
    lift $ mapM (\(y, s) -> cprintLn y s) mat
    lift $ cprintLn 0 ("     chunkNo:"++(show chunkNo) ++ "/" ++ show chunkNum)
    lift $ cprintLn 1 ("max spectrum:"++(show $ maximum arr ))
    lift $ cprintLn 2 ("num spectrum:"++(show $ length arr ))
    lift $ C.refresh
    spectrogram (chunkNo+1) chunkNum


concat' :: [[a]] -> [a]
concat' xss = foldr (++) [] xss



main = do
    tid <- myThreadId
    let handler = do
            putStrLn "keybord interupted"
            killThread tid
    installHandler keyboardSignal (Catch handler) Nothing

    input <- getWAVEFile "/tmp/no_poi.wav"
    let header = waveHeader input
    let resolution = 2^64 :: Double
    let channels = waveNumChannels header
    let samples = waveSamples input
    let sampleLength = fromMaybe 0 $waveFrames header
    let chunkSize = 2^10
    let chunkNum = (sampleLength `div` chunkSize) + 1

    s<-simpleNew Nothing "ninja" Play Nothing "youjo"
        (SampleSpec (S32 LittleEndian) (waveFrameRate header) 2) Nothing Nothing
        -- WAVESample is 32 bit, so you must set S32 in pulseaudio or casting.

    --runEffect $ each (E.chunksOf 2048 samples) >-> P.map concat' >-> P.mapM_ (simpleWrite s)

    (plotterOutput, plotterInput) <- spawn unbounded
    player <- async $ do
        runEffect $ each (((E.chunksOf (chunkSize)).(map$ map (`div` 3))) samples) >-> P.map concat' >-> P.chain(simpleWrite s) >-> toOutput plotterOutput
        performGC
    plotter <- async $ do
        CH.start
        C.refresh
        runEffect $ fromInput plotterInput >-> fft chunkSize>-> spectrogram 0 chunkNum
        C.endWin
        performGC
    mapM_ wait [player, plotter]

    simpleDrain s
    simpleFree s
