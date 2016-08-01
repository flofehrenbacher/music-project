module Time where

import Data.Time.Clock
import Data.IORef

computePassedTime :: IORef UTCTime -> IO NominalDiffTime
computePassedTime    startTimeRef  = do
    time <- getCurrentTime
    startTime <- readIORef startTimeRef
    return (diffUTCTime time startTime)