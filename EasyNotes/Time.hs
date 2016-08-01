module Time where

import Data.Time.Clock
import Data.IORef

-- | computes the difference between the current time and the specified time
computePassedTime :: IORef UTCTime -> IO NominalDiffTime
computePassedTime    startTimeRef  = do
    time <- getCurrentTime
    startTime <- readIORef startTimeRef
    return (diffUTCTime time startTime)
