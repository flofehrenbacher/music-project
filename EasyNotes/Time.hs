-- | This module provides functions concerning the current time
module Time where

import Data.Time.Clock
import Data.IORef
import Graphics.UI.GLUT

-- | computes the difference between the current time and the specified time
computePassedTime :: IORef UTCTime -> IO NominalDiffTime
computePassedTime    startTimeRef  = do
    time <- getCurrentTime
    startTime <- readIORef startTimeRef
    return (diffUTCTime time startTime)

-- | sets the start time to the current time
resetTime :: IORef UTCTime -> IO ()
resetTime    startTimeRef  = do
    curTime <- getCurrentTime
    startTimeRef $= curTime
