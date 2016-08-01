module StateRecorder where

import Control.Monad.State.Lazy
import Euterpea

data MyRecorder = MyRecorder { notes :: [Music Pitch] }

recordNote :: Music Pitch -> State MyRecorder ()
recordNote someNote = modify $ MyRecorder . (someNote : ) . notes
    
recordIO :: MyRecorder -> Music Pitch -> IO MyRecorder
recordIO recorder note = return $ snd $ runState (recordNote note) recorder