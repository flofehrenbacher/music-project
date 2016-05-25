module StateRecorder where

-- TODOs: import state standardbib
--        
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Euterpea

data State s a = State (s -> (s, a))

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure a = State $ \s -> (s, a)
    (<*>) = ap

instance Monad (State s) where
    (State aState) >>= g = State $ \s ->
        let
            (s', a) = aState s
            State h = g a
        in h s'

askFor :: (s -> a) -> State s a
askFor f = State $ \s -> (s, f s)

modifyWith :: (s -> s) -> State s ()
modifyWith f = State $ \s -> (f s, ())

runState :: State s a -> s -> (s, a)
runState (State f) = f

data MyRecorder = MyRecorder { notes :: [Music Pitch] }

recordNote :: Music Pitch -> State MyRecorder ()
recordNote someNote = modifyWith noteInList where
    noteInList recorder = recorder { notes = someNote : notes recorder } 
    -- modifyWith $ modfunciton (nimmt myrecorder notes)
    -- MyRecorder . ( someNote : ) . notes
    
    
recordIO :: MyRecorder -> Music Pitch -> IO MyRecorder
recordIO recorder note = do
    let (recorder', ()) = runState (recordNote note) recorder
    return recorder'
    -- standardbib
    -- fst <$> runState (recordNote note) recorder