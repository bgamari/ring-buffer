module Util.RingBuffer ( RingBuffer
                       , new
                       , clear
                       , append
                       , concat
                       , capacity
                       , length
                       , withItems
                       ) where

import Prelude hiding (length, concat)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Primitive

-- | A concurrent ring buffer
data RingBuffer v a
    = RingBuffer { ringBuffer :: (VG.Mutable v) (PrimState IO) a
                 , ringState  :: MVar RingState
                 }

data RingState = RingState { ringFull :: !Bool, ringHead :: !Int }

-- | We use the @Mutable@ vector type to ensure injectiveness
type RingM vm a = StateT RingState (ReaderT (vm (PrimState IO) a) IO)

-- | Atomically perform an action with the ring
withRing :: VG.Vector v a
         => RingBuffer v a
         -> RingM (VG.Mutable v) a r
         -> IO r
withRing rb action = do
    s <- takeMVar (ringState rb)
    (r, s') <- runReaderT (runStateT action s) (ringBuffer rb)
    putMVar (ringState rb) s'
    return r

advance :: (VGM.MVector v a) => Int -> RingM v a ()
advance n = do
    RingState full pos <- get
    cap <- capacity'
    let (a, pos') = (pos + n) `divMod` cap
    put $ RingState (full || a > 0) pos'

-- | Create a new ring of a given length
new :: (VG.Vector v a) => Int -> IO (RingBuffer v a)
new n = do
    buffer <- VGM.new n
    state <- newMVar $ RingState False 0
    return $ RingBuffer { ringBuffer=buffer, ringState=state }

-- | Reset the ringbuffer to its empty state
clear :: VG.Vector v a => RingBuffer v a -> IO ()
clear rb = withRing rb $ put $ RingState False 0

-- | Add an item to the end of the ring
append :: (VG.Vector v a) => a -> RingBuffer v a -> IO ()
append x rb = withRing rb $ do
    s <- get
    liftIO $ VGM.unsafeWrite (ringBuffer rb) (ringHead s) x
    advance 1

-- | Add multiple items to the end of the ring
-- This ignores any items above the length of the ring
concat :: (VG.Vector v a) => v a -> RingBuffer v a -> IO ()
concat xs rb = withRing rb $ do
    cap <- capacity'
    let takeN = min (VG.length xs) cap
    xs' <- liftIO $ VG.unsafeThaw $ VG.drop (VG.length xs - takeN) xs
    pos <- gets ringHead

    let untilWrap = cap - pos
        src  = VGM.take untilWrap xs'
        dest = VGM.take (min takeN untilWrap) $ VGM.drop pos $ ringBuffer rb
    liftIO $ VGM.copy dest src

    -- did we wrap around?
    when (takeN > untilWrap) $ do
        let src'  = VGM.drop untilWrap xs'
            dest' = VGM.take (takeN - untilWrap) $ ringBuffer rb
        liftIO $ VGM.copy dest' src'
    advance takeN

-- | The maximum number of items the ring can contain
capacity :: (VG.Vector v a) => RingBuffer v a -> Int
capacity rb = VGM.length (ringBuffer rb)

-- | The maximum number of items the ring can contain
capacity' :: (VGM.MVector v a) => RingM v a Int
capacity' = asks VGM.length

-- | The current filled length of the ring
length' :: (VGM.MVector v a) => RingM v a Int
length' = do
    RingState full pos <- get
    if full
      then capacity'
      else return pos

-- | The current filled length of the ring
length :: (VG.Vector v a) => RingBuffer v a -> IO Int
length rb = withRing rb length'

-- | Execute the given action with the items of the ring.
-- Note that no references to the vector may leak out of the action as
-- it will later be mutated. Moreover, the items in the vector are in
-- no particular order.
withItems :: VG.Vector v a => RingBuffer v a -> (v a -> IO b) -> IO b
withItems rb action = withRing rb $ do
    frozen <- liftIO $ VG.unsafeFreeze (ringBuffer rb)
    n <- length'
    liftIO $ action (VG.take n frozen)
