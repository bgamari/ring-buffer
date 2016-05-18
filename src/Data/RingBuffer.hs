-- | This is a thread-safe implementation of a mutable ring-buffer
-- built upon @vector@.

module Data.RingBuffer ( RingBuffer
                       , new
                       , clear
                       , append
                       , concat
                       , capacity
                       , length
                       , latest
                       , toList
                       , withItems
                       ) where

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Control.Applicative
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Primitive
import Prelude hiding (length, concat)

-- | A concurrent ring buffer.
data RingBuffer v a
    = RingBuffer { ringBuffer :: (VG.Mutable v) (PrimState IO) a
                 , ringState  :: MVar RingState
                 }

data RingState = RingState { ringFull :: !Bool, ringHead :: !Int }

-- | We use the @Mutable@ vector type to ensure injectiveness
type RingM m vm a = StateT RingState (ReaderT (vm (PrimState IO) a) m)

-- | Atomically perform an action with the ring
withRing :: (VG.Vector v a, MonadIO m)
         => RingBuffer v a
         -> RingM m (VG.Mutable v) a r
         -> m r
withRing rb action = do
    s <- liftIO $ takeMVar (ringState rb)
    (r, s') <- runReaderT (runStateT action s) (ringBuffer rb)
    liftIO $ putMVar (ringState rb) s'
    return r

advance :: (VGM.MVector v a, MonadIO m) => Int -> RingM m v a ()
advance n = do
    RingState full pos <- get
    cap <- capacity'
    let (a, pos') = (pos + n) `divMod` cap
    put $ RingState (full || a > 0) pos'

-- | Create a new ring of a given length
--
-- /Note:/ size must be non-zero
new :: (VG.Vector v a) => Int -> IO (RingBuffer v a)
new n = do
    when (n < 1) $ fail "Data.RingBuffer.new: invalid ring size"
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
capacity' :: (VGM.MVector v a, MonadIO m) => RingM m v a Int
capacity' = asks VGM.length

-- | The current filled length of the ring
length' :: (VGM.MVector v a, MonadIO m) => RingM m v a Int
length' = do
    RingState full pos <- get
    if full
      then capacity'
      else return pos

-- | The current filled length of the ring
length :: (VG.Vector v a) => RingBuffer v a -> IO Int
length rb = withRing rb length'

-- | Retrieve the $n$th most-recently added item of the ring
latest :: (VG.Vector v a) => RingBuffer v a -> Int -> IO (Maybe a)
latest rb n = withRing rb $ do
    len <- length'
    if n >= len
      then return Nothing
      else Just <$> latest' n

latest' :: (VGM.MVector v a, MonadIO m) => Int -> RingM m v a a
latest' n = do
    len <- length'
    cap <- capacity'
    when (n >= len) $ error "Data.RingBuffer.latest': invalid index"
    let idx = (cap + len - n - 1) `mod` cap
    buf <- ask
    liftIO $ VGM.unsafeRead buf idx

-- | Get the entire contents of the ring, with the most recently added element
-- at the head. Note that this is rather inefficient.
toList :: (VG.Vector v a) => RingBuffer v a -> IO [a]
toList rb = withRing rb $ do
    len <- length'
    mapM latest' [0..len-1]

-- | Execute the given action with the items of the ring.
-- Note that no references to the vector may leak out of the action as
-- it will later be mutated. Moreover, the items in the vector are in
-- no particular order.
withItems :: (MonadIO m, VG.Vector v a) => RingBuffer v a -> (v a -> m b) -> m b
withItems rb action = withRing rb $ do
    frozen <- liftIO $ VG.unsafeFreeze (ringBuffer rb)
    n <- length'
    lift $ lift $ action (VG.take n frozen)
