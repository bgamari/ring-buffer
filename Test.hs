import qualified Data.Vector as V
import Data.RingBuffer as RB

main = do
    rb <- RB.new 40 :: IO (RB.RingBuffer V.Vector Int)
    --mapM_ (flip RB.append rb) [0..44]
    RB.concat (V.fromList [0..4]) rb
    RB.concat (V.fromList [0..50]) rb
    RB.length rb >>= print
    RB.withItems rb print
