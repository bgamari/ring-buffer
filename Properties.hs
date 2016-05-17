import Control.Monad.IO.Class
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.RingBuffer as R
import qualified Data.Vector as V

testAppend :: (Eq a)
           => Positive Int -> [a] -> Property
testAppend (Positive cap) xs = monadicIO $ do
    r <- liftIO $ R.new cap :: PropertyM IO (R.RingBuffer V.Vector a)
    liftIO $ mapM_ (`R.append` r) (reverse xs)
    xs' <- liftIO $ R.toList r
    return $ xs' == take cap xs

main :: IO ()
main = quickCheck (testAppend :: Positive Int -> [Int] -> Property)
