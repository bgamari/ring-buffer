{-# LANGUAGE FlexibleContexts #-}

import Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import Data.RingBuffer as RB
import Test.HUnit

main :: IO ()
main = do
    runTestTT $ test [testConcat]
    return ()

testConcat :: Test
testConcat = TestCase $ do
    rb <- RB.new 40 :: IO (RB.RingBuffer V.Vector Int)
    --mapM_ (flip RB.append rb) [0..44]
    RB.concat (V.fromList [0..4]) rb
    RB.concat (V.fromList [0..50]) rb

    checkLength rb 40
    checkItems rb [50,49..11]
    withItems rb $ \items -> do
        let expected = [11..50]
        assertEqual "withItems: expected" expected (sort $ V.toList items)

checkLength :: VG.Vector v a => RingBuffer v a -> Int -> Assertion
checkLength rb expected = do
    len <- RB.length rb
    assertEqual ("Expected length "++show expected) expected len

checkItems :: (Eq a, Show a, VG.Vector v a)
           => RingBuffer v a -> [a] -> Assertion
checkItems rb expected = do
    items <- RB.toList rb
    assertEqual ("Expected items "++show expected) expected items
