module Dan.TestCombinators where


import Data.Conduit
import Data.Conduit.Combinators (sinkList)
import qualified Data.Conduit.Combinators as CC
import qualified Dan.Combinators as DC
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Test.Hspec

tests = hspec $ do
    describe "Producers-pure" $ do
        it "yieldMany" $ do
            let lst = [3,2,5]
            let go = DC.yieldMany lst .| sinkList
            runConduitPure go `shouldBe` lst

        it "unfold from list" $ do
            let lst = [6,7,2,3]
            let g seed = case seed of 
                            [] -> Nothing 
                            x : xs -> Just (x, xs)
            let go = DC.unfold g lst .| sinkList
            runConduitPure go `shouldBe` lst

        it "unfold range" $ do
            let (start, end) = (3,7)
            let g seed = if seed <= end then Just (seed, seed+1) else Nothing
            let go = DC.unfold g start .| sinkList
            runConduitPure go `shouldBe` [3..7]          

        it "enumFromTo" $ do
            runConduitPure (DC.enumFromTo 'm' 'q' .| sinkList) `shouldBe` "mnopq"

        it "iterate" $ do
            let m = 14
            runConduitPure (DC.iterate (+1) 1 .| CC.take m .| sinkList) `shouldBe` [1..m]

        it "repeat" $ do
            runConduitPure (DC.repeat 45 .| CC.take 4 .| sinkList) `shouldBe` [45, 45, 45, 45]

        it "replicate" $ do
            runConduitPure (DC.replicate 5 3 .| sinkList) `shouldBe` [3, 3, 3, 3, 3]

    describe "Producers - monadic" $ do
        it "runReader" $ do
            runReader ask 3 `shouldBe` 3
        it "runState" $ do
            let 
                next = do
                    s <- get
                    put (s+1)
                    return s
            let pipe = do
                    next
                    next
                    next
            runState pipe 0 `shouldBe` (2, 3) 
        it "repeatM in reader" $ do
            let pipe = DC.repeatM ask .| CC.take 3 .| CC.sum
            runReader (runConduit pipe) 6 `shouldBe` 18
        it "repeatM in state" $ do
            let
                next = do
                    s <- get
                    put (s+1)
                    return s
            let
                pipe = DC.repeatM next .| CC.take 4 .| sinkList
            runState (runConduit pipe) 3 `shouldBe` ([3,4,5,6], 7)

        it "repeatWhileM in state" $ do
            let
                next = do
                    s <- get
                    put (s+1)
                    return s
            let
                pipe = DC.repeatWhileM next (<5) .| sinkList
            runState (runConduit pipe) 2 `shouldBe` ([2..4], 6)

        it "replicateM in reader" $ do
            let
                pipe = DC.replicateM 3 ask .| sinkList
            runReader (runConduit pipe) 7 `shouldBe` [7, 7, 7]