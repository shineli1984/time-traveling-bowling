module LibSpec(spec) where

import Lib (emptyGame, addFrame, addLastFrame, Scores(..), Frame(..), LastFrame(..))

import Test.Hspec

spec :: Spec
spec = do
    let game = emptyGame
    describe "addFrame" $ do
        let Just (gameF1, Scores scores1) = addFrame Strike game
        let Just (gameF2, Scores scores2) = addFrame (Spare 9) gameF1
        let Just (gameF3, Scores scores3) = addFrame Strike gameF2
        let Just (gameF4, Scores scores4) = addFrame Strike gameF3
        let Just (gameF5, Scores scores5) = addFrame Strike gameF4
        let Just (gameF6, Scores scores6) = addFrame (Frame 8 1) gameF5
        let Just (gameF7, Scores scores7) = addFrame (Spare 7) gameF6
        let Just (gameF8, Scores scores8) = addFrame Strike gameF7
        let Just (gameF9, Scores scores9) = addFrame Strike gameF8
        let Just (gameF10, Scores scores10) = addLastFrame (LastStrike 10 10) gameF9
        it "adds a Strike" $ do
            scores1 `shouldBe` [0]
        it "then adds a Spare 9" $ do
            scores2 `shouldBe` [20, 0]
        it "then adds a Strike" $ do
            scores3 `shouldBe` [40, 20, 0]
        it "then adds a Strike" $ do
            scores4 `shouldBe` [40, 20, 0]
        it "then adds a Strike" $ do
            scores5 `shouldBe` [70, 40, 20, 0]
        it "then adds a Frame 8 1" $ do
            scores6 `shouldBe` [126, 117, 98, 70, 40, 20, 0]
        it "then adds a Spare 7" $ do
            scores7 `shouldBe` [126, 117, 98, 70, 40, 20, 0]
        it "then adds a Strike" $ do
            scores8 `shouldBe` [146, 126, 117, 98, 70, 40, 20, 0]
        it "then adds a Strike" $ do
            scores9 `shouldBe` [146, 126, 117, 98, 70, 40, 20, 0]
        it "then adds a LastStrike 10 10" $ do
            scores10 `shouldBe` [236, 206, 176, 146, 126, 117, 98, 70, 40, 20, 0]
           