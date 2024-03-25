import Lib (Player (O, X), emptyBoard, isWinner, placeMark)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "emptyBoard" $ do
    it "creates a 3x3 grid filled with Nothing" $ do
      let board = emptyBoard
      length board `shouldBe` 3
      all (== 3) (map length board) `shouldBe` True
      all (all (== Nothing)) board `shouldBe` True

  describe "placeMark" $ do
    it "places a player's mark on the board at a given position" $ do
      let board = placeMark emptyBoard (1, 1) X
      board !! 1 !! 1 `shouldBe` Just X

  describe "isWinner" $ do
    it "returns true if a player has won the game" $ do
      let board = replicate 3 [Just X, Nothing, Nothing]
      isWinner X board `shouldBe` True
    it "returns false if a player has not won the game" $ do
      let board = replicate 3 [Just X, Nothing, Nothing]
      isWinner O board `shouldBe` False