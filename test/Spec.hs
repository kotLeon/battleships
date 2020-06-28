import Board
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "creates board of correct size" $ do
        it "size of board is 10x10" $
            length (fields createBoard) `shouldBe` 100
    describe "correctly changes value of field" $ do
        it "gives Ship to Empty field" $
            value (changeValue (Field (1,1) Empty) Ship) `shouldBe` Ship
    describe "correct field insertion" $ do
        it "puts ship on field 1x1 (value)" $ -- correct, couse ship is inserted in the beginning of list
            value (fields (putShip createBoard (1,1) 1 Rght) !! 0) `shouldBe` Ship
        it "the new ship has correct coords" $
            coords (fields (putShip createBoard (1,1) 1 Rght) !! 0) `shouldBe` (1,1)
        
    -- describe "correctly read direction" $ do
        -- it "reads 2 as Rght with monad" $
            -- Left (readDirection 2) `shouldBe` True
                    
