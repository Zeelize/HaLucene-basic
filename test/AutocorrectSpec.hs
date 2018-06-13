module AutocorrectSpec (spec) where

import Test.Hspec    

import Autocorrect
    
spec :: Spec
spec = do
    describe "Mistakes of the first order" $ do
        it "with delete edit" $ do
            spell "hose" `shouldBe` "house"
            spell "corect" `shouldBe` "correct"
            spell "peson" `shouldBe` "person"
            spell "economc" `shouldBe` "economic"
            spell "stuborn" `shouldBe` "stubborn"
        it "with transpose edit" $ do
            spell "huose" `shouldBe` "house"
            spell "corretc" `shouldBe` "correct"
            spell "pesron" `shouldBe` "person"
            spell "eocnomic" `shouldBe` "economic"
            spell "stubbron" `shouldBe` "stubborn"
        it "with replace edit" $ do
            spell "houst" `shouldBe` "house"
            spell "cprrect" `shouldBe` "correct"
            spell "perdon" `shouldBe` "person"
            spell "economoc" `shouldBe` "economic"
            spell "stuvborn" `shouldBe` "stubborn"
        it "with insert edit" $ do
            spell "houyse" `shouldBe` "house"
            spell "correcrt" `shouldBe` "correct"
            spell "persone" `shouldBe` "person"
            spell "erconomic" `shouldBe` "economic"
            spell "sztubbborn" `shouldBe` "stubborn"
    describe "Mistakes of the second order" $ do
        it "general combination of two mistakes" $ do
            spell "bycicle" `shouldBe` "bicycle"
            spell "photograf" `shouldBe` "photograph"
            spell "petsonlity" `shouldBe` "personality"
            spell "paaperwokr" `shouldBe` "paperwork"
            spell "permissont" `shouldBe` "permission"
            spell "replacamen" `shouldBe` "replacement"
    describe "Correction of queries" $ do
        it "combination of mistakes and no mistakes in user queries" $ do
            spell "whitr fridfe" `shouldBe` "white fridge"
            spell "yelow car" `shouldBe` "yellow car"
            spell "smal telefone" `shouldBe` "small telephone"
            spell "electric kay" `shouldBe` "electric key"
            spell "haskell autocorect" `shouldBe` "haskell autocorect"
            spell "peper documens storega" `shouldBe` "paper document stores"
            
