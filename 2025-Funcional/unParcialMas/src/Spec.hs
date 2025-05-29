module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2

--4)

  describe "Test para verificar el correcto funcionamiento de la mision beberMissisipiQueen" $ do

    it "Un personaje que no tiene más de un powerUp no puede realizar la mision" $ do  
      puedeRealizarMision beberMissisipiQueen mordecai{powerUps = []} `shouldBe` False

    it "Un personaje que es vago no puede realizar la mision" $ do
      puedeRealizarMision beberMissisipiQueen rigby `shouldBe` False

    it "Un personaje que no se tiene un nombre apropiado no puede realizar la mision" $ do
      puedeRealizarMision beberMissisipiQueen (UnPersonaje "Papaleta" 10 10 [taeKwonMortal "Golpe"] []) `shouldBe` False

    it "Un personaje que no es vago, tiene un nombre apropiado y tiene mas de un powerUp puede realizar la mision" $ do
      puedeRealizarMision beberMissisipiQueen mordecai `shouldBe` True  

  describe "Test para verificar que pasaría si le paso una lista infinitas de misiones a un personaje" $ do 
    it "Un personaje que encuentre una mision que no pueda cumplir, contara hasta ese momento nada más" $ do 
      cuantasPuedeCompletar misionesInfinitas rigby `shouldBe` 0  