module Spec where
import PdePreludat
import Library
import Test.Hspec


correrTests :: IO ()
correrTests = hspec $ do
  describe "test automatizados para la funcion queLeEnvio" $ do
    it "Sólo se queda con los datos de los paquetes correspondientes" $ do
      queLeEnvio "192.168.1.102" "192.168.1.103" [paquete1, paquete2] `shouldBe` ["cómo"]
    it "Ordena los datos de acuerdo al número del paquerte que pertenezcan" $ do
      queLeEnvio "192.168.1.102" "192.168.1.103" [paquete1, paquete4] `shouldBe` ["Hola che", "cómo"]
