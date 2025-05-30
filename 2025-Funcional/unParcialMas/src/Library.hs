{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use repeat" #-}
module Library where
import PdePreludat
import GHC.Conc (labelThread)

doble :: Number -> Number
doble numero = numero + numero

--1)

type PowerUp = Personaje -> Personaje

data Personaje = UnPersonaje{
    nombre :: String,
    inteligencia :: Number,
    vagancia :: Number,
    powerUps :: [PowerUp],
    titulos :: [String]
}deriving(Show,Eq)

mordecai :: Personaje
mordecai = UnPersonaje "Mordecai" 90 60 [videoJuegos,taeKwonMortal "Bloqueo",picante] ["Extrahuevordinario","Golpe mortal"]

rigby :: Personaje
rigby = UnPersonaje "Rigby" 65 101 [cafeCafe 200, taeKwonMortal "Golpe"] []

--2a)

fiestaLosMartes :: PowerUp
fiestaLosMartes = nombreBullicio . sumarVagancia (-10)

--sumarVagancia es una funcion que sirve para sumarle un numero a la vagancia de un personaje sin repetir logica

sumarVagancia :: Number -> Personaje -> Personaje
sumarVagancia numero personaje = personaje{vagancia = max 0 (vagancia personaje + numero)}

nombreBullicio :: Personaje -> Personaje
nombreBullicio personaje = personaje{nombre = nombre personaje ++ " Bullicio"}

--2b)

--agregar titulo sirve para agregar titulos sin repetir logica

agregarTitulo :: String -> Personaje -> Personaje
agregarTitulo titulo personaje
    |not (yaLoTiene titulo personaje) = personaje{titulos = titulo : titulos personaje}
    |otherwise = personaje

yaLoTiene :: String -> Personaje -> Bool
yaLoTiene titulo personaje = titulo `elem` titulos personaje

movimientoMortal :: String -> String
movimientoMortal = (++" mortal")

taeKwonMortal :: String -> PowerUp
taeKwonMortal = agregarTitulo . movimientoMortal

--2c)

menteMax :: PowerUp
menteMax = incrementarPorcentualmenteInteligencia 10 . agregarTitulo "Moriones"

--incrementarPorcuentualmenteInteligencia es una funcion para agregarle un porcentaje de inteligencia al personaje sin repetir logica

incrementarPorcentualmenteInteligencia :: Number -> Personaje -> Personaje
incrementarPorcentualmenteInteligencia numero personaje = personaje{inteligencia = inteligencia personaje + inteligencia personaje * (numero/100)}

--2d)

videoJuegos :: PowerUp
videoJuegos = sumarVagancia 35 . incrementarPorcentualmenteInteligencia 5 . agregarTitulo "Maestro de Los videojuegos"

--2e)

cafeCafe :: Number -> PowerUp
cafeCafe numero = incrementarPorcentualmenteInteligencia (numero/200) . sumarVagancia 100

--2f)

picante :: PowerUp
picante = id

--3)

type Mision = Personaje -> Bool

requisitoBasico :: Personaje -> Bool
requisitoBasico = (>=1) . length . powerUps

puedeRealizarMision :: Mision -> Personaje -> Bool
puedeRealizarMision mision personaje = requisitoBasico personaje && mision personaje

--3a)

desafioExtrahuevordinario :: Mision
desafioExtrahuevordinario = yaLoTiene "Extrahuevordianario"


--3b)

darCarinio :: Mision
darCarinio UnPersonaje{nombre = "Rigby"} = False
darCarinio _ = True

--3c)

beberMissisipiQueen :: Mision
beberMissisipiQueen personaje = noEsVago personaje && personajeMissisipi personaje

noEsVago :: Personaje -> Bool
noEsVago = (<70) . vagancia

personajeMissisipi :: Personaje -> Bool
personajeMissisipi personaje = nombre personaje `elem` ["Mordecai","Benson","Rigby"]

--3d)

comerSandwichDeLaMuerte :: Mision
comerSandwichDeLaMuerte personaje = any esTituloMortal (titulos personaje)

esTituloMortal :: String -> Bool
esTituloMortal = (=="latrom") . take 6 . reverse

--5)

esGrupoRegular :: [Personaje] -> Mision -> Bool
esGrupoRegular personajes mision =  puedenHacerlaMasDeTres mision personajes || estaPapaleta personajes

estaPapaleta :: [Personaje] -> Bool
estaPapaleta = any esPapaleta

esPapaleta :: Personaje -> Bool
esPapaleta = (=="Papaleta") . nombre

puedenHacerlaMasDeTres :: Mision -> [Personaje] -> Bool
puedenHacerlaMasDeTres mision = (>3) . length .  filter (puedeRealizarMision mision)

--6)

versionSuprema :: Personaje -> Personaje
versionSuprema personaje = foldl (\alguien suPowerUp -> suPowerUp alguien) personaje (powerUps personaje)

--7)

--7a)

cualesPuedeCompletar :: [Mision] -> Personaje -> [Mision]
cualesPuedeCompletar [] _ = []
cualesPuedeCompletar (x:xs) personaje
    |x personaje = x : cualesPuedeCompletar xs personaje
    |otherwise = []

cuantasPuedeCompletar :: [Mision] -> Personaje -> Number
cuantasPuedeCompletar mision = length . cualesPuedeCompletar mision

--7b)

--En el caso de que haya una misión que no pueda realizar, gracias a lazy evaluation, haskell evaluara hasta esa mision y podra realizar
--El calculo, en caso de que el personaje cumpla infinitamente con las misiones, no podra arrojar el resultado esperado.


-- En el caso de que rigby intente, podra darnos un resultado = 0 pues ya la primera vez que intente realizar dar carinio, se corta la
-- recursion de la misión cualesPuedeCompletar (test en specs)

misionesInfinitas :: [Mision]
misionesInfinitas = repeat darCarinio

-- en el caso de que mordecai intente saber cuantas Puede completar de misionesInfinitas2, se quedara infinitamente realizandolas ya que
-- puede realizar esas dos misiones una infinidad de veces.

misionesInfinitas2 :: [Mision]
misionesInfinitas2 = cycle [comerSandwichDeLaMuerte,darCarinio]
