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

--incrementarPorcentualmenteInteligencia es una funcion para agregarle un porcentaje de inteligencia al personaje sin repetir logica

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

{-
Haskell utiliza lazy evaluation o evaluación perezosa, este tipo de evaluación implica que los parametros que reciba una funcion seran
evaluados lo más tarde posible, es decir, no se evaluan que no sean necesarios. Por eso Haskell, pese a recibir una lista infinita como
parametro, haskell va evaluando uno por uno los elementos. Esto permite que en el caso de que no se necesite evaluar la lista completa, 
de una lista infinita se pueda llegar a un resultado valido. Teniendo en cuenta esto:

En el caso de que se encuentre con una misión que no pueda realizar, se detendra la recursión y creara la lista de misiones posibles 
hasta ese momento, luego calcularle el length al ser una lista finita no sera un problema. Por otro lado, si el personaje puede
completar una infinidad de veces la misión, no podra realizar el calculo ya que no podría terminar nunca de evaluar la lista infinita
en cualesPuedeCompletar y si esa función no devuelve una lista finita, sera imposible calcularle la longitud.

En el caso de que rigby intente misionesInifintas, podra darnos un resultado = 0 pues ya la primera vez que intente realizar 
darCarinio no podra resolverla y se cortara la recursion de la función cualesPuedeCompletar (test en specs) 

En el caso de que mordecai intente saber cuantas Puede completar de misionesInfinitas2, no podra salir de la recursión de la funcion
cualesPuedeCompletar ya que podría completarlas infinitamente. Entonces, la función nunca terminaría de realizarse haciendo imposible
que tire un resultado para calcularle la longitud.
-}

misionesInfinitas :: [Mision]
misionesInfinitas = repeat darCarinio

misionesInfinitas2 :: [Mision]
misionesInfinitas2 = cycle [comerSandwichDeLaMuerte,darCarinio]
