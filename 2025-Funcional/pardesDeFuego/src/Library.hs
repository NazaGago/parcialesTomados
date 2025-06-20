module Library where
import PdePreludat
import Data.List (isInfixOf, sortOn)

estaIncluidaEn :: String -> String -> Bool
estaIncluidaEn = isInfixOf

ordenarSegun :: Ord b => (a -> b) -> [a] -> [a]
ordenarSegun = sortOn


--1)


type Origen = String
type Destino = String

data Paquete = UnPaquete{
    ipOrigen :: Origen,
    ipDestino :: Destino,
    numero :: Number,
    datos :: String
}deriving(Show,Eq)

type Red = [Paquete]

paquete1 :: Paquete
paquete1 = UnPaquete "192.168.1.102" "192.168.1.103" 5 "cómo"

paquete2 :: Paquete
paquete2 = UnPaquete "10.1.1.55" "192.168.1.102" 679 "<h1> Proximo"

paquete3 :: Paquete
paquete3 = UnPaquete "10.1.1.55" "192.168.1.102" 676 "<br/> genial "

paquete4 :: Paquete
paquete4 = UnPaquete "192.168.1.102" "192.168.1.103" 4 "Hola che"

paquete5 :: Paquete
paquete5 = UnPaquete "192.168.1.102" "192.168.1.103" 6 "estás?"

paquete6 :: Paquete
paquete6 = UnPaquete "10.1.1.55" "10.1.1.56" 7 "gbmtp"

paquete7 :: Paquete
paquete7 = UnPaquete "10.1.1.55" "10.1.1.56" 7 "gbmtp"


--2)


type Regla = Paquete -> Bool

data Firewall = UnFirewall{
    mascara :: String,
    reglas :: [Regla]
}deriving(Show,Eq)

--a)

positivos :: Regla
positivos = (>0) . numero

--b)

listaNegra :: [Origen] -> Regla
listaNegra listaNegra paquete = ipOrigen paquete `notElem` listaNegra

--c)

subMascara :: String -> Regla
subMascara subMascara paquete = estaIncluidaEn subMascara (ipOrigen paquete)

--d)

palabraClave :: String -> Regla
palabraClave palabra =  not . estaIncluidaEn palabra . datos

-- funcion dejaPasar

cumpleRegla :: Paquete -> Regla -> Bool
cumpleRegla paquete regla = regla paquete

dejaPasar :: Paquete -> Firewall -> Bool
dejaPasar paquete = all (cumpleRegla paquete) . reglas


--3)


fireWall1 :: Firewall
fireWall1 = UnFirewall "10.1" [palabraClave "apuesta",palabraClave "xxx"]

fireWall2 :: Firewall
fireWall2 = UnFirewall "192" [subMascara "192.168.1",palabraClave "apuesta",listaNegra ["192.168.1.104","10.1.1.55"]]


--4)


--a)

queLeEnvio :: Origen -> Destino -> Red -> [String]
queLeEnvio origen destino = map datos . paquetesEsperados origen destino

origenYDestinoDado :: Origen -> Destino -> Paquete -> Bool
origenYDestinoDado origen destino paquete = ipOrigen paquete == origen && ipDestino paquete == destino

paquetesEsperados :: Origen -> Destino -> Red -> Red
paquetesEsperados origen destino = ordenarSegun numero . filter (origenYDestinoDado origen destino)

--c)

sonConsecutivos :: Paquete -> Paquete -> Bool
sonConsecutivos paquete1 paquete2 = numero paquete1 + 1 == numero paquete2 

listaConsecutiva :: [Paquete] -> Bool
listaConsecutiva [] = True
listaConsecutiva (x:xs) = sonConsecutivos (head xs) x && listaConsecutiva xs 

estaCompleta :: Origen -> Destino -> Red -> Bool
estaCompleta origen destino = listaConsecutiva . paquetesEsperados origen destino

--d)

mensaje :: Origen -> Destino -> Red -> String
mensaje origen destino = separarDatos . queLeEnvio origen destino

separarDatos :: [String] -> String
separarDatos [] = ""
separarDatos [x] = x
separarDatos (x:xs) = x ++ " " ++ separarDatos xs

--e)

mensajeSeguro :: Firewall -> Origen -> Destino -> Red -> String
mensajeSeguro firewall origen destino red
    |all (`dejaPasar` firewall) red = mensaje origen destino red
    |otherwise = "Mensaje Incompleto" 