module Library where
import PdePreludat

--1)
data Guantelete = UnGuantelete {
    material :: String
,   gemas :: [Gema]
} deriving Show

data Personaje = UnPersonaje {
    nombre :: String
,    edad :: Number
,   energia :: Number
,   habilidades :: [String]
,   planeta :: String
} deriving Show

data Universo = UnUniverso {
    habitantes :: [Personaje]
} deriving Show

chasquido :: Guantelete -> Universo -> Universo
chasquido guantelete universo 
    |material guantelete == "uru" && length(gemas guantelete) == 6 = universo {habitantes = take (tomarMitad universo) (habitantes universo)}
    |otherwise = universo

tomarMitad :: Universo -> Number
tomarMitad  = floor.(/2).length.habitantes

universo1 = UnUniverso [ironMan, drStrange, groot, wolverine, viudaNegra]

guantelete1 = UnGuantelete "uru" [poder, mente 10, tiempo, loca tiempo, espacio "asgard", alma "groot"]
guantelete2 = UnGuantelete "hierro" [poder, mente 10, tiempo, loca tiempo, espacio "asgard", alma "groot"]
guantelete3 = UnGuantelete "madera" [poder, mente 10, tiempo]

ironMan = UnPersonaje "tony" 50 300 ["volar", "lanzar laceres"] "tierra"
drStrange = UnPersonaje "stephen" 60 1000 ["volar", "hacer magia","ser un facha"] "tierra"
groot = UnPersonaje "groot" 10 700 ["groot"] "espacio"
wolverine = UnPersonaje "logan" 100 800 ["regeneracion"] "tierra"
viudaNegra = UnPersonaje "natasha" 31 100 ["enamorar", "espiar"] "tierra"

--2)
aptoParaPendex :: Universo -> Bool
aptoParaPendex universo = any pendex (habitantes universo)

pendex :: Personaje -> Bool
pendex personaje = edad personaje < 45

energiaTotalUniverso :: Universo -> Number
energiaTotalUniverso universo = sum (map energia (habitantes universo)) 

menosDeXHabilidades :: Number -> Personaje -> Bool
menosDeXHabilidades numero personaje = length (habilidades personaje) < numero


--Segunda Parte) 


data Gema = UnaGema{
    habilidadParticular :: (Personaje -> Personaje)
,    reduccionDeEnergia :: (Personaje -> Personaje)
} deriving Show

aplicarGema :: Gema -> Personaje -> Personaje
aplicarGema gema  = (habilidadParticular gema).(reduccionDeEnergia gema)

modificadorDeEnergia :: Number -> Personaje -> Personaje
modificadorDeEnergia numero personaje  = personaje {energia = (energia personaje) - numero}

--mente)
mente :: Number -> Gema
mente numero = UnaGema habilidadParticularMente (reduccionDeEnergiaMente numero)

habilidadParticularMente :: Personaje -> Personaje
habilidadParticularMente personaje = personaje

reduccionDeEnergiaMente :: Number -> Personaje -> Personaje
reduccionDeEnergiaMente numero personaje = modificadorDeEnergia numero personaje 

--alma)
alma :: String -> Gema
alma habilidad = UnaGema (habilidadParticularAlma habilidad) reduccionDeEnergiaAlma

habilidadParticularAlma :: String -> Personaje -> Personaje
habilidadParticularAlma habilidad personaje = personaje{habilidades = filter (/= habilidad) (habilidades personaje)} 

reduccionDeEnergiaAlma :: Personaje -> Personaje
reduccionDeEnergiaAlma = modificadorDeEnergia 10

--espacio)
espacio :: String -> Gema
espacio nuevoPlaneta = UnaGema (habilidadParticularEspacio nuevoPlaneta) reduccionDeEnergiaEspacio

habilidadParticularEspacio :: String -> Personaje -> Personaje
habilidadParticularEspacio nuevoPlaneta personaje = personaje {planeta = nuevoPlaneta}

reduccionDeEnergiaEspacio :: Personaje -> Personaje
reduccionDeEnergiaEspacio = modificadorDeEnergia 20

--poder)
poder :: Gema
poder = UnaGema habilidadParticularPoder reduccionDeEnergiaPoder

habilidadParticularPoder :: Personaje -> Personaje
habilidadParticularPoder personaje
    | menosDeXHabilidades 2 personaje == True = personaje {habilidades = []}
    | otherwise = personaje

reduccionDeEnergiaPoder :: Personaje -> Personaje
reduccionDeEnergiaPoder personaje = modificadorDeEnergia (energia personaje) personaje


--tiempo)
tiempo :: Gema
tiempo = UnaGema habilidadParticularTiempo reduccionDeEnergiaTiempo

habilidadParticularTiempo :: Personaje -> Personaje
habilidadParticularTiempo personaje = personaje {edad = max (dividirEdad personaje) 18}

dividirEdad :: Personaje -> Number
dividirEdad = floor.(/2).edad

reduccionDeEnergiaTiempo :: Personaje -> Personaje
reduccionDeEnergiaTiempo = modificadorDeEnergia 50


--loca)
loca :: Gema -> Gema
loca gema = UnaGema (habilidadParticularLoca gema) (reduccionDeEnergiaLoca gema)

habilidadParticularLoca :: Gema -> Personaje -> Personaje
habilidadParticularLoca gema personaje = habilidadParticular gema (habilidadParticular gema personaje)

reduccionDeEnergiaLoca :: Gema -> Personaje -> Personaje
reduccionDeEnergiaLoca gema personaje = reduccionDeEnergia gema (reduccionDeEnergia gema personaje)

--4)

guanteleteGoma = UnGuantelete "goma" [tiempo, alma "usar Mjolnir",loca (alma "programación en Haskell")]

--5)

utilizar :: [Gema] -> Personaje -> Personaje
utilizar listaGemas personaje = foldl (flip aplicarGema) personaje listaGemas

--6)

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete personaje = buscarGemaMasPoderosa personaje (gemas guantelete)

buscarGemaMasPoderosa :: Personaje -> [Gema] -> Gema
buscarGemaMasPoderosa personaje [gema] = gema -- Paso base si queda una gema
buscarGemaMasPoderosa personaje (gema1:gema2:gemas)
    | cualGemaEsMasPoderosa personaje gema1 gema2 = buscarGemaMasPoderosa personaje (gema2:gemas)
    | otherwise = buscarGemaMasPoderosa personaje (gema1:gemas)

cualGemaEsMasPoderosa :: Personaje -> Gema -> Gema -> Bool
cualGemaEsMasPoderosa personaje gema1 gema2
    | energia (aplicarGema gema1 personaje) < energia (aplicarGema gema2 personaje) = True
    | otherwise = False    

--7)




-- mente, alma, espacio, poder, tiempo , loca
