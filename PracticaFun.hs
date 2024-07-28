module PracticaFun where
type Materiales = String

-- nueva practica 
type RomperHaskell = (Cazador,Demonio,String)
type Final = [String]

data Cazador = UnCazador {
    nombreCazador :: String,
    respiracion :: String,
    fuerzaCazador :: Float,
    velocidadCazador :: Float,
    demCazados :: Float,
    habilidades :: [Materiales]
} deriving (Show,Ord,Eq)

data Demonio = UnDemonio {
    nombreDemonio :: String,
    artedemoniaco :: String, 
    nivel :: Float,
    fuerzaDemonio :: Float,
    velocidadDemonio :: Float
}  deriving (Show,Ord,Eq)


data Jugador = UnJugador {
    nombre3 :: String, 
    puntos :: Int, 
    objetos :: [Materiales]
} deriving (Show,Eq)

data Lugar = UnLugar {nombre :: String}

type Situacion = String
--- DEFINICIONES 

espadaCarmesi,mundoTransparente,estadoDesinteresado,marcaDeCazador,saringan,ultraInstinto,ojoQueTodoLoRecuerda :: Materiales
espadaCarmesi       = "espadaCarmesi"
mundoTransparente   = "mundoTRansparente"
estadoDesinteresado = "estadoDesinteresado"
marcaDeCazador      = "marcaDeCazador"
saringan            = "saringan"
ultraInstinto       = "ultraInstinto"
ojoQueTodoLoRecuerda= "ojoQueTodoLoRecuerda"

yoriichi,matentaka,gyomei,giyu,sanemi,kyokuro,michikatsu :: Cazador 
yoriichi   = UnCazador  "YorichiTsugikuni"   "solar"  1000 1000 250 [espadaCarmesi,mundoTransparente,estadoDesinteresado,marcaDeCazador] -- fuerza 2000 *(1.25*4)*10, respiracion *10= 100000
gyomei     = UnCazador  "gyomei"             "roca"    800 700  150 [espadaCarmesi,mundoTransparente,marcaDeCazador]                     -- fuerza  respiracion * 7 = 39375 
giyu       = UnCazador  "Giyu"               "agua"    700 600   80 [espadaCarmesi,marcaDeCazador]                                       --  respiracion  *7 = 22750
sanemi     = UnCazador  "Sanemi"             "viento"  700 650   85 [espadaCarmesi,marcaDeCazador]                                       --  respiracion *7 = 23625
kyokuro    = UnCazador  "Kyokuro"            "llama"   700 550   65 []                                                                   -- respiracion * 7 = 8750
michikatsu = UnCazador  "michikatsu"         "lunar"   900 750   60 [mundoTransparente,marcaDeCazador,marcaDeCazador,mundoTransparente]   -- respiracion * 8 = 33000
matentaka  = UnCazador  "matentakaTsugikuni" "foton"  1000 1000 100 [espadaCarmesi,mundoTransparente,estadoDesinteresado,marcaDeCazador]

kokushibo,akaza,douma,muzan :: Demonio
kokushibo = UnDemonio "kokushibo" "CreacionDeArmas"    80  3000 3000  -- 48000
akaza     = UnDemonio "akaza"     "brujula"            65  2000 2000  -- 26000
douma     = UnDemonio "douma"     " creacionDeHielo"   70  2500 2500  -- 35000
muzan     = UnDemonio "muzan"     "creacionDeDemonios" 100 4000 4000  -- 80000 suma de la fuerza y velocidad por 0.1

fortalezaInfinita,aldeaDeHerreros,distritoRojo,campoAbierto :: Lugar 
fortalezaInfinita = UnLugar "FortalezaDimensionalInfinita"  
aldeaDeHerreros   = UnLugar "aldeaDeLosherreros"             
distritoRojo      = UnLugar "distritoRojo"                   
campoAbierto      = UnLugar "campoAbierto"      

cazadorHerido,demonioHerido,cazadorEntrenado,demonioConSangre,degradacion :: Situacion 
cazadorHerido    = "cazadorHerido"
demonioHerido    = "demonioHeridoPorEspadaCarmesi"
cazadorEntrenado =  "entrenamientoDePilar"
demonioConSangre =  "elDemoniohaRecibidoSangre"
degradacion      = "cazadorDegradado"
-- PRACTICA 1 
--- FUNCIONES PARA PRACTICA 2 si funcionan
--ganaEnPelea :: Cazador -> Demonio -> String
--ganaEnPelea cazador demonio 
-- | nombre cazador == "YorichiTsugikuni" = "obviamente nadie le puede ganar al maximo hermano menor talentoso"
-- | ((length (habilidades cazador) + 6) > nivel demonio) = nombre cazador
-- | otherwise = nombre1 demonio

--mejorCazador :: Cazador -> Cazador -> String
--mejorCazador cazador1 cazador2 
-- --| (demCazados cazador1) > (demCazados cazador2) = nombre cazador1 
-- | otherwise = nombre cazador2 
------------------------------------------------------------------------------------------------
-- funciones de habilidades primer intento 

--ganaHabilidadPorYoriichi ::  String -> Cazador -> Cazador 
--ganaHabilidadPorYoriichi nuevaHabilidad cazador 
-- | yaTieneHabilidad nuevaHabilidad cazador = cazador
-- | otherwise = agregarHabilidad nuevaHabilidad cazador
----------------------------------------
--ganaHabilidades :: [String] -> Cazador -> Cazador 
--ganaHabilidades nuevasHabilidades cazador 
-- | yaTieneHabilidades nuevasHabilidades cazador = cazador 
-- | otherwise = agregarHabilidades nuevasHabilidades cazador 
-------------------------------------------------------------------------------------------------
--- hacer una funcion que en vez de verifivar que si las habilidadedsz estan en las habliidaddes del cazador las elimine y despues se agreguen a las hablidades del cazador 
--- en la recursividad se vayan agregando las habalidades apenas se vaya desglosando la lista 

agregarHabilidad :: String -> Cazador -> Cazador
agregarHabilidad nuevahabilidad cazador = cazador {habilidades = nuevahabilidad:(habilidades cazador)}

agregarHabilidades :: [String] -> Cazador -> Cazador 
agregarHabilidades [] cazador = cazador
agregarHabilidades (habilidad:restoHabilidades) cazador = agregarHabilidades restoHabilidades (agregarHabilidad habilidad cazador) 

--agregarHabilidades1 :: [String] -> Cazador -> Cazador 
--agregarHabilidades1 [] cazador = cazador
--agregarHabilidades1 (habilidad:restoHabilidades) cazador = cazador {habilidades = agregarHabilidades restoHabilidades _ (habilidad:habilidades) }

------------------------------------------------------------------------------------
-- estas funciones si funcionan 
agregarNuevasHabilidades :: [String] -> Cazador -> Cazador 
agregarNuevasHabilidades nuevasHabilidades cazador = quitarHabilidadesRepetidas(agregarHabilidades nuevasHabilidades cazador)

quitarHabilidadesRepetidas :: Cazador -> Cazador 
quitarHabilidadesRepetidas cazador = cazador { habilidades = eliminarHabilidadesRepetidas (habilidades cazador) }

eliminarHabilidadesRepetidas :: [String] -> [String]
eliminarHabilidadesRepetidas [] = []
eliminarHabilidadesRepetidas (x:xs) = x : eliminarHabilidadesRepetidas (filter (/= x) xs)
------(filter (/= x) xs) filtra los elemntos que son distintos de x que es lo mismo aunque es mas facil que usar las funcion lambda para definir una nueva funcion en medio de todas las demas funciones 

eliminarHabilidadesRepetidas1 :: [String] -> [String]
eliminarHabilidadesRepetidas1 [] = []
eliminarHabilidadesRepetidas1 (x:xs) = x : eliminarHabilidadesRepetidas (filter (\y -> y /= x) xs)
-- funcion usanod la funcion lambda pero no es tan eficiente como la anterior puede no usa la funcion lambda comon tal asiqeu mejor usa la anteriro 
-------------------------------------------------------------------------------------------
--------------------funcion ´principal de enfrentamiento------------------------------
enfrentamientoCazadorDemonio :: Cazador -> Demonio -> Lugar -> String
enfrentamientoCazadorDemonio cazador demonio lugar 
 | cantidadDePoderCazador cazador * ventajasLugarCazador(lugar) > cantidadDePoderDemonio demonio * ventajasLugarDemonio(lugar) = nombreCazador cazador 
 | otherwise = nombreDemonio demonio 
 
------------------------------------ funciones auxiliares para enfrentamiento -------------------------------------

cantidadDePoderCazador :: Cazador -> Float
cantidadDePoderCazador cazador = (fuerzaCazador cazador + velocidadCazador cazador) * fromInteger (toInteger (length (habilidades cazador))) * 1.25 * poderDeRespiracion cazador

cantidadDePoderDemonio :: Demonio -> Float
cantidadDePoderDemonio demonio = (fuerzaDemonio demonio + velocidadDemonio demonio) * 0.1 * (nivel demonio)

poderDeRespiracion :: Cazador -> Float
poderDeRespiracion cazador 
 | respiracion cazador == "foton"  = 11
 | respiracion cazador == "solar"  = 10
 | respiracion cazador == "lunar"  = 8
 | respiracion cazador == " "      = 1 
 | otherwise  = 7

ventajasLugarDemonio :: Lugar -> Float
ventajasLugarDemonio lugar 
 | nombre lugar == "fortalezaDimensionalInfinita" = 1.1
 | nombre lugar == "aldeaDeLosHerreros"           = 1.05
 | nombre lugar == "distritoRojo"                 = 1.01
 | nombre lugar == "campoabierto"                 = 1
 | otherwise = 1

ventajasLugarCazador :: Lugar -> Float
ventajasLugarCazador lugar
 | nombre lugar == "fortalezaDimensionalInfinita" = 1
 | nombre lugar == "aldeaDeLosHerreros"           = 1.05
 | nombre lugar == "distritoRojo"                 = 1.01
 | nombre lugar == "campoAbierto"                 = 1.1
 | otherwise = 1
------------------------------------------------------------------------------------------------------
enfrentamientoCazadorDemonioversion2 :: Cazador -> Demonio -> Situacion -> String 
enfrentamientoCazadorDemonioversion2 cazador demonio situacion
 | cantidadDePoderCazador cazador *ventajasSituacionCazador(situacion) > cantidadDePoderDemonio demonio * ventajasSituacionDemonio(situacion) = nombreCazador cazador 
 | otherwise = nombreDemonio demonio 
 
ventajasSituacionCazador :: Situacion -> Float
ventajasSituacionCazador situacion 
  | situacion == "cazadorHerido"        = 0.80
  | situacion == "entrenamientoDePilar" = 1.15
  | situacion == "cazadorDegradado"     = 0.91231
  | otherwise = 1 
  

ventajasSituacionDemonio :: Situacion -> Float
ventajasSituacionDemonio situacion 
 |situacion == "elDemoniohaRecibidoSangre"     = 1.10
 | situacion == "demonioHeridoPorEspadaCarmesi" = 0.80
 | otherwise = 1 
----------------------------------------------------------------------------------------
enfrentamientoCazadoresDemonio :: Cazador -> Cazador -> Demonio -> Situacion -> String 
enfrentamientoCazadoresDemonio cazador1 cazador2 demonio situacion 
 | (cantidadDePoderCazador cazador1 *ventajasSituacionCazador(situacion) + cantidadDePoderCazador cazador2 * ventajasSituacionCazador(situacion)) > cantidadDePoderDemonio demonio * ventajasSituacionDemonio(situacion) = nombreCazador cazador1 ++ " y " ++ nombreCazador cazador2
 | otherwise = nombreDemonio demonio 
---------------PRACTICA DE TRATAMIENTO DE LISTAS CON LA FUNCION LAMBDA ----------------------
quitarHabilidad :: [String] -> [String] -> [String]
quitarHabilidad habilidad habilidades = filter (\x -> notElem x habilidad) habilidades

quitarHabilidadesCazador :: [String] -> Cazador -> Cazador 
quitarHabilidadesCazador habilidadesQuitar cazador = cazador {habilidades = quitarHabilidad habilidadesQuitar (habilidades cazador)}

------------------ version mejorada de la funcion de arriba para evitar el uso de varias funciones-----------------------------

quitarHabilidadesCazador1 :: [String] -> Cazador -> Cazador 
quitarHabilidadesCazador1 habilidadesQuitar cazador = cazador {habilidades = filter(\x -> notElem x habilidadesQuitar) (habilidades cazador)}

situacionCazador :: Cazador -> Situacion -> Cazador 
situacionCazador cazador situacion = cazador {fuerzaCazador = fuerzaCazador cazador * ventajasSituacionCazador(situacion),velocidadCazador = velocidadCazador cazador*ventajasSituacionCazador(situacion) }
--------------------------------- funcion de primer orden -------------------------------
-- se juegan con los parametros de entrada y salida de las funciones que se reciben como parametros y la conposicon de funciones
situacionMultipleCazador:: (a -> b)->(c -> b ->d)-> a -> c ->d 
situacionMultipleCazador funcion1 funcion2 valorfuncion1 valorfuncion2 = funcion2 valorfuncion2 (funcion1 valorfuncion1)
--funcion1 entrenamientoDePilar funcion 2 agregar hablidada


entrenamientoDePilar1 :: Cazador -> Cazador 
entrenamientoDePilar1 cazador = cazador{fuerzaCazador = fuerzaCazador cazador* 1.234,velocidadCazador = velocidadCazador cazador*1.345}


-----------------------------------------------------------------------------
--practica para romper haskell nueva version 
enfrentamientoCazadorDemonio1 :: Cazador -> Demonio -> Lugar -> RomperHaskell
enfrentamientoCazadorDemonio1 cazador demonio lugar 
 | cantidadDePoderCazador cazador * ventajasLugarCazador(lugar) > cantidadDePoderDemonio demonio * ventajasLugarDemonio(lugar) = (cazador , derrotaDemonio(demonio),"gana" ++ nombreCazador cazador)
 | otherwise = (derrotaCazador(cazador),demonio,"gana" ++ nombreDemonio demonio )
-----------funciones auxiliares ------------------------
derrotaCazador :: Cazador -> Cazador 
derrotaCazador cazador = cazador{velocidadCazador =velocidadCazador cazador*0.65231231,fuerzaCazador = fuerzaCazador cazador*0.652321}

derrotaDemonio :: Demonio -> Demonio
derrotaDemonio demonio = demonio{fuerzaDemonio = fuerzaDemonio demonio*0.6534,velocidadDemonio = velocidadDemonio demonio*0.6542353}
------------------------------------------------------------------
enfrentamientoContraDemonio ::[Cazador] -> Demonio -> Final
enfrentamientoContraDemonio listaDeCazadores demonio 
 | sumaDePoder1 listaDeCazadores > cantidadDePoderDemonio demonio = map nombreCazador listaDeCazadores
 | otherwise = [nombreDemonio muzan]
-----------------funciones auxiliares------------------- 
sumaDePoder1 :: [Cazador] -> Float
sumaDePoder1 xs = foldr ((+) . cantidadDePoderCazador) 0 xs
---- version mejorada de la funcion de arriba tener en cuenta ---------
sumaDePoder2 :: [Cazador] -> Float
sumaDePoder2 = foldr ((+) . cantidadDePoderCazador) 0
--sumaDePoder :: [Cazador] -> Float
--sumaDePoder [] = []
--sumaDePoder (x:xs) = cantidadDePoderCazador(x) + sumaDePoder(xs)

--enfrentamientoPilaresDemonios :: [a] -> [b] -> Final
--enfrentamientoPilaresDemonios lista1 lista2 
-- | sumade

---------------------------------------------------------------------------------
--notas---
--Destello Estelar: En esta postura, el usuario realiza un corte rápido y preciso con movimientos elegantes y fluidos, como si estuviera trazando una estrella fugaz en el aire.
--Centella Luminosa: El usuario ejecuta movimientos circulares y suaves, creando una esfera de luz que rodea su arma y luego la lanza hacia el enemigo como una centella brillante.
--Rayo Fotónico: La postura implica movimientos explosivos y rápidos, como un rayo de luz que corta el espacio en un destello de energía.
--Espejismo de Luz: El usuario utiliza movimientos engañosos y evasivos, creando ilusiones de luz para confundir al enemigo y esquivar ataques.
--Aurora Radiante: En esta postura, el usuario realiza movimientos expansivos y elevados, como si estuviera desplegando un amanecer luminoso, y luego irradia luz desde su arma.
--Chispa Cósmica: El usuario genera pequeñas explosiones de luz con movimientos rápidos y cortos, como chispas estelares que deslumbran al enemigo.
--Tormenta de Fotones: Los movimientos son frenéticos y caóticos, como una tormenta de partículas de luz que rodea al usuario y ataca al enemigo desde múltiples direcciones.
--Nebulosa Resplandeciente: El usuario ejecuta movimientos suaves y giratorios, creando una nebulosa de luz que envuelve al enemigo y disminuye su visión.
--Lluvia de Estrellas: En esta postura, el usuario realiza movimientos ascendentes y descendentes, como si estuviera invocando una lluvia de estrellas desde el cielo.
--Sol Radiante: Los movimientos son majestuosos y poderosos, como si el usuario estuviera canalizando la energía de un sol brillante para liberar un ataque deslumbrante.

--Destello Estelar: Esta postura podría representar la velocidad y la luminosidad de las estrellas, evocando la idea de movimientos tan rápidos como la luz.
--Centella Luminosa: La esfera de luz creada en esta postura refleja la idea de que los fotones pueden manifestarse en forma de partículas de luz brillante.
--Rayo Fotónico: La velocidad y potencia de un rayo de luz se reflejan en esta postura, donde los movimientos son explosivos y rápidos.
--Espejismo de Luz: Los fotones pueden doblarse y reflejarse, lo que se asemeja a la creación de ilusiones de luz en esta postura.
--Aurora Radiante: La postura evoca la idea de un amanecer o una aurora boreal, donde la luz es expansiva y hermosa.
--Chispa Cósmica: Los fotones pueden generar chispas de energía, y esta postura representa movimientos que generan explosiones de luz.
--Tormenta de Fotones: Los fotones pueden ser caóticos y viajar en todas direcciones, como lo hacen en una tormenta de partículas de luz.
--Nebulosa Resplandeciente: Las nebulosas son nubes de gas y polvo que emiten luz, y esta postura crea una nebulosa de luz alrededor del usuario.
--Lluvia de Estrellas: La lluvia de estrellas se compone de pequeños fragmentos de luz, y esta postura refleja la idea de lanzar ataques en cascada.
--Sol Radiante: El sol es una fuente inmensa de luz y energía, y esta postura canaliza esa energía en un ataque deslumbrante.