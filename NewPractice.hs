module NewPractice where

type Habilidades = String
type Romperhaskell =([Demonio],Resultado)
type Lugar = String 
type Situaciones = String 
type Resultado = [String]

data Cazador = UnCazador {
    nombreCazador :: String,
    respiracion :: Respiraciones,
    fuerzaCazador :: Float,
    velocidadCazador :: Float,
    habilidadCazador :: [Habilidades]
} deriving (Show,Ord,Eq) 

data Demonio = UnDemonio{
    nombreDemonio    :: String,
    arteDemoniaco    :: String,
    fuerzaDemonio    :: Float,
    velocidadDemonio :: Float   ,
    habilidadDemonio :: [Habilidades]
} deriving (Show,Ord,Eq) 

data Respiraciones = UnaRespiracion{
    nombre :: String,
    posturas :: [Habilidades],
    fuerzaRespiracion :: Float
}deriving (Show,Ord,Eq) 

--declaracion de variables
espadaCarmesi,mundoTransparente,estadoDesinteresado,marcaDeCazador,ojoQueTodoLoRecuerda :: Habilidades
espadaCarmesi = "espadaCarmesi"
mundoTransparente = "mundoTransparente"
estadoDesinteresado = "estadoDesinteresado"
marcaDeCazador = "marcaDeCazador"
ojoQueTodoLoRecuerda = "ojoQueTodoLoRecuerda"
-- declaracion de los lugares 
fortalezaInfinita,aldeaDeHerreros,distritoRojo,campoAbierto :: Lugar 
fortalezaInfinita = "FortalezaDimensionalInfinita"  
aldeaDeHerreros   = "aldeaDeLosherreros"             
distritoRojo      = "distritoRojo"                   
campoAbierto      = "campoAbierto" 
--declaracion de las situaciones diferentes 
cazadorHerido,demonioHerido,cazadorEntrenado,demonioConSangre,degradacion :: Situaciones 
cazadorHerido    = "cazadorHerido"
demonioHerido    = "demonioHeridoPorEspadaCarmesi"
cazadorEntrenado =  "entrenamientoDePilar"
demonioConSangre =  "elDemoniohaRecibidoSangre"
degradacion      = "cazadorDegradado"   
-- declaracion de las respiraciones y sus posturas 
respiracionSolar,respiracionFoton,respiracionAgua,respiracionRoca,respiracionViento :: Respiraciones
respiracionFoton  = UnaRespiracion "foton"  ["amanecer","Sol Radiante","Lluvia de Estrellas","Nebuosa Resplandeciente","Tormenta de Fotones","Chispa Cosmica","Aurora Radiante","Espejismo de Luz","Rayo Fotonico","Centella Luminosa","Destello Estelar"] 12 
respiracionSolar  = UnaRespiracion "solar"  ["Decimo Tercera Postura", "Vals Flameante", "Halo Solar del Dragon", "Luz Brillante de Gratitud", "Puesta de Sol", "Inmensa Bruma Llameante", "Lanza de Girasol", "Sol Abrasante", "Rueda de Fuego", "Parhelio Arcoiris", "Espejo Carmesi Sofocante", "Cielo Azul", "Vals Relampago"] 10 
respiracionAgua   = UnaRespiracion "agua"   [ "Calma", "El dragon del Cambio","Salpicadura Caotica","Cuenca de Cascada" , "Estocada de Ondas Concéntricas", "Vórtice de Remolino" , "Lluvia tras la Sequía", "Golpe de Marea", "Danza de las Corrientes","Rueda de Agua","Corte de la Superficie del Agua"] 8
respiracionRoca   = UnaRespiracion "Roca"   [ "Rueda de piedra firme","Riolita - Conquista rápida","Reflejo ígneo","Rotura de la superficie celestial","Hidra de serpentinita"] 8 
respiracionViento = UnaRespiracion "viento" [ "Tifon peligroso","Primer corte vendaval","Vendaval - Rafaga Repentina","Tormenta de humo negro","Vendaval de invierno","Tormenta de Polvo Ascendente","Arbol balanceándose en el aire de Tormenta","Garras de viento purificador","Torbellino de polvo"] 8 
-- declaracion de variables de los datas 
yoriichi,matemtaka,gyomei,giyu,sanemi :: Cazador
yoriichi  = UnCazador "yoriichi Tsugikuni"  respiracionSolar   5000  5000 [espadaCarmesi,mundoTransparente,estadoDesinteresado,marcaDeCazador]
matemtaka = UnCazador "matemtaka tsugikuni" respiracionFoton   5000  5000 [espadaCarmesi,mundoTransparente,estadoDesinteresado,marcaDeCazador,ojoQueTodoLoRecuerda]
gyomei    = UnCazador "gyomei"              respiracionRoca    1500  1300 [mundoTransparente,marcaDeCazador,espadaCarmesi]
giyu      = UnCazador "giyu"                respiracionAgua    1300  1200 [mundoTransparente,marcaDeCazador,espadaCarmesi]
sanemi    = UnCazador "sanemi"              respiracionViento  1400  1250 [mundoTransparente,marcaDeCazador,espadaCarmesi]

muzan,kokushibo,akaza,douma :: Demonio 
muzan     = UnDemonio "muzan" "progenitor"           10000 4000 []
kokushibo = UnDemonio "michokatsu" "creacionEspadas" 8000 3000  [marcaDeCazador,mundoTransparente]
akaza     = UnDemonio "akaza" "aguja"                7000 2500  []
douma     = UnDemonio "douma" "manipulacionHielo"    7500 2800  []

-- declaraciones y funciones para el calculo de poder para cazadores y demonios 
fuerzaArte :: Demonio -> Float
fuerzaArte demonio
 | arteDemoniaco demonio == "progenitor"        = 5
 | arteDemoniaco demonio == "creacionEspadas"   = 4
 | arteDemoniaco demonio == "aguja"             = 2
 | arteDemoniaco demonio == "manipulacionHielo" = 3
 | otherwise = 1

poderRespiracion :: Respiraciones -> Float
poderRespiracion = fuerzaRespiracion

fuerzaPorCazador :: Cazador -> Float
fuerzaPorCazador cazador = (fuerzaCazador cazador + velocidadCazador cazador) * poderRespiracion (respiracion cazador)

fuerzaCazadores :: [Cazador] -> Float
fuerzaCazadores cazadores = foldl (\acumulacion cazador -> acumulacion + fuerzaPorCazador cazador) 0 cazadores 

fuerzaPorDemonio :: Demonio -> Float 
fuerzaPorDemonio demonio = (fuerzaDemonio demonio + velocidadDemonio demonio ) * fuerzaArte demonio

fuerzaDemonios :: [Demonio] -> Float 
fuerzaDemonios demonios = foldl (\acumulacion demonio -> acumulacion + fuerzaPorDemonio demonio) 0 demonios 

-- declaraciones para agregar y quitar habilidades de cazadores
nuevaPostura :: Respiraciones -> String -> Respiraciones
nuevaPostura respiracion postura = respiracion{posturas = postura:(posturas respiracion)}

agregarPostura :: Cazador -> String -> Cazador 
agregarPostura cazador postura = cazador {respiracion = nuevaPostura (respiracion cazador) postura}

agregarHabilidad :: Cazador -> String -> Cazador 
agregarHabilidad cazador nuevahabilidad = cazador {habilidadCazador = nuevahabilidad:(habilidadCazador cazador)}

agregarHabilidades :: Cazador -> [Habilidades] -> Cazador
agregarHabilidades cazador [] = cazador
agregarHabilidades cazador (habilidad:restoHabilidad) = agregarHabilidades (agregarHabilidad cazador habilidad) restoHabilidad 

eliminarElementosRepetidos :: [String] -> [String]
eliminarElementosRepetidos [] = []
eliminarElementosRepetidos (x:xs) = x : eliminarElementosRepetidos (filter (/= x) xs)

quitarHabilidadesRepetidas :: Cazador -> Cazador
quitarHabilidadesRepetidas cazador = cazador {habilidadCazador = eliminarElementosRepetidos (habilidadCazador cazador)}

agregarHabilidadesv2 :: Cazador -> [Habilidades] -> Cazador
agregarHabilidadesv2 cazador nuevasHabilidades = quitarHabilidadesRepetidas(agregarHabilidades cazador nuevasHabilidades)

perderHabilidades :: Cazador -> [Habilidades] -> Cazador 
perderHabilidades cazador habilidades = cazador {habilidadCazador = filter (\x -> notElem x habilidades) (habilidadCazador cazador)}
--funciones para incrementar poder 
entrenamientoPilar :: Cazador -> Float -> Cazador 
entrenamientoPilar cazador entrenaDias = cazador {fuerzaCazador = fuerzaCazador cazador + entrenaDias*2.5,velocidadCazador = velocidadCazador cazador + entrenaDias*2.5}


--funciones para declarar diferentes ventajas y desventajas 
ventajasLugarDemonio :: Lugar -> Float
ventajasLugarDemonio lugar 
 | lugar == "fortalezaDimensionalInfinita" = 1.1
 | lugar == "aldeaDeLosHerreros"           = 1.05
 | lugar == "distritoRojo"                 = 1.01
 | lugar == "campoabierto"                 = 1
 | otherwise = 1
ventajasLugarCazador :: Lugar -> Float
ventajasLugarCazador lugar
 | lugar == "fortalezaDimensionalInfinita" = 1
 | lugar == "aldeaDeLosHerreros"           = 1.05
 | lugar == "distritoRojo"                 = 1.01
 | lugar == "campoAbierto"                 = 1.1
 | otherwise = 1
ventajasSituacionCazador :: Situaciones -> Float
ventajasSituacionCazador situacion 
  | situacion == "cazadorHerido"        = 0.80
  | situacion == "entrenamientoDePilar" = 1.15
  | situacion == "cazadorDegradado"     = 0.91231
  | otherwise = 1 
ventajasSituacionDemonio :: Situaciones -> Float
ventajasSituacionDemonio situacion 
 |situacion == "elDemoniohaRecibidoSangre"     = 1.10
 | situacion == "demonioHeridoPorEspadaCarmesi" = 0.80
 | otherwise = 1

derrotaCazador :: Cazador -> Cazador 
derrotaCazador cazador = cazador{velocidadCazador =velocidadCazador cazador*0.65231231,fuerzaCazador = fuerzaCazador cazador*0.652321}
derrotaCazadores :: [Cazador] -> [Cazador]
derrotaCazadores cazadores = map(\cazador -> derrotaCazador cazador) cazadores 
derrotaDemonio :: Demonio -> Demonio
derrotaDemonio demonio = demonio{fuerzaDemonio = fuerzaDemonio demonio*0.6534,velocidadDemonio = velocidadDemonio demonio*0.6542353}
derrotaDemonios :: [Demonio]-> [Demonio]
derrotaDemonios demonios = map(\demonio -> derrotaDemonio demonio) demonios 


--situaciones de los cazadores y demonios

situacionCazador :: Cazador -> Situaciones ->Lugar -> Cazador 
situacionCazador cazador situacion lugar = cazador {fuerzaCazador = fuerzaCazador cazador * ventajasSituacionCazador situacion * ventajasLugarCazador lugar,velocidadCazador = velocidadCazador cazador*ventajasSituacionCazador situacion * ventajasLugarCazador lugar}

situacionCazadores :: [Cazador]-> Situaciones -> Lugar -> [Cazador]
situacionCazadores cazadores situacion lugar = map (\cazador -> situacionCazador cazador situacion lugar ) cazadores 

situacionDemonio :: Demonio -> Situaciones-> Lugar -> Demonio 
situacionDemonio demonio situacion lugar = demonio {fuerzaDemonio= fuerzaDemonio demonio * ventajasSituacionDemonio situacion * ventajasLugarDemonio lugar,velocidadDemonio = velocidadDemonio demonio * ventajasSituacionDemonio situacion * ventajasLugarDemonio lugar}

situacionDemonios :: [Demonio]-> Situaciones -> Lugar -> [Demonio]
situacionDemonios demonios situacion lugar = map (\demonio -> situacionDemonio demonio situacion lugar) demonios 

-- lugar de ls enfrentamientos de los cazadores y demonios 


--funciones para declarar enfrentamientos -- funcionan pero el terminal es una mierda para correrlos 
matchCazadorDemonio :: [Cazador] -> [Demonio] -> Resultado
matchCazadorDemonio cazadores demonios 
 | fuerzaCazadores cazadores > fuerzaDemonios demonios = map nombreCazador cazadores 
 | otherwise = map nombreDemonio demonios 

matchCazadorDemoniokai :: [Cazador] -> [Demonio] -> Lugar -> Situaciones -> Situaciones -> Resultado
matchCazadorDemoniokai cazadores demonios lugar situacionCaz situacionDem
 | fuerzaCazadores (situacionCazadores cazadores situacionCaz lugar)  > fuerzaDemonios(situacionDemonios demonios situacionDem lugar) = map nombreCazador cazadores 
 | otherwise = map nombreDemonio demonios 

matchCazadorDemonioshin :: [Cazador] -> [Demonio] -> Lugar -> Situaciones -> Situaciones -> Romperhaskell
matchCazadorDemonioshin cazadores demonios lugar situacionCaz situacionDem
 | fuerzaCazadores (situacionCazadores cazadores situacionCaz lugar)  > fuerzaDemonios(situacionDemonios demonios situacionDem lugar) = (derrotaDemonios demonios ,map nombreCazador cazadores )
 | otherwise =  (demonios,map nombreCazador cazadores)

situacionesMultiples :: (a->b->c) ->(c->e->f)->a->b->e->f -- se declaran las variables para 
situacionesMultiples funcion1 funcion2 valorf1 valorf12 valorf2 = funcion2 (funcion1 valorf1 valorf12) valorf2 

mapearLista :: [Cazador] ->(Cazador->a->Cazador)-> a -> [Cazador]
mapearLista cazadores funcion1 valor = map(\cazador -> funcion1 cazador valor) cazadores 

mapearLista1 :: [a] ->(a->b->a)-> b -> [a]
mapearLista1 lista1 funcion valor = map(\elemento -> funcion elemento valor) lista1

mapearLista2:: [a] ->(a->b->a)->b->[a] 
mapearLista2 lista funcion valor = map(\elem -> funcion elem valor) lista 

agregarHabilidad1:: Cazador->String-> Cazador 
agregarHabilidad1 cazador habilidad = cazador{habilidadCazador = habilidad:(habilidadCazador cazador)}

nuevasHabilidades :: Cazador -> [String]-> Cazador 
nuevasHabilidades cazador [] = cazador 
nuevasHabilidades cazador (x:xs) = nuevasHabilidades (agregarHabilidad1 cazador x ) xs 

situacionMultiple :: a->b->(a->b->a)->[a]->[a]
situacionMultiple valor1 valor2 funcion lista = map(\elem -> funcion elem valor2) lista

--
