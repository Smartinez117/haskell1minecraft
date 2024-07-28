module SecondPractice where

type Habilidades = String 
type Lugar       = String
type Situacion   = String
type Resultado   = [String]

data Respiracion = UnaRespiracion {
    nombre           :: String,
    posturas         ::[String],
    poderRespiracion :: Float
}deriving(Show,Eq,Ord)

data Cazador = UnCazador {
 nombreCazador    :: String,
 respiracion      :: Respiracion,
 fuerzaCazador    :: Float,
 velocidadCazador :: Float,
 habilidadCazador :: [String]
}deriving(Show,Eq,Ord)

data Demonio = UnDemonio {
    nombreDemonio    :: String,
    arteDemonio       :: String,
    fuerzaDemonio    :: Float, 
    velocidadDemonio :: Float,
    habilidadDemonio :: [String] 
}deriving(Show,Eq,Ord)

--declaracion de variables
espadaCarmesi,mundoTransparente,estadoDesinteresado,marcaDeCazador,ojoQueTodoLoRecuerda :: Habilidades
espadaCarmesi        = "espadaCarmesi"
mundoTransparente    = "mundoTransparente"
estadoDesinteresado  = "estadoDesinteresado"
marcaDeCazador       = "marcaDeCazador"
ojoQueTodoLoRecuerda = "ojoQueTodoLoRecuerda"
-- declaracion de los lugares 
fortalezaInfinita,aldeaDeHerreros,distritoRojo,campoAbierto :: Lugar 
fortalezaInfinita    = "FortalezaDimensionalInfinita"  
aldeaDeHerreros      = "aldeaDeLosherreros"             
distritoRojo         = "distritoRojo"                   
campoAbierto         = "campoAbierto" 
--declaracion de las situaciones diferentes 
cazadorHerido,demonioHerido,cazadorEntrenado,demonioConSangre,degradacion :: Situacion
cazadorHerido        = "cazadorHerido"
demonioHerido        = "demonioHeridoPorEspadaCarmesi"
cazadorEntrenado     =  "entrenamientoDePilar"
demonioConSangre     =  "elDemoniohaRecibidoSangre"
degradacion          = "cazadorDegradado"   
-- declaracion de las respiraciones y sus posturas 
respiracionSolar,respiracionFoton,respiracionAgua,respiracionRoca,respiracionViento :: Respiracion
respiracionFoton     = UnaRespiracion "foton"  ["amanecer","Sol Radiante","Lluvia de Estrellas","Nebuosa Resplandeciente","Tormenta de Fotones","Chispa Cosmica","Aurora Radiante","Espejismo de Luz","Rayo Fotonico","Centella Luminosa","Destello Estelar"] 12 
respiracionSolar     = UnaRespiracion "solar"  ["Decimo Tercera Postura", "Vals Flameante", "Halo Solar del Dragon", "Luz Brillante de Gratitud", "Puesta de Sol", "Inmensa Bruma Llameante", "Lanza de Girasol", "Sol Abrasante", "Rueda de Fuego", "Parhelio Arcoiris", "Espejo Carmesi Sofocante", "Cielo Azul", "Vals Relampago"] 10 
respiracionAgua      = UnaRespiracion "agua"   [ "Calma", "El dragon del Cambio","Salpicadura Caotica","Cuenca de Cascada" , "Estocada de Ondas Concéntricas", "Vórtice de Remolino" , "Lluvia tras la Sequía", "Golpe de Marea", "Danza de las Corrientes","Rueda de Agua","Corte de la Superficie del Agua"] 8
respiracionRoca      = UnaRespiracion "Roca"   [ "Rueda de piedra firme","Riolita - Conquista rápida","Reflejo ígneo","Rotura de la superficie celestial","Hidra de serpentinita"] 8 
respiracionViento    = UnaRespiracion "viento" [ "Tifon peligroso","Primer corte vendaval","Vendaval - Rafaga Repentina","Tormenta de humo negro","Vendaval de invierno","Tormenta de Polvo Ascendente","Arbol balanceándose en el aire de Tormenta","Garras de viento purificador","Torbellino de polvo"] 8 
-- declaracion de variables de los datas 
yoriichi,matemtaka,gyomei,giyu,sanemi :: Cazador
yoriichi  = UnCazador "yoriichi Tsugikuni"  respiracionSolar   5000  5000 [espadaCarmesi,mundoTransparente,estadoDesinteresado,marcaDeCazador]
matemtaka = UnCazador "matemtaka tsugikuni" respiracionFoton   5000  5000 [espadaCarmesi,mundoTransparente,estadoDesinteresado,marcaDeCazador,ojoQueTodoLoRecuerda]
gyomei    = UnCazador "gyomei"              respiracionRoca    1500  1300 [mundoTransparente,marcaDeCazador,espadaCarmesi]
giyu      = UnCazador "giyu"                respiracionAgua    1300  1200 [mundoTransparente,marcaDeCazador,espadaCarmesi]
sanemi    = UnCazador "sanemi"              respiracionViento  1400  1250 [mundoTransparente,marcaDeCazador,espadaCarmesi]

muzan,kokushibo,akaza,douma :: Demonio 
muzan     = UnDemonio "muzan" "progenitor"           10000 4000 []
kokushibo = UnDemonio "michokatsu" "creacionEspadas" 8000 3000 [marcaDeCazador,mundoTransparente]
akaza     = UnDemonio "akaza" "aguja"                7000 2500 []
douma     = UnDemonio "douma" "manipulacionHielo"    7500 2800 []

-- declaraciones y funciones para el calculo de poder para cazadores y demonios 
fuerzaArte :: Demonio -> Float
fuerzaArte demonio
 | arteDemonio demonio == "progenitor" = 5
 | arteDemonio demonio == "creacionEspadas" = 4
 | arteDemonio demonio == "aguja" = 2
 | arteDemonio demonio == "manipulacionHielo" = 3
 | otherwise = 1

fuerzaRespiracion :: Respiracion -> Float
fuerzaRespiracion = poderRespiracion 

fuerzaPorCazador :: Cazador -> Float 
fuerzaPorCazador cazador = (fuerzaCazador cazador + velocidadCazador cazador + fromIntegral (length(habilidadCazador cazador)) ) * fuerzaRespiracion (respiracion cazador)

fuerzaPorCazadores :: [Cazador] -> Float
fuerzaPorCazadores cazadores = sum (map fuerzaPorCazador cazadores)

fuerzaPorDemonio :: Demonio -> Float 
fuerzaPorDemonio demonio = (fuerzaDemonio demonio + velocidadDemonio demonio + fromIntegral(length(habilidadDemonio demonio))) * fuerzaArte demonio

fuerzaPorDemonios :: [Demonio] -> Float
fuerzaPorDemonios demonios = sum(map fuerzaPorDemonio demonios)

-- funciones para agregar habilidades y esas cosas 
agregarHabilidad :: Cazador -> String -> Cazador
agregarHabilidad cazador habilidad = cazador {habilidadCazador = habilidad:(habilidadCazador cazador)}

-- FUNCION PARA AGREGAR HABILIDADES DE FORMA DE CONJUNTOS ----------
agregarHabilidades :: Cazador -> [String]-> Cazador 
agregarHabilidades cazador [] = eliminarRepetidas cazador 
agregarHabilidades cazador (habilidad:restoHabilidades) = agregarHabilidades (agregarHabilidad cazador habilidad) restoHabilidades

eliminarRepetidas :: Cazador -> Cazador 
eliminarRepetidas cazador = cazador {habilidadCazador = eliminarElementosRepetidos2 (habilidadCazador cazador)}

eliminarElementosRepetidos2 :: [String] -> [String] 
eliminarElementosRepetidos2 [] = [] 
eliminarElementosRepetidos2 (x:xs) = x : eliminarElementosRepetidos2 (filter (/= x) xs) 

agregarPostura :: Respiracion -> String -> Respiracion 
agregarPostura respiracion nuevaPostura = respiracion {posturas = nuevaPostura:posturas respiracion}

cazadorNuevaPostura :: Cazador -> String -> Cazador 
cazadorNuevaPostura cazador postura = cazador {respiracion = agregarPostura (respiracion cazador) postura } 
----------------QUITAR HABILIDADES DE UN CAZADOR -----------
quitarHabilidad :: Cazador -> String -> Cazador 
quitarHabilidad cazador habilidad = cazador {habilidadCazador = filter (== habilidad) (habilidadCazador cazador) }

quitarHabilidades :: Cazador -> [String] -> Cazador 
quitarHabilidades cazador habilidades = cazador {habilidadCazador = eliminarElementos habilidades (habilidadCazador cazador)}
   
eliminarElementos :: [String] -> [String] -> [String] 
eliminarElementos lista1 lista2 = filter (\x -> notElem x lista1) lista2
-----------FUNCION PARA DECLARAR ENTRENAMIENTO PARA HACERSE MAS FUERTE 
entrenamiento :: Cazador -> Float -> Cazador 
entrenamiento cazador dias = cazador{fuerzaCazador = fuerzaCazador cazador + 10*dias,velocidadCazador = velocidadCazador cazador + 8*dias}

-- FUNCIONES PARA AGREGAR Y QUITAR VENTAJAS Y DESVENTAJAS 

-- LOS DATAS LUGAR,SITUACION DEFINEN STRINGS 
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
ventajasSituacionCazador :: Situacion -> Float
ventajasSituacionCazador situacion 
  | situacion == "cazadorHerido"        = 0.80
  | situacion == "entrenamientoDePilar" = 1.15
  | situacion == "cazadorDegradado"     = 0.91231
  | otherwise = 1 
ventajasSituacionDemonio :: Situacion -> Float
ventajasSituacionDemonio situacion 
 |situacion == "elDemoniohaRecibidoSangre"      = 1.10
 | situacion == "demonioHeridoPorEspadaCarmesi" = 0.80
 | otherwise = 1

-- FUNCIONES PARA DECLARAR ENFRENTAMIENTOS ENTRE DEMONIOS Y CAZADORES 
--ESTAS FUNCIONES BAJAN EL PODER DE UN CAZADOR Y DEMONIO DEPENDIENDO SI PERDIO O GANO 

derrotaCazador :: Cazador -> Cazador 
derrotaCazador cazador = cazador{velocidadCazador = velocidadCazador cazador*0.65231231,fuerzaCazador = fuerzaCazador cazador*0.652321}
derrotaCazadores :: [Cazador] -> [Cazador]
derrotaCazadores cazadores = map(\cazador -> derrotaCazador cazador) cazadores 
derrotaDemonio :: Demonio -> Demonio
derrotaDemonio demonio = demonio{fuerzaDemonio = fuerzaDemonio demonio*0.6534,velocidadDemonio = velocidadDemonio demonio*0.6542353}
derrotaDemonios :: [Demonio]-> [Demonio]
derrotaDemonios demonios = map(\demonio -> derrotaDemonio demonio) demonios 
-- VICTORIA DE CAZADORES EN LA CUAL GANAN EXPERIENCIA Y SE VUELVE MAS PODEROSO 
victoriaCazador :: Cazador -> Cazador 
victoriaCazador cazador = cazador {fuerzaCazador = fuerzaCazador cazador *1.3,velocidadCazador = velocidadCazador cazador * 1.3 }

victoriaCazadores :: [Cazador] -> [Cazador]
victoriaCazadores cazadores = map(\cazador -> victoriaCazador cazador) cazadores

victoriaDemonio :: Demonio -> Demonio 
victoriaDemonio demonio = demonio {fuerzaDemonio = fuerzaDemonio demonio*1.3,velocidadDemonio = velocidadDemonio demonio *1.3}

victoriaDemonios :: [Demonio] -> [Demonio]
victoriaDemonios demonios = map (\demonio -> victoriaDemonio demonio) demonios 

--situaciones de los cazadores y demonios

situacionCazador :: Cazador -> Situacion ->Lugar -> Cazador 
situacionCazador cazador situacion lugar = cazador {fuerzaCazador = fuerzaCazador cazador * ventajasSituacionCazador situacion * ventajasLugarCazador lugar,velocidadCazador = velocidadCazador cazador*ventajasSituacionCazador situacion * ventajasLugarCazador lugar}

situacionCazadores :: [Cazador]-> Situacion -> Lugar -> [Cazador]
situacionCazadores cazadores situacion lugar = map (\cazador -> situacionCazador cazador situacion lugar ) cazadores 

situacionDemonio :: Demonio -> Situacion-> Lugar -> Demonio 
situacionDemonio demonio situacion lugar = demonio {fuerzaDemonio= fuerzaDemonio demonio * ventajasSituacionDemonio situacion * ventajasLugarDemonio lugar,velocidadDemonio = velocidadDemonio demonio * ventajasSituacionDemonio situacion * ventajasLugarDemonio lugar}

situacionDemonios :: [Demonio]-> Situacion -> Lugar -> [Demonio]
situacionDemonios demonios situacion lugar = map (\demonio -> situacionDemonio demonio situacion lugar) demonios 

--  ENFRENTAMIENTOS ENTRE CAZADORES Y DEMONIOS !!

matchCazadorDemonios :: [Cazador]-> [Demonio] -> Resultado
matchCazadorDemonios cazadores demonios
 | fuerzaPorCazadores cazadores > fuerzaPorDemonios demonios = map nombreCazador cazadores
 | otherwise = map nombreDemonio demonios  
match :: [Cazador]->[Demonio]-> Situacion -> Lugar -> Resultado
match cazadores demonios situacion lugar 
 | fuerzaPorCazadores (situacionCazadores cazadores situacion lugar) > fuerzaPorDemonios(situacionDemonios demonios situacion lugar) = map nombreCazador cazadores 
 | otherwise = map nombreDemonio demonios 

match2 :: [Cazador]->[Demonio]->Lugar -> Situacion -> Situacion -> Resultado
match2 cazadores demonios lugar sitcaz sitdem 
  | fuerzaPorCazadores (situacionCazadores cazadores sitcaz lugar) > fuerzaPorDemonios(situacionDemonios demonios sitdem lugar) = map nombreCazador cazadores 
  | otherwise = map nombreDemonio demonios 


situacionesMultiples :: (a->b->c) ->(c->e->f)->a->b->e->f -- se declaran las variables para 
situacionesMultiples funcion1 funcion2 valorf1 valorf12 valorf2 = funcion2 (funcion1 valorf1 valorf12) valorf2 

cazadores = [yoriichi,matemtaka,gyomei,giyu,sanemi]
demonios = [muzan,kokushibo,akaza,douma]