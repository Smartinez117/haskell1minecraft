module Practicaminecraf where
type Materiales = String

data Jugador = UnJugador {
    nombre :: String, 
    puntos :: Int, 
    objetos :: [Materiales]
} deriving (Show,Ord,Eq)

data Receta = UnaReceta {
    materiales :: [Materiales],
    tiempo :: Int, 
    resultado :: String
}  deriving (Show,Ord,Eq)

-----declaraciones de objetos -------------------------
madera,fosforo,pollo,lana,agujas,tintura,fogata,polloAsado,sueter,katana,hierro,bronce,hielo,iglues,lobos,arena,plantas :: String
madera     = "madera"
fosforo    = "fosforo"
pollo      = "pollo"
lana       = "lana"
agujas     = "agujas"
tintura    = "tintura"
fogata     = "fogata"
polloAsado = "polloAsado"
sueter     = "sueter"
katana     = "katana"
--espada     = "espada"
hierro     = "hierro" 
bronce     = "bronce"
hielo      = "hielo"
iglues     = "iglues"
lobos      = "lobos"
arena      = "arena"
plantas    = "plantas"
  
---------------declaracion de recetas -------------------------------
recetaFogata,recetaPolloAsado,recetaSueter,recetaKatana :: Receta
recetaFogata     = UnaReceta [madera,fosforo]       10   "fogata"
recetaPolloAsado = UnaReceta [fogata,pollo]         300  "polloAsado"
recetaSueter     = UnaReceta [lana,agujas,tintura]  600  "sueter"
recetaKatana     = UnaReceta [hierro,madera]        500  "katana"
-- recetaEspada  = UnaReceta [hierro,madera,bronce]   550  "espada"

-------------------declaraciones de los personajes ------------------
tomas,gabriel,nicolas,juan,lucas :: Jugador 
tomas   = UnJugador "Tomas"   435 [madera,fosforo,pollo,hierro,lana,agujas,tintura]--puede fogata,katana,
gabriel = UnJugador "Gabriel" 279 [hierro,madera,bronce,espada,hacha]----- espada 
nicolas = UnJugador "Nicolas" 520 [lana,agujas,tintura,pico] -- sueter 
juan    = UnJugador "Juan"    390 [bronce,agujas,fosforo] --
lucas   = UnJugador "Lucas"   340 [madera,hierro,lana]

---------------------------------funciones -------------------

craftearObjetos :: Receta -> Jugador -> Jugador --no modifica los puntos del jugador 
craftearObjetos receta jugador 
 | tieneMaterialesJugador receta jugador = eliminarObjetos2 receta(agregarObjeto receta jugador) -- agregarObjeto receta (eliminarObjetos receta jugador)
 | otherwise = modificarPuntos jugador (-100)  

craftearObjetos1 :: Receta -> Jugador -> Jugador  
craftearObjetos1 receta jugador 
 | tieneMaterialesJugador receta jugador = eliminarObjetos2 receta(agregarObjeto receta (modificarPuntos jugador (tiempo receta*10 )))
 | otherwise = modificarPuntos jugador (-100)  

--eliminarObjetos receta (agregarObjeto receta jugador)
 ----reducir puntos 
-------funciones auxiliares para craftear -----------
modificarPuntos :: Jugador -> Int -> Jugador 
modificarPuntos jugador valor = jugador {puntos = (puntos jugador) + valor}

agregarObjeto :: Receta -> Jugador -> Jugador 
agregarObjeto receta jugador = jugador {objetos = resultado receta: objetos jugador} 


tieneMateriales :: [String] -> [String] -> Bool
tieneMateriales xs ys = all (\x -> elem x ys) xs

tieneMaterial :: String -> [String] -> Bool
tieneMaterial material listaDeMateriales 
 | elem material listaDeMateriales = True 
 | otherwise = False 

tieneMaterialesJugador :: Receta -> Jugador -> Bool
tieneMaterialesJugador receta jugador = tieneMateriales (materiales receta) (objetos jugador) 

quitarUnaVez :: Eq a => [a] -> [a] -> [a] -- -> NO FUNCIONA PUES NO HACE NADA
quitarUnaVez _ [] = []
quitarUnaVez material (m:ms)
  | m `elem` material = quitarUnaVez material ms
  | otherwise = m : quitarUnaVez material ms

quitarUnaVezUnaOcurrencia :: Eq a => [a] -> [a] -> [a]
quitarUnaVezUnaOcurrencia _ [] = []        -- Caso base: Si la lista está vacía, no hay más elementos para verificar, así que se devuelve una lista vacía.
quitarUnaVezUnaOcurrencia [] lista = lista -- Caso base: Si la lista de elementos a eliminar está vacía, se devuelve la lista sin cambios.
quitarUnaVezUnaOcurrencia (x:xs) (m:ms)
  | x == m = ms                            -- Si el primer elemento a eliminar 'x' es igual a 'm', se omite 'm' y se llama recursivamente a 'quitarUnaVezUnaOcurrencia' con el resto de la lista 'ms'.
  | otherwise = m : quitarUnaVezUnaOcurrencia (x:xs) ms 
--------------------------------------------------------------------
eliminarObjetos1 :: Receta -> Jugador -> Jugador 
eliminarObjetos1 receta jugador = jugador {objetos = filter (\x -> notElem x (materiales receta)) (objetos jugador) }
eliminarObjetos2 :: Receta -> Jugador -> Jugador 
eliminarObjetos2 receta jugador = jugador {objetos = quitarUnaVezUnaOcurrencia (materiales receta) (objetos jugador)}

-----punto 2 -----------
objetosDuplicarPuntos :: Jugador -> Receta -> Bool
objetosDuplicarPuntos jugador receta 
 | puntos (craftearObjetos1 receta jugador) > 2 * puntos jugador = True
 | otherwise = False

busquedaDeObejetos :: [Receta] -> Jugador -> [String]
busquedaDeObejetos listaDeRecetas jugador = map resultado (filter (objetosDuplicarPuntos jugador) listaDeRecetas)

craftearSucesivamente :: [Receta] -> Jugador -> Jugador 
craftearSucesivamente [] jugador = jugador
craftearSucesivamente (x:xs) jugador =  craftearSucesivamente xs (craftearObjetos1 x jugador)
------------------------parte dos del tp ----------------------------------
data Bioma = UnBioma {
  materialNecesario :: String,
  materialesDelBioma :: [Materiales]
} deriving (Show,Ord,Eq)
type Herramienta = String
pico,hacha,espada :: Herramienta
pico = "pico"
hacha = "hacha"
espada = "espada"

artico,desierto :: Bioma 
artico = UnBioma "sueter" [hielo,iglues,lobos]
desierto = UnBioma "sombrero" [arena,plantas]

---funciones principales ---------------
minar :: Herramienta -> Bioma -> Jugador -> Jugador
minar herramienta bioma jugador 
 | tieneMaterial (materialNecesario bioma) (objetos jugador) = modificarPuntos (agregarMaterialSegun herramienta bioma jugador) 50
 | otherwise = jugador 

agregarMaterial :: Jugador -> String -> Jugador
agregarMaterial jugador material = jugador {objetos = material:objetos jugador}

agregarMaterialSegun :: Herramienta -> Bioma ->Jugador -> Jugador 
agregarMaterialSegun herramienta bioma jugador 
 | herramienta == "hacha"  = agregarMaterial jugador (last (materialesDelBioma bioma))
 | herramienta == "espada" = agregarMaterial jugador (head (materialesDelBioma bioma)) 
 | otherwise = jugador 
-- si cuenta con el elemento necesario, agrega a su inventario uno de los materiales del bioma y gana 50 puntos.
-- La forma de elegir cuál es el material del bioma a conseguir, depende de la herramienta que use al minar.
-- Por ejemplo, el hacha hace que se mine el último de los materiales del bioma,
-- mientras que la espada actúa sobre el primero de ellos. Existe tambien el pico,
-- que por ser más preciso permite apuntar a una determinada posición de los materiales.
-- Por ejemplo, si un personaje con un sueter en su inventario mina el artico con un pico de precisión 1,
-- agrega un iglú a su inventario.
-- En caso de no poder minar por no tener lo necesario el personaje se va con las manos vacías y sigue como antes.