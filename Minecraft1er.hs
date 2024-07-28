--module Minecraft1er.
type Material = String 

data Jugador = UnJugador {
    nombre :: String,
    puntaje :: Int,
    inventario :: [Material]
} deriving (Show,Ord,Eq) 

-- type para las recetas de los materiales 

data Receta = UnaReceta {
    materiales :: [Material],
    tiempo     :: Int,
    resultado :: Material
} deriving (Show,Ord,Eq)

-- MATERIALES 

fogata,fosforo, madera,polloAsado,pollo,sueter,hielo,lobos,iglues,lana,agujas,tintura :: Material
fogata = "fogata"
fosforo = "fosforo"
madera = "madera"
pollo = "pollo"
polloAsado = "pollo asado"
sueter = "sueter"
hielo = "hielo"
iglues = "iglues"
lobos = "lobos"
lana = "lana"
agujas = "agujas"
tintura = "tintura"


--- RECETAS DE MATERIALES 

recetaFogata,recetaPollo :: Receta 
recetaFogata = UnaReceta [madera,fosforo] 10 fogata
recetaPollo  = UnaReceta [fogata,pollo] 300 polloAsado
recetaSueter = UnaReceta [lana,agujas,tintura] 600 sueter 

--- FUNCIONES PARA CRAFTEAR LOS OBJETOS 

intentarCraftear :: Receta -> Jugador -> Jugador
intentarCraftear receta jugador 
    | tienemateriales receta jugador = craftear receta jugador 
    | otherwise = alterarPuntaje (-100) jugador

--La función craftear toma una Receta como primer argumento 
--y devuelve una función que espera recibir un Jugador como segundo argumento.
--Es decir, la función craftear se ha parcialmente aplicado con la Receta dada,
--y ahora se comporta como una función que solo necesita el Jugador para producir un resultado.
--Cuando llamas a craftear receta, obtienes una función que espera recibir un Jugador.
--Ahora, tomas un Jugador y lo pasas a la función resultante. Esta función realiza una serie de pasos a través de la composición de funciones:
--a. Primero, se aplica la función quitarMateriales (materiales receta) al Jugador.
--Esta función toma los materiales de la receta y los elimina del inventario del Jugador,
--produciendo así un nuevo Jugador que ya no tiene los materiales que requiere la receta.
--b. A continuación, se aplica la función agregarMaterial (resultado receta) al Jugador actualizado 
--después del paso anterior. 
--Esta función agrega el material resultante del crafteo al inventario del Jugador,
--generando otro nuevo Jugador con el material resultante agregado.
--c. Por último, se aplica la función alterarPuntaje (10 * tiempo receta) al Jugador resultante del paso anterior.
--Esta función aumenta el puntaje del Jugador en función del tiempo de la receta multiplicado por 10,
--produciendo así el Jugador final después del crafteo.
--En resumen, la función craftear se aplica primero a la Receta, lo que da como resultado una función que espera un Jugador. Luego, esta función toma un Jugador y pasa por una secuencia de pasos para modificarlo y producir un nuevo Jugador que representa el resultado después del crafteo de la receta dada. Cada paso se realiza a través de la composición de funciones, donde el resultado de una función se convierte en el argumento de la siguiente función en la secuencia.

-- craftear agrega un material, quita materiales y suma punto s un jugador 
craftear :: Receta -> Jugador -> Jugador 
craftear receta jugador = alterarpuntaje (10*tiempo receta).agregarMaterial(resultado receta).quitarMaterial()

alterarPuntaje :: Int -> Jugador -> Jugador 
alterarPuntaje numero jugador = jugador {puntaje = puntaje jugador + (numero)}

