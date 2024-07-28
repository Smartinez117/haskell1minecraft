--pero que util es tener estas funciones declaradas para el tratamiento de listas 
--elimina un elemento de una lista 
import NewPractice
filtrarLista :: [Int] -> Int -> [Int]
filtrarLista lista1 numero = filter(\x -> x == numero) lista1


--esta funcion elimina los elementos de la lista1 en la lista2 
eliminarElementos :: [Int] -> [Int] -> [Int]
eliminarElementos lista1 lista2 = filter (\x -> notElem x lista1) lista2


-- esta funcion elimina los elementos de la lista 2 que no estan en la lista1 
eliminarElementos1 :: [Int] -> [Int] -> [Int]
eliminarElementos1 lista1 lista2 = filter (\x -> elem x lista1) lista2
----- verifica si los elementos de una lista estan en la otra
areAllElementsInList :: [Int] -> [Int] -> Bool
areAllElementsInList xs ys = all (\x -> elem x ys) xs
---- elimina los elemenyos repetidos en una lista 
eliminarElementosRepetidos1 :: [String] -> [String]
eliminarElementosRepetidos1[] = []
eliminarElementosRepetidos1 (x:xs) = x : eliminarElementosRepetidos1 (filter (\y -> y /= x) xs)
---funcion mejora de la anterior 
eliminarElementosRepetidos2 :: [String] -> [String]
eliminarElementosRepetidos2 [] = []
eliminarElementosRepetidos2 (x:xs) = x : eliminarElementosRepetidos2 (filter (/= x) xs)

--La sintaxis Eq a => se refiere a una clase de tipos en Haskell llamada Eq.
--Esta clase se utiliza para tipos cuyos valores se pueden comparar para la igualdad y la desigualdad. 
--En otras palabras,
--los tipos que son instancias de la clase Eq son aquellos para los cuales podemos usar los operadores == (igual) y /= (diferente).
-- QUITA UNA SOLA VEZ UN ELEMENTO GENERICO EN UNA LISTA 
quitarUnaVez1:: Eq a => a -> [a] -> [a]
quitarUnaVez1 _ [] = []
quitarUnaVez1 material (m:ms)  
 | material == m = ms
 | otherwise = m:quitarUnaVez1 material ms 
 -- ejemplo de funcionamiento
--quitarUnaVez1 2 [1, 2, 3, 1, 4, 5, 2]
-- Resultado: [1, 3, 1, 4, 5, 2]

---- ahora vamos a hacerlo mas generico donde quiza una vez los elementos de una lista de otra 
quitarUnaVez :: Eq a => [a] -> [a] -> [a]
quitarUnaVez _ [] = []
quitarUnaVez material (m:ms)
  | m `elem` material = quitarUnaVez material ms
  | otherwise = m : quitarUnaVez material ms
-- ejemplo de prueba
-- quitarUnaVez [1, 2] [1, 2, 3, 1, 4, 5, 2]
-- Resultado: [3, 4, 5]


tomarElementoLista :: [String] -> String
tomarElementoLista lista = last lista 
-------------------tratamiento de listas ------------------
agregarHabilidad :: String -> Cazador -> Cazador
agregarHabilidad nuevahabilidad cazador = cazador {habilidades = nuevahabilidad:(habilidades cazador)}

agregarHabilidades :: [String] -> Cazador -> Cazador 
agregarHabilidades [] cazador = cazador
agregarHabilidades (habilidad:restoHabilidades) cazador = agregarHabilidades restoHabilidades (agregarHabilidad habilidad cazador) 
-----nueva version mejorada ----------------
agregarNuevasHabilidades :: [String] -> Cazador -> Cazador 
agregarNuevasHabilidades nuevasHabilidades cazador = quitarHabilidadesRepetidas(agregarHabilidades nuevasHabilidades cazador)
------------version inversa de agregar Habilidades -------------
quitarHabilidadesRepetidas :: Cazador -> Cazador 
quitarHabilidadesRepetidas cazador = cazador { habilidades = eliminarHabilidadesRepetidas (habilidades cazador) }
------------------------tratamiento de lista con recursividad -----------------------
eliminarHabilidadesRepetidas :: [String] -> [String]
eliminarHabilidadesRepetidas [] = []
eliminarHabilidadesRepetidas (x:xs) = x : eliminarHabilidadesRepetidas (filter (/= x) xs)
------(filter (/= x) xs) filtra los elemntos que son distintos de x que es lo mismo aunque es mas facil que usar las funcion lambda para definir una nueva funcion en medio de todas las demas funciones 

--------------------------otras versiones con interesantes funciones-----------------------
eliminarHabilidadesRepetidas1 :: [String] -> [String]
eliminarHabilidadesRepetidas1 [] = []
eliminarHabilidadesRepetidas1 (x:xs) = x : eliminarHabilidadesRepetidas (filter (\y -> y /= x) xs)
-- funcion usanod la funcion lambda pero no es tan eficiente como la anterior puede no usa la funcion lambda comon tal asiqeu mejor usa la anteriro 
quitarHabilidad :: [String] -> [String] -> [String]
quitarHabilidad habilidad habilidades = filter (\x -> notElem x habilidad) habilidades

quitarHabilidadesCazador :: [String] -> Cazador -> Cazador 
quitarHabilidadesCazador habilidadesQuitar cazador = cazador {habilidades = quitarHabilidad habilidadesQuitar (habilidades cazador)}

------------------ version mejorada de la funcion de arriba para evitar el uso de varias funciones-----------------------------

quitarHabilidadesCazador1 :: [String] -> Cazador -> Cazador 
quitarHabilidadesCazador1 habilidadesQuitar cazador = cazador {habilidades = filter(\x -> notElem x habilidadesQuitar) (habilidades cazador)}

-- funcion de primer orden -----------
-- se juegan con los parametros de entrada y salida de las funciones que se reciben como parametros y la conposicon de funciones
situacionMultipleCazador:: (a -> b)->(c -> b ->d)-> a -> c ->d 
situacionMultipleCazador funcion1 funcion2 valorfuncion1 valorfuncion2 = funcion2 valorfuncion2 (funcion1 valorfuncion1)
--funcion1 entrenamientoDePilar funcion 2 agregar hablidada

----- los errores son por las falta de la declaracion de los data 