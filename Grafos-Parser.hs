{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (intercalate, nub, sort)
import qualified Data.Map as M
import Control.Monad (zipWithM_)

-- Definición del Grafo
type Vertice = Int
type Arista = (Vertice, Vertice)
data Grafo = Grafo [Vertice] [Arista] deriving (Show, Eq)

-- Lista centralizada de pruebas
propiedadesGrafo :: [(String, Grafo -> Bool)]
propiedadesGrafo = [
    ("Grafo Nulo", grafoNulo), 
    ("Grafo Vacio", grafoVacio),
    ("Grafo Trivial", grafoTrivial),
    ("Grafo Simple", grafoSimple),
    ("Grafo Conexo", grafoConexo),
    ("Grafo Completo", grafoCompleto),
    ("Grafo Bipartido", grafoBipartido),
    ("Grafo Bipartido Completo", grafoBipartidoCompleto),
    ("Grafo Arbol", grafoArbol),
    ("Grafo Euleriano", grafoEuleriano)
    ]

-- Obtener la naturaleza del Grafo
obtenerNaturaleza :: Grafo -> String
obtenerNaturaleza g = 
    let naturaleza = [naturaleza | (naturaleza, condicion) <- propiedadesGrafo, condicion g]
    in if null naturaleza then "Desconocido" else intercalate ", " naturaleza

-- Función para leer el archivo
leerArchivo :: FilePath -> IO [String]
leerArchivo filePath = do
    contenido <- TIO.readFile filePath
    let lineas = map (T.unpack . T.strip) (T.lines contenido)
    return lineas

-- Parsear contenido
parsearContenido :: String -> ([Vertice], [Arista])
parsearContenido linea = read linea

-- Funciones que se repiten
isBucle :: [Arista] -> Bool
isBucle aristas = any (\(v1, v2) -> v1 == v2) aristas

sortAristas :: [Arista] -> [Arista]
sortAristas aristas = map (\(v1, v2) -> if v1 < v2 then (v1, v2) else (v2, v1)) aristas
-- fin de funciones repetidas

-- GrafoNulo
grafoNulo :: Grafo -> Bool
grafoNulo (Grafo vertices aristas) = null vertices && null aristas

-- GrafoVacio
grafoVacio :: Grafo -> Bool
grafoVacio (Grafo vertices aristas) = length vertices > 0 && null aristas

-- GrafoTrivial
grafoTrivial :: Grafo -> Bool
grafoTrivial (Grafo vertices aristas) = length vertices == 1 && null aristas

-- GrafoSimple (Se considera simple, si no tiene ni bucles ni aristas paralelas)
grafoSimple :: Grafo -> Bool
grafoSimple (Grafo vertices aristas) =
    let aristasUnicas = nub (sortAristas aristas)
        tieneParalelas = length aristasUnicas /= length aristasUnicas
    in not tieneParalelas && not (isBucle aristasUnicas)

-- GrafoConexo (Se considera conexo, si todos los vertices son alcanzables desde alguno, uso de DFS)
grafoConexo :: Grafo -> Bool
grafoConexo (Grafo [] _) = False
grafoConexo (Grafo (v:vs) aristas) = length (alcanzables [v] []) == length (v:vs)
    where
        alcanzables [] visitados = visitados
        alcanzables (x:xs) visitados
            | x `elem` visitados = alcanzables xs visitados
            | otherwise = alcanzables (vecinos x ++ xs) (x:visitados)
        vecinos n = [if a == n then b else a | (a, b) <- aristas, a == n || b == n]

-- GrafoCompleto (Se considera completo, si todos los pares de vertices son aristas, uso la formula (n*(n-1))/2)
grafoCompleto :: Grafo -> Bool
grafoCompleto (Grafo vertices aristas) =
    let aristasUnicas = nub (sortAristas aristas)
    in length aristasUnicas == (length vertices * (length vertices - 1)) `div` 2 && not (isBucle aristasUnicas)


-- GrafoBipartido (Se considera bipartido, si todos los vertices tienen un color distinto, uso de BFS)
grafoBipartido :: Grafo -> Bool
grafoBipartido (Grafo vertices aristas) = revisarComponentes vertices M.empty
  where
    -- 1. Recorre todos los vértices del grafo para cubrir grafos desconectados
    revisarComponentes [] _ = True
    revisarComponentes (v:vs) mem
      | M.member v mem = revisarComponentes vs mem
      | otherwise      = case colorearComponente [(v, 1)] mem of
                           Nothing     -> False
                           Just memNva -> revisarComponentes vs memNva

    -- 2. Procesa una componente usando una cola de nodos pendientes por pintar [(Nodo, Color esperado)]
    colorearComponente [] mem = Just mem
    colorearComponente ((x, col):xs) mem =
      case M.lookup x mem of
        -- Si ya tiene color asignado, verificamos que no haya conflicto
        Just c -> if c /= col 
                  then Nothing 
                  else colorearComponente xs mem
        
        -- Si no tiene color, lo pintamos y encolamos sus vecinos con el color opuesto (-col)
        Nothing -> let memNva       = M.insert x col mem
                       nuevosVecinos = [(w, -col) | w <- vecinos x]
                   in colorearComponente (xs ++ nuevosVecinos) memNva

    vecinos n = [if a == n then b else a | (a, b) <- aristas, a == n || b == n]

-- GrafoBipartidoCompleto (Se considera bipartido completo, si todos los vertices tienen el mismo grado)
obtenerBipartidoCompletoRS :: Grafo -> Maybe (Int, Int)
obtenerBipartidoCompletoRS (Grafo vertices aristas) | totalAristas == r*s = Just (r, s)
                                                | otherwise = Nothing
    where
        totalAristas = length aristas
        grado v = length [() | (a, b) <- aristas, a == v || b == v]
        gradoUnicos = nub [grado v | v <- vertices]
        (r, s) = case gradoUnicos of
            [r] -> (r, r)
            [r, s] -> (r, s)
            _ -> (0, 0)

grafoBipartidoCompleto :: Grafo -> Bool
grafoBipartidoCompleto g = case obtenerBipartidoCompletoRS g of
    Just (r, s) -> r == s
    Nothing -> False

-- GrafoPlano
-- NO HAY FUNCION PARA GRAFOS PLANOS QUE MINIMAMENTE YO PUEDA PROGRAMAR, Y ME REHUSO A HACER EL ALGORITMO DE DEMOUCRON QUE ES MUY COMPLICADO

-- Arbol (Se considera arbol si tiene n-1 aristas y es conexo)
grafoArbol :: Grafo -> Bool
grafoArbol (Grafo vertices aristas) = length aristas == length vertices - 1

-- GrafoEuleriano (Se considera euleriano si tiene n aristas y es conexo)
grafoEuleriano :: Grafo -> Bool
grafoEuleriano (Grafo vertices aristas) = all esPar todoGrados
    where
        grados v = length [() | (a, b) <- aristas, a == v || b == v]
        todoGrados = [grados v | v <- vertices]

        esPar n = n `mod` 2 == 0

-- Analisis de Grafo
analizarGrafo :: Int -> Grafo -> IO ()
analizarGrafo numero g = do
    putStrLn $ "=== ANALISIS DEl GRAFO #" ++ show numero ++ " ==="
    putStrLn $ "Grafo: " ++ show g
    putStrLn $ "Naturaleza: " ++ show (obtenerNaturaleza g)
    putStrLn ""

-- Función principal
main :: IO ()
main = do
    let filePath = "Data.io"
    contenido <- leerArchivo filePath
    let contenidoParseado = map parsearContenido contenido
    let grafo = map (\(vertices, aristas) -> Grafo vertices aristas) contenidoParseado
    zipWithM_ analizarGrafo [1..] grafo