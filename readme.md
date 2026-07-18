# Analizador y Parser de Grafos en Racket y Haskell

Este repositorio contiene una implementación en **Racket y Haskell** de un analizador y clasificador de grafos, portado directamente desde una implementación original en Haskell. El programa es capaz de leer representaciones de grafos desde un archivo de texto, parsear su estructura matemática y evaluar de forma automatizada múltiples propiedades teóricas de los mismos.

## 🚀 Características y Propiedades Evaluadas

El programa analiza de forma centralizada cada grafo provisto y determina si cumple con las siguientes clasificaciones de la teoría de grafos:

*   **Grafo Nulo:** No posee vértices ni aristas.
*   **Grafo Vacío:** Posee uno o más vértices pero ninguna arista.
*   **Grafo Trivial:** Posee exactamente un vértice y ninguna arista.
*   **Grafo Simple:** No contiene bucles (aristas de un nodo a sí mismo) ni aristas paralelas (múltiples aristas entre los mismos dos nodos).
*   **Grafo Conexo:** Todos los vértices son alcanzables entre sí. Se determina utilizando una búsqueda en profundidad (**DFS**).
*   **Grafo Completo:** Cada par de vértices distintos está conectado por una arista única ($n(n-1)/2$ aristas).
*   **Grafo Bipartido:** Los vértices pueden dividirse en dos conjuntos disjuntos de modo que no haya aristas entre vértices del mismo conjunto. Se evalúa mediante un algoritmo de coloreo bicolor por búsqueda en anchura (**BFS**).
*   **Grafo Bipartido Completo:** Un grafo bipartido donde cada vértice del primer conjunto está conectado con todos los vértices del segundo conjunto.
*   **Árbol:** Un grafo conexo que posee exactamente $n-1$ aristas.
*   **Grafo Euleriano:** Un grafo donde todos sus vértices tienen un grado par.

## 📋 Prerrequisitos

Para ejecutar este proyecto, necesitas tener instalado **Racket o Haskell** en tu sistema (versión 8.0 o superior recomendada).

*   [Descargar e instalar Racket](https://racket-lang.org/)
*   [Descargar e instalar Haskell](https://www.haskell.org/downloads/)

## 📂 Estructura del Proyecto

```text
├── Grafos-Parser.rkt  # Código fuente principal en Racket
├── Grafos-Parser.hs  # Código fuente principal en Racket
├── Data.io            # Archivo de entrada con los grafos a analizar
└── README.md          # Documentación del proyecto