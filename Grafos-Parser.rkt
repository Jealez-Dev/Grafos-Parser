#lang racket

(require racket/list
         racket/set
         racket/string
         racket/match)

;; Definición del Grafo
(struct grafo (vertices aristas) #:transparent)

;; Funciones que se repiten
(define (is-bucle aristas)
  (ormap (lambda (a) (equal? (car a) (cadr a))) aristas))

(define (sort-aristas aristas)
  (map (lambda (a)
         (if (< (car a) (cadr a))
             a
             (list (cadr a) (car a))))
       aristas))
;; fin de funciones repetidas


;; GrafoNulo
(define (grafo-nulo? g)
  (and (empty? (grafo-vertices g)) 
       (empty? (grafo-aristas g))))

;; GrafoVacio
(define (grafo-vacio? g)
  (and (> (length (grafo-vertices g)) 0) 
       (empty? (grafo-aristas g))))

;; GrafoTrivial
(define (grafo-trivial? g)
  (and (= (length (grafo-vertices g)) 1) 
       (empty? (grafo-aristas g))))

;; GrafoSimple (Se considera simple, si no tiene ni bucles ni aristas paralelas)
(define (grafo-simple? g)
  (let* ([aristas (grafo-aristas g)]
         [aristas-unicas (remove-duplicates (sort-aristas aristas))]
         ;; Nota: Se corrigió la comparación que en Haskell era length vs length del mismo elemento.
         [tiene-paralelas? (not (= (length aristas-unicas) (length aristas)))])
    (and (not tiene-paralelas?) 
         (not (is-bucle aristas-unicas)))))

;; GrafoConexo (Se considera conexo, si todos los vertices son alcanzables desde alguno, uso de DFS)
(define (grafo-conexo? g)
  (define vertices (grafo-vertices g))
  (define aristas (grafo-aristas g))
  (if (empty? vertices)
      #f
      (let ([v (car vertices)])
        (define (vecinos n)
          (filter-map (lambda (a)
                        (cond [(equal? (car a) n) (cadr a)]
                              [(equal? (cadr a) n) (car a)]
                              [else #f]))
                      aristas))
        (define (alcanzables frontera visitados)
          (if (empty? frontera)
              visitados
              (let ([x (car frontera)]
                    [xs (cdr frontera)])
                (if (member x visitados)
                    (alcanzables xs visitados)
                    (alcanzables (append (vecinos x) xs) (cons x visitados))))))
        (= (length (alcanzables (list v) '())) (length vertices)))))

;; GrafoCompleto (Se considera completo, si todos los pares de vertices son aristas, uso la formula (n*(n-1))/2)
(define (grafo-completo? g)
  (let* ([vertices (grafo-vertices g)]
         [aristas (grafo-aristas g)]
         [aristas-unicas (remove-duplicates (sort-aristas aristas))]
         [n (length vertices)])
    (and (= (length aristas-unicas) (/ (* n (- n 1)) 2)) 
         (not (is-bucle aristas-unicas)))))

;; GrafoBipartido (Se considera bipartido, si todos los vertices tienen un color distinto, uso de BFS)
(define (grafo-bipartido? g)
  (define vertices (grafo-vertices g))
  (define aristas (grafo-aristas g))
  
  (define (vecinos n)
    (filter-map (lambda (a)
                  (cond [(equal? (car a) n) (cadr a)]
                        [(equal? (cadr a) n) (car a)]
                        [else #f]))
                aristas))
  
  ;; 1. Recorre todos los vértices del grafo para cubrir grafos desconectados
  (define (revisar-componentes vs mem)
    (if (empty? vs)
        #t
        (let ([v (car vs)])
          (if (hash-has-key? mem v)
              (revisar-componentes (cdr vs) mem)
              (let ([mem-nva (colorear-componente (list (cons v 1)) mem)])
                (if mem-nva
                    (revisar-componentes (cdr vs) mem-nva)
                    #f))))))
  
  ;; 2. Procesa una componente usando una cola de nodos pendientes por pintar
  (define (colorear-componente cola mem)
    (if (empty? cola)
        mem
        (let* ([item (car cola)]
               [x (car item)]
               [col (cdr item)]
               [xs (cdr cola)])
          (if (hash-has-key? mem x)
              ;; Si ya tiene color asignado, verificamos que no haya conflicto
              (if (not (= (hash-ref mem x) col))
                  #f
                  (colorear-componente xs mem))
              ;; Si no tiene color, lo pintamos y encolamos sus vecinos con el color opuesto
              (let* ([mem-nva (hash-set mem x col)]
                     [nuevos-vecinos (map (lambda (w) (cons w (- col))) (vecinos x))])
                (colorear-componente (append xs nuevos-vecinos) mem-nva))))))
  
  (revisar-componentes vertices (hash)))

;; GrafoBipartidoCompleto (Se considera bipartido completo, si todos los vertices tienen el mismo grado)
(define (obtener-bipartido-completo-rs g)
  (define vertices (grafo-vertices g))
  (define aristas (grafo-aristas g))
  (define total-aristas (length aristas))
  
  (define (grado v)
    (count (lambda (a) (or (equal? (car a) v) (equal? (cadr a) v))) aristas))
  
  (define grados-unicos (remove-duplicates (map grado vertices)))
  
  (define-values (r s)
    (match grados-unicos
      [(list x) (values x x)]
      [(list x y) (values x y)]
      [_ (values 0 0)]))
  
  (if (= total-aristas (* r s))
      (cons r s)
      #f))

(define (grafo-bipartido-completo? g)
  (let ([rs (obtener-bipartido-completo-rs g)])
    (if rs
        (= (car rs) (cdr rs))
        #f)))

;; GrafoPlano
;; NO EXISTE ACA TAMPOCO JAJAJAJAJJAJAJA (Perdonadme Arabia por mi incapacidad para crear el algoritmo)

;; Arbol (Se considera arbol si tiene n-1 aristas y es conexo)
(define (grafo-arbol? g)
  (= (length (grafo-aristas g)) 
     (- (length (grafo-vertices g)) 1)))

;; GrafoEuleriano (Se considera euleriano si tiene n aristas y es conexo)
(define (grafo-euleriano? g)
  (define vertices (grafo-vertices g))
  (define aristas (grafo-aristas g))
  
  (define (grado v)
    (count (lambda (a) (or (equal? (car a) v) (equal? (cadr a) v))) aristas))
  
  (define (es-par? n) (= (modulo n 2) 0))
  
  (andmap es-par? (map grado vertices)))

;; Lista centralizada de pruebas
(define propiedades-grafo
  (list
   (cons "Grafo Nulo" grafo-nulo?)
   (cons "Grafo Vacio" grafo-vacio?)
   (cons "Grafo Trivial" grafo-trivial?)
   (cons "Grafo Simple" grafo-simple?)
   (cons "Grafo Conexo" grafo-conexo?)
   (cons "Grafo Completo" grafo-completo?)
   (cons "Grafo Bipartido" grafo-bipartido?)
   (cons "Grafo Bipartido Completo" grafo-bipartido-completo?)
   (cons "Grafo Arbol" grafo-arbol?)
   (cons "Grafo Euleriano" grafo-euleriano?)))

;; Obtener la naturaleza del Grafo
(define (obtener-naturaleza g)
  (define naturaleza
    (filter-map (lambda (prop)
                  (if ((cdr prop) g) (car prop) #f))
                propiedades-grafo))
  (if (empty? naturaleza)
      "Desconocido"
      (string-join naturaleza ", ")))

;; Función para leer el archivo
(define (leer-archivo file-path)
  (if (file-exists? file-path)
      (file->lines file-path)
      (error "El archivo no existe")))

;; Parsear contenido
(define (parsear-contenido linea)
  (define clean-line 
    (string-replace 
     (string-replace 
      (string-replace linea "[" "(") 
      "]" ")") 
     "," " "))
  (let ([sexp (with-input-from-string clean-line read)])
    ;; Retorna la estructura grafo con la lista de vértices y la lista de aristas
    (grafo (car sexp) (cadr sexp))))

;; Analisis de Grafo
(define (analizar-grafo numero g)
  (printf "=== ANALISIS DEL GRAFO #~a ===\n" numero)
  (printf "Grafo: ~v\n" g)
  (printf "Naturaleza: ~v\n\n" (obtener-naturaleza g)))

;; Función principal
(define (main)
  (define file-path "Data.io")
  (let* ([contenido (leer-archivo file-path)]
         [grafos (map parsear-contenido contenido)])
    (for ([g grafos]
          [i (in-naturals 1)])
      (analizar-grafo i g))))

;; Llamar a la función principal
(main)