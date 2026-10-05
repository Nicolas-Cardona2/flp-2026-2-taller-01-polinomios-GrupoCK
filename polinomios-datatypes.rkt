#lang eopl
;Autores: Santiago Serrano Morales 202477006

;; Taller 1 — Polinomios dispersos.
;; Parte 3: representación con datatypes.
;;
;; Interfaz del TAD. Cada función va comentada con su nombre, su contrato
;; (entrada -> salida) y su propósito, y ninguna recorre la lista de términos
;; más de una vez ni la ordena al final.
;;
;;   polinomio-cero    : symbol -> polinomio
;;   insertar-termino  : polinomio x coeficiente x exponente -> polinomio
;;   coeficiente-de    : polinomio x exponente -> coeficiente
;;   eliminar-termino  : polinomio x exponente -> polinomio
;;   sumar             : polinomio x polinomio -> polinomio


;;Interfaz

;;Los Datatypes 

(define-datatype variable variable?
  (nombre-var
   (s symbol?)))

(define-datatype terminos terminos?
  (sin-terminos)
  (mas-terminos
   (term termino?)
   (resto terminos?)))

(define-datatype termino-tad termino?
  (termino
   (coef coeficiente?)
   (expo exponente?)))

(define-datatype coeficiente coeficiente?
  (coef-ent
   (n integer?))
  (coef-rac
   (num integer?)
   (den integer?)))

(define-datatype exponente exponente?
  (expo-nat
   (k integer?)))

(define-datatype polinomio polinomio?
  (poli
   (var variable?)
   (terms terminos?)))

;;Funciones Auxiliares - Insertar Termino

;; hacer-coeficiente
;; Contrato: numero -> coeficiente
;; Proposito: convertir un numero al tipo de coeficiente que usamos

(define hacer-coeficiente
  (lambda (c)
    (cond
      [(integer? c) (coef-ent c)]
      [else
       (coef-rac (numerator c) (denominator c))])))

;; valor-coeficiente
;; Contrato: coeficiente -> numero
;; Proposito: sacar el valor del coeficiente

(define valor-coeficiente
  (lambda (c)
    (cases coeficiente c
      (coef-ent (n) n)
      (coef-rac (num den)
        (/ num den)))))


;; valor-exponente
;; Contrato: termino -> numero
;; Proposito: sacar el exponente de un termino

(define valor-exponente
  (lambda (t)
    (cases termino-tad t
      (termino (c e)
        (cases exponente e (expo-nat (k) k))))))


;; valor-coef
;; Contrato: termino -> numero
;; Proposito: sacar el coeficiente de un termino

(define valor-coef
  (lambda (t)
    (cases termino-tad t
      (termino (c e)
        (valor-coeficiente c)))))


;; hacer-termino
;; Contrato: numero x numero -> termino
;; Proposito: crear un termino con el coeficiente y el exponente recibidos

(define hacer-termino
  (lambda (coef expo)
    (termino
     (hacer-coeficiente coef)
     (expo-nat expo))))


;; insertar-terminos
;; Contrato: terminos x numero x numero -> terminos
;; Proposito: insertar un termino en la posicion que le corresponde

(define insertar-terminos
  (lambda (ts coef expo)
    (cases terminos ts
      ;; Si no hay terminos, agregamos el nuevo
      (sin-terminos ()
        (mas-terminos
         (hacer-termino coef expo)
         (sin-terminos)))
      ;; Si ya hay terminos, comparamos los exponentes
      (mas-terminos (termino-actual resto)
        (let ((expo-actual (valor-exponente termino-actual)))
          (cond
            ;; El nuevo termino va antes del actual
            [(> expo expo-actual) (mas-terminos (hacer-termino coef expo) ts)]
            ;; El termino actual se queda y seguimos buscando
            [(< expo expo-actual) (mas-terminos termino-actual (insertar-terminos resto coef expo))]
            ;; Los exponentes son iguales
            [else
             (let ((suma (+ (valor-coef termino-actual) coef)))
               ;; Si la suma da cero, el termino desaparece
               (if (zero? suma)
                   resto
                   ;; Si no da cero, dejamos el termino con la suma
                   (mas-terminos (hacer-termino suma expo-actual) resto)))]))))))

;; Funciones Auxiliares - Coeficiente de

;; buscar-coef
;; Contrato: terminos x numero -> numero
;; Proposito: buscar el coeficiente del exponente indicado

(define buscar-coef
  (lambda (ts expo)
    (cases terminos ts
      (sin-terminos () (eopl:error 'coeficiente-de "El polinomio no tiene termino con ese exponente"))
      (mas-terminos (termino-actual resto)
                    (let ((expo-actual (valor-exponente termino-actual)))
                          (cond
                            [(= expo expo-actual) (valor-coef termino-actual)]
                            [(> expo expo-actual) (eopl:error 'coeficiente-de "El polinomio no tiene termino con ese exponente")]
                            [else
                             (buscar-coef resto expo)]))))))

   
;; Funciones Auxiliares - Eliminar Termino

;; eliminacion-terminos
;; Contrato: terminos x numero -> terminos
;; Proposito: buscar y eliminar el termino con el exponente indicado

(define eliminacion-terminos
  (lambda (ts expo)
    (cases terminos ts
      (sin-terminos ()
        (eopl:error 'eliminar-termino "El polinomio no tiene termino con ese exponente"))
      (mas-terminos (termino-actual resto)
                    (let ((expo-actual (valor-exponente termino-actual)))
                      (cond
                        [(= expo expo-actual) resto]
                        [(> expo expo-actual) (eopl:error 'eliminar-termino "El polinomio no tiene termino con ese exponente")]
                        [else
                         (mas-terminos termino-actual
                          (eliminacion-terminos resto expo))]))))))
                                                
    

;; Funciones Auxiliares - Sumar

;; sumar-terminos
;; Contrato: terminos x terminos -> terminos
;; Proposito: sumar los terminos de dos polinomios

(define sumar-terminos
  (lambda (ts1 ts2)
    (cases terminos ts1
      ;; Si la primera lista se llega a acabar, se dejan los terminos de la segunda
      (sin-terminos () ts2)
      ;; Si todavia hay terminos en la primera lista
      (mas-terminos (term1 rest1)
                    (cases terminos ts2
                      ;; En caso de que la segunda lista se acabe, se dejan los terminos de la primera
                      (sin-terminos () ts1)
                      ;;Si las dos listas tienen terminos
                      (mas-terminos (term2 rest2)
                                    (let ((expo1 (valor-exponente term1)) (expo2 (valor-exponente term2)))
                                      (cond
                                        [(> expo1 expo2) (mas-terminos term1 (sumar-terminos rest1 ts2))]
                                        [(< expo1 expo2) (mas-terminos term2 (sumar-terminos ts1 rest2))]
                                        [else
                                         (let ((suma
                                                (+ (valor-coef term1) (valor-coef term2))))
                                           (if (zero? suma)
                                               (sumar-terminos rest1 rest2)
                                               (mas-terminos
                                                (hacer-termino suma expo1)
                                                (sumar-terminos rest1 rest2))))]))))))))
      
;; Area del Programador -  Las funciones del Taller

;;(provide polinomio-cero insertar-termino coeficiente-de eliminar-termino sumar)
(provide polinomio-cero insertar-termino coeficiente-de eliminar-termino sumar nombre-var sin-terminos mas-terminos termino coef-ent coef-rac expo-nat poli)

;; Funciones

;; polinomio-cero
;; Contrato: symbol -> polinomio
;; Proposito: construir el polinomio nulo para una variable

(define polinomio-cero
  (lambda (variable)
    (poli
     (nombre-var variable)
     (sin-terminos))))

;; insertar-termino
;; Contrato: polinomio x coeficiente x exponente -> polinomio
;; Proposito: insertar un termino conservando el orden de los exponentes y combinarlo con otro termino si tienen el mismo exponente


(define insertar-termino
  (lambda (p coef expo)

    (cond
      ;; El exponente no puede ser negativo
      [(not (and (integer? expo) (>= expo 0))) (eopl:error 'insertar-termino "El exponente debe ser un entero no negativo")]
      ;; El coeficiente debe ser un numero exacto
      [(not (and (number? coef) (exact? coef) (rational? coef))) (eopl:error 'insertar-termino "El coeficiente debe ser un numero exacto")]
      ;; Si el coeficiente es cero, no hacemos ningun cambio
      [(zero? coef) p]
      [else
       (cases polinomio p
         (poli (var terms)
           (poli var
            (insertar-terminos terms coef expo))))])))

;; coeficiente-de
;; Contrato: polinomio x exponente -> coeficiente
;; Proposito: obtener el coeficiente del termino con el exponente indicado

(define coeficiente-de
  (lambda (p exponente)
    (cases polinomio p
      (poli (var terms)
            (buscar-coef terms exponente)))))
    

;; eliminar-termino
;; Contrato: polinomio x exponente -> polinomio
;; Proposito: eliminar el termino que tiene el exponente indicado

(define eliminar-termino
  (lambda (p exponente)
    (cases polinomio p
      (poli (var terms)
            (poli var (eliminacion-terminos terms exponente))))))

;; sumar
;; Contrato: polinomio x polinomio -> polinomio
;; Proposito: sumar dos polinomios que estan definidos sobre la misma variable

(define sumar
  (lambda (p q)
    (cases polinomio p
      (poli (var1 terms1)
            (cases polinomio q
              (poli (var2 terms2)
                    (cases variable var1
                      (nombre-var (x1)
                      (cases variable var2
                        (nombre-var (x2)
                        (cond
                          [(not (eqv? x1 x2)) (eopl:error 'sumar "Los polinomios deben estar en la misma variable")]
                          [else (poli var1 (sumar-terminos terms1 terms2))])))))))))))

;; ============================================================
;; Ejemplos de la parte 3: Datatypes
;; ============================================================

;; ------------------------------------------------------------
;; Ejemplos de construcción con los datatypes
;; ------------------------------------------------------------

;; Ejemplo 1: construcción de una variable
(define dato-1
  (nombre-var 'u))

;; Ejemplo 2: construcción de un coeficiente entero
(define dato-2
  (coef-ent -11))

;; Ejemplo 3: construcción de un coeficiente racional
(define dato-3
  (coef-rac 7 9))

;; Ejemplo 4: construcción de un término
(define dato-4
  (termino
   (coef-ent 13)
   (expo-nat 5)))

;; Ejemplo 5: construcción completa de un polinomio
;; 4v^6 - 2v^3 + 9
(define dato-5
  (poli
   (nombre-var 'v)
   (mas-terminos
    (termino (coef-ent 4) (expo-nat 6))
    (mas-terminos
     (termino (coef-ent -2) (expo-nat 3))
     (mas-terminos
      (termino (coef-ent 9) (expo-nat 0))
      (sin-terminos))))))


;; ------------------------------------------------------------
;; Ejemplos de polinomio-cero
;; ------------------------------------------------------------

;; Ejemplo 1
(define cero-dato-1
  (polinomio-cero 'r))

;; Ejemplo 2
(define cero-dato-2
  (polinomio-cero 's))

;; Ejemplo 3
(define cero-dato-3
  (polinomio-cero 'z))


;; ------------------------------------------------------------
;; Ejemplos de insertar-termino
;; ------------------------------------------------------------

;; Ejemplo 1: insertar un término en un polinomio vacío
(define insertar-dato-1
  (insertar-termino
   (polinomio-cero 'g)
   14
   7))

;; Ejemplo 2: insertar un término con un exponente menor
(define insertar-dato-2
  (insertar-termino
   insertar-dato-1
   -6
   3))

;; Ejemplo 3: insertar otro término con el mismo exponente
;; para comprobar la suma de coeficientes.
(define insertar-dato-3
  (insertar-termino
   insertar-dato-2
   2
   3))


;; ------------------------------------------------------------
;; Ejemplos de coeficiente-de
;; ------------------------------------------------------------

;; Ejemplo 1: consultar el coeficiente del término de mayor grado
(define coeficiente-dato-1
  (coeficiente-de
   insertar-dato-3
   7))

;; Ejemplo 2: consultar el coeficiente del término combinado
(define coeficiente-dato-2
  (coeficiente-de
   insertar-dato-3
   3))

;; Ejemplo 3: consultar un término independiente
(define coeficiente-dato-3
  (coeficiente-de
   dato-5
   0))


;; ------------------------------------------------------------
;; Ejemplos de eliminar-termino
;; ------------------------------------------------------------

;; Ejemplo 1: eliminar el término de mayor exponente
(define eliminar-dato-1
  (eliminar-termino
   insertar-dato-3
   7))

;; Ejemplo 2: eliminar el término combinado
(define eliminar-dato-2
  (eliminar-termino
   insertar-dato-3
   3))

;; Ejemplo 3: eliminar el término independiente
(define eliminar-dato-3
  (eliminar-termino
   dato-5
   0))


;; ------------------------------------------------------------
;; Ejemplos de sumar
;; ------------------------------------------------------------

;; Ejemplo 1: suma de dos polinomios con exponentes diferentes
;; p = 6a^5 + 3a^2
;; q = 4a^3 + 8
;; resultado = 6a^5 + 4a^3 + 3a^2 + 8
(define suma-dato-1
  (sumar
   (poli
    (nombre-var 'a)
    (mas-terminos
     (termino (coef-ent 6) (expo-nat 5))
     (mas-terminos
      (termino (coef-ent 3) (expo-nat 2))
      (sin-terminos))))
   (poli
    (nombre-var 'a)
    (mas-terminos
     (termino (coef-ent 4) (expo-nat 3))
     (mas-terminos
      (termino (coef-ent 8) (expo-nat 0))
      (sin-terminos))))))

;; Ejemplo 2: suma de términos con el mismo exponente
;; p = 9b^4 - 2b
;; q = -9b^4 + 5b
;; resultado = 3b
(define suma-dato-2
  (sumar
   (poli
    (nombre-var 'b)
    (mas-terminos
     (termino (coef-ent 9) (expo-nat 4))
     (mas-terminos
      (termino (coef-ent -2) (expo-nat 1))
      (sin-terminos))))
   (poli
    (nombre-var 'b)
    (mas-terminos
     (termino (coef-ent -9) (expo-nat 4))
     (mas-terminos
      (termino (coef-ent 5) (expo-nat 1))
      (sin-terminos))))))

;; Ejemplo 3: suma con coeficientes racionales
;; p = 2/3c^6 + c^2
;; q = 1/3c^6 - 4
;; resultado = c^6 + c^2 - 4
(define suma-dato-3
  (sumar
   (poli
    (nombre-var 'c)
    (mas-terminos
     (termino (coef-rac 2 3) (expo-nat 6))
     (mas-terminos
      (termino (coef-ent 1) (expo-nat 2))
      (sin-terminos))))
   (poli
    (nombre-var 'c)
    (mas-terminos
     (termino (coef-rac 1 3) (expo-nat 6))
     (mas-terminos
      (termino (coef-ent -4) (expo-nat 0))
      (sin-terminos))))))

;; ------------------------------------------------------------
;; Ejemplos de error -- Adicionales a los solicitados 
;; ------------------------------------------------------------

;; Error 1: intentar insertar un exponente negativo
;; (insertar-termino (polinomio-cero 'd) 5 -2)

;; Error 2: intentar insertar un coeficiente decimal
;; (insertar-termino (polinomio-cero 'e) 2.5 4)

;; Error 3: consultar un exponente que no existe
;; (coeficiente-de dato-5 10)

;; Error 4: eliminar un exponente que no existe
;; (eliminar-termino dato-5 8)

;; Error 5: intentar sumar polinomios de variables diferentes
;; (sumar
;;  (polinomio-cero 'x)
;;  (polinomio-cero 'y))