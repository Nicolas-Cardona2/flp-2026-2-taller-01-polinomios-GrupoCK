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

;;Datatypes 

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
      
;; Area del Programador -  Funciones del Taller

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

