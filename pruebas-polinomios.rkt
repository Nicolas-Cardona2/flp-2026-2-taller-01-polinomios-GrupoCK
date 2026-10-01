#lang eopl
;Autores: Santiago Serrano Morales 2477006, Nombre2 Codigo2, Nombre3 Codigo3, Nombre4 Codigo4

;; Taller 1 — Polinomios dispersos.
;; Parte 4: la misma batería de pruebas sobre las tres representaciones.

(require rackunit)
(require (prefix-in listas: "polinomios-listas.rkt"))
(require (prefix-in procs:  "polinomios-procedimientos.rkt"))
(require (prefix-in dt:     "polinomios-datatypes.rkt"))

;; Una prueba tiene esta forma; la batería completa está descrita en la
;; Parte 4 del enunciado.
;;
;; (check-equal?
;;   (listas:coeficiente-de
;;     (listas:insertar-termino (listas:polinomio-cero 'x) 7 0) 0)
;;   7)

;; =========================================================
;; PRUEBAS PARTE 1 — REPRESENTACIÓN CON LISTAS
;; Samuel
;; =========================================================

;; Construcciones de polinomios

;; Construccion del polinomio nulo 
(define mi-var (nombre-var 'y))
(define terms-nulos (sin-terminos))
(define poli-vacio (poli mi-var terms-nulos))

(define term1 (termino (coef-ent 5) (expo-nat 2)))
(define poli-un-term (poli (nombre-var 'x) (mas-terminos term1 (sin-terminos))))

(define coef-racional (coef-rac 3 4))
(define term2 (termino coef-racional (expo-nat 1)))
(define poli-racional (poli (nombre-var 'z) (mas-terminos term2 (sin-terminos))))

;; Uso de observadores 
(poli? poli-un-term) ;; Retorna #t
(nombre-var->s (poli->var poli-un-term)) ;; Retorna el símbolo 'x

(define term-extraido (mas-terminos->term (poli->terms poli-racional)))
(coef-rac->num (termino->coef term-extraido)) ;; Retorna 3
(coef-rac->den (termino->coef term-extraido)) ;; Retorna 4

;; =========================================================
;; PRUEBAS PARTE 2 — REPRESENTACIÓN CON PROCEDIMIENTOS
;; Compañero 2
;; =========================================================


;; =========================================================
;; PRUEBAS PARTE 3 — REPRESENTACIÓN CON DATATYPES
;; Santiago
;; =========================================================

;; Construcciones con los datatypes

(define dato-1
  (dt:nombre-var 'x))

(define dato-2
  (dt:coef-ent 12))

(define dato-3
  (dt:coef-rac -7 3))

(define dato-4
  (dt:termino
   (dt:coef-rac 5 4)
   (dt:expo-nat 7)))

(define dato-5
  (dt:poli
   (dt:nombre-var 'w)
   (dt:mas-terminos
    (dt:termino
     (dt:coef-ent 6)
     (dt:expo-nat 9))
    (dt:mas-terminos
     (dt:termino
      (dt:coef-rac -5 2)
      (dt:expo-nat 4))
     (dt:mas-terminos
      (dt:termino
       (dt:coef-ent 3)
       (dt:expo-nat 0))
      (dt:sin-terminos))))))

;; Polinomio Cero

(check-exn
 (lambda (e) #t)
 (lambda ()
   (dt:coeficiente-de
    (dt:polinomio-cero 'm)
    4)))

(check-equal?
 (dt:coeficiente-de
  (dt:insertar-termino
   (dt:polinomio-cero 'm)
   13
   4)
  4)
 13)

(check-exn
 (lambda (e) #t)
 (lambda ()
   (dt:coeficiente-de
    (dt:insertar-termino
     (dt:polinomio-cero 'q)
     -7/3
     5)
    2)))


;; Insertar Termino

(define p-prueba
  (dt:insertar-termino
   (dt:insertar-termino
    (dt:polinomio-cero 'r)
    7
    1)
   -5/2
   6))

;; -- Insertar un exponente en medio --
(check-equal?
 (dt:coeficiente-de
  (dt:insertar-termino p-prueba 11 4)
  4)
 11)

;; -- Insertar sobre un exponente que ya existe --
(check-equal?
 (dt:coeficiente-de
  (dt:insertar-termino p-prueba 3/2 6)
  6)
 -1)

;; -- Insertar Coeficiente cero --
(check-exn
 (lambda (e) #t)
 (lambda ()
   (dt:coeficiente-de
    (dt:insertar-termino p-prueba 0 9)
    9)))

;; -- Si los coeficientes se cancelan, el termino desaparece --
(check-exn
 (lambda (e) #t)
 (lambda ()
   (dt:coeficiente-de
    (dt:insertar-termino p-prueba 5/2 6)
    6)))

;; -- No se permite un exponente negativo --
(check-exn
 (lambda (e) #t)
 (lambda ()
   (dt:insertar-termino p-prueba 8 -2)))


;; Coeficiente de

;; -- Buscar un termino que fue agregado --
(check-equal?
 (dt:coeficiente-de
  (dt:insertar-termino p-prueba 9 10)
  10)
 9)

;; -- Buscar el termino de menor exponente --
(check-equal?
 (dt:coeficiente-de p-prueba 1)
 7)

;; -- Buscar un exponente que no existe --
(check-exn
 (lambda (e) #t)
 (lambda ()
   (dt:coeficiente-de p-prueba 8)))

;; -- Buscar el termino constante --
(check-equal?
 (dt:coeficiente-de
  (dt:insertar-termino
   (dt:polinomio-cero 't)
   15
   0)
  0)
 15)

;; Eliminar Termino

;; --Eliminar un termino y comprobar que los demas siguen --
(check-equal?
 (dt:coeficiente-de
  (dt:eliminar-termino p-prueba 6)
  1)
 7)

;; -- Eliminar el termino de menor exponente --
(check-exn
 (lambda (e) #t)
 (lambda ()
   (dt:coeficiente-de
    (dt:eliminar-termino p-prueba 1)
    1)))

;; -- Eliminar un exponente que no existe --
(check-exn
 (lambda (e) #t)
 (lambda ()
   (dt:eliminar-termino p-prueba 10)))

;; Sumar

;; -- Aqui creamos los polinomios para sumar --

(define p-suma-1
  (dt:insertar-termino
   (dt:insertar-termino
    (dt:polinomio-cero 's)
    7
    8)
   -3
   2))

(define p-suma-2
  (dt:insertar-termino
   (dt:insertar-termino
    (dt:polinomio-cero 's)
    4
    5)
   3
   2))

;; -- Comprobamos los terminos que permanecen --

(check-equal?
 (dt:coeficiente-de
  (dt:sumar p-suma-1 p-suma-2)
  8)
 7)

(check-equal?
 (dt:coeficiente-de
  (dt:sumar p-suma-1 p-suma-2)
  5)
 4)

;; -- Comprobamos el que desaparecen

(check-exn
 (lambda (e) #t)
 (lambda ()
   (dt:coeficiente-de
    (dt:sumar p-suma-1 p-suma-2)
    2)))

;; -- Una suma que se cancela completamente

(define p-cancelar-1
  (dt:insertar-termino
   (dt:insertar-termino
    (dt:polinomio-cero 'v)
    6
    9)
   -4
   4))

(define p-cancelar-2
  (dt:insertar-termino
   (dt:insertar-termino
    (dt:polinomio-cero 'v)
    -6
    9)
   4
   4))

(check-exn
 (lambda (e) #t)
 (lambda ()
   (dt:coeficiente-de
    (dt:sumar p-cancelar-1 p-cancelar-2)
    9)))

(check-exn
 (lambda (e) #t)
 (lambda ()
   (dt:coeficiente-de
    (dt:sumar p-cancelar-1 p-cancelar-2)
    4)))

;; -- Variables diferentes -- ( Se espera un Error )
(check-exn
 (lambda (e) #t)
 (lambda ()
   (dt:sumar
    (dt:polinomio-cero 'x)
    (dt:polinomio-cero 'y))))

;; =========================================================
;; PRUEBAS GENERALES DE LA PARTE 4
;; Compañero 4
;; =========================================================