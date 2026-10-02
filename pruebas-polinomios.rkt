#lang eopl
;Autores: Nicolas Cardona Garcia 2477349-3743, Nombre2 Codigo2

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

;; aqui iran los mensajes de error
(define msg-expo-negativo #rx"xponente debe ser un entero no negativo")
(define msg-sin-termino #rx"no tiene termino con ese exponente")
;; cualquier otro error x
(define cualquier-error #rx"")

;;aqui exponentes que se consultan para revisar que un polinomio no tienen mas terminos de los esperados
(define sondeo '(0 1 2 3 4 5 6 7 8 1000))

;;funcion verificar polinomio

(define verificar-poli
  (lambda (coef p esperados)
    (for-each
     (lambda (par)
       (check-equal? (coef p (car par)) (cdr par)))
     esperados)
    (for-each
     (lambda (e)
       (cond
         [(assv e esperados) #t]
         [else (check-exn msg-sin-termino (lambda () (coef p e)))]))
     sondeo)))

;;bateria comun
(define bateria
  (lambda (nombre cero ins coef elim)
    (let ((p (ins (ins  (ins (cero 'x) 7 0) -3/2 2) 4 5)))  ; 4x^5 - 3/2 x^2 + 7
      (test-case (string-append nombre " construccion con insertar-termino") ;;casos funcionales
        (verificar-poli coef p '((5 . 4) (2 . -3/2) (0 . 7))))

      (test-case (string-append nombre " el orden de insercion no importa")
        (let ((q (ins (ins (ins (cero 'x) 4 5) 7 0) -3/2 2))
              (r (ins (ins (ins (cero 'x) -3/2 2) 4 5) 7 0)))
          (verificar-poli coef q '((5 . 4) (2 . -3/2) (0 . 7)))
          (verificar-poli coef r '((5 . 4) (2 . -3/2) (0 . 7)))))

      (test-case (string-append nombre " insertar sobre exponente existente suma")
        (verificar-poli coef (ins p 1 2) '((5 . 4) (2 . -1/2) (0 . 7))))

      (test-case (string-append nombre " suma de racionales que da entero")
        (verificar-poli coef
                        (ins (ins (cero 'x) 1/3 4) 2/3 4)
                        '((4 . 1))))

      (test-case (string-append nombre " coeficientes racionales negativos y enteros")
        (check-equal? (coef p 2) -3/2)
        (check-equal? (coef p 5) 4)
        (check-equal? (coef p 0) 7))

      (test-case (string-append nombre " exponente muy grande")
        (verificar-poli coef
                        (ins (ins (cero 'x) 5 1000) 2 0)
                        '((1000 . 5) (0 . 2))))

      (test-case (string-append nombre " eliminar-termino")
        (verificar-poli coef (elim p 2) '((5 . 4) (0 . 7)))
        (verificar-poli coef (elim p 5) '((2 . -3/2) (0 . 7)))
        (verificar-poli coef (elim p 0) '((5 . 4) (2 . -3/2))))

      (test-case (string-append nombre " eliminar no modifica el original")
        (elim p 2)
        (verificar-poli coef p '((5 . 4) (2 . -3/2) (0 . 7))))

      (test-case (string-append nombre " eliminar todos los terminos deja el nulo")
        (verificar-poli coef (elim (elim (elim p 5) 2) 0) '()))

      ;; polinomio nulo como caso base
      (test-case (string-append nombre " nulo - consultar")
        (check-exn msg-sin-termino (lambda () (coef (cero 'x) 0)))
        (check-exn msg-sin-termino (lambda () (coef (cero 'x) 3))))

      (test-case (string-append nombre " nulo - eliminar")
        (check-exn msg-sin-termino (lambda () (elim (cero 'x) 0)))
        (check-exn msg-sin-termino (lambda () (elim (cero 'y) 4))))

      (test-case (string-append nombre " nulo - insertar")
        (verificar-poli coef (ins (cero 'x) 7 0) '((0 . 7)))
        (verificar-poli coef (ins (cero 'x) -3/2 2) '((2 . -3/2))))

      ;; cancelacion
      (test-case (string-append nombre " la suma cero elimina el termino")
        (verificar-poli coef (ins p 3/2 2) '((5 . 4) (0 . 7))))

      (test-case (string-append nombre " cancelar el unico termino deja el nulo")
        (verificar-poli coef (ins (ins (cero 'x) 5 3) -5 3) '()))

      (test-case (string-append nombre " cancelar el termino de mayor grado")
        (verificar-poli coef (ins p -4 5) '((2 . -3/2) (0 . 7))))

      ;;insercion con coeficioente cero
      (test-case (string-append nombre " insertar 0 con exponente nuevo no altera")
        (verificar-poli coef (ins p 0 3) '((5 . 4) (2 . -3/2) (0 . 7))))

      (test-case (string-append nombre " insertar 0 con exponente existente no altera")
        (verificar-poli coef (ins p 0 2) '((5 . 4) (2 . -3/2) (0 . 7))))

      (test-case (string-append nombre " insertar 0 en el nulo sigue siendo nulo")
        (verificar-poli coef (ins (cero 'x) 0 4) '()))

      ;; errores
      (test-case (string-append nombre " error - exponente negativo")
        (check-exn msg-expo-negativo (lambda () (ins p 5 -1)))
        (check-exn msg-expo-negativo (lambda () (ins (cero 'x) 1 -10))))

      (test-case (string-append nombre " error - coeficiente no exacto")
        (check-exn cualquier-error (lambda () (ins p 1.5 2)))
        (check-exn cualquier-error (lambda () (ins (cero 'x) 2.0 0))))

      (test-case (string-append nombre " error - coeficiente-de sin ese exponente")
        (check-exn msg-sin-termino (lambda () (coef p 3)))
        (check-exn msg-sin-termino (lambda () (coef p 6)))
        (check-exn msg-sin-termino (lambda () (coef p 1))))

      (test-case (string-append nombre " error - eliminar-termino sin ese exponente")
        (check-exn msg-sin-termino (lambda () (elim p 3)))
        (check-exn msg-sin-termino (lambda () (elim p 100))))

      (test-case (string-append nombre " error - eliminar dos veces el mismo exponente")
        (check-exn msg-sin-termino (lambda () (elim (elim p 2) 2)))))))

;;aqui la misma bateria sobre las 3 representaciones

(bateria "listas"
         listas:polinomio-cero listas:insertar-termino
         listas:coeficiente-de listas:eliminar-termino)

(bateria "procedimientos"
         procs:polinomio-cero procs:insertar-termino
         procs:coeficiente-de procs:eliminar-termino)

(bateria "datatypes"
         dt:polinomio-cero dt:insertar-termino
         dt:coeficiente-de dt:eliminar-termino)

;;pruebas exclusivas del datatype (sumasr)
(define construir-dt
  (lambda (var pares)
    (cond
      [(null? pares) (dt:polinomio-cero var)]
      [else (dt:insertar-termino (construir-dt var (cdr pares))
                                 (cdr (car pares))
                                 (car (car pares)))])))

(define p-dt (construir-dt 'x '((5 . 4) (2 . -3/2) (0 . 7))))   ; 4x^5 - 3/2 x^2 + 7
(define q-dt (construir-dt 'x '((5 . -4) (2 . 1/2) (1 . 2))))   ; -4x^5 + 1/2 x^2 + 2x

(test-case "datatypes sumar"
  ;; Resultado: -x^2 + 2x + 7
  (verificar-poli dt:coeficiente-de (dt:sumar p-dt q-dt)
                  '((2 . -1) (1 . 2) (0 . 7))))

(test-case "datatypes sumar es conmutativa"
  (verificar-poli dt:coeficiente-de (dt:sumar q-dt p-dt)
                  '((2 . -1) (1 . 2) (0 . 7))))

(test-case "datatypes sumar - cancelacion total da el polinomio nulo"
  (let ((menos-p (construir-dt 'x '((5 . -4) (2 . 3/2) (0 . -7)))))
    (verificar-poli dt:coeficiente-de (dt:sumar p-dt menos-p) '())))

(test-case "datatypes sumar - el nulo es neutro"
  (verificar-poli dt:coeficiente-de (dt:sumar p-dt (dt:polinomio-cero 'x))
                  '((5 . 4) (2 . -3/2) (0 . 7)))
  (verificar-poli dt:coeficiente-de (dt:sumar (dt:polinomio-cero 'x) p-dt)
                  '((5 . 4) (2 . -3/2) (0 . 7))))

(test-case "datatypes sumar - nulo + nulo"
  (verificar-poli dt:coeficiente-de
                  (dt:sumar (dt:polinomio-cero 'x) (dt:polinomio-cero 'x))
                  '()))

(test-case "datatypes sumar - exponentes disjuntos se intercalan en orden"
  (verificar-poli dt:coeficiente-de
                  (dt:sumar (construir-dt 'x '((6 . 1) (2 . 1)))
                            (construir-dt 'x '((4 . 3) (0 . 5))))
                  '((6 . 1) (4 . 3) (2 . 1) (0 . 5))))

(test-case "datatypes sumar - racionales que se suman a entero"
  (verificar-poli dt:coeficiente-de
                  (dt:sumar (construir-dt 'x '((3 . 1/2)))
                            (construir-dt 'x '((3 . 1/2))))
                  '((3 . 1))))

(test-case "datatypes sumar - no modifica los operandos"
  (dt:sumar p-dt q-dt)
  (verificar-poli dt:coeficiente-de p-dt '((5 . 4) (2 . -3/2) (0 . 7)))
  (verificar-poli dt:coeficiente-de q-dt '((5 . -4) (2 . 1/2) (1 . 2))))

(test-case "datatypes sumar - error con variables distintas"
  (check-exn cualquier-error
             (lambda () (dt:sumar p-dt (construir-dt 'y '((1 . 1))))))
  (check-exn cualquier-error
             (lambda () (dt:sumar (dt:polinomio-cero 'x) (dt:polinomio-cero 'y)))))