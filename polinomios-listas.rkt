#lang eopl
;Autores: Samuel Peña Jaramillo 202477399 

;; Taller 1 — Polinomios dispersos.
;; Parte 1: representación basada en listas.
;;
;; Interfaz del TAD. Cada función va comentada con su nombre, su contrato
;; (entrada -> salida) y su propósito, y ninguna recorre la lista de términos
;; más de una vez ni la ordena al final.
;;
;;   polinomio-cero    : symbol -> polinomio
;;   insertar-termino  : polinomio x coeficiente x exponente -> polinomio
;;   coeficiente-de    : polinomio x exponente -> coeficiente
;;   eliminar-termino  : polinomio x exponente -> polinomio


;; Constructores y observadores

;; poli
;; Contrato: variable x terminos -> polinomio
;; Proposito: Construye un polinomio a partir de una variable y una lista de terminos.

(define poli
  (lambda (var terms)
    (list 'poli var terms)))

;; poli?
;; Contrato: cualquier-valor -> booleano
;; Proposito: Verifica si el valor dado es un polinomio.

(define poli?
  (lambda (x)
    (and (list? x) (eqv? (car x) 'poli))))

;; poli->var
;; Contrato: polinomio -> variable
;; Proposito: Extrae la variable de un polinomio.

(define poli->var
  (lambda (p)
    (cadr p)))

;; poli->terms
;; Contrato: polinomio -> terminos
;; Proposito: Extrae los términos de un polinomio.

(define poli->terms
  (lambda (p)
    (caddr p)))

;; nombre-var
;; Contrato: simbolo -> variable
;; Proposito: Construye una variable a partir de un simbolo.

(define nombre-var
  (lambda (s)
    (list 'nombre-var s)))

;; nombre-var?
;; Contrato: cualquier-valor -> booleano
;; Proposito: Verifica si el valor es una variable.

(define nombre-var?
  (lambda (x)
    (and (list? x) (eqv? (car x) 'nombre-var))))

;; nombre-var->s
;; Contrato: variable -> simbolo
;; Proposito: Extrae el simbolo de una variable abstracta.

(define nombre-var->s
  (lambda (v)
    (cadr v)))

;; sin-terminos
;; Contrato: vacio -> terminos
;; Proposito: Construye una lista de terminos vacia.

(define sin-terminos
  (lambda ()
    (list 'sin-terminos)))

;; sin-terminos?
;; Contrato: cualquier-valor -> booleano
;; Proposito: Verifica si el valor corresponde a la lista de terminos vacia.

(define sin-terminos?
  (lambda (x)
    (and (list? x) (eqv? (car x) 'sin-terminos))))

;; mas-terminos
;; Contrato: termino x terminos -> terminos
;; Proposito: Construye una lista de terminos agregando un termino al inicio.

(define mas-terminos
  (lambda (term resto)
    (list 'mas-terminos term resto)))

;; mas-terminos?
;; Contrato: cualquier-valor -> booleano
;; Proposito: Verifica si el valor es una lista de terminos no vacia.

(define mas-terminos?
  (lambda (x)
    (and (list? x) (eqv? (car x) 'mas-terminos))))

;; mas-terminos->term
;; Contrato: terminos -> termino
;; Proposito: Extrae el primer termino de una lista de terminos.

(define mas-terminos->term
  (lambda (t)
    (cadr t)))

;; mas-terminos->resto
;; Contrato: terminos -> terminos
;; Proposito: Extrae el resto de los terminos de una lista.

(define mas-terminos->resto
  (lambda (t)
    (caddr t)))

;; termino
;; Contrato: coeficiente x exponente -> termino
;; Proposito: Construye un termino a partir de un coeficiente y un exponente.

(define termino
  (lambda (coef expo)
    (list 'termino coef expo)))

;; termino?
;; Contrato: cualquier-valor -> booleano
;; Proposito: Verifica si el valor es un termino.

(define termino?
  (lambda (x)
    (and (list? x) (eqv? (car x) 'termino))))

;; termino->coef
;; Contrato: termino -> coeficiente
;; Proposito: Extrae el coeficiente de un termino.

(define termino->coef
  (lambda (t)
    (cadr t)))

;; termino->expo
;; Contrato: termino -> exponente
;; Proposito: Extrae el exponente de un termino.

(define termino->expo
  (lambda (t)
    (caddr t)))

;; coef-ent
;; Contrato: entero -> coeficiente
;; Proposito: Construye un coeficiente entero.

(define coef-ent
  (lambda (n)
    (list 'coef-ent n)))

;; coef-ent?
;; Contrato: cualquier-valor -> booleano
;; Proposito: Verifica si el valor es un coeficiente entero.

(define coef-ent?
  (lambda (x)
    (and (list? x) (eqv? (car x) 'coef-ent))))

;; coef-ent->n
;; Contrato: coeficiente -> entero
;; Proposito: Extrae el valor numerico de un coeficiente entero.

(define coef-ent->n
  (lambda (c)
    (cadr c)))

;; coef-rac
;; Contrato: entero x entero -> coeficiente
;; Proposito: Construye un coeficiente racional a partir de numerador y denominador.

(define coef-rac
  (lambda (num den)
    (list 'coef-rac num den)))

;; coef-rac?
;; Contrato: cualquier-valor -> booleano
;; Proposito: Verifica si el valor es un coeficiente racional.

(define coef-rac?
  (lambda (x)
    (and (list? x) (eqv? (car x) 'coef-rac))))

;; coef-rac->num
;; Contrato: coeficiente -> entero
;; Proposito: Extrae el numerador de un coeficiente racional.

(define coef-rac->num
  (lambda (c)
    (cadr c)))

;; coef-rac->den
;; Contrato: coeficiente -> entero
;; Proposito: Extrae el denominador de un coeficiente racional.

(define coef-rac->den
  (lambda (c)
    (caddr c)))

;; expo-nat
;; Contrato: entero-no-negativo -> exponente
;; Proposito: Construye un exponente natural.

(define expo-nat
  (lambda (k)
    (list 'expo-nat k)))

;; expo-nat?
;; Contrato: cualquier-valor -> booleano
;; Proposito: Verifica si el valor es un exponente natural.

(define expo-nat?
  (lambda (x)
    (and (list? x) (eqv? (car x) 'expo-nat))))

;; expo-nat->k
;; Contrato: exponente -> entero-no-negativo
;; Proposito: Extrae el valor numerico de un exponente natural.

(define expo-nat->k
  (lambda (e)
    (cadr e)))


;; Funciones auxiliares


;; numero->coeficiente
;; Contrato: numero-exacto -> coeficiente
;; Proposito: Convierte un numero exacto de Racket a la representacion abstracta.

(define numero->coeficiente
  (lambda (n)
    (if (integer? n)
        (coef-ent n)
        (coef-rac (numerator n) (denominator n)))))

;; coeficiente->numero
;; Contrato: coeficiente -> numero-exacto
;; Proposito: Convierte la representacion abstracta a un numero exacto de Racket.

(define coeficiente->numero
  (lambda (c)
    (if (coef-ent? c)
        (coef-ent->n c)
        (/ (coef-rac->num c) (coef-rac->den c)))))

;; aux-insertar-termino
;; Contrato: coeficiente x exponente x terminos -> terminos
;; Proposito: Inserta un termino abstracto en la lista de terminos manteniendo el orden estricto decreciente.

(define aux-insertar-termino
  (lambda (coef-nuevo expo-nuevo terminos)
    (if (sin-terminos? terminos)
        (mas-terminos (termino coef-nuevo expo-nuevo) (sin-terminos))
        (let* ((term-actual (mas-terminos->term terminos))
               (resto (mas-terminos->resto terminos))
               (coef-actual (termino->coef term-actual))
               (val-expo-nuevo (expo-nat->k expo-nuevo))
               (val-expo-actual (expo-nat->k (termino->expo term-actual))))
          (cond
            ((> val-expo-nuevo val-expo-actual)
             (mas-terminos (termino coef-nuevo expo-nuevo) terminos))
            ((< val-expo-nuevo val-expo-actual)
             (mas-terminos term-actual (aux-insertar-termino coef-nuevo expo-nuevo resto)))
            (else
             (let ((suma (+ (coeficiente->numero coef-nuevo)
                            (coeficiente->numero coef-actual))))
               (if (= suma 0)
                   resto
                   (mas-terminos (termino (numero->coeficiente suma) expo-nuevo) resto)))))))))

;; aux-coeficiente-de
;; Contrato: entero x terminos -> numero-exacto
;; Proposito: Busca recursivamente el coeficiente asociado a un exponente en la lista de terminos.

(define aux-coeficiente-de
  (lambda (exponente terminos)
    (if (sin-terminos? terminos)
        (eopl:error 'coeficiente-de "El polinomio no tiene termino con ese exponente")
        (let* ((term-actual (mas-terminos->term terminos))
               (expo-actual (expo-nat->k (termino->expo term-actual))))
          (cond
            ((= exponente expo-actual) (coeficiente->numero (termino->coef term-actual)))
            ((> exponente expo-actual) (eopl:error 'coeficiente-de "El polinomio no tiene termino con ese exponente"))
            (else (aux-coeficiente-de exponente (mas-terminos->resto terminos))))))))

;; aux-eliminar-termino
;; Contrato: entero x terminos -> terminos
;; Proposito: Busca y elimina el termino asociado al exponente dado en la lista de terminos.

(define aux-eliminar-termino
  (lambda (exponente terminos)
    (if (sin-terminos? terminos)
        (eopl:error 'eliminar-termino "El polinomio no tiene termino con ese exponente")
        (let* ((term-actual (mas-terminos->term terminos))
               (resto (mas-terminos->resto terminos))
               (expo-actual (expo-nat->k (termino->expo term-actual))))
          (cond
            ((= exponente expo-actual) resto)
            ((> exponente expo-actual) (eopl:error 'eliminar-termino "El polinomio no tiene termino con ese exponente"))
            (else (mas-terminos term-actual (aux-eliminar-termino exponente resto))))))))


;; Funciones del taller


(provide polinomio-cero insertar-termino coeficiente-de eliminar-termino
         poli poli? poli->var poli->terms
         nombre-var nombre-var? nombre-var->s
         sin-terminos sin-terminos?
         mas-terminos mas-terminos? mas-terminos->term mas-terminos->resto
         termino termino? termino->coef termino->expo
         coef-ent coef-ent? coef-ent->n
         coef-rac coef-rac? coef-rac->num coef-rac->den
         expo-nat expo-nat? expo-nat->k)

;; polinomio-cero
;; Contrato: simbolo -> polinomio
;; Proposito: Retornar el polinomio nulo para la variable dada.

(define polinomio-cero
  (lambda (variable)
    (poli (nombre-var variable) (sin-terminos))))

;; insertar-termino
;; Contrato: polinomio x numero-exacto x entero -> polinomio
;; Proposito: Insertar un nuevo termino en el polinomio o lo opera con uno existente y lanza error si los datos son invalidos.

(define insertar-termino
  (lambda (polinomio coeficiente exponente)
    (if (not (exact? coeficiente))
        (eopl:error 'insertar-termino "El coeficiente no es un numero exacto")
        (if (< exponente 0)
            (eopl:error 'insertar-termino "El exponente debe ser un entero no negativo")
            (if (= coeficiente 0)
                polinomio
                (poli (poli->var polinomio)
                      (aux-insertar-termino (numero->coeficiente coeficiente)
                                             (expo-nat exponente)
                                             (poli->terms polinomio))))))))

;; coeficiente-de
;; Contrato: polinomio x entero -> numero-exacto
;; Proposito: Retornar el coeficiente concreto del polinomio correspondiente al exponente dado y lanza error si no existe. 

(define coeficiente-de
  (lambda (polinomio exponente)
    (aux-coeficiente-de exponente (poli->terms polinomio))))

;; eliminar-termino
;; Contrato: polinomio x entero -> polinomio
;; Proposito: Eliminar del polinomio el termino asociado al exponente dado y lanza error si el termino no existe.

(define eliminar-termino
  (lambda (polinomio exponente)
    (poli (poli->var polinomio)
          (aux-eliminar-termino exponente (poli->terms polinomio)))))

