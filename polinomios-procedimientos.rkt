#lang eopl
;Autores: Laura Sofía Echeverry González 2477067

;; Taller 1 — Polinomios dispersos.
;; Parte 2: representación basada en procedimientos.
;;
;; Interfaz del TAD. Cada función va comentada con su nombre, su contrato
;; (entrada -> salida) y su propósito, y ninguna recorre la lista de términos
;; más de una vez ni la ordena al final.
;;
;;   polinomio-cero    : symbol -> polinomio
;;   insertar-termino  : polinomio x coeficiente x exponente -> polinomio
;;   coeficiente-de    : polinomio x exponente -> coeficiente
;;   eliminar-termino  : polinomio x exponente -> polinomio

(provide polinomio-cero insertar-termino coeficiente-de eliminar-termino)

; °❀⋆.ೃ࿔*:・°❀⋆.ೃ࿔*:・
#| 1. constructores y observadores internos ★
en esta parte cada dato se guarda como si fuera un procedimiento,
y ese procedimiento se recibe por mensaje, por ejemplo 'tipo, 'n, 'coef, 'expo,
y responde con el dato correspondiente, así nadie puede mirar directamente
cómo está construido el polinomio y solamente puede usar los observadores |#

; . ݁+ ⊹ . ݁ ⟡ ݁ . ⊹ + ݁.
#| primero variables! esta función se llama nombre-var, tiene contrato symbol -> variable 
y su propósito es construir una variable del polinomio, por ejemplo la variable 'x |#

(define nombre-var
  (lambda (s)
    (lambda (mensaje)
      (cond
        ((eq? mensaje 'tipo) 'nombre-var)
        ((eq? mensaje 's) s)
        (else
         (eopl:error 'nombre-var
                     "Mensaje desconocido: ~s"
                     mensaje))))))

; . ݁+ ⊹ . ݁ ⟡ ݁ . ⊹ + ݁.
#| esta función se llama nombre-var?, tiene contrato variable -> boolean 
y su propósito es verificar si un dato fue construido usando nombre-var |#

(define nombre-var?
  (lambda (dato)
    (eq? (dato 'tipo) 'nombre-var)))

; . ݁+ ⊹ . ݁ ⟡ ݁ . ⊹ + ݁.
#| esta función se llama nombre-var->s, tiene contrato variable -> symbol 
y su propósito es extraer el símbolo guardado dentro de una variable |#

(define nombre-var->s
  (lambda (dato)
    (dato 's)))

; *ੈ✩‧+ ̊༺☆༻*ੈ✩‧+ ̊
#| ahora coeficientes enteros! esta función se llama coef-ent, tiene contrato int -> coeficiente 
y su propósito es construir un coeficiente entero, por ejemplo 4 o -7 |#

(define coef-ent
  (lambda (n)
    (lambda (mensaje)
      (cond
        ((eq? mensaje 'tipo) 'coef-ent)
        ((eq? mensaje 'n) n)
        (else
         (eopl:error 'coef-ent
                     "Mensaje desconocido: ~s"
                     mensaje))))))
					 
#| esta función se llama coef-ent?, tiene contrato coeficiente -> boolean 
y su propósito es verificar si un dato fue construido usando coef-ent |#

(define coef-ent?
  (lambda (dato)
    (eq? (dato 'tipo) 'coef-ent)))
	
#| esta función se llama coef-ent->n, tiene contrato coeficiente-entero -> int 
y su propósito es extraer el número entero almacenado dentro de un coeficiente entero |#

(define coef-ent->n
  (lambda (dato)
    (dato 'n)))

; ✩+ ̊.⋆☾⋆++✧
#| sigue coeficiente racional! esta función se llama coef-rac, tiene contrato int x int -> coeficiente 
y su propósito es construir un coeficiente racional usando su numerador y su denominador por separado, por ejemplo -3 y 2 para -3/2 |#

(define coef-rac
  (lambda (num den)

    (lambda (mensaje)
      (cond
        ((eq? mensaje 'tipo) 'coef-rac)
        ((eq? mensaje 'num) num)
        ((eq? mensaje 'den) den)
        (else
         (eopl:error 'coef-rac
                     "Mensaje desconocido: ~s"
                     mensaje))))))
					 
#| esta función se llama coef-rac?, tiene contrato coeficiente -> boolean 
y su propósito es verificar si un dato fue construido usando coef-rac |#

(define coef-rac?
  (lambda (dato)
    (eq? (dato 'tipo) 'coef-rac)))
	
#| esta función se llama coef-rac->num, tiene contrato coeficiente-racional -> int 
y su propósito es extraer el numerador de un coeficiente racional |#

(define coef-rac->num
  (lambda (dato)
    (dato 'num)))
	
#| esta función se llama coef-rac->den, tiene contrato coeficiente-racional -> int 
y su propósito es extraer el denominador de un coeficiente racional |#

(define coef-rac->den
  (lambda (dato)
    (dato 'den)))

#| para los exponentes naturales! esta función se llama expo-nat, tiene contrato int -> exponente 
y su propósito es construir un exponente natural, por ejemplo 0, 2 o 5 |#

(define expo-nat
  (lambda (k)
    (lambda (mensaje)
      (cond
        ((eq? mensaje 'tipo) 'expo-nat)
        ((eq? mensaje 'k) k)
        (else
         (eopl:error 'expo-nat
                     "Mensaje desconocido: ~s"
                     mensaje))))))

#| esta función se llama expo-nat?, tiene contrato exponente -> boolean 
y su propósito es verificar si un dato fue construido usando expo-nat |#

(define expo-nat?
  (lambda (dato)
    (eq? (dato 'tipo) 'expo-nat)))

#| esta función se llama expo-nat->k, tiene contratoexponente -> int 
y su propósito es extraer el númeroentero almacenado como exponente |#

(define expo-nat->k
  (lambda (dato)
    (dato 'k)))

#| para el término! esta función se llama termino, tiene contrato coeficiente x exponente -> termino 
y su propósito es construir un término de un polinomio. por ejemplo, el término 4x^5 se representa
usando un coeficiente entero 4 y un exponente natural 5 |#

(define termino
  (lambda (coef expo)
    (lambda (mensaje)
      (cond
        ((eq? mensaje 'tipo) 'termino)
        ((eq? mensaje 'coef) coef)
        ((eq? mensaje 'expo) expo)
        (else
         (eopl:error 'termino
                     "Mensaje desconocido: ~s"
                     mensaje))))))

#| esta función se llama termino?, tiene contrato dato -> boolean 
y su propósito es verificar si un dato fue construido usando termino |#

(define termino?
  (lambda (dato)
    (eq? (dato 'tipo) 'termino)))

#| esta función se llama termino->coef, tiene contrato termino -> coeficiente 
y su propósito es extraer el coeficiente guardado dentro de un término |#

(define termino->coef
  (lambda (dato)
    (dato 'coef)))

#| esta función se llama termino->expo, tiene contratotermino -> exponente 
y su propósito es extraer elexponente guardado dentro de un término |#

(define termino->expo
  (lambda (dato)
    (dato 'expo)))

#| para la lista vacía de términos! esta función se llama sin-terminos, tiene contrato () -> terminos 
y su propósito es construir una lista vacía de términos. esta representa el polinomio cero |#

(define sin-terminos
  (lambda ()
    (lambda (mensaje)
      (cond
        ((eq? mensaje 'tipo) 'sin-terminos)
        (else
         (eopl:error 'sin-terminos
                     "Mensaje desconocido: ~s"
                     mensaje))))))
					 
#| esta función se llama sin-terminos?, tiene contrato terminos -> boolean 
y su propósito es verificar si una lista de términos está vacía |#

(define sin-terminos?
  (lambda (dato)
    (eq? (dato 'tipo) 'sin-terminos)))

#| para la lista no vacía de términos! esta función se llama mas-terminos, tiene contrato termino x terminos -> terminos 
y su propósito es construir una lista no vacía de términos. guarda el primer término y el resto de los términos |#

(define mas-terminos
  (lambda (term resto)
    (lambda (mensaje)
      (cond
        ((eq? mensaje 'tipo) 'mas-terminos)
        ((eq? mensaje 'term) term)
        ((eq? mensaje 'resto) resto)
        (else
         (eopl:error 'mas-terminos
                     "Mensaje desconocido: ~s"
                     mensaje))))))

#| esta función se llama mas-terminos?, tiene contrato terminos -> boolean 
y su propósito es verificar si una lista de términos tiene al menos un término |#

(define mas-terminos?
  (lambda (dato)
    (eq? (dato 'tipo) 'mas-terminos)))

#| esta función se llama mas-terminos->term, tiene contrato terminos-no-vacios -> termino 
y su propósito es extraer el primer término de una lista no vacía de términos |#

(define mas-terminos->term
  (lambda (dato)
    (dato 'term)))
	
#| esta función se llama mas-terminos->resto, tiene contrato terminos-no-vacios -> terminos 
y su propósito es extraer todos los términos restantes después del primer término |#

(define mas-terminos->resto
  (lambda (dato)
    (dato 'resto)))

#| para los polinomios! esta función se llama poli, tiene contrato variable x terminos -> polinomio 
y su propósito es construir un polinomio usando una variable y una lista ordenada de términos |#

(define poli
  (lambda (var terms)
    (lambda (mensaje)
      (cond
        ((eq? mensaje 'tipo) 'poli)
        ((eq? mensaje 'var) var)
        ((eq? mensaje 'terms) terms)
        (else
         (eopl:error 'poli
                     "Mensaje desconocido: ~s"
                     mensaje))))))


#| esta función se llama poli?, tiene contrato dato -> boolean 
y su propósito es verificar si un dato fue construido usando poli |#

(define poli?
  (lambda (dato)
    (eq? (dato 'tipo) 'poli)))
	
#| esta función se llama poli->var, tiene contrato polinomio -> variable 
y su propósito es extraer la variable de un polinomio |#

(define poli->var
  (lambda (dato)
    (dato 'var)))
	
#| esta función se llama poli->terms, tiene contrato polinomio -> terminos 
y su propósito es extraer la lista de términos de un polinomio |#

(define poli->terms
  (lambda (dato)
    (dato 'terms)))

; °❀⋆.ೃ࿔*:・°❀⋆.ೃ࿔*:・
; 2. funciones auxiliares ಄

#| esta función se llama exponente-valido?, tiene contrato dato -> boolean y su propósito es verificar si 
un valor puede usarse como exponente concreto. un exponente válido debe ser un entero mayor o igual a cero |#

(define exponente-valido?
  (lambda (exponente)
    (and (exact? exponente)
         (integer? exponente)
         (>= exponente 0))))


#| esta función se llama validar-exponente, tiene contrato dato -> int y su propósito es revisar que el exponente 
recibido sea un entero no negativo. si sí es válido, lo devuelve sin cambiar. si no es válido, genera el error pedido por el profe |#

(define validar-exponente
  (lambda (exponente)
    (if (exponente-valido? exponente)
        exponente
        (eopl:error 'insertar-termino
                    "El exponente debe ser un entero no negativo"))))


#| esta función se llama validar-coeficiente, tiene contrato dato -> numero-exacto y su propósito es revisar que 
el coeficiente recibido sea un número exacto de racket, como 4, -7 o -3/2. si recibe un decimal como 0.5, 
o algo que no sea un número, genera un error porque no se puede guardar como coeficiente |#

(define validar-coeficiente
  (lambda (coeficiente)
    (if (exact? coeficiente)
        coeficiente
        (eopl:error 'insertar-termino
                    "El coeficiente debe ser un numero exacto"))))


#| esta función se llama coeficiente-concreto->abstracto, tiene contrato numero-exacto -> coeficiente y su propósito 
es convertir un número normal de racket al tipo abstracto coeficiente que construimos arriba. 
por ejemplo: 4 se transforma en (coef-ent 4) -3/2 se transforma en (coef-rac -3 2) |#

(define coeficiente-concreto->abstracto
  (lambda (coeficiente)
    (cond
      ((integer? coeficiente)
       (coef-ent coeficiente))
      (else
       (coef-rac (numerator coeficiente)
                 (denominator coeficiente))))))


#| esta función se llama coeficiente-abstracto->concreto, tiene contrato coeficiente -> numero-exacto y su propósito 
es hacer la conversión contraria: recibe un coeficiente abstracto y devuelve un número normal de racket. 
por ejemplo: (coef-ent 4) se transforma en 4 (coef-rac -3 2) se transforma en -3/2 |#

(define coeficiente-abstracto->concreto
  (lambda (coeficiente)
    (cond
      ((coef-ent? coeficiente)
       (coef-ent->n coeficiente))
      ((coef-rac? coeficiente)
       (/ (coef-rac->num coeficiente)
          (coef-rac->den coeficiente)))
      (else
       (eopl:error 'coeficiente-abstracto->concreto
                   "El dato no es un coeficiente valido")))))


#| esta función se llama exponente-concreto->abstracto, tiene contrato int -> exponente y su propósito es 
construir un exponente abstracto a partir de un entero no negativo de racket. por ejemplo: 5 se transforma en (expo-nat 5) |#

(define exponente-concreto->abstracto
  (lambda (exponente)
    (expo-nat exponente)))


#| esta función se llama exponente-abstracto->concreto, tiene contrato exponente -> int y su propósito es 
extraer el entero guardado dentro de un exponente abstracto. por ejemplo: (expo-nat 5) se transforma en 5 |#

(define exponente-abstracto->concreto
  (lambda (exponente)
    (expo-nat->k exponente)))

#| esta función se llama insertar-en-terminos, tiene contrato  terminos x numero-exacto x int -> terminos y su propósito 
es insertar un coeficiente y un exponente dentro de una lista de términos que ya está ordenada de mayor a menor exponente.
la función recorre los términos una sola vez. si encuentra el mismo exponente, suma los coeficientes. 
si esa suma da cero, elimina el término en vez de guardarlo |#

(define insertar-en-terminos
  (lambda (terminos coeficiente exponente)
    (cond

      ; caso 1: no hay términos. entonces el nuevo término será el único término de la lista
      ((sin-terminos? terminos)
       (mas-terminos
        (termino (coeficiente-concreto->abstracto coeficiente)
                 (exponente-concreto->abstracto exponente))
        (sin-terminos)))

      ; caso 2: sí hay al menos un término. ahora se compara al exponente nuevo con el exponente del primer término
      ((mas-terminos? terminos)
       (let ((term-actual (mas-terminos->term terminos))
             (resto-actual (mas-terminos->resto terminos)))

         (let ((expo-actual
                (exponente-abstracto->concreto
                 (termino->expo term-actual))))

           (cond

             ; caso 2a: el exponente nuevo es mayor. debe ir antes porque los términos se guardan de mayor a menor
             ((> exponente expo-actual)
              (mas-terminos
               (termino (coeficiente-concreto->abstracto coeficiente)
                        (exponente-concreto->abstracto exponente))
               terminos))

             ; caso 2b: ambos exponentes son iguales. se suman sus coeficientes para no dejar exponentes repetidos
             ((= exponente expo-actual)
              (let ((suma
                     (+ coeficiente
                        (coeficiente-abstracto->concreto
                         (termino->coef term-actual)))))

                (cond

                  ; si la suma es cero, el término desaparece
                  ((zero? suma)
                   resto-actual)

                  ; si la suma no da cero, se reemplaza el término actual por uno con el coeficiente actualizado
                  (else
                   (mas-terminos
                    (termino
                     (coeficiente-concreto->abstracto suma)
                     (exponente-concreto->abstracto exponente))
                    resto-actual)))))

             ; caso 2c: el exponente nuevo es menor.
             ; se conserva el término actual y se sigue buscando la posición correcta únicamente dentro del resto
             (else
              (mas-terminos
               term-actual
               (insertar-en-terminos
                resto-actual
                coeficiente
                exponente))))))))))

#| esta función se llama coeficiente-en-terminos, tiene contrato terminos x int -> numero-exacto y su propósito es buscar 
el coeficiente de un término cuyo exponente coincide con el exponente que se está buscando. 
la lista de términos ya está ordenada de mayor a menor. por eso, si encuentra un exponente menor que el buscado, 
sabe que el término no existe y puede generar el error sin seguir recorriendo |#

(define coeficiente-en-terminos
  (lambda (terminos exponente-buscado)
    (cond

      ; caso 1: se llegó al final de los términos y no apareció el exponente buscado
      ((sin-terminos? terminos)
       (eopl:error 'coeficiente-de
                   "El polinomio no tiene termino con ese exponente"))

      ; caso 2: hay un término actual y se compara su exponente con el exponente que se quiere buscar
      ((mas-terminos? terminos)
       (let ((termino-actual
              (mas-terminos->term terminos))
             (resto-actual
              (mas-terminos->resto terminos)))

         (let ((exponente-actual
                (exponente-abstracto->concreto
                 (termino->expo termino-actual))))

           (cond

             ; si los exponentes son iguales, se encontró el término y se devuelve su coeficiente como número normal de racket
             ((= exponente-buscado exponente-actual)
              (coeficiente-abstracto->concreto
               (termino->coef termino-actual)))

             ; si el actual ya es menor que el buscado, la búsqueda pasó la posición donde habría estado ese exponente
             ((< exponente-actual exponente-buscado)
              (eopl:error 'coeficiente-de
                          "El polinomio no tiene termino con ese exponente"))

             ; si el actual es mayor, se conserva la búsqueda y se continúa únicamente con el resto de los términos
             (else
              (coeficiente-en-terminos
               resto-actual
               exponente-buscado)))))))))

#| esta función se llama eliminar-en-terminos, tiene contrato terminos x int -> terminos y su propósito es eliminar 
el término que tenga el exponente recibido dentro de una lista de términos ordenada de mayor a menor. si el término existe, 
devuelve una nueva lista igual a la original, pero sin ese término. si no existe, genera el error pedido por el profe. 
la función recorre los términos una sola vez! |#

(define eliminar-en-terminos
  (lambda (terminos exponente-buscado)
    (cond

      ; caso 1: se llegó al final y no apareció el término
      ((sin-terminos? terminos)
       (eopl:error 'eliminar-termino
                   "El polinomio no tiene termino con ese exponente"))

      ; caso 2: hay un término actual. se compara su exponente con el exponente que se desea eliminar
      ((mas-terminos? terminos)
       (let ((termino-actual
              (mas-terminos->term terminos))
             (resto-actual
              (mas-terminos->resto terminos)))

         (let ((exponente-actual
                (exponente-abstracto->concreto
                 (termino->expo termino-actual))))

           (cond

             ; si los exponentes son iguales, se elimina el término. por eso devolvemos directamente el resto, sin conservar el término actual
             ((= exponente-buscado exponente-actual)
              resto-actual)

             ; si el exponente actual ya es menor que el buscado, entonces ya pasó la posición donde habría aparecido
             ((< exponente-actual exponente-buscado)
              (eopl:error 'eliminar-termino
                          "El polinomio no tiene termino con ese exponente"))

             ; si el exponente actual es mayor, ese término se conserva y se continúa eliminando solamente dentro del resto
             (else
              (mas-terminos
               termino-actual
               (eliminar-en-terminos
                resto-actual
                exponente-buscado))))))))))

; *ੈ✩‧+ ̊༺☆༻*ੈ✩‧+ ̊
; 3. interfaz completa

#| esta función se llama polinomio-cero, tiene contrato symbol -> polinomio y su propósito es crear el polinomio cero
para una variable dada. el polinomio cero no tiene términos, pero sí guarda la variable que recibe.
por ejemplo, (polinomio-cero 'x) representa el polinomio 0 en la variable x |#

(define polinomio-cero
  (lambda (variable)
    (poli (nombre-var variable)
          (sin-terminos))))

#| esta función se llama insertar-termino, tiene contrato polinomio x coeficiente x exponente -> polinomio 
y su propósito es insertar un término dentro de un polinomio. si ya existe un término con el mismo exponente, suma ambos coeficientes. 
si la suma da cero, elimina el término. si el coeficiente nuevo es cero, no modifica el polinomio. también revisa que el coeficiente 
sea un número exacto y que el exponente sea un entero no negativo |#

(define insertar-termino
  (lambda (polinomio coeficiente exponente)
    (let ((coeficiente-valido
           (validar-coeficiente coeficiente))
          (exponente-valido
           (validar-exponente exponente)))

      (if (zero? coeficiente-valido)

          ; si el coeficiente que quieren insertar es cero, no se debe agregar ni cambiar ningún término
          polinomio

          ; si el coeficiente no es cero, se construye un polinomio nuevo con la misma variable
          ; y con los términos resultantes de insertar el nuevo término
          (poli
           (poli->var polinomio)
           (insertar-en-terminos
            (poli->terms polinomio)
            coeficiente-valido
            exponente-valido))))))

#| esta función se llama coeficiente-de, tiene contrato polinomio x exponente -> coeficiente y su propósito es buscar 
el término que tiene el exponente recibido y devolver su coeficiente como un número normal de racket. 
si el polinomio no tiene un término con ese exponente, genera el error "el polinomio no tiene termino con ese exponente". |#

(define coeficiente-de
  (lambda (polinomio exponente)
    (coeficiente-en-terminos
     (poli->terms polinomio)
     exponente)))

#| esta función se llama eliminar-termino, tiene contrato polinomio x exponente -> polinomio y su propósito es devolver 
un polinomio nuevo igual al polinomio recibido, excepto que no incluye el término cuyo exponente coincide con el recibido. 
si el polinomio no tiene un término con ese exponente, genera el error "El polinomio no tiene termino con ese exponente". |#

(define eliminar-termino
  (lambda (polinomio exponente)
    (poli
     (poli->var polinomio)
     (eliminar-en-terminos
      (poli->terms polinomio)
      exponente))))

;; ============================================================
;; Ejemplos de la parte 2: Procedimientos
;; ============================================================

;; ------------------------------------------------------------
;; Ejemplos de construcción de polinomios
;; ------------------------------------------------------------

;; Ejemplo 1: polinomio 8x^6 - 4x^3 + 5
(define ejemplo-proc-1
  (poli
   (nombre-var 'x)
   (mas-terminos
    (termino (coef-ent 8) (expo-nat 6))
    (mas-terminos
     (termino (coef-ent -4) (expo-nat 3))
     (mas-terminos
      (termino (coef-ent 5) (expo-nat 0))
      (sin-terminos))))))

;; Ejemplo 2: polinomio -2y^8 + 7y^4
(define ejemplo-proc-2
  (poli
   (nombre-var 'y)
   (mas-terminos
    (termino (coef-ent -2) (expo-nat 8))
    (mas-terminos
     (termino (coef-ent 7) (expo-nat 4))
     (sin-terminos)))))

;; Ejemplo 3: polinomio 5/6z^7 - 3z^2 + 1
(define ejemplo-proc-3
  (poli
   (nombre-var 'z)
   (mas-terminos
    (termino (coef-rac 5 6) (expo-nat 7))
    (mas-terminos
     (termino (coef-ent -3) (expo-nat 2))
     (mas-terminos
      (termino (coef-ent 1) (expo-nat 0))
      (sin-terminos))))))

;; Ejemplo 4: polinomio 10w^5 + 3/4w
(define ejemplo-proc-4
  (poli
   (nombre-var 'w)
   (mas-terminos
    (termino (coef-ent 10) (expo-nat 5))
    (mas-terminos
     (termino (coef-rac 3 4) (expo-nat 1))
     (sin-terminos)))))

;; Ejemplo 5: polinomio -9t^9 + 2t^5 - 6
(define ejemplo-proc-5
  (poli
   (nombre-var 't)
   (mas-terminos
    (termino (coef-ent -9) (expo-nat 9))
    (mas-terminos
     (termino (coef-ent 2) (expo-nat 5))
     (mas-terminos
      (termino (coef-ent -6) (expo-nat 0))
      (sin-terminos))))))


;; ------------------------------------------------------------
;; Ejemplos de polinomio-cero
;; ------------------------------------------------------------

;; Ejemplo 1
(define cero-proc-1
  (polinomio-cero 'c))

;; Ejemplo 2
(define cero-proc-2
  (polinomio-cero 'd))

;; Ejemplo 3
(define cero-proc-3
  (polinomio-cero 'h))

;; Ejemplo 4
(define cero-proc-4
  (polinomio-cero 'n))

;; Ejemplo 5
(define cero-proc-5
  (polinomio-cero 'v))


;; ------------------------------------------------------------
;; Ejemplos de insertar-termino
;; ------------------------------------------------------------

;; Ejemplo 1: insertar el primer término
(define insertar-proc-1
  (insertar-termino
   (polinomio-cero 'k)
   15
   4))

;; Ejemplo 2: insertar un término con mayor exponente
(define insertar-proc-2
  (insertar-termino
   insertar-proc-1
   -2
   8))

;; Ejemplo 3: insertar un término con menor exponente
(define insertar-proc-3
  (insertar-termino
   insertar-proc-2
   11
   1))

;; Ejemplo 4: combinar un término con el mismo exponente
(define insertar-proc-4
  (insertar-termino
   insertar-proc-3
   -5
   4))

;; Ejemplo 5: insertar coeficiente cero
;; El polinomio debe permanecer sin cambios.
(define insertar-proc-5
  (insertar-termino
   insertar-proc-4
   0
   12))


;; ------------------------------------------------------------
;; Ejemplos de coeficiente-de
;; ------------------------------------------------------------

;; Ejemplo 1: consultar el coeficiente de x^8
(define coeficiente-proc-1
  (coeficiente-de
   insertar-proc-2
   8))

;; Ejemplo 2: consultar el coeficiente de x^4 después de combinar
(define coeficiente-proc-2
  (coeficiente-de
   insertar-proc-4
   4))

;; Ejemplo 3: consultar el coeficiente de x^1
(define coeficiente-proc-3
  (coeficiente-de
   insertar-proc-4
   1))

;; Ejemplo 4: consultar el coeficiente de z^7
(define coeficiente-proc-4
  (coeficiente-de
   ejemplo-proc-3
   7))

;; Ejemplo 5: consultar el término independiente de t
(define coeficiente-proc-5
  (coeficiente-de
   ejemplo-proc-5
   0))


;; ------------------------------------------------------------
;; Ejemplos de eliminar-termino
;; ------------------------------------------------------------

;; Ejemplo 1: eliminar el término de mayor exponente
(define eliminar-proc-1
  (eliminar-termino
   insertar-proc-2
   8))

;; Ejemplo 2: eliminar el término combinado
(define eliminar-proc-2
  (eliminar-termino
   insertar-proc-4
   4))

;; Ejemplo 3: eliminar el término de menor exponente
(define eliminar-proc-3
  (eliminar-termino
   insertar-proc-4
   1))

;; Ejemplo 4: eliminar el término independiente
(define eliminar-proc-4
  (eliminar-termino
   ejemplo-proc-5
   0))

;; Ejemplo 5: eliminar un término intermedio
(define eliminar-proc-5
  (eliminar-termino
   ejemplo-proc-3
   2))


;; ------------------------------------------------------------
;; Ejemplos de error
;; ------------------------------------------------------------

;; Exponente negativo:
;; (insertar-termino (polinomio-cero 'j) 6 -3)

;; Coeficiente no exacto:
;; (insertar-termino (polinomio-cero 'j) 1.5 4)

;; Exponente que no existe:
;; (coeficiente-de ejemplo-proc-1 10)

;; Intentar eliminar un exponente inexistente:
;; (eliminar-termino ejemplo-proc-5 7)
