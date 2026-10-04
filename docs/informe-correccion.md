# Informe de corrección — Taller 1: polinomios dispersos

> **Plantilla de entrega.** Copie este archivo a
> `docs/informe-correccion.md` dentro del repositorio del grupo y
> reemplace los marcadores `{{...}}` con su contenido. **No elimine
> las secciones obligatorias.** No se aceptan PDF, DOCX ni imágenes
> insertadas: todo el documento debe ser Markdown, las fórmulas en
> LaTeX (`$...$` / `$$...$$`) y los diagramas, si los hay, en Mermaid.
>
> Las demostraciones se hacen una sola vez, sobre la estructura
> recursiva que define la gramática, porque la lógica de las funciones
> es la misma en las tres representaciones.

**Curso:** Fundamentos de Interpretación y Compilación de Lenguajes
de Programación — Universidad del Valle, Sede Tuluá.

**Integrantes del grupo:**

| Nombre | Código | Correo institucional |
|--------|--------|----------------------|
| Samuel Peña Jaramillo | 202477399 | samuel.pena@correounivalle.edu.co |
| Laura Sofía Echeverry González | 2477067 | echeverry.laura@correounivalle.edu.co |
| Santiago Serrano Morales | 202477006 | serrano.santiago@correounivalle.edu.co |
| Nicolas Cardona Garcia | 2477349-3743 | nicolas.cardona.garcia@correounivalle.edu.co |

---

## 1. Marco formal

### 1.1 Corrección de programas recursivos

Sea $f : A \to B$ una función y $A$ un conjunto definido
recursivamente. Sea $P_f$ un programa recursivo en Racket que pretende
calcular $f$. Decimos que $P_f$ es correcto con respecto a su
especificación si se cumple:

$$ \forall a \in A \;:\; P_f(a) = f(a) $$

La estrategia de demostración es **inducción estructural** sobre $A$.

Aquí $A$ es el conjunto de listas de términos que genera la gramática:

- **Caso base:** $a = \text{sin-terminos}()$, y se verifica $P_f(a) = f(a)$ directamente.

- **Caso inductivo:** $a = \text{mas-terminos}(t, r)$. Se asume la
- **hipótesis de inducción** $P_f(r) = f(r)$ sobre el resto de la lista y se demuestra $P_f(a) = f(a)$.

Si alguna de sus funciones quedó escrita con un acumulador en lugar de
recursión estructural, la corrección se argumenta con una invariante
del acumulador y no con la hipótesis de inducción: se enuncia la
invariante, se demuestra que vale al inicio, que cada paso la conserva
y que al terminar implica la post-condición.

### 1.2 El invariante de la representación

Las cuatro condiciones del enunciado se enuncian como una única
propiedad sobre polinomios. Sea $p$ un polinomio con términos
$t_1, t_2, \ldots, t_n$, donde $t_i = (c_i, e_i)$:

$$
\mathrm{Inv}(p) \equiv
\underbrace{\forall i < n : e_i > e_{i+1}}_{\text{orden estricto}}
\;\land\;
\underbrace{\forall i : c_i \neq 0}_{\text{sin ceros}}
\;\land\;
\underbrace{\forall i : e_i \in \mathbb{N}}_{\text{exponentes naturales}}
\;\land\;
\underbrace{\forall i : \mathrm{red}(c_i)}_{\text{racionales reducidos}}
$$

donde $\mathrm{red}\left(\frac{a}{b}\right)$ abrevia
$b > 0 \;\land\; \mathrm{mcd}(|a|, b) = 1$, y un coeficiente entero se
toma como el racional de denominador $1$.

---

### 2.1 Corrección de `coeficiente-de`

**Especificación.**

- **Tipo:** `coeficiente-de : polinomio × exponente -> coeficiente`

- **Pre-condición:** $\mathrm{Inv}(p)$ y $e \in \mathbb{N}$.

- **Post-condición:** si el exponente $e$ aparece en $p$, el resultado
  $r$ es el coeficiente asociado a dicho exponente:

  $r = c \text{ si } (c,e) \text{ pertenece a los términos de } p$.

  Si el exponente $e$ no aparece en $p$, la función levanta
  `eopl:error`.

**El Código es:**

```racket
;; coeficiente-de
;; Contrato: polinomio x exponente -> coeficiente
;; Proposito: obtener el coeficiente del termino con el exponente indicado

(define coeficiente-de
  (lambda (p exponente)
    (cases polinomio p
      (poli (var terms)
            (buscar-coef terms exponente)))))
```

La función `coeficiente-de` recibe un polinomio y un exponente. Primero
analiza el polinomio mediante `cases` y obtiene su lista de términos.
Después llama a `buscar-coef`, que realiza la búsqueda sobre la
estructura recursiva de los términos.

La función auxiliar utilizada es:

```racket
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
```

**Demostración.**

La demostración se realiza por inducción estructural sobre la lista de
términos.

- **Caso base** (`sin-terminos`):

  En este caso la lista no contiene ningún término. Por lo tanto, el
  exponente buscado $e$ no aparece en el polinomio.

  La función ejecuta:

  ```racket
  (eopl:error
   'coeficiente-de
   "El polinomio no tiene termino con ese exponente")
  ```

  Esto coincide con la especificación, ya que cuando el exponente no
  está registrado en el polinomio la función debe levantar un error.

- **Caso inductivo** (`mas-terminos(t,r)`):

  Sea $t$ el término actual y $r$ el resto de la lista. Se compara el
  exponente buscado $e$ con el exponente del término actual.

  **Primer subcaso:**

  $e = expo(t)$

  El exponente buscado coincide con el exponente del término actual.
  Por lo tanto, el programa ejecuta:

  ```racket
  (valor-coef termino-actual)
  ```

  y devuelve el coeficiente asociado a $e$. Por lo tanto, se cumple la
  post-condición.

  **Segundo subcaso:**

   $e > expo(t)$

  Por el invariante, los exponentes están ordenados estrictamente de
  mayor a menor. Como los exponentes están ordenados estrictamente de mayor a menor,
  si $e > \text{expo}(t)$,
  entonces un término con exponente \(e\) tendría que haber aparecido antes en la lista.
  Como ya se pasó esa posición, \(e\) no puede aparecer en el resto.

  Como ya se llegó a un exponente menor que el buscado sin encontrarlo,
  el exponente $e$ no puede aparecer en el resto $r$. Por lo tanto, la
  función levanta `eopl:error` directamente y no necesita recorrer el
  resto de la lista.

  **Tercer subcaso:**

  $e < expo(t)$

  El exponente buscado puede encontrarse en el resto de la lista. Por
  esta razón, el programa realiza la llamada recursiva:

  ```racket
  (buscar-coef resto expo)
  ```

  En esta llamada se aplica la hipótesis de inducción sobre $r$.
  Por hipótesis de inducción, si $e$ aparece en $r$, la llamada devuelve
  su coeficiente correcto; si no aparece, la llamada termina levantando
  el error correspondiente.

**Levantamiento del error.**

El error se levanta solamente cuando el exponente buscado no aparece en
el polinomio.

Esto puede ocurrir de dos formas. La primera es llegar al caso base
`sin-terminos`, lo que significa que se recorrieron todos los términos
sin encontrar el exponente. La segunda ocurre cuando
$e > \text{expo}(t)$, porque el orden estrictamente decreciente
permite concluir que el exponente buscado ya quedó atrás y no puede
aparecer en el resto.

Cuando el exponente sí aparece, la función devuelve su coeficiente y no
levanta el error.

**Terminación.**

La medida utilizada es la cantidad de términos que quedan por recorrer.

En cada llamada recursiva se pasa de:

$mas-terminos(t,r)$

a la lista $r$, que contiene un término menos que la lista anterior.

Por lo tanto, la medida disminuye estrictamente en cada llamada
recursiva y está acotada inferiormente por $0$. Cuando se alcanza
`sin-terminos` termina la recursión.

**Conclusión:** `coeficiente-de` es correcta porque devuelve el
coeficiente asociado al exponente solicitado cuando este existe y
levanta `eopl:error` cuando el exponente no aparece. Además, la
recursión termina porque en cada llamada se reduce la cantidad de
términos pendientes de revisar.

---

### 2.2 Corrección de `eliminar-termino`

**Especificación.**

- **Tipo:** `eliminar-termino : polinomio × exponente -> polinomio`

- **Pre-condición:** $\mathrm{Inv}(p)$ y $e \in \mathbb{N}$.

- **Post-condición:** si el exponente $e$ aparece en $p$, el resultado es
  el mismo polinomio pero sin el término cuyo exponente es $e$.

  Si el exponente $e$ no aparece en $p$, la función levanta
  `eopl:error`.

**El Código es:**

```racket
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
```

La función principal `eliminar-termino` utiliza esta función auxiliar para
realizar la búsqueda sobre la lista de términos:

```racket
;; eliminar-termino
;; Contrato: polinomio x exponente -> polinomio
;; Proposito: eliminar el termino que tiene el exponente indicado

(define eliminar-termino
  (lambda (p exponente)
    (cases polinomio p
      (poli (var terms)
            (poli var (eliminacion-terminos terms exponente))))))
```

La función `eliminar-termino` recibe un polinomio y un exponente. Primero
analiza el polinomio mediante `cases` y obtiene su lista de términos.
Después llama a `eliminacion-terminos`, que busca el término cuyo exponente
coincide con el solicitado y construye la lista resultante sin dicho
término.

**Demostración.**

La demostración se realiza por inducción estructural sobre la lista de
términos.

- **Caso base** (`sin-terminos`):

  En este caso la lista no contiene ningún término. Por lo tanto, el
  exponente buscado $e$ no aparece en el polinomio.

  La función ejecuta:

  ```racket
  (eopl:error
   'eliminar-termino
   "El polinomio no tiene termino con ese exponente")
  ```

  Esto coincide con la especificación, ya que si el exponente no aparece
  en el polinomio se debe levantar un error.

- **Caso inductivo** (`mas-terminos(t,r)`):

  Sea $t$ el término actual y $r$ el resto de la lista. Se compara el
  exponente buscado $e$ con el exponente del término actual.

  **Primer subcaso:**

  $e = expo(t)$

  El exponente buscado coincide con el exponente del término actual.

  En este caso la función devuelve directamente:

  ```racket
  resto
  ```

  Al devolver `resto`, se elimina el término actual y se conservan todos
  los términos que estaban después de él.

  Por lo tanto, el resultado es exactamente la lista original sin el
  término cuyo exponente es $e$.

  **Segundo subcaso:**

  $e > expo(t)$

  Como los exponentes están ordenados estrictamente de mayor a menor,
  si $e$ es mayor que el exponente del término actual, entonces un término
  con exponente $e$ tendría que haber aparecido antes en la lista.

  Como los exponentes están ordenados estrictamente de mayor a menor,
  el resto de la lista contiene exponentes todavía menores. Por ende,
  $e$ no puede aparecer en el resto de la lista.

  **Tercer subcaso:**

  $e < expo(t)$

  El exponente buscado puede encontrarse en el resto de la lista. Por
  esta razón, la función realiza la llamada recursiva:

  ```racket
  (eliminacion-terminos resto expo)
  ```

  En este caso el término actual se conserva y únicamente se continúa la
  búsqueda en el resto.

  Por hipótesis de inducción, la llamada recursiva elimina correctamente
  el término si el exponente aparece en $r$, o levanta el error si no
  aparece.

  Como el término actual se conserva mediante:

  ```racket
  (mas-terminos termino-actual ...)
  ```

  todos los términos diferentes del que se desea eliminar permanecen en
  el resultado y conservan su orden.

**Levantamiento del error.**

El error se levanta cuando el exponente buscado no aparece en el
polinomio.

Esto puede ocurrir de dos formas. La primera es llegar al caso base
`sin-terminos`, lo que significa que se recorrieron todos los términos
sin encontrar el exponente.

La segunda ocurre cuando:

$e > expo(t)$

porque el orden estrictamente decreciente permite concluir que el
exponente buscado tendría que haber aparecido antes y, por lo tanto, no
puede aparecer en el resto.

Cuando el exponente sí aparece, la función elimina únicamente el término
correspondiente y conserva los demás.

**Terminación.**

La medida utilizada es la cantidad de términos que quedan por recorrer.

En cada llamada recursiva se pasa de:

$mas-terminos(t,r)$

a la lista $r$, que contiene un término menos que la lista anterior.

Por lo tanto, la medida disminuye estrictamente en cada llamada recursiva
y está acotada inferiormente por $0$. Cuando se alcanza `sin-terminos`
termina la recursión.

**Conclusión:** `eliminar-termino` es correcta porque elimina exactamente
el término cuyo exponente coincide con el solicitado y conserva los
demás términos en el mismo orden. Si el exponente no aparece, la función
levanta `eopl:error`. Además, la recursión termina porque en cada llamada
se reduce la cantidad de términos pendientes de revisar.

---

### 2.3 Corrección de `insertar-termino`

**Especificación.**

- **Tipo:** `insertar-termino : polinomio × coeficiente × exponente -> polinomio`

- **Pre-condición:** $\mathrm{Inv}(p)$, $c$ es un coeficiente válido y
  $e \in \mathbb{N}$.

- **Post-condición:** el polinomio resultante conserva el invariante
  $\mathrm{Inv}$ y contiene el término $(c,e)$ combinado con cualquier
  término que ya tuviera el mismo exponente.

  Si el coeficiente resultante es cero, el término correspondiente se
  elimina de la representación.

**El Código es:**

```racket
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
```

La función principal `insertar-termino` realiza las validaciones y llama a
`insertar-terminos` para realizar la inserción:

```racket
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
```

La función `insertar-termino` valida primero que el exponente sea un entero
no negativo y que el coeficiente sea un número exacto. Si el coeficiente es
cero, devuelve el polinomio sin realizar cambios. En caso contrario,
obtiene la lista de términos del polinomio y utiliza `insertar-terminos`
para realizar la inserción conservando las propiedades de la
representación.

**Demostración de preservación del invariante.**

La demostración se realiza sobre la estructura recursiva de la lista de
términos.

Recordemos el invariante definido anteriormente:

$$
\mathrm{Inv}(p) \equiv
\underbrace{\forall i<n:e_i>e_{i+1}}_{\text{orden estricto}}
\land
\underbrace{\forall i:c_i\neq0}_{\text{sin ceros}}
\land
\underbrace{\forall i:e_i\in\mathbb{N}}_{\text{exponentes naturales}}
\land
\underbrace{\forall i:\mathrm{red}(c_i)}_{\text{racionales reducidos}}
$$

Se consideran el caso base y los tres casos indicados por la función.

- **Caso base: lista sin términos**

  Si la lista de términos es `sin-terminos`, el nuevo término se agrega
  directamente:

  ```racket
  (mas-terminos
   (hacer-termino coef expo)
   (sin-terminos))
  ```

  Como `insertar-termino` valida que el exponente sea un entero no
  negativo, el nuevo exponente pertenece a $\mathbb{N}$.

  Además, el término solo llega a `insertar-terminos` cuando el coeficiente
  es diferente de cero, por lo que no se agrega un término con coeficiente
  cero.

  Al existir solamente un término, no hay otro exponente con el cual se
  pueda romper el orden. Por lo tanto, el resultado cumple el invariante.

- **Caso 1: el exponente es nuevo**

  Supongamos que el exponente que se desea insertar no aparece en la
  lista.

  Si:

  $e > expo(t)$

  la función ejecuta:

  ```racket
  (mas-terminos (hacer-termino coef expo) ts)
  ```

  En este caso el nuevo término se coloca antes del término actual.

  Como la lista original ya estaba ordenada estrictamente de mayor a
  menor y el nuevo exponente es mayor que el exponente del término actual,
  la inserción mantiene el orden de los exponentes.

  El nuevo exponente pertenece a $\mathbb{N}$ porque `insertar-termino`
  valida que sea un entero no negativo.

  Además, el término se inserta únicamente cuando el coeficiente es
  diferente de cero, por lo que no se introduce un término con coeficiente
  cero.

  Por lo tanto, se conservan las condiciones del invariante.

  Si:

  $e < expo(t)$

  el término actual se conserva y la función continúa buscando en
  `resto`:

  ```racket
  (mas-terminos termino-actual
    (insertar-terminos resto coef expo))
  ```

  En este punto todavía puede existir un término con exponente $e$ en el
  resto de la lista. Por hipótesis de inducción, la inserción sobre `resto`
  conserva el invariante. Al conservar `termino-actual`, el orden con
  respecto al término insertado también se mantiene.

  Por lo tanto, el resultado continúa cumpliendo el invariante.

- **Caso 2: el exponente ya existe y la suma de coeficientes es
  diferente de cero**

  Cuando el exponente buscado coincide con el exponente del término
  actual se calcula:

  ```racket
  (let ((suma (+ (valor-coef termino-actual) coef)))
  (if (zero? suma)
      resto
      (mas-terminos (hacer-termino suma expo-actual) resto)))
  ```

  Si:

  $c_{actual} + c \neq 0$

  la función crea un nuevo término con el coeficiente resultante:

  ```racket
  (mas-terminos
    (hacer-termino suma expo-actual)
    resto)
  ```

  El exponente no cambia, por lo que se mantiene su posición dentro de la
  lista y, por tanto, se conserva el orden.

  Como la suma es diferente de cero, el nuevo término no contiene un
  coeficiente cero.

  Además, la suma de dos coeficientes racionales produce nuevamente un
  racional, por lo que el coeficiente continúa perteneciendo al tipo
  permitido por la representación.

  Por lo tanto, se mantienen las condiciones del invariante.

- **Caso 3: el exponente ya existe y la suma de coeficientes es cero**

  Si los exponentes coinciden y:

  $c_{actual} + c = 0$

  la función ejecuta:

  ```racket
  resto
  ```

  Esto elimina el término actual de la representación.

  Como el término eliminado tenía el mismo exponente que el término que
  se estaba insertando y sus coeficientes se cancelaron, no se introduce
  ningún término con coeficiente cero.

  Además, `resto` ya formaba parte de una lista que cumplía el invariante.
  Por lo tanto, al eliminar el término actual se conservan los demás
  términos y su orden.

  En consecuencia, el resultado continúa cumpliendo el invariante.

**Terminación.**

La medida utilizada es la cantidad de términos que quedan por recorrer.

Cuando el exponente buscado es menor que el exponente del término actual,
la función realiza una llamada recursiva sobre `resto`:

```racket
(insertar-terminos resto coef expo)
```

`resto` contiene un término menos que la lista anterior. Por lo tanto, la
medida disminuye estrictamente en cada llamada recursiva.

Cuando el término se inserta, se combina con uno existente o se produce
una cancelación, no se realiza otra llamada recursiva.

Por lo tanto, la función termina.

**Conclusión:** `insertar-termino` preserva el invariante de la
representación. En el caso base se construye una lista válida con el nuevo
término. Cuando el exponente es nuevo, el término se coloca en la posición
correspondiente manteniendo el orden. Cuando el exponente ya existe, los
coeficientes se combinan; si la suma es diferente de cero, se mantiene el
término con el coeficiente resultante, y si la suma es cero, el término se
elimina. En todos los casos se mantienen el orden estricto, la ausencia de
coeficientes cero y los exponentes naturales. Además, la recursión termina
porque la cantidad de términos pendientes disminuye en cada llamada
recursiva.

---

## 3. Equivalencia entre las representaciones con listas y procedimientos

### Idea general

Las representaciones con listas y con procedimientos utilizan mecanismos diferentes para representar los datos de un polinomio, pero mantienen la misma interfaz del TAD.

Esto permite que las operaciones principales sobre los polinomios se utilicen de la misma manera, independientemente de cómo se encuentren representados internamente los datos. La diferencia se encuentra en la forma en que cada representación construye y consulta sus estructuras.

---

### Representación con listas

En la representación con listas, los datos se construyen utilizando listas de Racket. Cada estructura contiene una etiqueta que permite identificar el tipo de dato y los valores que forman parte de él.

Por ejemplo, un polinomio se representa mediante una lista que contiene la etiqueta, la variable y la lista de términos. De manera similar, un término contiene su etiqueta, coeficiente y exponente.

Los observadores recuperan los valores de estas estructuras utilizando las posiciones correspondientes dentro de la lista, mediante operaciones como `car`, `cadr` y `caddr`.

Por lo tanto, en esta representación la información está almacenada directamente en la estructura de listas.

---

### Representación con procedimientos

En la representación con procedimientos, cada dato se representa mediante una función. El procedimiento recibe un mensaje y responde con la información correspondiente. El código utiliza mensajes como `'tipo`, `'coef`, `'expo`, `'term` y `'resto` para consultar los datos.

Por ejemplo, el procedimiento que representa un término puede recibir un mensaje para obtener su coeficiente o su exponente. De la misma manera, el procedimiento que representa un polinomio permite consultar su variable y sus términos.

En este caso, la información no se obtiene directamente de una estructura de lista, sino mediante la comunicación con el procedimiento a través de los mensajes definidos.

---

### ¿En qué consiste la equivalencia?

La equivalencia entre ambas representaciones se encuentra en que las dos proporcionan la misma interfaz y permiten realizar las mismas operaciones sobre el TAD.

| Aspecto | Representación con listas | Representación con procedimientos |
|---|---|---|
| Representación interna | Listas de Racket | Procedimientos |
| Acceso a los datos | `car`, `cadr`, `caddr`, etc. | Mensajes enviados al procedimiento |
| Construcción de datos | Se crean listas con etiquetas | Se crean funciones que responden a mensajes |
| Interfaz del TAD | La misma | La misma |
| Operaciones principales | Las mismas | Las mismas |

En ambos casos se mantienen las operaciones `polinomio-cero`, `insertar-termino`, `coeficiente-de` y `eliminar-termino`, con los mismos contratos. 

Por ejemplo, cuando se necesita obtener el coeficiente de un término, la forma de acceder al dato cambia según la representación, pero la operación que utiliza el resto del programa sigue siendo `coeficiente-de`.

---

### Relación con la abstracción de datos

De acuerdo con la idea de representación de datos de EOPL §2.2, es posible separar la representación interna de los datos de las operaciones que trabajan sobre ellos.

En este caso, las funciones de la interfaz actúan como una capa de abstracción. El usuario del TAD no necesita conocer si el polinomio está representado mediante listas o mediante procedimientos; solamente necesita utilizar las operaciones definidas por la interfaz.

Por esta razón, cambiar la representación interna no implica cambiar la forma en que se utilizan las operaciones principales del polinomio.

---

### Conclusión

Las representaciones con listas y con procedimientos son diferentes internamente, pero pueden proporcionar el mismo comportamiento observable a través de la interfaz del TAD.

La representación con listas almacena los datos directamente en estructuras de lista, mientras que la representación con procedimientos encapsula los datos dentro de funciones que responden a mensajes.

Por lo tanto, la equivalencia se establece a nivel de la **interfaz y del comportamiento del TAD**, mientras que la implementación interna de cada representación permanece diferente.

---

## 4. Referencias

- Friedman, D. P., & Wand, M. (2008). *Essentials of Programming Languages* (3.ª ed.). MIT Press. Secciones 2.1, 2.2 y 2.4.
