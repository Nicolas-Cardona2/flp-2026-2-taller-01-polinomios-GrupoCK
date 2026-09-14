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
| {{Nombre 1}} | {{Código 1}} | {{correo1@correounivalle.edu.co}} |
| {{Nombre 2}} | {{Código 2}} | {{correo2@correounivalle.edu.co}} |

---

## 1. Marco formal

### 1.1 Corrección de programas recursivos

Sea $f : A \to B$ una función y $A$ un conjunto definido
recursivamente. Sea $P_f$ un programa recursivo en Racket que pretende
calcular $f$. Decimos que $P_f$ es correcto con respecto a su
especificación si se cumple:

$$
\forall a \in A \,:\, P_f(a) = f(a)
$$

La estrategia de demostración es **inducción estructural** sobre $A$.
Aquí $A$ es el conjunto de listas de términos que genera la gramática:

- **Caso base:** $a = \text{sin-terminos}()$, y se verifica
  $P_f(a) = f(a)$ directamente.
- **Caso inductivo:** $a = \text{mas-terminos}(t, r)$. Se asume la
  **hipótesis de inducción** $P_f(r) = f(r)$ sobre el resto de la
  lista y se demuestra $P_f(a) = f(a)$.

Si alguna de sus funciones quedó escrita con un acumulador en lugar de
recursión estructural, la corrección se argumenta con una invariante
del acumulador y no con la hipótesis de inducción: enuncie la
invariante, demuestre que vale al inicio, que cada paso la conserva y
que al terminar implica la post-condición.

### 1.2 El invariante de la representación

Las cuatro condiciones del enunciado se enuncian como una única
propiedad sobre polinomios. Sea $p$ un polinomio con términos
$t_1, t_2, \ldots, t_n$, donde $t_i = (c_i, e_i)$:

$$
\mathrm{Inv}(p) \equiv
\underbrace{\forall i < n : e_i > e_{i+1}}_{\text{orden estricto}}
\ \land\
\underbrace{\forall i : c_i \neq 0}_{\text{sin ceros}}
\ \land\
\underbrace{\forall i : e_i \in \mathbb{N}}_{\text{exponentes naturales}}
\ \land\
\underbrace{\forall i : \mathrm{red}(c_i)}_{\text{racionales reducidos}}
$$

donde $\mathrm{red}\left(\frac{a}{b}\right)$ abrevia
$b > 0 \,\land\, \mathrm{mcd}(|a|, b) = 1$, y un coeficiente entero se
toma como el racional de denominador $1$.

{{Si prefiere escribir el invariante con otra notación, hágalo, pero
las cuatro condiciones deben quedar todas y de forma que se puedan
verificar término por término.}}

---

## 2. Funciones analizadas

### 2.1 Corrección de `coeficiente-de`

**Especificación.**

- **Tipo:** `coeficiente-de : polinomio × exponente -> coeficiente`
- **Pre-condición:** $\mathrm{Inv}(p)$ y {{condición sobre el
  exponente consultado}}.
- **Post-condición:** $\text{Post}(p, e, r) \equiv {{\ldots}}$ cuando
  el exponente $e$ aparece en $p$; y la función levanta
  `eopl:error` cuando no aparece.

**Código.**

```racket
; coeficiente-de : {{contrato}}
; Propósito: {{...}}
(define (coeficiente-de p e)
  ...)
```

**Demostración.**

- **Caso base** ($\text{sin-terminos}$): {{qué hace el programa y por
  qué eso es exactamente levantar el error.}}

  $$
  {{\ldots}}
  $$

- **Caso inductivo** ($\text{mas-terminos}(t, r)$): distinga los tres
  subcasos según la comparación entre el exponente de $t$ y el
  exponente buscado. {{Uno de ellos usa la hipótesis de inducción
  sobre $r$; explique por qué el orden estricto del invariante permite
  cortar la búsqueda sin recorrer el resto de la lista.}}

  $$
  {{\ldots}}
  $$

- **Levantamiento del error.** Demuestre que el error se levanta
  cuando el exponente no está y **solo** en ese caso.

- **Terminación.** {{Medida que decrece estrictamente en cada llamada
  y cota inferior.}}

**Conclusión:** {{...}}

---

### 2.2 Corrección de `eliminar-termino`

**Especificación.**

- **Tipo:** `eliminar-termino : polinomio × exponente -> polinomio`
- **Pre-condición:** $\mathrm{Inv}(p)$ y {{...}}.
- **Post-condición:** el resultado contiene **exactamente** los
  términos de $p$ menos el de exponente $e$. Formalmente:
  $$
  \text{terminos}(r) = \text{terminos}(p) \setminus \{{\ldots}\}
  $$
  y la función levanta `eopl:error` si $e$ no aparece en $p$.

**Código.**

```racket
(define (eliminar-termino p e)
  ...)
```

**Demostración.** Siga el esquema de 2.1: caso base, caso inductivo
con hipótesis de inducción, error y terminación. {{Además de la
igualdad de conjuntos de términos, argumente que el resultado sigue
cumpliendo $\mathrm{Inv}$: quitar un término no rompe el orden
estricto ni introduce ceros.}}

---

### 2.3 `insertar-termino` preserva el invariante

**Enunciado.** Si $\mathrm{Inv}(p)$ vale antes de la llamada, entonces
$\mathrm{Inv}(\texttt{insertar-termino}(p, c, e))$ vale sobre el
resultado.

**Código.**

```racket
(define (insertar-termino p c e)
  ...)
```

**Demostración por casos.** Cubra los tres casos del enunciado y
verifique en cada uno las cuatro condiciones del invariante:

- **Caso A — el exponente es nuevo.** {{Dónde queda el término
  insertado y por qué el orden estricto se conserva. Qué pasa si el
  coeficiente que llega es cero.}}

- **Caso B — el exponente ya existía y la suma no es cero.** {{El
  término se reemplaza por uno con el coeficiente sumado; el orden no
  cambia porque el exponente es el mismo. Argumente que el coeficiente
  resultante queda reducido y con denominador positivo.}}

- **Caso C — el exponente ya existía y la suma es cero.** {{El término
  desaparece. Argumente que quitarlo conserva el orden estricto y que
  el resultado no queda con un cero, que es justo lo que exige la
  segunda condición.}}

**Terminación.** {{...}}

**Conclusión:** {{...}}

---

## 3. Equivalencia de las dos representaciones

Argumente por qué las funciones de la interfaz son las mismas para la
representación basada en listas y la basada en procedimientos, y qué
propiedad de la interfaz impide que el cliente las distinga. Basta una
explicación conceptual apoyada en la sección 2.2 de EOPL, sin
demostración formal.

Conviene que la explicación responda a esto:

- {{Qué ve el cliente de un polinomio: qué operaciones tiene
  disponibles y qué no puede hacer.}}
- {{Qué cambia entre las dos representaciones y por qué ese cambio
  queda del lado de adentro de la interfaz.}}
- {{Qué habría que hacer para que el cliente sí notara la diferencia,
  y por qué eso significaría que la abstracción se rompió.}}

---

## 4. Referencias

- Friedman, D. P., & Wand, M. *Essentials of Programming Languages*,
  3.ª ed., MIT Press, 2008. Sección 2.1 (especificación de datos),
  sección 2.2 (representación basada en listas y basada en
  procedimientos), sección 2.4 (`define-datatype` y `cases`).
- {{Otras referencias que hayan consultado.}}
