# Calificaciones

## Practicas

### Practica 1

Buena practica, sigan así.

**Calificación: 10**

### Practica 2

Su implementación de setvalueA tiene un pequeño bug

> (setvalueA (MArray 7 '(1 1 1 1 1 0 0)) 6 3)
(MArray 7 '(0 1 1 1 1 1 3))

cuando debería regresar (MArray 7 '(1 1 1 1 1 0 3))

igual su función printML tiene un pequeño bug con esta entrada:
(printML (MCons (MCons 1 (MCons 2 (MEmpty))) (MCons (MCons 2 (MCons 3 (MEmpty))) (MEmpty))))

debería regresar "[[1, 2], [2, 3]]"

Pero muy bien sigan así.

**Calificación: 10**

### Practice 3
Unos detalles en la implementación de su collapse-trackpoints pero
en general muy bien.

**Calificación: 10**

### Practice 4

Muy bien, aunque tienen un pequeño bug en el interprete con with multi-parametricos.

**Calificación: 10**

### Practice 5

Todo bien pero les falto la implementacion
de recursion usando procedimientos.

**Calificación: 8**

### Practice 6

**Calificación: 10**

## Tareas

### Tarea 1

Problema 1: Solo hay que especificar que un Hash es O(1) en obtener un valor
dada una llave si no hay colisiones.
Problema 2: Excelente bien explicado.
Problema 3: Les falto escribir las llamadas a subst y interp.

**Calificación: 9**

### Tarea 2

Problema 1: Muy bien, aunque su explicación de como usar el combinador Y para
obtener funciones recursivas no es la mejor.

Problema 2:  Los operadores lógicos tienen una semantica short-circuit como indican pero esta semantica es usada en lenguajes sean perezosos o glotones así que tu programa no ayuda a indicar que Java es glotón, aunque lo es.

Problema 3: Error mio de la cadena de error, el error debe decir que x no esta
en el ambiente, pero si están en lo correcto con respecto a los dos interpretes.

Problema 4: Bien su razonamiento, aunque escribir un programa de ejemplo hubiera sido lo mejor.

**Calificación: 9**

### Tarea 3

Problema I: Falto el juicio de fibonacci.

Problema II: Muy bien.

Problema III: No hicieron nada.

Problema IV: Bien.

Problema V: Bien.

Problema VI: Muy bien.

**Calificación: 7.5**
