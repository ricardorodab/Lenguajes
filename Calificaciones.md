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
