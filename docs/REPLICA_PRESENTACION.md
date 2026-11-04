# Auditoría: ¿se obtienen los resultados de la presentación "Entregas 1 y 2"?

> **Nota v5:** este documento es la auditoría de la presentación hecha en la v4 y se conserva como historia. En la v5 el script 11 ya no es el puente con la presentación (ahora es el diseño municipal); la verificación vigente está en `scripts/resultados/13_verificacion_entregable3.R`.

**Respuesta corta: sí.** Las 40 cifras verificables se reproducen desde los datos crudos con la construcción original de `modelo.py` (`scripts/resultados/11_puente_presentacion.R` y `output/tablas/t11_verificacion_presentacion.csv`).

## Lo que coincide

| Lámina | Presentación | Obtenido (script 11) |
|---|---|---|
| Muestra principal / sin corresidentes / con migrantes | 15,558 / 12,705 / 17,789 | Idénticas |
| Varianza del PC1, origen / destino | 21.0 % / 16.1 % | 21.0 % / 16.1 % |
| Intensidad: DE / Morelos / Yucatán | 0.409 / +1.20 / −0.52 | 0.409 / +1.199 / −0.522 |
| Pendiente base | 0.650 | 0.650 |
| b3 (EE), por DE: principal / sin corresidentes / con migrantes / solo homicidio | +0.016 (0.026) / +0.009 (0.024) / +0.004 (0.023) / +0.010 (0.022) | Idénticos |
| Efecto mínimo detectable | 0.071 | 0.0714 |
| Permutación | p = 0.64 | p = 0.625 (depende de la semilla) |
| Varianza de la dosis: entidad / cohorte / residual | 21 / 21 / 59 % | 20.6 / 20.8 / 58.9 % |
| Tendencias previas | F = 0.42, p = 0.80 | F = 0.41, p = 0.80 |
| Correlación entre entidades | −0.19 | −0.19 |
| Correlación cofenética / Rand ajustado / k por silueta | 0.603 / 0.849 / 3 | 0.603 / 0.849 / 3 |
| Conglomerados | 15 / 11 / 6 | Idénticos, entidad por entidad |
| b3 por conglomerado (t; p de permutación) | +0.085 (2.59; 0.194), −0.035 (−2.64; 0.373), −0.031 (−0.30; 0.80) | +0.085 (2.59; 0.205), −0.035 (−2.62; 0.395), −0.032 (−0.30; 0.775) |

## Cómo se corrigieron los tres matices en la v4

1. **Estudio de eventos.**
   - El `modelo.py` omitía la pendiente por año de nacimiento, y sin ella la prueba falla: F = 2.88, p = 0.04.
   - La v4 la incluye siempre (script 09) y deja la otra versión como diagnóstico.
   - Con la construcción corregida: F = 0.38, p = 0.82 (con el término) y F = 1.92, p = 0.13 (sin él).
2. **Unidades.**
   - Todo se reporta por DE de intensidad, con una columna en unidades crudas.
   - En unidades crudas, el rango nulo de la permutación es [−0.146, +0.157] y el intervalo analítico es [−0.088, +0.167], de ancho similar.
   - Por DE, son [−0.059, +0.064] y [−0.036, +0.068] (`t11_permutacion_presentacion_unidades`).
3. **Detalles de construcción:**
   - **Índice.** Se queda el promedio y se corrige el texto: el componente principal da carga negativa al robo en carreteras y explica 35 % (D06).
   - **Robo en carreteras antes de 2002.** Pasa a faltante (D07).
   - **Pendiente entre cohortes.** Con la muestra analítica es 0.554 → 0.665. La cifra de la lámina, 0.516 → 0.652, usa la muestra completa.
   - **Varianza de la dosis.** Ponderada queda en 24 / 8 / 68 %; sin ponderar, 22 / 16 / 62 %. Ambas son con la intensidad corregida.
   - **Rural.** p21 = 5.
   - **Efecto mínimo detectable.** Con t(30): 2.90 × EE.

## Otros errores de la presentación

- "Nacido en 1996, uno": es 8/9; el 1 es para los nacidos en 1997-1998.
- "Repunte tardío · 11 entidades": en la estimación son 10, porque Tlaxcala no tiene muestra.
- "Cota inferior": no se sostiene formalmente, porque la selección por migración tiene signo indeterminado (APRENDIZAJES 2.5).
- La estructura de 3 conglomerados depende del cero artificial del robo en 1997-2001 (D13).

## Lo que no se pudo verificar

La lámina "Lo que intentamos" no tiene código ni datos en la carpeta: destino educativo, transiciones entre quintiles, EMOVI 2017 combinada y conteos municipales. Tampoco DBSCAN con 533 municipios. Por decisión del equipo, lo municipal ya no se hará.
