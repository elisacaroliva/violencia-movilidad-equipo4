# Bitácora

## v5, 3 de noviembre de 2026: el Entregable 3 como especificación

**Por qué hay una v5.** El Entregable 3 describe, de forma resumida, un análisis más amplio que la v4: diseño municipal, primera etapa, transiciones, combinación con la EMOVI 2017 y más validación de los conglomerados. Varias de sus cifras salían de código en Python que no estaba en el proyecto. La v5 lleva todo a R, tomando el texto del artículo como instrucciones, y verifica 111 cifras.

**Qué cambia:**
1. **Construcción principal.** Pasa a ser la del artículo: población censal interpolada, robo 1997-2001 = 0, empates por orden e IC normales. Las correcciones de la v4 quedan en `t07_robustez` (D18).
2. **Nuevo:**
   - Tabla 1 con expuestos y no expuestos;
   - escolaridad propia y de los padres (D19);
   - pendiente en cinco bandas de nacimiento;
   - correlación entre entidades con IC de Fisher, con y sin Campeche;
   - Tabla 3 de capas de controles;
   - errores agrupados por año de nacimiento;
   - validación completa de los conglomerados: silueta de Ward y k-medias, R² contra k = 1, Calinski-Harabasz, Davies-Bouldin, 500 submuestras e invarianza en k = 2-4 (D20);
   - Tabla 6: rango de escolaridad, transiciones con tipo de cambio y primera etapa.
3. **Diseño municipal de vuelta, como módulo omitible (script 11).** Se descargaron el archivo municipal del SESNSP y las proyecciones municipales de CONAPO (D21).
4. **EMOVI 2017 como módulo omitible (script 12).** Está listo, pero falta el archivo.
5. **El script 13** sustituye al puente de la presentación y verifica contra el Entregable 3.

**Resultados:**

| | v5 | Entregable 3 |
|---|---|---|
| b3 (EE) | +0.0160 (0.0255) | +0.0161 (0.0255) |
| p de permutación | 0.62 | 0.62 |
| Tendencias previas | F = 0.41, p = 0.80 | F = 0.42, p = 0.79 |
| Tabla 3 | 0.030 → 0.001 → 0.047 → 0.016 | idéntica |
| Conglomerados | 15/11/6, cofenética 0.603, Rand ajustado 0.849, estabilidad 0.862 (p5 0.621) | 0.865 (p5 0.631) |
| Primera etapa: escolaridad / activos / migración | 0.034 / −0.028 / 0.008 | idéntica |
| Municipal: conteo | −0.0344 (0.0232) | idéntico |
| Municipal: tasa | 0.0066 (0.0177) | 0.0084 (0.0175) |
| EMD estatal / municipal | 11.0 % / 7.6 % | 11.0 % / 7.6 % |

- Verificación: 97 de 111 cifras coinciden, 9 difieren (explicadas en DECISIONES F) y 5 están omitidas (EMOVI 2017).

**Hallazgos para el texto del artículo:**
- Los efectos fijos entidad × cohorte sí son estimables (D23).
- El pico del homicidio está en 2017, no en 2011.
- El grupo tardío tiene 10 entidades en la estimación, no 11.
- El cero del robo en 1997-2001 debería explicarse en la Fig. 1.

**Técnico:** el pipeline completo tarda unos 2 minutos.

## v4, 3 de noviembre de 2026: modelo por entidad de la presentación, corregido

**Por qué hay una v4.** La v3 implementaba la minuta del 19 de octubre: años de intensidad alta en 2011-2025, umbral en el punto medio y empalme de metodologías. Eso no corresponde a la presentación *Entregas 1 y 2*, que ahora es la referencia. La v4 implementa en R el modelo de la presentación y agrega las correcciones. La versión municipal queda descartada.

**Qué se conserva de la presentación:**
- Serie estatal del SESNSP, metodología anterior, 1997-2017.
- Cuatro delitos.
- Intensidad = promedio del índice en 2007-2017.
- Dosis = intensidad × fracción de la ventana 10-18 posterior a 2007.
- Triple diferencia con efectos fijos de entidad y de año de nacimiento, y la pendiente propia de cada uno.
- Errores agrupados por entidad, permutación y conglomerados de trayectorias.

La lógica de `modelo.py` se conserva entera en el script 11, que reproduce **40 de 40** cifras de la presentación, incluidas las de conglomerados.

**Correcciones (ver DECISIONES):**
1. **Matiz 1.** El estudio de eventos lleva la pendiente por año de nacimiento (D12).
2. **Matiz 2.** Todo se reporta por DE de intensidad, y la permutación aparece en las dos unidades (D02).
3. **Matiz 3:**
   - el índice es el promedio, y se demuestra por qué el componente principal no sirve (D06);
   - el robo de 1997-2001 se trata como faltante (D07);
   - la pendiente entre cohortes se calcula con la muestra analítica (0.554 → 0.665);
   - la varianza de la dosis va ponderada (24/8/68);
   - rural = p21 = 5;
   - el efecto mínimo detectable es exacto.
4. **Otras:**
   - población CONAPO (D08);
   - empates de rango compartidos (D03);
   - t de 30 grados de libertad (D15);
   - "nacido en 1996" = 8/9 (D09);
   - redacción "probablemente atenuado" en vez de "cota inferior" (APRENDIZAJES 2.5).
5. **Nuevo:**
   - placebo con adultos jóvenes;
   - destino por ingreso;
   - dosis con calendario propio por conglomerado (lámina "Lo que falta");
   - prueba de estabilidad de los conglomerados;
   - bootstrap salvaje.

**Resultados:**

| | b3 por DE (EE) | p t(30) | p permutación | p bootstrap salvaje |
|---|---|---|---|---|
| Principal | +0.017 (0.025) | 0.49 | 0.59 | 0.55 |
| Solo homicidio | +0.010 (0.022) | 0.64 | 0.76 | 0.68 |
| Calendario por conglomerado | +0.042 (0.032) | 0.20 | 0.26 | 0.25 |
| Calendario (partición de la presentación) | +0.058 (0.032) | 0.09 | 0.17 | 0.18 |

- Efecto mínimo detectable: 0.073 = **11 % de la pendiente** (0.650).
- Tendencias previas: F = 0.38, p = 0.82. Sin la pendiente por año de nacimiento: F = 1.92, p = 0.13.
- Placebo: −0.001 (0.033).
- Las 11 pruebas de robustez son nulas.
- Por conglomerado, el grupo temprano da +0.091 (t = 2.5) con p de permutación de 0.16: es una pista, no un hallazgo.

**Hallazgo de la auditoría:** la estructura de 3 conglomerados se debilita al tratar el robo de 1997-2001 como faltante. La silueta queda casi plana, y solo el grupo temprano es estable.

**Técnico:**
- La permutación ahora se calcula por lotes con la misma secuencia aleatoria, lo que da resultados idénticos y es unas 7 veces más rápida.
- Se agregó una tolerancia numérica para empates en la permutación.
- El pipeline completo tarda unos 3.5 minutos.

## v3, 30 de octubre de 2026 (con la corrida del 3 de noviembre)

Implementó la minuta adaptada a entidad (años altos 2011-2025, empalme de metodologías). El 3 de noviembre se corrió con la serie nueva real del SESNSP. Sus pendientes quedaron en `v3/PENDIENTES_REVISION_v3.md`; la mayoría desaparece por diseño en la v4, que no usa la serie nueva ni el umbral de punto medio.
