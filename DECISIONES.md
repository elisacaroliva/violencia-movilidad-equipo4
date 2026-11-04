# Decisiones metodológicas, versión 5

Las decisiones D01-D17 vienen de la v4 y siguen vigentes, salvo donde la sección E dice otra cosa. Las decisiones nuevas de la v5 son D18-D25, y la sección F explica cada diferencia con el Entregable 3.

Cada decisión dice qué se hizo, por qué y dónde está en el código.

- **Presentación:** viene de la presentación *Entregas 1 y 2* y se conserva.
- **Corrección:** cambia respecto de la presentación o de `modelo.py`. Es obligatoria y la versión anterior sigue disponible en el script 11.
- **Nuevo:** no estaba en la presentación.

## A. Pregunta y parámetro

**D01. Pregunta (Presentación).** ¿La exposición a la violencia del crimen organizado entre los 10 y los 18 años aumenta la persistencia intergeneracional? El nivel geográfico principal es la entidad. **v5:** la variación municipal dentro de la entidad vuelve como módulo omitible (D21).

**D02. Parámetro (Presentación).** b3 es el cambio en la pendiente rango-rango por desviación estándar (DE) de intensidad para quien vivió toda su ventana 10-18 después de 2007.
- Todas las tablas usan esa unidad.
- La columna `b3_crudo` da el coeficiente por unidad del índice. Es una corrección del matiz 2: la presentación mezclaba ambas unidades.
- Para dar el tamaño, se reporta el porcentaje de la pendiente base (0.650) y el efecto de pasar del percentil 10 al 90 de intensidad entre entidades (2.03 DE).

**D03. Movilidad relativa con índice de activos, no con ingreso (Presentación, más una robustez nueva).** Origen y destino se miden con el primer componente principal de activos, servicios y vivienda: 43 reactivos de origen y 40 de destino. Los rangos se calculan dentro de cada cohorte de edad del CEEY. Por qué no se usa ingreso:
1. **No existe para el origen.** La EMOVI no pregunta el ingreso del hogar a los 14 años, y recordar un ingreso de hace 10 a 50 años no es confiable. Los activos, como tener refrigerador, auto o piso firme, sí se recuerdan bien.
2. **Medir igual los dos extremos.** Si el origen es de activos, el destino también debe serlo. Medidas distintas en cada extremo meten error diferencial en la pendiente.
3. **El ingreso actual es ruidoso y de ciclo de vida.** `ingc_pc` es un solo periodo, imputado por el CEEY, y a los 25-30 años está lejos del ingreso permanente. Ese error atenúa la pendiente intergeneracional (Solon). Los activos se acercan más al ingreso permanente. Es además la práctica del CEEY.
4. **Robustez nueva:** destino por ingreso (`r_des_ing`). b3 sale 0.027 (EE 0.031), en la misma dirección. El efecto mínimo detectable sube de 11 % a 14 % de la pendiente: es más ruidoso, como se esperaba.

Los empates en el índice comparten rango (Corrección). En la Entrega 2 se rompían por el orden de las filas, que es arbitrario.

## B. Violencia

**D04. Una sola serie, sin empalmes (Presentación).**
- La serie es la estatal del SESNSP con la metodología anterior, 1997-2017. Es la única que llega a 1997 y cubre lo previo al repunte.
- La dosis solo necesita años hasta 2016 (el más joven nació en 1998 y cumplió 18 en 2016) y la intensidad termina en 2017. Por eso la serie nueva no hace falta y no se mezclan metodologías.
- La v3 sí las mezclaba y necesitaba un empalme (ver BITÁCORA).

**D05. Delitos (Presentación).** Homicidio doloso, secuestro, extorsión y robo en carreteras con violencia (todos sus subtipos). Los patrones están en `config/mapa_delitos.csv`.

**D06. Índice = promedio de las cuatro tasas estandarizadas (Corrección del texto, no del código; matiz 3).**
- La lámina decía "primer componente principal", pero el código lo promediaba.
- La v4 calcula el componente principal (`t03_cargas_pc1_violencia`): explica solo 35 % de la varianza y da al robo en carreteras una carga **negativa** (−0.07). Es decir, el componente principal saca del índice a uno de los cuatro delitos.
- El índice es formativo: los cuatro delitos *definen* el concepto, no son reflejos de un solo factor latente. Por eso lo correcto es la ponderación igual.
- Robustez: con el componente principal, b3 = 0.026 (EE 0.022).

**D07. Robo en carreteras antes de 2002 = faltante (Corrección, matiz 3).**
- No tiene registro nacional en 1997-2001. La Entrega 2 lo trataba como cero, con lo que esos años parecían "pacíficos" en ese delito.
- Ahora, en esos años el índice promedia los tres delitos disponibles.
- No afecta la intensidad (2007-2017), pero sí las trayectorias de los conglomerados (D13).

**D08. Población CONAPO (Corrección).**
- La Entrega 2 interpolaba entre los censos de 2010 y 2020. Para 1997-2009 eso es extrapolar la tendencia 2010-2020 hacia atrás; en 1997 difiere de CONAPO hasta 18 % en algunas entidades (`t03_poblacion_censal_vs_conapo`).
- Robustez: con la población censal, b3 = 0.017, idéntico.

**D09. Intensidad y dosis (Presentación).**
- Intensidad = promedio del índice en 2007-2017, dividido entre su DE entre las 31 entidades de la muestra (0.403).
- Dosis = intensidad × fracción de la ventana 10-18 vivida desde 2007.
- La lámina dice "nacido en 1996, uno"; el valor correcto es 8/9. El 1 corresponde a los nacidos en 1997 y 1998.

## C. Muestra

**D10. Muestra (Presentación).**
- Flujo: 17,843 → 17,789 (con entidad a los 14 dentro del país) → 15,558 (viven en la misma entidad que a los 14).
- Tlaxcala no aparece porque nadie en la encuesta vivía ahí a los 14.
- Robustez: con migrantes (17,789) y sin corresidentes (12,705).
- **Nota teórica (E3 en APRENDIZAJES):** excluir a los migrantes condiciona en una variable posterior al tratamiento. La especificación con migrantes, con la exposición asignada por la entidad a los 14 (que es previa al tratamiento), es el "intento de tratamiento" y no tiene ese problema. Debe reportarse siempre junto a la principal.

**D11. Origen rural = localidad de menos de 2,500 habitantes a los 14 años (p21 = 5) (Corrección, matiz 3).** Es la definición del INEGI. La Entrega 2 usaba p21 de 3 a 5, que incluye ciudades de 15 mil a 100 mil habitantes.

## D. Identificación e inferencia

**D12. Efectos fijos y estudio de eventos (Corrección del código, matiz 1).**
- El modelo y el estudio de eventos llevan los mismos efectos fijos: entidad, año de nacimiento y la pendiente propia de cada uno.
- El `modelo.py` que circuló omitía la pendiente por año de nacimiento en el estudio de eventos. Sin ella, la subida de la pendiente con la edad contamina los coeficientes previos. Con los datos de la presentación, la prueba de tendencias previas falla (F = 2.88, p = 0.04; `t11`).
- La versión sin ese término se reporta solo como diagnóstico (`t09`).

**D13. Conglomerados (Presentación, más una prueba de estabilidad nueva).**
- Se agrupan las trayectorias 1997-2017 estandarizadas dentro de cada entidad con Ward, k-medias (silueta para k = 2 a 8) y DBSCAN.
- Con el índice de la Entrega 2 se reproduce todo: correlación cofenética 0.603, índice de Rand ajustado 0.849, k = 3 y grupos de 15, 11 y 6 entidades.
- **Hallazgo nuevo:** con el índice corregido (robo faltante antes de 2002), la estructura se debilita.
  - La silueta queda casi plana (0.21-0.23) y el mejor k es 5.
  - El índice de Rand ajustado baja a 0.65.
  - Solo el grupo de repunte temprano es estable: 14 de 15 entidades coinciden.
  - Los grupos "tardío" y "sin repunte" se reacomodan. Parte de "sin repunte" dependía del cero artificial del robo.
- Se usa k = 3 con las dos particiones y se reportan ambas.
- Una silueta menor a 0.25 indica estructura débil (Kaufman y Rousseeuw), lo que conviene decir en el texto.

**D14. Dosis con calendario propio por conglomerado (Nuevo; lámina "Lo que falta", punto 2).**
- El repunte de cada grupo empieza el primer año desde el cual su perfil medio queda por encima del promedio de la entidad hasta su pico: 2008 en el grupo temprano y 2010 en el tardío.
- El grupo sin repunte tiene dosis 0.
- Resultado: b3 = 0.042 (p de permutación = 0.26). Con la partición de la presentación, b3 = 0.058 (p = 0.17).
- Es la pista más grande y va en el signo de la hipótesis, pero no es significativa.

**D15. Inferencia (Presentación, con una corrección).**
- Los errores se agrupan por entidad.
- **Corrección:** con G = 31 conglomerados, los valores p, los intervalos y el efecto mínimo detectable usan la t de 30 grados de libertad (MDE = 2.90 × EE en lugar de 2.8 × EE). La normal sobreestima la precisión con pocos conglomerados.
- Permutación: la intensidad de cada entidad se reasigna a otra y se conserva el componente de cohorte de la dosis. En la dosis de calendario se reasigna el par intensidad y año de inicio.
- Bootstrap salvaje con pesos de Webb.
- Con pocos conglomerados manda la permutación.
- Por conglomerado, la permutación es solo entre las entidades del grupo. Con 3 entidades, como el grupo "sin repunte" de la v4, solo existen 6 permutaciones y el valor p no es informativo.

**D16. Placebo (Nuevo).**
- Muestra: nacidos en 1988 o antes, cuya ventana 10-18 es previa a 2007.
- "Dosis": la intensidad por la fracción de los 19 a 27 años vivida desde 2007.
- Es la prueba directa de que el efecto, de existir, es del periodo formativo. Resultado: b3 = −0.001 (EE 0.033).

**D17. Tolerancias del puente (script 11).**
- Los valores p de permutación de la presentación se comparan con una tolerancia de 0.035, que son 3 errores Monte Carlo con 2,000 repeticiones.
- El valor exacto depende de la semilla. Con otra semilla, el audit anterior obtuvo 0.376 y 0.796 frente a 0.394 y 0.775 aquí.


## E. Versión 5: el Entregable 3 como especificación

**D18. Construcción principal = la del Entregable 3 (Nuevo en v5; cambia D06-D08 y D15).**
- El índice es el promedio de las cuatro tasas. Usa población interpolada entre los censos 2010 y 2020, y el robo en carreteras de 1997-2001 entra como cero.
- Los rangos aplican la regla [cumsum(w) − w/2]/Σw sobre el orden del índice, con empates por orden de fila.
- IC normales; EMD = 2.8 × EE.
- Se eligió así para que el código produzca exactamente lo que dice el artículo.
- Las correcciones de la v4 no se pierden. Aparecen en `t07_robustez` y ninguna cambia la conclusión:

| Variante | b3 por DE (EE) |
|---|---|
| Construcción v4 completa | 0.017 (0.025) |
| Solo CONAPO | 0.016 |
| Solo robo como faltante | 0.017 |
| Empates compartidos | 0.016 |

- Cada tabla trae también la columna `p_t`, con t(30).
- **Pendiente para el texto:** la Fig. 1 muestra el robo en carreteras en cero en 1997-2001, lo cual es un artefacto de no tener registro. Conviene decirlo en la nota de la figura.

**D19. Escolaridad (Nuevo).**
- La variable p9 tiene 13 niveles más "ninguno" (97). Se traduce a años completos así: 97 → 0, primaria 6, secundaria 9, preparatoria 12, licenciatura 16, posgrado 18, y las carreras técnicas y normales según su antecedente.
- Escolaridad de los padres = máximo de p42a y p42b; 98 ("no sabe") se trata como faltante.
- El rango de escolaridad se calcula dentro de cada cohorte del CEEY.

**D20. Selección del número de conglomerados y validación (Nuevo; Tabla 4).**
- Silueta de Ward y de k-medias para k de 2 a 8. Se agregan R² contra el modelo k = 1 (el calendario común de la dosis), Calinski-Harabasz y Davies-Bouldin.
- Estabilidad: 500 submuestras del 80 % de las entidades.
- Invarianza del hallazgo en k = 2, 3 y 4 (`t04_invarianza_k`): el grupo temprano es idéntico en los tres valores y el grupo tardío permanece junto.
- DBSCAN con radios de 2.5 a 4 y mínimo de 3 puntos: un solo grupo, con 3 a 14 entidades como ruido.

**D21. Diseño municipal, omitible (Nuevo; Tabla 5).** Se replica `municipal.py`:
- Im = log(1 + tasa de homicidio doloso por 100 mil) en 2011-2017, con población municipal de CONAPO promediada en esos años, estandarizado entre encuestados.
- Dm = Im − media de su entidad.
- dosis_m = Dm × φ.
- Muestra: 15,529 personas en 821 municipios. Errores agrupados por municipio.
- Fuentes: el archivo municipal del SESNSP es el oficial (SharePoint del SESNSP, liga en gob.mx). La población municipal viene de las proyecciones CONAPO 2010-2030 en la versión ordenada de github.com/segasi/conapo_proyecciones, porque el portal de CONAPO y datos.gob.mx no respondieron desde el entorno de trabajo. Es otra edición que la que usó el artículo (ver F).

**D22. Transiciones y tipo de cambio (Nuevo; Tabla 6 y 5.7).**
- Modelo lineal de probabilidad sobre Q1 con la dosis, el rango de origen y efectos fijos de entidad y de nacimiento.
- El EMD se traduce a unidades de pendiente con la pendiente de p11 sobre la pendiente rango-rango en 49 celdas entidad × cohorte del CEEY.

**D23. Efectos fijos entidad × cohorte en el diseño municipal (Hallazgo).**
- El artículo (5.5) dice que la especificación "no es estimable". fixest sí la estima, absorbiendo entidad × grupo de nacimiento y su pendiente: b3 = 0.003 (EE 0.017), del mismo orden que la especificación (2).
- **Conviene corregir esa frase del artículo** o reportar este resultado.

**D24. EMOVI 2017, omitible (Nuevo; 5.3 y 5.7).**
- Se implementó la armonización de `pool.py`: 26 reactivos de origen, 23 de destino, componente principal conjunto y rangos por ronda × nacimiento.
- El código se probó solo con un archivo sintético, porque `entrevistado_2017.dta` no está en la carpeta.
- El artículo no dice cuáles son las tres bandas del ejercicio de ciclo de vida. Se usaron 1959-69, 1970-80 y 1981-91; si eran otras, se cambian en el script 12.

**D25. Semilla 20261102 (Nuevo).** Es la que declara el artículo. Los valores p por permutación y la estabilidad no coinciden al decimal con los de Python porque los generadores aleatorios son distintos.

## F. Diferencias con las cifras del Entregable 3 (`t13_verificacion_entregable3.csv`)

| Cifra | Entregable 3 | v5 | Por qué |
|---|---|---|---|
| Silueta de k-medias, k = 4 | 0.167 | 0.235 | k-medias depende de los inicios. Con 200 inicios aparece otra solución; en ambas el máximo sigue en k = 3. |
| b3 con rango de escolaridad | 0.0197 | 0.034 (EE 0.038) | Con miles de empates en escolaridad, el orden de desempate (quicksort de numpy) es arbitrario. Con empates compartidos: 0.036 (0.035). Nulo en todos los casos. |
| N de Q1 | 2,236 | 2,237 | Un caso en la frontera del quintil por el desempate. |
| EMD de transiciones (% de la pendiente) | 25 | 23.9 | Se arrastra del punto anterior; se redondearía a 24 %. |
| b3 municipal con tasa (EE) | 0.0084 (0.0175) | 0.0066 (0.0177) | Edición distinta de la población municipal de CONAPO (D21). El EMD (7.6 %) y todo lo basado en conteos coinciden exactamente. |
| Tasa + tamaño de localidad | 0.0076 | 0.0061 | La misma causa. |
| Correlación de la tasa con ruralidad, en niveles | −0.16 | −0.166 | La misma causa. |
| p de tendencias previas municipales (con / sin tamaño) | 0.13 / 0.22 | 0.11 / 0.18 | La misma causa. La lectura no cambia: no se rechaza, pero hay deriva negativa (t ≈ −1.75 y −1.83). |

**Otras discrepancias del texto, que no son cifras:**
- La Fig. 1 dice que el homicidio llega a su máximo "alrededor de 2011", pero la serie llega a su valor más alto en 2017 (21 contra 19.7).
- "Tlaxcala no tiene observaciones" es correcto, pero la Tabla 4 cuenta 11 entidades en el grupo tardío incluyendo a Tlaxcala. En la estimación son 10.
