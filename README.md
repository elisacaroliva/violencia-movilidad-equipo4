# Equipo_4_v5: metodología del Entregable 3, con el diseño por entidad como principal

Violencia del crimen organizado y movilidad social en México. Equipo 4 (Revilla, Flores, Ortega, Oliva).

La v5 implementa en R lo que describe el Entregable 3 (*Organized-crime violence and intergenerational persistence: evidence and bounds from Mexico*). El texto del artículo funciona como especificación: cada tabla, figura y cifra del manuscrito tiene un script que la produce, y el script 13 las compara una por una con lo publicado.

**Sustituye a la v4.** La v4 implementaba la presentación *Entregas 1 y 2* con correcciones. La v5 toma como principal la construcción que usa el artículo y conserva las correcciones de la v4 como pruebas de robustez con nombre propio. También recupera el diseño municipal y agrega todos los análisis nuevos del artículo.

## Pregunta y modelo

¿La exposición a la violencia del crimen organizado entre los 10 y los 18 años aumenta la persistencia intergeneracional?

```
R_des = b1·R_ori + b2·D + b3·(R_ori × D) + EF_entidad + EF_nacimiento
        + EF_entidad × R_ori + EF_nacimiento × R_ori + e
D = I(entidad a los 14) × φ(año de nacimiento)
```

| Término | Construcción principal (Entregable 3) |
|---|---|
| Índice de violencia | Promedio de las tasas estandarizadas de homicidio doloso, secuestro, extorsión y robo en carreteras con violencia. Población interpolada entre los censos 2010 y 2020; robo en carreteras 1997-2001 = 0. |
| I(e) | Promedio del índice en 2007-2017. Se reporta por DE entre las 31 entidades (0.409). |
| φ(c) | Fracción de la ventana de 10 a 18 años en 2007 o después. |
| R_ori, R_des | Rango ponderado dentro de la cohorte del CEEY, calculado como [cumsum(w) − w/2]/Σw sobre el orden del índice (primer componente de 42+1 reactivos de origen y 39+1 de destino). |
| Inferencia | Errores agrupados por entidad e IC normales; EMD = 2.8 × EE. Al lado se reporta el valor p con t(30) y se agregan permutación (2,000) y bootstrap salvaje (999). |

## Resultado en una línea

b3 = **+0.016 (EE 0.026)** por DE de intensidad. La p de permutación es 0.62, las tendencias previas no se rechazan (F = 0.41, p = 0.80) y el placebo da −0.001. El EMD es 0.071, es decir, **11 % de la pendiente base de 0.650**. Con el diseño municipal (módulo opcional) el EMD baja a **7.6 %**.

El script 13 verifica 111 cifras del artículo: **97 coinciden**, 9 difieren y las 5 de EMOVI 2017 quedan omitidas porque falta ese archivo. Las diferencias se explican en `DECISIONES.md`, sección F.

## Módulos omitibles

El resultado principal es por entidad y no depende de estos dos módulos. Se controlan con interruptores en `scripts/00_configuracion.R`:

| Interruptor | Script | Insumos | Estado actual |
|---|---|---|---|
| `EJECUTAR_MUNICIPAL` | 11_diseno_municipal.R | `data/raw/sesnsp_municipal/IDM_MAnterior_ago2024.csv`, `data/raw/conapo_municipal/proy_pob_mpo_conapo.csv` | Datos descargados; corre |
| `EJECUTAR_POOL_2017` | 12_emovi_2017_ciclo_de_vida.R | `data/raw/emovi_2017/entrevistado_2017.dta` | **Falta el archivo** (el CEEY lo entrega a petición); se omite |

Si el interruptor está en `FALSE` o falta el archivo, el script escribe `t11_estado.csv` o `t12_estado.csv` con "omitido". El pipeline sigue y el script 13 marca esas cifras como omitidas. Ver `INSTRUCCIONES_DATOS.md`.

## Cómo correrlo

1. Abre `Equipo_4_v5.Rproj` en RStudio.
2. Instala los paquetes, una sola vez:
   ```r
   install.packages(c("here","dplyr","tidyr","readr","stringr","stringi","purrr","tibble","haven",
                      "fixest","survey","FactoMineR","cluster","dbscan","ggplot2","scales",
                      "sessioninfo","jsonlite","ranger"))
   ```
3. Corre `source("scripts/00_correr_todo.R")`. Tarda unos 4 minutos.
4. Revisa:
   - `output/logs/tiempos_y_estado.csv`: los 14 scripts deben decir "ok".
   - `output/tablas/t13_verificacion_entregable3.csv`.

## Del artículo al código

| Entregable 3 | Script | Salida |
|---|---|---|
| Fig. 1 (delitos) | 03 | fig01 |
| Fig. 2 (componentes de la dosis) | 05 | fig02a, fig02b |
| Tabla 1 (descriptivos) | 06 | t06_tabla1_descriptivos, t06_expuestos |
| 4.1: correlación entre entidades y pendiente por cohorte | 06 | t06_correlacion_entidades, t06_pendiente_rango_rango |
| Tabla 2 | 07 | t07_tabla2_principal |
| Tabla 3 (capas de controles) | 07 | t07_tabla3_capas |
| 5.1: permutación | 08 | t08_inferencia, fig09 |
| Fig. 3a y 5.3 (estudio de eventos) | 09 | t09_*, fig10 |
| Tabla 4 y Fig. 4 (conglomerados) | 04 y 10 | t04_*, t10_conglomerados_permutacion, fig04a-c |
| Tabla 5 y Fig. 3b (municipal) | 11 (opcional) | t11_* |
| Tabla 6 (heterogeneidad, transiciones, primera etapa) | 10 | t10_tabla6_heterogeneidad, t10_tipo_de_cambio_transiciones |
| 5.7: EMD | 07, 10, 11, 12 | t07_potencia y otras |
| 5.3: ciclo de vida con EMOVI 2017 | 12 (opcional) | t12_* |
| Robustez (correcciones v4 y otras) | 07 | t07_robustez |
| Validación predictiva: entrenamiento/prueba por UPM, validación cruzada, modelo de referencia, importancia por permutación | 14 | t14_*, fig11 |

## Estructura

```
config/   entidades.csv · mapa_delitos.csv · objetivos_entregable3.csv (las 111 cifras que verifica el 13)
data/raw/ emovi/ sesnsp/ conapo/                      insumos del diseño por entidad
          sesnsp_municipal/ conapo_municipal/         módulo municipal
          emovi_2017/                                 módulo de ciclo de vida (vacío)
scripts/  00_configuracion.R (todos los parámetros e interruptores) · 00_correr_todo.R
          limpieza/   01 EMOVI · 02 índices y rangos · 03 panel de violencia · 04 conglomerados · 05 dosis
          resultados/ 06 descriptivos · 07 modelo · 08 inferencia · 09 supuesto y placebo
                      10 heterogeneidad · 11 municipal* · 12 EMOVI 2017* · 13 verificación
                      14 validación predictiva (bosque aleatorio vs. modelo lineal de referencia)
output/   tablas/ figuras/ logs/
```
