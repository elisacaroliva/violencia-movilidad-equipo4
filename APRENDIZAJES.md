# Aprendizajes del proyecto

Sirve para no repetir errores y para responder preguntas difíciles. Actualizado en la v5.

## 1. Sobre el diseño

1. **El problema nunca fue de datos, sino de identificación.**
   - Comparar entidades confunde la violencia con la geografía (r = −0.20, con el signo contrario a la hipótesis).
   - Comparar cohortes confunde la violencia con el ciclo de vida: la pendiente pasa de 0.554 a 0.665 entre los nacidos en 1959-66 y en 1975-84, y ninguno de esos grupos estuvo expuesto.
   - La triple diferencia exige que el efecto aparezca en las dos dimensiones a la vez.
2. **Como el resultado es una pendiente, el parámetro vive en una interacción.** Sin las pendientes propias por entidad y por año de nacimiento, b3 recoge la geografía y el envejecimiento. Lo mismo vale para el estudio de eventos.
3. **El calendario importa tanto como el nivel.** La dosis supone que el repunte empezó en 2007 en todo el país. Con el calendario de cada conglomerado, b3 sube de 0.017 a 0.042-0.058, aunque sin significancia. Es la pista principal.
4. **Una sola fuente es mejor que un empalme.** Como la dosis solo necesita años hasta 2016, la serie estatal anterior (1997-2017) basta. Mezclar metodologías del SESNSP obliga a empalmar, y el empalme introduce supuestos (lección de la v3).
5. **Un faltante no es un cero.** Tratar el robo en carreteras de 1997-2001 como cero sostenía parte de la estructura de conglomerados de la presentación.

## 2. Sobre la inferencia

1. **Con 31 conglomerados, un t "significativo" puede ser un espejismo.** Por conglomerado salen t de 2.6 y valores p de permutación de 0.16 a 0.44. Manda la permutación.
2. **Usar la t con G − 1 grados de libertad, no la normal.** Con 31 conglomerados, el efecto mínimo detectable es 2.90 × EE, no 2.8 × EE.
3. **Un nulo bien estimado también es un resultado.** La frase correcta es "se descartan efectos mayores a 11 % de la pendiente por DE de intensidad", no "la violencia no afecta".
4. **Primero se agrupa y después se mira el resultado.** Los conglomerados se forman sin mirar la movilidad. Aun así, hay que reportar su estabilidad: la silueta es baja (menor a 0.25).
5. **"Cota inferior" no es la palabra correcta.** Para que el efecto estimado sea una cota inferior, *todos* los sesgos tendrían que empujar b3 hacia cero:
   - **Sobrevivencia** (no están quienes murieron, desaparecieron o están presos). La violencia mató sobre todo a hombres jóvenes de hogares pobres en los lugares expuestos. Los pobres que sobreviven tienen, en promedio, mejores destinos que los que faltan. Eso aplana la pendiente de los expuestos y **atenúa** b3.
   - **Error de medición** (exposición estatal asignada a cada persona, enlace ecológico). Es error clásico en la dosis, así que **atenúa** b3.
   - **Selección por migración** (la muestra excluye a quienes cambiaron de entidad y la encuesta no ve a quienes emigraron al extranjero). Migrar es una *respuesta* a la violencia, así que excluir a los migrantes condiciona en una variable posterior al tratamiento. El signo del sesgo es **indeterminado**: depende de si se fueron los de origen alto con buenos destinos o los de origen bajo con malos destinos.
   - **Lo teóricamente correcto:** decir que el efecto "probablemente está atenuado", reportar siempre la especificación con migrantes (intento de tratamiento, asignado por la entidad a los 14, que es previa al tratamiento) y, si se quiere una cota formal, construirla con supuestos explícitos (cotas de Lee o de Manski).

## 3. Sobre el código y la reproducibilidad

1. **El código que circula tiene que ser el que produjo las láminas.** El `modelo.py` no reproducía el estudio de eventos presentado. Cada versión debe correr de punta a punta y verificar sus propias cifras (script 11: 40 de 40).
2. **Escribir siempre las unidades.** "Por DE de intensidad" y "por unidad cruda" no son intercambiables.
3. **El texto debe describir lo que hace el código.** La lámina decía "primer componente", "rural" y "nacido en 1996, uno", y el código hacía otra cosa en los tres casos.
4. **Ningún parámetro escondido.** Todo vive en `00_configuracion.R` y en `config/`. Las rutas se arman con `here()`, sin `setwd()`.
5. **Validar insumos y detenerse ante el error.** Si una categoría del SESNSP no aparece o se cuenta dos veces, el pipeline se detiene.

## 4. Sobre la comunicación

1. **Nombrar las amenazas antes de que las nombre el revisor:**
   - La pandemia, el fin de PROSPERA y el de Escuelas de Tiempo Completo golpearon a las mismas cohortes. Es la amenaza sin respuesta todavía.
   - La corresidencia: 55 % en la cohorte de 1997-1998.
   - La sobrevivencia.
2. **Si el resultado es nulo**, conviene el abstract de "cotas y evidencia nula" y revistas que valoran el diseño: Estudios Económicos (Colmex), Economía Mexicana (CIDE) y Research in Social Stratification and Mobility.
3. **Declarar con precisión el uso de IA** en cada entrega.

## 5. Lo que agregó la v5

1. **El texto como especificación y una verificación automática.** Si una cifra del manuscrito no sale de un script, no existe. El script 13 compara 111 cifras y obliga a explicar cada diferencia.
2. **El denominador decide.** Medida con conteos, la violencia municipal da −0.034 y parece un efecto. Con tasas desaparece, porque el conteo mide el tamaño de la ciudad (correlación con ruralidad de −0.30 contra −0.11).
3. **Los empates no son un detalle cuando la variable es discreta.** Con la escolaridad, cambiar el desempate mueve b3 de 0.020 a 0.034. La conclusión (nulo) no cambia, pero la cifra no es reproducible si el desempate es arbitrario. Es preferible usar empates compartidos.
4. **Los módulos opcionales deben poder apagarse.** El resultado principal no puede depender de un archivo que se solicita al CEEY.
5. **"No es estimable" hay que comprobarlo.** Con un estimador que absorbe efectos fijos, entidad × cohorte sí se estima. La singularidad era un problema de la implementación con variables indicadoras, no del diseño.
