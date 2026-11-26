# Detección de transacciones con riesgo de fraude

Proyecto grupal del curso de Ciencia de Datos. Modelo de clasificación para priorizar transacciones con riesgo de fraude, con una decisión de negocio basada en costos de revisión y pérdidas evitadas.

**Mi aporte:**  preparación de datos, filtro, creacion y seleccion de los modleos a entrenar, creación de predictores, evaluación de negocio.

## Problema

Una institución financiera puede revisar manualmente un número limitado de transacciones. Cada revisión cuesta $3.000 y cada fraude no detectado genera una pérdida de $42.000, de la cual se evita el 30 % al revisarlo. El objetivo es decidir qué transacciones revisar para maximizar el beneficio neto.

## Datos

- Archivo: `tig_grupo_12.xlsx` 
- Variables: monto, hora de la transacción, transacciones en 24 horas, intentos fallidos, dispositivos nuevos en 30 días, distancia al lugar habitual, antigüedad de la cuenta, saldo promedio, categoría de comercio, canal de compra, segmento del cliente y fraude confirmado.
- Tasa de fraude: aproximadamente 17 %, por lo que el conjunto está desbalanceado.

## Metodología

1. **Análisis exploratorio:** vacíos, valores extremos y diferencias entre transacciones con y sin fraude.
2. **Predictores nuevos:** razón entre el monto y el saldo promedio, y uso de dispositivos nuevos en 30 días.
3. **Partición 60/20/20 estratificada** en entrenamiento, validación y prueba.
4. **Receta de preprocesamiento** ajustada solo con entrenamiento: imputación por mediana y variables dummy.
5. **Modelos comparados** en validación: regresión logística y bosque aleatorio con 16 combinaciones de `mtry` y `min_n`.
6. **Evaluación final** en prueba, una sola vez, con el modelo seleccionado.
7. **Decisión de negocio:** revisar solo las transacciones cuya probabilidad supera el umbral económico.

## Métrica

Se usa la **precisión en el 12 % más riesgoso** de las transacciones, porque es la capacidad real de revisión del equipo. La accuracy no se usa porque, con 17 % de fraude, un modelo que siempre responde "No" alcanza cerca de 83 %.

## Resultados principales

- Modelo seleccionado: regresión logística
- Umbral económico: probabilidad mayor a 0,238 (3.000 / 12.600).


Las tablas y figuras completas están en la carpeta `resultados/`.

## Cómo reproducirlo

Requisitos: R con los paquetes `readxl`, `readr`, `dplyr`, `ggplot2`, `tibble`, `rsample`, `recipes`, `parsnip`, `workflows`, `tune`, `yardstick` y `ranger`.

```r
# Desde la carpeta del proyecto, con el archivo de datos en la misma carpeta
source("script.R")
```

El script crea la carpeta `resultados/` con todas las tablas (.csv) y figuras (.png).

## Estructura

```
.
├── script.R          # análisis completo
├── README.md
├── .gitignore
└── resultados/       # tablas y figuras generadas
```

## Limitaciones

- Los predictores se diseñaron revisando el conjunto completo de datos; esto no afecta la prueba final, pero conviene tenerlo en cuenta al interpretar los resultados.
- El supuesto de costo de revisión, pérdida y proporción evitada se tomó del enunciado del trabajo y no proviene de datos reales de la institución.
- Los datos son de un curso y no representan necesariamente transacciones reales.
