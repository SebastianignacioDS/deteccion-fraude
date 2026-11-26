# TIG Grupo 12 - Detección de transacciones con riesgo de fraude
# Ciencia de Datos 

#   0. Configuración: paquetes, parámetros y lectura de datos
#   A.1 Análisis exploratorio ............ Tablas 1-4, Figuras 1-2
#   A.3 Preparación y predictores nuevos . Tablas 5a-5b
#   A.2 Partición 60/20/20 ............... Tabla 6
#   A.4 Receta
#   B.  Modelos y selección (validación) . Tabla 7, Figura 3
#   C.  Evaluación final (prueba) ........ Tablas 8-9
#   D.  Decisión de negocio (prueba) ..... Tabla 10, Figura 4


library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(tibble)
library(rsample)
library(recipes)
library(parsnip)
library(workflows)
library(tune)
library(yardstick)

#  Parámetros del trabajo 
semilla <- 2026            # semilla 
limite_revision <- 0.12    # se puede revisar hasta el 12 % de las transacciones
costo_revision <- 3000     # costo de revisar una transacción
perdida_fraude <- 42000    # pérdida que causa un fraude
prop_evitada <- 0.30       # de los fraudes revisados, se evita el 30 %

#Estos son los supuestos dados en el trabajo.

# Carpeta donde se guardan las tablas (.csv) y figuras (.png) del informe
dir.create("resultados", showWarnings = FALSE)

# Lectura de los datos
datos <- readxl::read_excel("tig_grupo_12.xlsx")


# =====================================================================
# A.1 Análisis exploratorio (base completa, antes de transformar)
# =====================================================================

# Resumen general: en las columnas numéricas, "NA's" indica los vacíos
summary(datos)

# Tabla 1: vacíos por variable
tabla_1 <- datos |>
  dplyr::summarize(
    na_distancia = sum(is.na(distancia_habitual_km)),
    na_saldo = sum(is.na(saldo_promedio_pesos)),
    na_categoria = sum(is.na(categoria_comercio)),
    na_canal = sum(is.na(canal_compra)),
    na_segmento = sum(is.na(segmento_cliente)),
    na_fraude = sum(is.na(fraude_confirmado))
  )
tabla_1
readr::write_csv(tabla_1, "resultados/tabla_1_faltantes.csv")

# Tabla 2: valores extremos
tabla_2 <- datos |>
  dplyr::summarize(
    monto_maximo = max(monto_pesos),
    n_monto_maximo = sum(monto_pesos == max(monto_pesos)),
    saldo_maximo = max(saldo_promedio_pesos, na.rm = TRUE),
    n_saldo_maximo = sum(saldo_promedio_pesos == max(saldo_promedio_pesos, na.rm = TRUE), na.rm = TRUE),
    saldo_minimo = min(saldo_promedio_pesos, na.rm = TRUE),
    n_saldo_minimo = sum(saldo_promedio_pesos == min(saldo_promedio_pesos, na.rm = TRUE), na.rm = TRUE)
  )
tabla_2
readr::write_csv(tabla_2, "resultados/tabla_2_extremos.csv")

# Tabla 3: cuántos fraudes hay (desbalance de clases)
tabla_3 <- datos |>
  dplyr::group_by(fraude_confirmado) |>
  dplyr::summarize(n = dplyr::n()) |>
  dplyr::mutate(porcentaje = round(100 * n / sum(n), 1))
tabla_3
readr::write_csv(tabla_3, "resultados/tabla_3_fraudes.csv")

# Tabla 4: promedios de las variables numéricas, con y sin fraude
tabla_4 <- datos |>
  dplyr::group_by(fraude_confirmado) |>
  dplyr::summarize(
    monto = mean(monto_pesos),
    transacciones_24h = mean(transacciones_24h),
    intentos_fallidos = mean(intentos_fallidos_24h),
    dispositivos_nuevos = mean(dispositivos_nuevos_30d),
    distancia = mean(distancia_habitual_km, na.rm = TRUE),
    antiguedad = mean(antiguedad_cuenta_meses),
    saldo = mean(saldo_promedio_pesos, na.rm = TRUE)
  )
tabla_4
readr::write_csv(tabla_4, "resultados/tabla_4_promedios.csv")

# Figura 1: distribución del monto (asimétrica, con tope en $400.000)
figura_1 <- datos |>
  ggplot2::ggplot(ggplot2::aes(x = monto_pesos)) +
  ggplot2::geom_histogram(bins = 50) +
  ggplot2::labs(title = "Distribución del monto", x = "Monto (pesos)", y = "Transacciones") +
  ggplot2::theme_minimal()
figura_1
ggplot2::ggsave("resultados/figura_1_monto.png", figura_1, width = 8, height = 5)

# Figura 2: distancia al lugar habitual según fraude
figura_2 <- datos |>
  dplyr::filter(!is.na(distancia_habitual_km)) |>
  ggplot2::ggplot(ggplot2::aes(x = fraude_confirmado, y = distancia_habitual_km)) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(title = "Distancia al lugar habitual según fraude", x = "Fraude", y = "Distancia (km)") +
  ggplot2::theme_minimal()
figura_2
ggplot2::ggsave("resultados/figura_2_distancia.png", figura_2, width = 8, height = 5)


# =====================================================================
# A.3 Preparación de columnas y predictores nuevos
# (fila a fila y con reglas fijas: va antes de la partición sin leakage)
# =====================================================================

datos <- dplyr::mutate(datos,
                       # Respuesta como factor: "Si" primero, porque es el evento que buscamos
                       fraude_confirmado = factor(fraude_confirmado, levels = c("Si", "No")),
                       # La categoría vacía pasa a ser una categoría propia
                       categoria_comercio = factor(dplyr::case_when(is.na(categoria_comercio) ~ "Desconocida",
                                                                    TRUE ~ categoria_comercio)),
                       canal_compra = factor(canal_compra),
                       segmento_cliente = factor(segmento_cliente),
                       # Predictor nuevo 1: compra grande en relación con el saldo habitual
                       ratio_monto_saldo = monto_pesos / saldo_promedio_pesos,
                       # Predictor nuevo 2: usó al menos un dispositivo nuevo en 30 días (1 = sí)
                       dispositivo_nuevo = dplyr::case_when(dispositivos_nuevos_30d > 0 ~ 1, TRUE ~ 0)
)

#  Tabla 5a: tasa de fraude según dispositivo_nuevo
tabla_5a <- datos |>
  dplyr::group_by(dispositivo_nuevo) |>
  dplyr::summarize(n = dplyr::n(), tasa_fraude = round(mean(fraude_confirmado == "Si"), 3))
tabla_5a
readr::write_csv(tabla_5a, "resultados/tabla_5a_dispositivo_nuevo.csv")

# Tabla 5b: razón monto/saldo según fraude 
tabla_5b <- datos |>
  dplyr::group_by(fraude_confirmado) |>
  dplyr::summarize(
    promedio_ratio = mean(ratio_monto_saldo, na.rm = TRUE),
    mediana_ratio = median(ratio_monto_saldo, na.rm = TRUE)
  )
tabla_5b
readr::write_csv(tabla_5b, "resultados/tabla_5b_ratio_monto_saldo.csv")


# =====================================================================
# A.2 Partición 60 / 20 / 20 estratificada
# =====================================================================

set.seed(semilla)
division <- rsample::initial_validation_split(
  datos,
  prop = c(0.6, 0.2),
  strata = fraude_confirmado
)

entrenamiento <- rsample::training(division)
validacion <- rsample::validation(division)
prueba <- rsample::testing(division)

#  Tabla 6: tamaño y tasa de fraude de cada parte
tabla_6 <- tibble::tibble(
  conjunto = c("Entrenamiento", "Validación", "Prueba"),
  n = c(nrow(entrenamiento), nrow(validacion), nrow(prueba)),
  tasa_fraude = c(mean(entrenamiento$fraude_confirmado == "Si"),
                  mean(validacion$fraude_confirmado == "Si"),
                  mean(prueba$fraude_confirmado == "Si"))
)
tabla_6
readr::write_csv(tabla_6, "resultados/tabla_6_particion.csv")


# =====================================================================
# A.4 Receta (se prepara solo con entrenamiento al ajustar cada workflow)
# =====================================================================

receta <- recipes::recipe(
  fraude_confirmado ~ monto_pesos + hora_transaccion + transacciones_24h +
    intentos_fallidos_24h + dispositivos_nuevos_30d + distancia_habitual_km +
    antiguedad_cuenta_meses + saldo_promedio_pesos + categoria_comercio +
    canal_compra + segmento_cliente + ratio_monto_saldo + dispositivo_nuevo,
  data = entrenamiento
) |>
  # 1. Rellenar vacíos con la mediana de entrenamiento
  recipes::step_impute_median(distancia_habitual_km, saldo_promedio_pesos, ratio_monto_saldo) |>
  # 2. Convertir categorías en columnas 0/1
  recipes::step_dummy(categoria_comercio, canal_compra, segmento_cliente)


# =====================================================================
# B. Modelos y selección (solo con validación)
# =====================================================================
# Métrica principal: precisión en el 12 % más riesgoso.
# Con un número fijo de revisiones (k), el beneficio 12.600a - 3.000k solo
# sube si sube a (fraudes encontrados). Más precisión = más beneficio.
# Accuracy se descarta: con 17 % de fraude, decir siempre "No" acierta 83 %.

# ---- B.1 Modelo 1: regresión logística (línea base, sin hiperparámetros) ----
modelo_logistico <-
  parsnip::logistic_reg() |>
  parsnip::set_engine("glm") |>
  parsnip::set_mode("classification")

flujo_logistico <-
  workflows::workflow() |>
  workflows::add_recipe(receta) |>
  workflows::add_model(modelo_logistico)

ajuste_logistico <- parsnip::fit(flujo_logistico, data = entrenamiento)

# Precisión en validación: ordenar por probabilidad de fraude, tomar el
# 12 % más riesgoso y calcular la proporción de fraudes
revisadas_logistico <- ajuste_logistico |>
  stats::predict(new_data = validacion, type = "prob") |>
  dplyr::bind_cols(validacion) |>
  dplyr::arrange(dplyr::desc(.pred_Si)) |>
  dplyr::slice_head(prop = limite_revision)

precision_logistico <- mean(revisadas_logistico$fraude_confirmado == "Si")
precision_logistico

# B.1 y B.2 Modelo 2: bosque aleatorio con 2 hiperparámetros
# mtry  = cuántas variables mira cada árbol en cada división
# min_n = tamaño mínimo de un grupo para seguir dividiéndolo
# 4 x 4 = 16 configuraciones (máximo 20)
modelo_bosque <-
  parsnip::rand_forest(mtry = tune::tune(), min_n = tune::tune()) |>
  parsnip::set_engine("ranger") |>
  parsnip::set_mode("classification")

grilla <- tibble::tibble(
  mtry = rep(c(2, 4, 6, 8), each = 4),
  min_n = rep(c(10, 25, 50, 100), times = 4)
)

# Entrenar con entrenamiento y evaluar con validación
muestras <- rsample::validation_set(division)

# configuración. (tune_grid calcula además roc_auc, accuracy y brier_class)

set.seed(semilla)
resultados_bosque <- tune::tune_grid(
  modelo_bosque,
  preprocessor = receta,
  resamples = muestras,
  grid = grilla,
  control = tune::control_grid(save_pred = TRUE)
)

# Precisión en el 12 % más riesgoso de cada configuración (.config)
precision_bosques <- tune::collect_predictions(resultados_bosque) |>
  dplyr::arrange(.config, dplyr::desc(.pred_Si)) |>
  dplyr::group_by(.config, mtry, min_n) |>
  dplyr::slice_head(prop = limite_revision) |>
  dplyr::summarize(precision = mean(fraude_confirmado == "Si"), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(precision))
precision_bosques

# B.3 Tabla 7: los 17 modelos en validación, del mejor al peor 
tabla_7 <- tibble::tibble(
  modelo = c("Regresión logística", rep("Bosque aleatorio", nrow(precision_bosques))),
  mtry = c(NA, precision_bosques$mtry),
  min_n = c(NA, precision_bosques$min_n),
  precision = c(precision_logistico, precision_bosques$precision)
) |>
  dplyr::arrange(dplyr::desc(precision))
tabla_7
readr::write_csv(tabla_7, "resultados/tabla_7_validacion.csv")

#  Figura 3: precisión de cada configuración del bosque
# (línea punteada = regresión logística)
figura_3 <- precision_bosques |>
  ggplot2::ggplot(ggplot2::aes(x = factor(min_n), y = precision, color = factor(mtry))) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_abline(slope = 0, intercept = precision_logistico, linetype = "dashed") +
  ggplot2::labs(title = "Precisión en el 12 % más riesgoso (validación)",
                x = "min_n", y = "Precisión", color = "mtry") +
  ggplot2::theme_minimal()
figura_3
ggplot2::ggsave("resultados/figura_3_validacion.png", figura_3, width = 8, height = 5)

#  B.3 Elección del modelo
# La primera fila de la Tabla 7 es el ganador
mejor <- dplyr::slice_head(tabla_7, n = 1)
mejor

# Según la Tabla 7, ganó la regresión logística (0,479 contra 0,417 del
# mejor bosque). Ante empate se elegiría el modelo más simple.
modelo_final <- modelo_logistico
modelo_final


# =====================================================================
# C. Evaluación final (la prueba se usa una sola vez)
# =====================================================================

# ---- C.1 Ajuste del modelo final solo con entrenamiento ----
flujo_final <-
  workflows::workflow() |>
  workflows::add_recipe(receta) |>
  workflows::add_model(modelo_final)

set.seed(semilla)
ajuste_final <- parsnip::fit(flujo_final, data = entrenamiento)

# ---- C.1 Predicción en prueba: probabilidad de fraude ----
pred_prueba <-
  ajuste_final |>
  stats::predict(new_data = prueba, type = "prob") |>
  dplyr::bind_cols(prueba)

# Regla del negocio: se marca para revisión el 12 % más riesgoso.
# Se toma ese 12 % y su probabilidad mínima (el corte)
revisadas_prueba <- pred_prueba |>
  dplyr::arrange(dplyr::desc(.pred_Si)) |>
  dplyr::slice_head(prop = limite_revision)
corte_12 <- min(revisadas_prueba$.pred_Si)
corte_12

# "Si" a toda transacción con probabilidad igual o mayor al corte
pred_prueba <- pred_prueba |>
  dplyr::mutate(revisar = factor(dplyr::case_when(.pred_Si >= corte_12 ~ "Si",
                                                  TRUE ~ "No"),
                                 levels = c("Si", "No")))

# ---- C.2 Matriz de confusión ----
matriz_prueba <- yardstick::conf_mat(pred_prueba, truth = fraude_confirmado, estimate = revisar)
matriz_prueba

# Tabla 8: matriz de confusión como conteos
tabla_8 <- pred_prueba |>
  dplyr::group_by(revisar, fraude_confirmado) |>
  dplyr::summarize(n = dplyr::n(), .groups = "drop")
tabla_8
readr::write_csv(tabla_8, "resultados/tabla_8_matriz_confusion.csv")

#  Tabla 9: métricas en prueba 
metricas <- yardstick::metric_set(
  yardstick::precision,
  yardstick::recall,
  yardstick::specificity,
  yardstick::accuracy
)

tabla_9 <- metricas(pred_prueba, truth = fraude_confirmado, estimate = revisar)
tabla_9
readr::write_csv(tabla_9, "resultados/tabla_9_metricas_prueba.csv")

# Referencia para interpretar la precisión: tasa de fraude en prueba
# (la precisión que se obtendría revisando al azar)
tasa_fraude_prueba <- mean(prueba$fraude_confirmado == "Si")
tasa_fraude_prueba


# =====================================================================
# D. Decisión de negocio (transacciones de prueba)
# =====================================================================

# Ahorro por cada fraude revisado: 30 % de 42.000 = 12.600
ahorro_fraude <- prop_evitada * perdida_fraude
ahorro_fraude

# Umbral económico: revisar conviene solo si
# probabilidad de fraude x 12.600 > 3.000, es decir, si la probabilidad > 0,238
umbral_economico <- costo_revision / ahorro_fraude
umbral_economico

# ---- D.1 Selección: dentro del 12 % más riesgoso, las que superan el umbral ----
seleccion <- revisadas_prueba |>
  dplyr::filter(.pred_Si > umbral_economico)

k <- nrow(seleccion)                            # transacciones revisadas
a <- sum(seleccion$fraude_confirmado == "Si")   # fraudes entre las revisadas
k
a

# D.2 Beneficio neto de la política: 12.600a - 3.000
beneficio_selectivo <- ahorro_fraude * a - costo_revision * k

# D.3 Revisar las mismas k al azar: a = k x tasa de fraude en prueba
a_aleatorio <- k * tasa_fraude_prueba
beneficio_aleatorio <- ahorro_fraude * a_aleatorio - costo_revision * k

# Referencia: revisar el 12 % completo, sin aplicar el umbral
k_12 <- nrow(revisadas_prueba)
a_12 <- sum(revisadas_prueba$fraude_confirmado == "Si")
beneficio_12 <- ahorro_fraude * a_12 - costo_revision * k_12

#  D.4 Tabla 10: resumen para la recomendación
tabla_10 <- tibble::tibble(
  politica = c("Selectiva (modelo + umbral)", "Aleatoria (mismo k)", "Selectiva (12 % completo)"),
  revisadas_k = c(k, k, k_12),
  fraudes_a = c(a, a_aleatorio, a_12),
  beneficio_neto = c(beneficio_selectivo, beneficio_aleatorio, beneficio_12)
) |>
  dplyr::mutate(beneficio_por_revision = beneficio_neto / revisadas_k)
tabla_10
readr::write_csv(tabla_10, "resultados/tabla_10_negocio.csv")

# Figura 4: beneficio neto de cada política 
figura_4 <- tabla_10 |>
  ggplot2::ggplot(ggplot2::aes(x = politica, y = beneficio_neto)) +
  ggplot2::geom_col() +
  ggplot2::labs(title = "Beneficio neto esperado por política (prueba)",
                x = "Política", y = "Beneficio neto (pesos)") +
  ggplot2::theme_minimal()
figura_4
ggplot2::ggsave("resultados/figura_4_negocio.png", figura_4, width = 8, height = 5)

# D.4 Se recomienda la revisión selectiva si su beneficio neto es positivo y
# mayor que el aleatorio. Al azar, cada revisión encuentra fraude con
# probabilidad = tasa de fraude (cerca de 0,17): 0,17 x 12.600 = 2.142 < 3.000,
# por lo que revisar al azar pierde dinero.
