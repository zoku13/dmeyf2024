# Limpieza y configuraciones iniciales
format(Sys.time(), "%a %b %d %X %Y")
rm(list = ls(all.names = TRUE))
gc(full = TRUE, verbose = FALSE)

dir.create("~/buckets/b1/exp/lineademuertevf/", showWarnings = FALSE)
setwd("~/buckets/b1/exp/lineademuertevf/")

require("data.table")
require("lightgbm")

# Leer el dataset
dataset <- fread("~/buckets/b1/datasets/competencia_03_crudo.csv.gz")

# Preparar datos como en el código original (cálculo de periodos, lags, deltas, etc.)
setorder(dataset, numero_de_cliente, foto_mes)
dataset[, periodo0 := as.integer(foto_mes / 100) * 12 + foto_mes %% 100]
periodo_ultimo <- dataset[, max(periodo0)]
periodo_anteultimo <- periodo_ultimo - 1
dataset[, c("periodo1", "periodo2") := shift(periodo0, n = 1:2, fill = NA, type = "lead"), numero_de_cliente]
dataset[periodo0 < periodo_anteultimo, clase_ternaria := "CONTINUA"]
dataset[periodo0 < periodo_ultimo & (is.na(periodo1) | periodo0 + 1 < periodo1), clase_ternaria := "BAJA+1"]
dataset[periodo0 < periodo_anteultimo & (periodo0 + 1 == periodo1) & (is.na(periodo2) | periodo0 + 2 < periodo2), clase_ternaria := "BAJA+2"]
dataset[, c("periodo0", "periodo1", "periodo2") := NULL]

# Feature engineering
cols_lagueables <- setdiff(colnames(dataset), c("numero_de_cliente", "foto_mes", "clase_ternaria"))
dataset[, paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"), by = numero_de_cliente, .SDcols = cols_lagueables]
dataset[, paste0(cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"), by = numero_de_cliente, .SDcols = cols_lagueables]
for (vcol in cols_lagueables) {
  dataset[, paste0(vcol, "_delta1") := get(vcol) - get(paste0(vcol, "_lag1"))]
  dataset[, paste0(vcol, "_delta2") := get(vcol) - get(paste0(vcol, "_lag2"))]
}

# Columnas buenas y fold de entrenamiento
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria"))
dataset[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0, 1)]
dataset[, azar := runif(nrow(dataset))]
dataset[, fold_train := foto_mes <= 202107 & !(foto_mes %in% c(201903, 201904, 202003, 202004, 202103, 202104)) & (clase_ternaria %in% c("BAJA+1", "BAJA+2") | azar < 0.02)]

# Definir semillas
semillas <- c(103301, 100183, 100189, 333331, 900001)

# Preparar datos futuros
dfuture <- dataset[foto_mes == 202109]

# Inicializar tabla de probabilidades
probabilidades_acumuladas <- data.table(numero_de_cliente = dfuture$numero_de_cliente)

# Entrenar modelos con cada semilla y predecir probabilidades
for (semilla in semillas) {
  set.seed(semilla)
  
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset[fold_train == TRUE, campos_buenos, with = FALSE]),
    label = dataset[fold_train == TRUE, clase01],
    weight = dataset[fold_train == TRUE, ifelse(foto_mes <= 202106, 1.0, 0.0)],
    free_raw_data = TRUE
  )
  
  param <- list(
    objective = "binary",
    metric = "auc",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    verbosity = -100,
    force_row_wise = TRUE,
    seed = semilla,
    max_bin = 31,
    learning_rate = 0.03,
    feature_fraction = 0.5,
    num_leaves = 64,
    min_data_in_leaf = 100,
    num_iterations = 100
  )
  
  model <- lgb.train(data = dtrain, param = param, verbose = -100)
  
  prediccion <- predict(model, data.matrix(dfuture[, campos_buenos, with = FALSE]))
  probabilidades_acumuladas[, paste0("prob_", semilla) := prediccion]
}

# Promedio de probabilidades
probabilidades_acumuladas[, prob_promedio := rowMeans(.SD), .SDcols = patterns("prob_")]

# Ordenar por probabilidad promedio
setorder(probabilidades_acumuladas, -prob_promedio)

# Asignar Predicted según las primeras 11,000 observaciones
probabilidades_acumuladas[, Predicted := 0L]
probabilidades_acumuladas[1:11000, Predicted := 1L]

# Guardar resultados
fwrite(probabilidades_acumuladas[, .(numero_de_cliente, Predicted)], file = "lineademuerte_11000vf.csv")

format(Sys.time(), "%a %b %d %X %Y")
