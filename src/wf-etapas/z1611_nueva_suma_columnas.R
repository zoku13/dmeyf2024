#!/usr/bin/env Rscript
cat("ETAPA z1611_nueva_suma_columnas.r INIT\n")

# Limpiar memoria
rm(list = ls(all.names = TRUE)) 
gc(full = TRUE, verbose = FALSE)

require("data.table")
require("yaml")

# Leer el dataset generado por CN_canaritos_asesinos
input_dataset <- "./dataset.csv.gz"
dataset <- fread(input_dataset)

# Leer las columnas importantes desde el archivo impo_1.txt
important_columns <- readLines("./impo_1.txt")[1:10]

# Filtrar las 10 columnas más importantes
important_data <- dataset[, ..important_columns]

# Generar todas las combinaciones de suma entre las 10 columnas
combination_names <- combn(important_columns, 2, FUN = paste, collapse = "_plus_")
combinations <- combn(important_columns, 2, FUN = function(cols) important_data[[cols[1]]] + important_data[[cols[2]]])

# Agregar las combinaciones al dataset original
for (i in seq_along(combination_names)) {
  dataset[[combination_names[i]]] <- combinations[, i]
}

# Guardar el dataset extendido
output_dataset <- "./dataset.csv.gz"
fwrite(dataset, output_dataset, compress = "gzip")

cat("ETAPA z1611_nueva_suma_columnas.r END\n")
