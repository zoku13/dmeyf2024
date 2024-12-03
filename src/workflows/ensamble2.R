# Configuración inicial
envg <- list()
envg$EXPENV <- list(
  bucket_dir = "~/buckets/b1",
  exp_dir = "~/buckets/b1/expw227/",
  wf_dir = "~/buckets/b1/flow227/",
  repo_dir = "~/dmeyf2024/",
  datasets_dir = "~/buckets/b1/datasets/",
  messenger = "~/install/zulip_enviar.sh"
)

# Librerías necesarias
library(data.table)

# Definimos las carpetas a procesar
folders <- sprintf("SC-%04d", 10:18) # Genera SC-0010, SC-0011, ..., SC-0018
bucket_dir <- file.path(envg$EXPENV$exp_dir)
files <- file.path(bucket_dir, folders, "tb_future_prediccion.txt")

# Lista para almacenar los datos combinados
combined_data <- list()

# Procesar cada archivo
for (i in seq_along(files)) {
  file_path <- files[i]
  
  # Verificar si el archivo existe antes de leerlo
  if (file.exists(file_path)) {
    # Leer las columnas necesarias
    message(sprintf("Procesando: %s", file_path))
    data <- fread(
      file_path,
      select = c("numero_de_cliente", "foto_mes", "sem_1_1")
    )
    
    # Renombrar la columna "sem_1_1" según el número de carpeta
    pred_col_name <- paste0("pred-", substr(folders[i], 4, 7)) # Obtiene el número de la carpeta
    setnames(data, "sem_1_1", pred_col_name)
    
    # Agregar a la lista de resultados
    combined_data[[i]] <- data
  } else {
    message(sprintf("Archivo no encontrado: %s", file_path))
  }
}

# Combinar todos los datos en una única tabla
if (length(combined_data) > 0) {
  final_data <- Reduce(function(x, y) merge(x, y, by = c("numero_de_cliente", "foto_mes"), all = TRUE), combined_data)
} else {
  stop("No se encontraron archivos para procesar.")
}

# Crear una nueva columna que sea el promedio de todas las predicciones por fila
cols_pred <- setdiff(names(final_data), c("numero_de_cliente", "foto_mes")) # Excluir columnas no relacionadas con predicciones
final_data[, avg_pred := rowMeans(.SD, na.rm = TRUE), .SDcols = cols_pred] # Calcular promedio de predicciones

# Ordenar por avg_pred en forma descendente
final_data <- final_data[order(-avg_pred)]

# Crear la columna Predicted
final_data[, Predicted := ifelse(.I <= 11000, 1, 0)] # Asignar 1 a los 11000 primeros, 0 al resto

# Guardar el archivo completo con todas las columnas
output_file_full <- file.path(envg$EXPENV$datasets_dir, "tb_future_prediccion_with_predicted.csv")
fwrite(final_data, output_file_full)
message(sprintf("Archivo completo guardado en: %s", output_file_full))

# Crear un segundo archivo solo con numero_de_cliente y Predicted
output_file_summary <- file.path(envg$EXPENV$datasets_dir, "tb_future_prediccion_summary.csv")
summary_data <- final_data[, .(numero_de_cliente, Predicted)]
fwrite(summary_data, output_file_summary)
message(sprintf("Archivo resumen guardado en: %s", output_file_summary))
