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
      select = c("numero_de_cliente", "foto_mes", "semilla_1_1")
    )
    
    # Renombrar la columna "semilla_1_1" según el número de carpeta
    pred_col_name <- paste0("pred-", substr(folders[i], 4, 7)) # Obtiene el número de la carpeta
    setnames(data, "semilla_1_1", pred_col_name)
    
    # Agregar a la lista de resultados
    combined_data[[i]] <- data
  } else {
    message(sprintf("Archivo no encontrado: %s", file_path))
  }
}

# Combinar todos los datos en una única tabla
if (length(combined_data) > 0) {
  final_data <- rbindlist(combined_data, use.names = TRUE, fill = TRUE)
} else {
  stop("No se encontraron archivos para procesar.")
}

# Guardar el resultado final en el directorio datasets_dir
output_file <- file.path(envg$EXPENV$datasets_dir, "tb_future_prediccion_combined.csv")
fwrite(final_data, output_file)

message(sprintf("Archivo combinado guardado en: %s", output_file))
