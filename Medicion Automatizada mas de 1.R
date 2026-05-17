
# ============================================================
# ANÁLISIS AUTOMATIZADO DE MORFOLOGÍA FOLIAR
# Exporta múltiples variables morfológicas a Excel
# ============================================================

rm(list = ls())

library(pliman)
library(tidyverse)
library(openxlsx)

# ============================================================
# 1. Directorio de trabajo
# ============================================================

# Directorios
input_dir  <- "JPG"
output_dir <- "PNG"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# ============================================================
# 2. Variables morfológicas a extraer
# ============================================================

variables <- c(
  "area",
  "area_ch",
  "perimeter",
  "length",
  "width",
  "caliper",
  "radius_mean",
  "radius_min",
  "radius_max",
  "diam_mean",
  "diam_min",
  "diam_max",
  "solidity",
  "convexity",
  "elongation",
  "circularity",
  "circularity_haralick",
  "circularity_norm",
  "eccentricity"
)

# ============================================================
# 3. Dataframe para guardar resultados
# ============================================================

results <- data.frame()

# ============================================================
# 4. Procesar imágenes automáticamente
# ============================================================

for (i in 1:25) {
  
  cat("Procesando imagen:", i, "\n")
  
  archivo_imagen <- file.path(input_dir, paste0("Hoja_", i, ".jpg"))
  
  if (!file.exists(archivo_imagen)) {
    warning(paste("No existe:", archivo_imagen))
    next
  }
  
  # Nombre automático de la imagen
  nombre_imagen <- tools::file_path_sans_ext(
    basename(archivo_imagen)
  )
  
  # Importar imagen
  img <- image_import(archivo_imagen)
  
  # Redimensionar
  img_resized <- image_resize(img, rel_size = 30)
  
  # Segmentar hoja
  segmented <- image_segment(
    img_resized,
    index = "R",
    fill_hull = TRUE
  )
  
  # Exportar imagen segmentada
  export_path <- file.path(output_dir, paste0(nombre_imagen, ".png"))
  image_export(segmented, export_path)
  
  # Extraer contorno
  cont <- object_contour(
    segmented,
    index = "R",
    watershed = FALSE
  )
  
  # Calcular medidas
  measures <- poly_measures(cont) |> 
    round_cols()
  
  # Corregir escala usando área de referencia
  meas <- get_measures(
    measures,
    id = 2,
    area ~ 4
  ) |> 
    t()
  
  # Convertir a dataframe
  meas_df <- as.data.frame(t(meas))
  
  # Quedarse solo con una medición
  meas_df <- meas_df[1, , drop = FALSE]
  
  # Verificar variables existentes
  variables_existentes <- variables[
    variables %in% colnames(meas_df)
  ]
  
  # Extraer variables seleccionadas
  resul <- meas_df[
    ,
    variables_existentes,
    drop = FALSE
  ]
  
  # Renombrar variables al español
  resul_final <- resul %>%
    rename(
      area_cm2                 = area,
      area_convexa_cm2         = area_ch,
      perimetro_cm             = perimeter,
      longitud_cm              = length,
      ancho_cm                 = width,
      diametro_feret_cm        = caliper,
      radio_promedio_cm        = radius_mean,
      radio_minimo_cm          = radius_min,
      radio_maximo_cm          = radius_max,
      diametro_promedio_cm     = diam_mean,
      diametro_minimo_cm       = diam_min,
      diametro_maximo_cm       = diam_max,
      solidez                  = solidity,
      convexidad               = convexity,
      elongacion               = elongation,
      circularidad             = circularity,
      circularidad_haralick    = circularity_haralick,
      circularidad_normalizada = circularity_norm,
      excentricidad            = eccentricity
    )
  
  # Crear variables derivadas
  resul_final <- resul_final %>%
    mutate(
      relacion_longitud_ancho = longitud_cm / ancho_cm,
      indice_forma = area_cm2 / (longitud_cm * ancho_cm),
      compacidad = (perimetro_cm^2) / area_cm2,
      razon_area_convexa = area_cm2 / area_convexa_cm2
    )
  
  # Agregar nombre automático de imagen
  resul_final <- resul_final %>%
    mutate(
      muestra = nombre_imagen,
      .before = 1
    )
  
  # Acumular resultados
  results <- bind_rows(results, resul_final)
}

# ============================================================
# 5. Exportar resultados a Excel
# ============================================================

write.xlsx(
  results,
  file = "Medidas_morfologicas_Grupal.xlsx",
  overwrite = TRUE
)

# Ver resultados
print(results)
