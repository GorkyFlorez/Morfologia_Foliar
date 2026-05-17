rm(list = ls())
# ============================================================
# CARGA DE LIBRERÍAS
# ============================================================

library(pliman)     
# Librería especializada en análisis de imágenes vegetales.
# Permite segmentar hojas, medir área, perímetro, longitud,
# ancho y múltiples descriptores morfológicos.

library(dplyr)      
# Librería para manipulación y transformación de datos.

library(openxlsx)   
# Librería para exportar resultados a archivos Excel.

# ============================================================
# IMPORTAR IMAGEN
# ============================================================
archivo_imagen <- "JPG/Hoja_2.jpg"

# ============================================================
# IMPORTAR IMAGEN
# ============================================================

img <- image_import(archivo_imagen)

# Importa la imagen llamada "Hoja_3.jpg".
# La imagen se almacena como un objeto manipulable en R.

# ============================================================
# REDIMENSIONAR IMAGEN
# ============================================================

img_resized <- image_resize(img, rel_size = 30)
# Reduce el tamaño de la imagen para acelerar el procesamiento.
# rel_size = 30 significa que la imagen se reduce aproximadamente
# al 30% de su tamaño original.

# ============================================================
# SEGMENTACIÓN DE LA HOJA
# ============================================================
segmented <- image_segment(
  img_resized,
  index = "R",
  fill_hull = TRUE
)

# Segmenta automáticamente la hoja del fondo.
#
# index = "R"
# Utiliza el canal rojo (Red) para separar la hoja.
#
# fill_hull = TRUE
# Rellena pequeños huecos internos dentro de la hoja,
# mejorando la forma del objeto segmentado.

# ============================================================
# EXTRAER CONTORNO DEL OBJETO
# ============================================================
cont <- object_contour(
  segmented,
  index = "R",
  watershed = FALSE
)

# Detecta el contorno del objeto segmentado.
#
# watershed = FALSE
# Evita dividir la hoja en múltiples objetos.

# ============================================================
# CALCULAR MEDIDAS MORFOLÓGICAS
# ============================================================

measures <- poly_measures(cont) |> round_cols()
# poly_measures()
# Calcula múltiples variables morfológicas:
#
# - área
# - perímetro
# - longitud
# - ancho
# - circularidad
# - convexidad
# - elongación
# etc.
#
# round_cols()
# Redondea los valores numéricos.

# ============================================================
# CORREGIR ESCALA DE MEDICIÓN
# ============================================================

meas <- get_measures(
  measures,
  id = 2,
  area ~ 4
) |> 
  t()

# get_measures()
# Corrige las medidas utilizando una referencia conocida.
#
# id = 2
# Selecciona el objeto número 2.
#
# area ~ 4
# Indica que el área real del objeto de referencia es 4 cm².
#
# t()
# Transpone la matriz para organizar correctamente las variables.

# ============================================================
# CONVERTIR A DATA FRAME
# ============================================================

meas_df <- as.data.frame(t(meas))

# Convierte las medidas en un data frame,
# facilitando su manipulación.

# ============================================================
# QUEDARSE SOLO CON UNA FILA
# ============================================================
meas_df <- meas_df[1, , drop = FALSE]

# Mantiene únicamente la primera medición.
#
# Esto evita duplicados tipo:
# - Total
# - Average

# ============================================================
# VARIABLES MORFOLÓGICAS A EXTRAER
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

# Lista de variables morfológicas que se desean extraer.

# ============================================================
# VERIFICAR VARIABLES EXISTENTES
# ============================================================
variables_existentes <- variables[
  variables %in% colnames(meas_df)
]
# Verifica cuáles variables realmente existen
# dentro del data frame.

# ============================================================
# EXTRAER VARIABLES SELECCIONADAS
# ============================================================

resul <- meas_df[
  ,
  variables_existentes,
  drop = FALSE
]

# Selecciona únicamente las variables deseadas.

# ============================================================
# RENOMBRAR VARIABLES AL ESPAÑOL
# ============================================================

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

# Traduce las variables al español
# para facilitar su interpretación.

# ============================================================
# CREAR VARIABLES DERIVADAS
# ============================================================

resul_final <- resul_final %>%
  mutate(
    relacion_longitud_ancho =
      longitud_cm / ancho_cm,
    indice_forma =
      area_cm2 / (longitud_cm * ancho_cm),
    compacidad =
      (perimetro_cm^2) / area_cm2,
    razon_area_convexa =
      area_cm2 / area_convexa_cm2
  )

# Calcula nuevos índices morfológicos:
#
# relacion_longitud_ancho
# Relación entre largo y ancho de la hoja.
#
# indice_forma
# Describe la forma general de la hoja.
#
# compacidad
# Mide qué tan compacta o irregular es la hoja.
#
# razon_area_convexa
# Relación entre el área real y el área convexa.

# ============================================================
# AGREGAR IDENTIFICADOR DE MUESTRA
# ============================================================

nombre_imagen <- tools::file_path_sans_ext(
  basename(archivo_imagen)
)

resul_final <- resul_final %>%
  mutate(
    muestra = nombre_imagen,
    .before = 1
  )
# Agrega una columna con el nombre de la muestra.
#
# .before = 1
# Inserta la columna al inicio del data frame.

# ============================================================
# VISUALIZAR RESULTADOS
# ============================================================
print(resul_final)
# Muestra los resultados finales en consola.

# ============================================================
# EXPORTAR A EXCEL
# ============================================================
write.xlsx(
  resul_final,
  file = "Medidas_morfologicas_Individual.xlsx",
  overwrite = TRUE
)

# Exporta las variables morfológicas
# a un archivo Excel.
#
# overwrite = TRUE
# Permite sobrescribir el archivo si ya existe.






