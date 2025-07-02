library(sf)
library(terra)
library(mapgl)
library(shiny)
library(ggplot2)
library(ggspatial)


ndvi2017 <- terra::rast("cambios vegetacion/Mid/ndvi_2017_cortado.tif")
ndvi2020 <- terra::rast("cambios vegetacion//Mid/ndvi2020_cortado.tif")
ndvi2024 <- terra::rast("cambios vegetacion/Mid/ndvi2024_cortado.tif")
cambios <- terra::rast("Mid/NDVI_negativo.tif")
merida<-st_read("Mid/Merida.geojson")


coloresverdes <- rev(c(
  "#00541B",  # verde muy oscuro
  "#1B7837",  # verde bosque
  "#4DAF4A",  # verde medio
  "#7FBC41",  # verde lima
  "#B8E186",  # verde paste  
  "#FFFFFF"  # blanco
))

coloresmag <- colorRampPalette(c("#000000", "#FF0000"))(10)

## pestana 1
m1 <- maplibre(zoom = 9, center = c(-89.617, 20.967)) |>
  add_image_source(
    id             = "2017",
    data         = ndvi2017,
    colors         =coloresverdes ) |>
  add_raster_layer(
    id             = "2017",
    source         = "2017",
    raster_opacity = 0.8)

m2 <- maplibre(zoom = 9, center = c(-89.617, 20.967)) |>
  add_image_source(
    id             = "2024",
    data         = ndvi2024,
    colors         = coloresverdes) |>
  add_raster_layer(
    id             = "2024",
    source         = "2024",
    raster_opacity = 0.8)

mapacomparadoveg <- mapgl::compare(m1, m2)
mapacomparadoveg

### pestana 2
cam <-maplibre(zoom = 9, center = c(-89.617, 20.967)) |>
  add_image_source(
    id             = "cambios",
    data         = cambios,
    colors         = coloresmag) |>
  add_raster_layer(
    id             = "cambios",
    source         = "cambios",
    raster_opacity = 0.8)

cam

# ========================================
# ANÁLISIS DE DEFORESTACIÓN Y CAMBIOS NDVI
# ========================================

# Calcular la diferencia entre los rásters NDVI (2024 - 2017)
diff_ndvi <- ndvi2024 - ndvi2017

# Definir una paleta de colores divergente
ncolors <- 100
diverging_palette <- colorRampPalette(c("blue", "white", "red"))(ncolors)

# Definir umbral para considerar el cambio como significativo
umbral <- 0.1

# Reproyectar el ráster de diferencia a un sistema proyectado para calcular áreas
# EPSG:32616 corresponde a UTM zona 16N (Mérida, Yucatán)
diff_ndvi_proj <- project(diff_ndvi, "EPSG:32616")

# Calcular el área de cada celda en hectáreas
areas_ha <- cellSize(diff_ndvi_proj, unit = "ha")

# Crear una máscara para celdas donde el cambio absoluto es mayor al umbral
mask_cambio <- abs(diff_ndvi_proj) > umbral

# Calcular el área total de cambio significativo
area_total_ha <- global(areas_ha * mask_cambio, fun = "sum", na.rm = TRUE)[1]

# Preparar el texto para el caption
texto_area <- paste("Umbral de cambio: |", umbral, "|   Área total de cambio significativo: ", round(area_total_ha, 2), " ha", sep = "")

# Graficar la diferencia con la paleta definida
plot(diff_ndvi,
     col = diverging_palette,
     main = "Cambio NDVI (2024 - 2017)",
     xlab = "Longitud",
     ylab = "Latitud",
     sub = texto_area,
     cex.sub = 1.1)

# ========================================
# ANÁLISIS CON GGPLOT2
# ========================================

# Convertir el ráster de diferencia a un data frame para usar en ggplot
diff_df <- as.data.frame(diff_ndvi, xy = TRUE)
colnames(diff_df)[3] <- "diff"

# Crear el gráfico con ggplot2
p <- ggplot(diff_df) +
  geom_raster(aes(x = x, y = y, fill = diff)) +
  scale_fill_gradientn(colours = c("blue", "white", "red"),
                       name = "Cambios NDVI") +
  labs(title = "Cambio NDVI (2024 - 2017)",
       subtitle = paste("Área con cambio significativo:", round(area_total_ha, 2), "ha"),
       x = "Longitud", 
       y = "Latitud") +
  theme_minimal() +
  coord_equal() +
  annotation_north_arrow(location = "tr", 
                         which_north = "true",
                         style = north_arrow_fancy_orienteering)

# Mostrar el gráfico
print(p)

# Guardar el gráfico en PNG
ggsave("NDVI_change.png", plot = p, dpi = 600, width = 10, height = 8, units = "in")

# ========================================
# ANÁLISIS SOLO DE CAMBIOS NEGATIVOS (DEFORESTACIÓN)
# ========================================

# Definir umbrales para cambios negativos
umbral_neg <- -0.28   # valores menores a este se consideran cambio negativo fuerte
umbral_pos <-  0.2   # valores mayores a este se consideran cambio positivo fuerte

# Crear la variable categórica
diff_df$NDVI_clase <- cut(diff_df$diff,
                          breaks = c(-Inf, umbral_neg, umbral_pos, Inf),
                          labels = c("Cambio negativo (deforestación)",
                                     "Sin cambio significativo",
                                     "Cambio positivo"))

# Filtrar solo las observaciones con cambio negativo fuerte
diff_df_neg <- diff_df[diff_df$NDVI_clase == "Cambio negativo (deforestación)", ]

# Calcular área de deforestación
# Crear máscara para cambios negativos significativos
mask_deforestacion <- diff_ndvi_proj < umbral_neg
area_deforestacion_ha <- global(areas_ha * mask_deforestacion, fun = "sum", na.rm = TRUE)[1]

# Graficar solo las áreas con cambio negativo (deforestación)
o <- ggplot(diff_df_neg) +
  geom_raster(aes(x = x, y = y, fill = NDVI_clase)) +
  scale_fill_manual(values = c("Cambio negativo (deforestación)" = "darkred"),
                    name = "Cambio NDVI") +
  labs(title = "Deforestación detectada (2024 - 2017)",
       subtitle = paste("Área deforestada:", round(area_deforestacion_ha, 2), "ha"),
       x = "Longitud", 
       y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_equal() +
  annotation_north_arrow(location = "tr", 
                         which_north = "true",
                         style = north_arrow_fancy_orienteering)

# Mostrar el gráfico de deforestación
print(o)

# Guardar el gráfico de deforestación
ggsave("NDVI_deforestacion.png", plot = o, dpi = 600, width = 10, height = 8, units = "in")

# ========================================
# ANÁLISIS CON SHAPEFILE DE MÉRIDA
# ========================================

# Graficar deforestación con límites de Mérida
o_con_shape <- ggplot() +
  geom_raster(data = diff_df_neg, aes(x = x, y = y, fill = NDVI_clase)) +
  geom_sf(data = merida, fill = NA, color = "black", size = 0.8) +
  scale_fill_manual(values = c("Cambio negativo (deforestación)" = "darkred"),
                    name = "Deforestación") +
  labs(title = "Deforestación en Mérida (2024 - 2017)",
       subtitle = paste("Área deforestada:", round(area_deforestacion_ha, 2), "ha"),
       x = "Longitud", 
       y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_sf() +
  annotation_north_arrow(location = "tr", 
                         which_north = "true",
                         style = north_arrow_fancy_orienteering)

# Mostrar el gráfico con shapefile
print(o_con_shape)

# Guardar el gráfico con shapefile
ggsave("NDVI_deforestacion_merida.png", plot = o_con_shape, dpi = 600, width = 10, height = 8, units = "in")

# ========================================
# RESUMEN DE RESULTADOS
# ========================================

cat("\n=== RESUMEN DE ANÁLISIS DE DEFORESTACIÓN ===\n")
cat("Período analizado: 2017 - 2024\n")
cat("Umbral de cambio significativo:", umbral, "\n")
cat("Umbral de deforestación:", umbral_neg, "\n")
cat("Área total con cambios significativos:", round(area_total_ha, 2), "hectáreas\n")
cat("Área deforestada:", round(area_deforestacion_ha, 2), "hectáreas\n")
cat("Porcentaje de área deforestada:", round((area_deforestacion_ha/area_total_ha)*100, 2), "%\n")
cat("=============================================\n")



