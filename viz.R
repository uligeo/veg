

library(sf)
library(terra)
library(mapgl)

ndvi2017 <- terra::rast("cambios vegetacion/Mid/ndvi_2017_cortado.tif")
ndvi2020 <- terra::rast("cambios vegetacion//Mid/ndvi2020_cortado.tif")
ndvi2024 <- terra::rast("cambios vegetacion/Mid/ndvi2024_cortado.tif")




coloresverdes <- rev(c(
  "#00541B",  # verde muy oscuro
  "#1B7837",  # verde bosque
  "#4DAF4A",  # verde medio
  "#7FBC41",  # verde lima
  "#B8E186",  # verde paste  
  "#FFFFFF"  # blanco
))

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

library(htmlwidgets)


## guardar como html
saveWidget(mapacomparadoveg, "mapacomparadoveg.html", selfcontained = TRUE)
