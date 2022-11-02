# imagenes satelitales

###################################

library(raster)
library(mapview)
library(sf)

# Cargamos imagen satelital de las condes 2021 ----
LC = brick("data/landsat8_30m/l8_2021.tif")

# nombres a las bandas
names(LC) 

names(LC) = c("B1","B2", "B3", "B4", "B5", "B6", "B7", "B9" )

# Color Natural con contraste lineal
plotRGB(LC, r = 4, g = 3, b = 2, stretch = "lin")

# Color Natural con contraste de quiebres naturales
plotRGB(LC, r = 4, g = 3, b = 2, stretch = "hist")


# Visualizamos falso color ----

# Infrarojo (5,4,3)
plotRGB(LC, r = 5, g = 4, b = 3, stretch = "lin")

# Agricultura (6,5,2)
# Penetración de la Radiación en la Atmósfera (7,6,5)
# Uso del Suelo / Masas de Agua (5,6,4)
# Infrarojo de Onda Corta (7,5,4)
# Análisis de Vegetación (6,5,4)
# Análisis de Vegetación Sana (5,6,2)



# Manipulamos el raster ----

# recortamos extension de la imagen

# definimos fronteras (extent)
ext = extent(c(356985, 367485,  6291205, 6303485))
# recortamos
LC_crop = crop(x = LC, y = ext, snap="out")
# visualizamos
plotRGB(LC_crop, r = 4, g = 3, b = 2, stretch = "hist")



# Reproyectamos la Imagen  

### proyeccion geografica longlat
crs_ll = "+proj=longlat +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
# reproyectamos
LC_ll = projectRaster(LC_crop, crs = crs_ll)
# visualizamos
plotRGB(LC_ll, r = 4, g = 3, b = 2, stretch = "hist")


# Exploramos un canal

# normalizamos los valores del canal 5 (x-mean)/sd
infrared = scale(LC_crop[[5]])

# dibujamos el infrarojo
plot(infrared)

# extraemos zonas con infrarojo alto, que aproximan a nieve
nieve_IR = calc(infrared, fun = function(x) ifelse(x <= 4, NA, x))

plot(nieve_IR)

# calculamos indices espectrales

source("R/funciones_caso2.R")

plot(NDSI(LC_crop))

nieve = calc(NDSI(LC_crop), fun = function(x) ifelse(x <= 0.9, NA, x))

plot(nieve)



# pasamos pixeles a poligonos
poligonos_espectral = 
  rasterToPolygons(nieve, digits = 16) %>% 
  st_as_sf() %>% 
  st_union() %>% # unimos vecinos
  st_cast("POLYGON")  # aislamos los poligonos resultantes

plot(poligonos_espectral)


# extraemos los valores del raster original sobre cada poligono resultante y lo guardamos en un df
raster_poly = data.frame(nieve = 
                            raster::extract(nieve, 
                                            st_as_sf(poligonos_espectral), 
                                            fun=mean)) 

# le asignamos al df las geometrias de los poligonos
st_geometry(raster_poly) = st_sfc(poligonos_espectral)

plot(raster_poly)

LasCondes = read_sf("data/shapefile/LasCondes.shp")

plotRGB(LC_crop, r=4, g=3, b=2, stretch = "hist")
plot(LasCondes$geometry, add = TRUE)
plot(raster_poly, add = TRUE)


# visualizamos en mapa interactivo
mapview(LasCondes, color = "#05A39B", alpha.region =0)+
  viewRGB(LC_crop, r = 4, g = 3, b = 2, na.color = "transparent") +
  mapview(raster_poly, na.color = "transparent") 
