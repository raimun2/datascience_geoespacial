# georeferenciacion -----

library(tidygeocoder)
library(sf)
library(mapview)
library(tidyverse)
library(osmdata)

# leemos poligonos de Las Condes
poligonos <- read_rds("data/MBHT_LC.rds") 
LasCondes <- read_sf("data/shapefile/LasCondes.shp")

# leemos direcciones
direcciones <- read_csv("data/direcciones.csv")

#exploramos listado
direcciones

# intentamos georeferenciar direcciones asi tal como vienen
p1 <- geo(direcciones$DIRECCION, method="arcgis")

# vemos los resultados preliminares
p1 %>% summary()

# visualizamos en un mapa.. hay resultados en todo el mundo
p1 %>% drop_na() %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  mapview(legend = FALSE)

# agregamos el nombre de la comuna de las condes al texto de busqueda
p2 <- geo(paste0(direcciones$DIRECCION," Las Condes"), method="arcgis") 

# vemos si hay cambios respecto a p1
p2 %>% summary()

# visualizamos en el mapa
p2 %>% drop_na() %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  mapview(legend = FALSE)

# intentamos agregando el nombre de Chile tambien
p3 <- geo(paste0(direcciones$DIRECCION," Las Condes, Chile"), method="arcgis") 

# vemos las estadisticas descriptivas
p3 %>% summary()

# generamos objeto espacial con la misma CRS que el objeto base
p3_sf <- p3 %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(st_crs(poligonos)) 

# visualizamos
p3_sf %>% mapview(legend = FALSE)

# validar la georeferencia ----

# hacemos geocoding inverso, para validar las direcciones
dirs_rev <- reverse_geocode(p3, lat = lat, long = long,  method = "arcgis")

# vemos las diferencias en el texto de las direcciones
dirs_rev$diferencias <- mapply(adist, dirs_rev$address...4, dirs_rev$address...1)

hist(dirs_rev$diferencias)


# limpiamos un poco
# quitamos el numero de departamento de las direcciones y preservamos casos unicos
dirs_dp <- sub(" DP.*", "", direcciones$DIRECCION) %>% unique()

# geocodificamos valores unicos
p4 <- geo(paste0(dirs_dp," Las Condes, Chile"), method="arcgis") %>% as_tibble()

# geocoding inverso nuevamente
dirs_rev2 <- reverse_geocode(p4, lat = lat, long = long,  method = "arcgis")

# quitamos las unidades superiores para comparar solo nombre de calle y numero
dirs_rev2$dir4 <- gsub(", Las Condes, Santiago, Región Metropolitana de Santiago, 7550000, CHL", "", dirs_rev2$address...4)

# volvemos a calcular las diferencias
dirs_rev2$diferencias <- mapply(adist, dirs_rev2$dir4, dirs_dp)

hist(dirs_rev2$diferencias)


# accesibilidad ----

# accesibilidad tiene relacion con la presencia de servicios disponibles para la comunidad

# hay recursos que son puntos, como las juntas de vecinos

# podemos calcular la distancia de cada poligono a cada junta de vecinos
distancia_puntos <- st_distance(poligonos, p3_sf)

# preservamos la menor distancia para asociar una manzana a una JJVV
# generamos indicador de accesibilidad a jjvv
poligonos <- 
  poligonos %>% 
  mutate(acc_JJVV = apply(distancia_puntos, 1, function(x) min(x)))

# visualizamos
ggplot() +
  geom_sf(data=poligonos, aes(fill=acc_JJVV))+
  geom_sf(data = p3_sf, size = 2, col = "white") +
  scale_fill_viridis_c()

# existen otros recursos en forma de lineas, como los recorridos del transporte publico

# usaremos la libreria de osm para extraer las calles principales de Las Condes
# (asumiremos que por estas pasa el transporte publico)

# identificamos el bbox de las condes
LC_bbox <- getbb("Las Condes, Santiago, Chile")

# vemos las categorias presentes en la libreria OSM
available_tags("highway")

# extraigo las calles principales y sus nodos desde OSM
LC_hway <- opq(LC_bbox) %>% # identifico bbox
  add_osm_feature( # agrego atributo del listado anterior
    key = "highway",
    value = c("motorway", "primary", "motorway_link", "primary_link") # se pueden agregar despues secondary terciary available
  ) %>% 
  osmdata_sf() %>% # transformo en sf
  pluck("osm_lines") %>% # preservo solo elementos de osm_lines
  st_transform(crs=st_crs(poligonos))   # dejo en misma proyeccion que objeto base
  

mapview(LC_hway)


# extramos solo las calles que intersectan con las condes
calles_LC <- 
  LC_hway %>% # tomamos toda la base
  filter(LC_hway %>%  #fitramos cuando hay interseccion entre las calles y 
           st_intersects(LasCondes %>% # el poligono del perimetro de las condes
                           st_buffer(dist = 15)) %>%  # con un margen de 15 metros
           summary() %>% # el resumen de intersect devuelve 1 si hay interseccion 0 eoc
           .[,1] %>% # este valor de 1 viene en la primera columna
           as.numeric() == 1) # pero viene como texto, asi que lo paso a numerico 
                              # verifico si el resultado de esta operacion es 1


# visualizamos recorridos de transporte publico
mapview(calles_LC)

# calculamos distancias a cada linea
distancia_calles <- st_distance(poligonos, calles_LC)

# generamos indicador de accesibilidad a ttpp
poligonos <- 
  poligonos %>% 
  mutate(acc_TTPP = apply(distancia_calles, 1, function(x) min(x)))

# visualizamos
ggplot() +
  geom_sf(data=poligonos, aes(fill=acc_TTPP))+
  geom_sf(data = calles_LC, col = "white") +
  scale_fill_viridis_c()
