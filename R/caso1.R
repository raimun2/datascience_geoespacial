# invocamos librerias que utilizaremos
library(tidyverse)
library(sf)
library(spdep)
set.seed(42)

# cargar funciones para el curso
source("R/funciones_caso1.R")

# cargar datos y explorarlos
poligonos <- read_rds("data/MBHT_LC.rds") 

# vemos los nombres de las variables
names(poligonos)

# vemos los rangos de las variables
summary(poligonos)


# visualizamos poligonos
ggplot() + 
  geom_sf(data=poligonos)

# calculo de pesos espaciales

pesos_espaciales <- poly2listw(poligonos)

plot(pesos_espaciales, coords = st_coordinates(st_centroid(poligonos)), col='red')

# Analisis Univariado ----

# Analizaremos la variable ibt

poligonos$ibt %>% summary()

hist(poligonos$ibt, main=NULL)

boxplot(poligonos$ibt, horizontal = TRUE)

ggplot() + 
  geom_sf(data=poligonos, aes(fill = ibt)) + 
  scale_fill_viridis_c(option = "D", direction = -1)


# Autocorrelacion de Moran ----

# Moran global de la variable ibt
I <- moran(poligonos$ibt, pesos_espaciales, nrow(poligonos), Szero(pesos_espaciales))
I

moran.test(poligonos$ibt,pesos_espaciales, alternative="greater")

variable <- poligonos$ibt

# Moran local y significancia

corr_local <- cuadrant_moran(poligonos$ibt, pesos_espaciales)

(p1 <- ggplot() + 
  geom_sf(data=poligonos, aes(fill = corr_local$cuadrant)) + 
  theme_minimal() +
  theme(legend.title = element_blank(), 
          legend.position = "bottom") )

# Analisis Multivariado ----

# clustering ----

# extraigo variables numericas

polig_num <- 
  poligonos %>% 
  select(ibt:E65YMAS) %>% 
  drop_na() %>% 
  mutate(bienestar = ibt*PERSONAS)

# escalo las variables

polig_num_sc <- 
  polig_num %>% 
  mutate(across(where(is.numeric), ~ as.numeric(scale(.))))


# calculo modelo kmedias
clusters_k <- kmeans(st_drop_geometry(polig_num_sc), 10)

(p2 <- ggplot() + 
  geom_sf(data=polig_num, aes(fill = factor(clusters_k$cluster))) + 
  theme_minimal() +
  theme(legend.title = element_blank(), 
          legend.position = "bottom") )


# calculo modelo jerarquico

clusters_hier <- cutree(hclust(dist(polig_num_sc)), k = 10)

(p3 <- ggplot() + 
  geom_sf(data=polig_num_sc, aes(fill = factor(clusters_hier))) + 
  theme_minimal() +
  theme(legend.title = element_blank(), 
          legend.position = "bottom") )


# regionalizacion ----

regiones_hier <- cutree(regionalization(polig_num_sc, pesos_espaciales), k = 10)

(p4 <- ggplot() + 
  geom_sf(data=polig_num, aes(fill = factor(regiones_hier))) + 
  theme_minimal() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom") )

# junto todos los graficos

library(patchwork)

(p1 + p2) / (p3 + p4)
