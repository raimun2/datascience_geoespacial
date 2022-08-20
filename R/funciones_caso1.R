# funcion para transformar un objeto sf de poligonos desprolijos en una lista de vecinos
# hay 2 metodos, por interseccion con buffer o por limite de un radio 
# (utiliza el inverso de la distancia dentro de ese buffer)
poly2listw <- function(polygons, dist = NULL, radius = FALSE){
  if(!radius){ 
    if(is.null(dist)){
      # si no se entrega tamaño del buffer lo estima a partir de las geometrias
      dist <- mean( sqrt( st_area(polygons) / pi )/ 4)
    }
    
    poly_buff <- 
      polygons %>% 
      st_buffer(dist=20) %>% 
      st_cast("MULTIPOLYGON")
    
    # construye matriz de interseccion
    overlap_mat <- st_overlaps(poly_buff, sparse=FALSE)
    
    # transforma matriz en lista de vecinos
    listw <- mat2listw(overlap_mat)
    
    
  }else{
    if(is.numeric(radius)){ # si viene un radio (sobreescribe buffer)
      
      # calculo centroides de poligonos
      centroids <- 
        st_point_on_surface(x = polygons) 
      
      # calcula matriz de distancias
      dist_mat <- 
        st_distance(centroids, centroids) 
      
      # calcula el inverso y asigna 0 en la diagonal
      distancias.inv <- 1/dist_mat
      diag(distancias.inv) <- 0
      
      # asigna 0 si distancia es mayor al radio
      distancias.inv[as.numeric(dist_mat) > radius] <- 0
      
      # matriz de pesos
      sp_w <- matrix(as.numeric(distancias.inv), nrow =  nrow(distancias.inv))
      
      # lista de pesos
      listw <- mat2listw(sp_w)
      
    }
  }
  return(listw)
}


# funcion para calcular los cuadrantes de moran de una variable a partir de sus posiciones
# respecto de la mediana de una variable en particular
cuadrant_moran <- function(variable, pesos_espaciales){
  
  # calculo el indicador local de moran
  lmoran <- localmoran(variable, pesos_espaciales) %>% as.data.frame()
  
  # genero matriz de I local y sus pvalues, y asigno cuadrantes
  matrix <- data.frame(value = ifelse(variable > median(variable), "H", "L"),
                       correlation = ifelse(lmoran$Ii > median(lmoran$Ii),"H","L"),
                       significance = lmoran$`Pr(z != E(Ii))`) %>% 
    mutate(cuadrant = ifelse(significance < 0.1, paste0(value,correlation), NA))
  
  return(matrix)
  
}

# funcion de regionalizacion
# genera zonas continuas en el espacio
regionalization <- function(df_variables, pesos_espaciales){
  
  # pasa los pesos espaciales a una matriz binaria
  matriz_vec <- map(1:nrow(df_variables), 
                    function(i) as.numeric(1:nrow(df_variables) %in% pesos_espaciales[[2]][[i]])) %>% 
    unlist() %>% 
    matrix(nrow=nrow(df_variables))
  
  # calculo distancia de atributos y transformo a matriz
  matriz_atr <- dist(df_variables) %>% as.matrix()
  
  # genero matriz de atributo vecindad
  # si es vecino preservo la distancia de atributo
  # si no es vecino, multiplica el valor por un numero muy muy grande (10E6*max(df_variable))
  matriz_atr_vec <- (1-matriz_vec)*(matriz_atr %>% max())*1000000 + matriz_vec*matriz_atr
  
  # transformo matriz a objeto dist
  dist_atr_vec <- matriz_atr_vec %>% as.dist()
  
  # aplico hclust con nueva matriz de atr vec
  regionalization <- hclust(dist_atr_vec, method = "average")
  
  return(regionalization)
}
