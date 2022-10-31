Tarea asincrónica Modelamiento Territorial
================
Raimundo Sanchez
02-11-2022

## Instalación de R y RStudio

### ¿Qué es R?

<img src="img/Rlogo.png" style="width:10.0%" />

R es un lenguaje de computación estadística (statistical computing), de
código abierto y de uso libre. Está diseñado para trabajar con datos, y
por lo tanto es una herramienta muy apropiada para trabajar en campos de
estudio que se basan en datos o modelos matemáticos, como la economía,
las finanzas, la ciencia de datos, el aprendizaje de máquinas, etc.

Al ser un lenguaje de programación, en lugar de una herramienta con una
interfaz de usuario, es muy rápido y eficiente para ejecutar cálculos,
pero requiere que el usuario tenga una alta capacidad de abstracción
para ordenar las tareas en las líneas de código.

### ¿Qué es RStudio?

RStudio es un editor de código de R open source, y es el más utilizado
en el mundo de analistas de datos. Esta herramienta es un ambiente de
desarrollo integrado, el cual nos permitirá escribir código de manera
más fácil.

La interfaz de RStudio se divide en 4 cuadrantes:

-   Izquierda superior: El editor de código propiamente tal. El código
    se escribe en archivos de texto plano que tienen extensión .R, de
    manera de poder reproducir los análisis que vayamos realizando.

-   Derecha superior: Environment, o entorno de trabajo, en este
    cuadrante se van visualizando todos los datos que vamos utilizando
    en nuestro análisis.

-   Derecha inferior: Ventana de salida, acá se visualizan los gráficos
    resultantes de nuestros análisis.

-   Izquierda inferior: Consola, acá es donde se muestra el resultado de
    la ejecución de cada línea de código.

Cabe señalar que cada cuadrante tiene pestañas adicionales con otras
funcionalidades.

<center>
<img src="img/rstudio-editor.png" style="width:90.0%" />
</center>

### Instalación de R

Para instalar R se sugiere hacerlo desde el respositorio CRAN (The
Comprehensive R Archive Network) de Rstudio (<https://cran.rstudio.com>)
donde solo se debe seleccionar de acuerdo a su sistema operativo,
idealmente la última versión. Aquellos que tienen una versión menores de
4.1 se recomienda actualizar a la ultima versión.

En el proceso de instalación se pregunta sobre idioma, cuya decisión es
libre, pero recomendamos elegir *inglés* esto cobrará importancia cuando
se busque documentación en la web o libros sobre algún eventuales
“*warning*” o “*error*”.

<center>
<img src="img/cran_rstudio.png" style="width:60.0%" />
</center>

#### Instalación de R para Windows

<center>
<img src="img/install_r_w.gif" style="width:90.0%" />
</center>

#### Instalación de R para Mac

<center>
<img src="img/install_r_mac.gif" style="width:90.0%" />
</center>

#### Instalación de R para Linux

Para Linux se puede instalar desde la terminal

``` r
sudo apt update

sudo apt -y upgrade

sudo apt -y install r-base
```

#### Instalación de RStudio

RStudio puede ser descargado directamente de la su página oficial
<https://rstudio.com/products/rstudio/download/#download>, donde se debe
seleccionar el sistema operativo que corresponde.

<center>
<img src="img/instaladores_rstudio.png" style="width:60.0%" />
</center>

Se aconseja revisar las siguiente documentación en el siguiente
[link](https://b-rodrigues.github.io/modern_R/getting-to-know-rstudio.html),
donde se explicativa diferentes paneles de Rstudio de manera gráfica.

### R Projects

Los R Projects son estructuras de proyecto dentro de RStudio y permiten
tener el codigo, los datos y los resultados ordenados en un mismo
directorio de trabajo.

Para este curso vamos a descargar un proyecto desde esta misma pagina
haciendo click en Code y luego en Download ZIP. Una vez descargada la
carpeta ZIP, descomprimala en su computador y abra el archivo del
proyecto, llamado modelamiento_territorial.Rproj. Esto abrira RStudio y
podremos comenzar a trabajar.

<center>
<img src="img/rproject.png" style="width:60.0%" />
</center>

### Instalar Librerías

Las librerías o paquetes en R corresponden a una colección de funciones
encapsuladas y diseñadas para atender una tarea específica. Por ejemplo,
hay paquetes para visualización, o para analisis geoespacial, o análisis
psicométricos, mineria de datos, interacción con servicios de internet y
muchas otras cosas más.

Estos paquetes se encuentran alojados principalmente en
[CRAN](https://cran.r-project.org) (Comprehensive R Archive Network),
así que pasan por un control riguroso antes de estar disponibles para su
uso generalizado. En el siguiente enlace se deja una lista de temas y
sus librerías asociadas disponibles, con una breve descripción
([enlace](https://cran.r-project.org/web/views/))

Aunque otras librerías no oficiales pueden estar disponible en
[Github](https://github.com) de cada autor.

Podemos instalar paquetes usando la función `install.packages()`, dando
como argumento el nombre del paquete que deseamos instalar, entre
comillas.

Para instalar librerías se puede hacer directamente desde la consola de
RStudio, por ejemplo:

``` r
install.packages(c("dplyr","ggplot2", "tidyverse","sf")) # Se puede instalar más de una librería a la vez
```

También se pude instalar librerías del Panel “Packages”

<center>
<img src="img/install_pack2.png" style="width:90.0%" />
</center>

### Ejecutar codigo

Para ejecutar una línea de código debe seleccionarla y apretar
Ctrl+Enter

Al hacerlo se vera la instrucción en la consola. Algunas líneas de
código modifican o crean variables, y otras líneas de código generan
directamente un output, imprimiendo en la consola el resultado o
generando algún grafico

``` r
# esto es un comentario 
# los comentarios van en el codigo pero no se ejecutan
# sirven para ir señalando que hace cada linea de codigo

# aca creamos una variable y almacenamos en ella el resultado de una operacion simple
resultado_de_una_suma = 3 + 5

# imprimimos el valor almacenado en esta variable
print(resultado_de_una_suma)

# ploteamos 
plot(resultado_de_una_suma)
```

## Tarea asincrónica

-   Paso 1: Abra el proyecto modelamiento_territorial.Rproj que acaba de
    descargar

-   Paso 2: En la pestaña File, crear un nuevo script de R y guardarlo
    como nombre_apellido.R

-   Paso 3: Copie el codigo a continuacion y peguelo en su script (debe
    guardarlo en la carpeta R de este mismo proyecto)

``` r
# cargamos las librerias (deben haberlas instalado previamente)
library(tidyverse)
library(sf)

# leemos datos de la MBHT de Las Condes
poligonos = read_rds("data/MBHT_LC.rds") 

# imprimimos en la consola los nombres de las columnas
names(poligonos)

# generamos un grafico con el ibt como variable de relleno
ggplot(data=poligonos) + 
  aes(fill = ibt) +
  geom_sf()
```

-   Paso 4: Cambie la linea donde dice fill = ibt, y seleccione otra
    variable diferente de ibt, y generar otro grafico.

![](readme_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Referencias

Página Web Oficial R <https://www.r-project.org>  
Página Rstudio <https://rstudio.com>  
Pagina libro de R para Data Science <https://es.r4ds.hadley.nz/>  
