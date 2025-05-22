library(Rcmdr)


# Para eso librarý(Rcmdr) < R Commander < Datos < Importar datos
Dataset <- read.table("C:/Users/diego/OneDrive/Escritorio/Diplomado Data Science/Diplomado PUCV/Módulo_2_Componentes_Principales/autos.txt",
                      header=TRUE,stringsAsFactors=TRUE, sep="",
                      na.strings="NA", dec=".", strip.white=TRUE,row.names="NOMBRE")

# PASO 2: Realizamos una matriz de correlación. Para eso librarý(Rcmdr) < R Commander < Estadísticos < Resumenes < Matriz de correlaciones
# Son correlaciones de 2 a 2 variables

