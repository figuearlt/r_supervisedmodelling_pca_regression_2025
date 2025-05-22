library(dplyr)
library(tidyverse)
library(FactoMineR)
library(EFAtools)
library(factoextra)
library(ggplot2)
library(corrplot)



##############
## Importar ##
##############

# Importar archivo .TXT
autos<-read.table("autos.txt",header = 1,row.names = "NOMBRE")
head(autos)

# Importar archivo .CSV
clusters_sobre_pca <- read.csv("clusters_sobre_acp.csv",row.names = "X")
head(clusters_sobre_pca)

# Trabajar con la data
clusters_trabajo <- clusters_sobre_pca %>% select(-"clust")
head(clusters_trabajo)


#############################
## Información descriptiva ##
#############################

# Información descriptiva
summary(autos)
str(autos)
dim(autos)
colnames(autos)


##################
## Análisis EDA ##
##################

# Análisis EDA:
#Distribución Precio
promedio_precio <- mean(autos$PRECIO) 
media_precio <- median(autos$PRECIO) 
ggplot(autos,aes(x=PRECIO)) + 
  geom_histogram(fill="steelblue", bins=10, color="white") + 
  geom_vline(aes(xintercept = promedio_precio),color='red',linetype='dashed',size=1,) +
  geom_text(aes(x=promedio_precio,y=5, 
            label=paste("Promedio: ",round(promedio_precio,0))),
            color='red',fontface='italic' ,hjust=-.1) +
  geom_vline(aes(xintercept = media_precio),color='darkgreen',size=1,) +
  geom_text(aes(x=media_precio,y=4.5, 
                label=paste("Mediana: ",round(media_precio,0))),
            color='darkgreen',fontface='italic' ,hjust=-.2) +
  labs(title = "Distribución de precios", x="Precio", y="Cantidad de autos", y="Cantidad de autos")

  
  

#  Distribución según el tipo de auto
ggplot(autos,aes(x=LUJO)) +
       geom_bar(fill='darkgreen',width = .5,color='white') +
         labs(title="Cantidad de autos según categoría de Lujo",x="Lujo",y="Cantidad")
         
# Boxplot del PRECIO según LUJO
ggplot(autos,aes(x=LUJO,y=PRECIO)) +
  geom_boxplot(inherit.aes = TRUE,fill="lightgreen",color='darkgreen',,outlier.color ='red' ) +
  labs(title = "Boxplot del Precio de Autos", y = "Precio")+theme_minimal()

# Heatmap entre las variables

autos_numerico <- autos %>% select(-"LUJO")
matriz_corr <- cor(autos_numerico,method = 'pearson')
corrplot(matriz_corr,method = 'circle', type='upper',
         addCoef.col = 'white', number.cex = 0.5,tl.srt = 40,
         title = "Heatmap de Correlación")


# Nivel avanzado
library("GGally")


# Seleccionar las variables numéricas para el análisis bivariado
numeric_vars <- c("CYL", "POT", "LAR", "ANCHO", "PESO", "VEL", "PRECIO")
df_numeric <- autos[, numeric_vars]

# Crear la matriz de gráficos de dispersión con ggpairs()
ggpairs(df_numeric,
        title = "Relaciones Bivariadas entre Variables",
        lower = list(continuous = "smooth"), # Gráficos de dispersión suavizados en la parte inferior
        diag = list(continuous = "density"),   # Gráficos de densidad en la diagonal
        upper = list(continuous = "cor"))     # Coeficientes de correlación en la parte superior



# Relación Bivariada PRECIO vs POTENCIA

cor_precio_pot <- cor(x=autos$POT,y=autos$PRECIO,method = 'pearson')
ggplot(autos,aes(x=POT,y=PRECIO)) +
  geom_point(,color='darkgreen') +
  
  geom_text(aes(x=min(POT),y=max(PRECIO),
            label=paste("Corr: ", round(cor_precio_pot,3))),
            hjust = 0, vjust = 1) +
  
  labs(title="Scatterplot entre PRECIO vs POTENCIA", x= "Potencia",y= "Precio")+
  theme_minimal()




# Relación Bivariada PRECIO vs POTENCIA
ggplot(autos,aes(x=POT,y=PRECIO)) +
  geom_smooth(,color='red',method='lm') +
  labs(title="Scatterplot entre PRECIO vs POTENCIA", x= "Potencia",y= "Precio")


# Matriz de gráficos de dispersión para algunas variables numéricas

pairs(autos_numerico)



##############################
## Reducción de Dimensiones ##
##############################

# Probar que es necesario ocupar Reducción de Dimensiones

# Test de Barlett: Evalúa si hay relaciones lineales significativas entre las variables

BARTLETT(autos_numerico,cor_method = "pearson") # Se comprueba con un p value menor a 0.05 que es aceptable realizar una reducción de dimensiones

# Luego, KMO: Evalúa si las variables están lo suficientemente correlacionadas para poder hacer un PCA o análisis factorial.


KMO(autos_numerico,cor_method = 'pearson')

# Si el KMO individual de alguna variable es muy bajo, podrías considerar eliminar esa variable antes del análisis.


# Aplicar PCA
corr_autos <- cor(autos_numerico,method = "pearson")
corrplot(corr_autos,method = 'circle',type = 'upper',
         addCoef.col ='white',
         number.cex = 0.5,tl.srt = 70,tl.cex =0.8, 
         title = 'Correlación entre variables Base Autos',
         mar = c(0, 0, 1, 0))
head(autos)
autos.PCA <- autos[,c("CYL", "POT", "LAR" ,"ANCHO" ,"PESO", "VEL" ,"LUJO","PRECIO")] # subconjunto
autos_pca<-PCA(autos.PCA,scale.unit = TRUE,ncp = 5,quali.sup = 7,quanti.sup = 8,graph=FALSE)
head(autos_pca)


print(plot(autos_pca,choix="ind", habillage="none",
           col.var="green",col.ind="black", col.ind.sup="blue", col.quali="magenta", 
           label=c("ind","ind.sup", "quali"),
           new.plot=TRUE, title="PCA graph of individuals"))
print(plot(autos_pca,choix="var",
           col.quanti.sup="darkgreen", col.quali.sup="magenta", col.var="black",
           #label=c("ind","ind.sup", "quali"),
           new.plot=TRUE, title="PCA graph of individuals"))


summary(autos_pca, nb.dec = 3, nbelements=10, nbind = 10, ncp = 3, file="")



###############################################################
## Generación de Cluster Jerárquicos por Método Aglomerativo ##
###############################################################

# Cluster Jerárquico

library(patchwork)

methods_to_compare <- c("ward", "single", "complete", "average","median","centroid")
autos_hcpc <- HCPC(autos_pca,nb.clust = 3,method='ward',consol =TRUE,graph=FALSE)

# Visualización
fviz_cluster(autos_hcpc,repel = TRUE,
             cex = 0.6, #Tamaño de etiqueta
             palette = 'lancet', # Para ver más opciones de colores tecleat ?ggpubr::ggpar
             show.clust.cent=FALSE, #Aquí puedes ver el centroide de los clusters
             rect_border = 'lancet',
             ggtheme = theme_minimal(),
             labels_track_height = 2.,
             main = "Cluster de Individuos")

fviz_dend(autos_hcpc,cex = 0.6, #Tamaño de etiqueta
          palette = 'lancet',rect_fill = TRUE,rect = TRUE,ggtheme = theme_minimal(),
          rect_border = 'lancet',
          labels_track_height = 2. )


# Le pedimos que nos despliegue las variables más significativas para cada cluster
autos_hcpc$desc.var$quanti


