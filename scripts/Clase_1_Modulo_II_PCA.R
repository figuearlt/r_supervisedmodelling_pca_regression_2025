
Dataset <- 
  read.table("C:/Users/diego/OneDrive/Escritorio/Diplomado Data Science/Diplomado PUCV/Módulo_2_Componentes_Principales/autos.txt",
   header=TRUE, stringsAsFactors=TRUE, sep="", na.strings="NA", dec=".", 
  strip.white=TRUE)
Dataset.PCA<-Dataset[, c("CYL", "POT", "LAR", "ANCHO", "PESO", "VEL", 
  "PRECIO", "LUJO")]
res<-PCA(Dataset.PCA , scale.unit=TRUE, ncp=5, quanti.sup=c(7: 7), 
  quali.sup=c(8: 8), graph = FALSE)
print(plot.PCA(res, axes=c(1, 2), choix="ind", habillage="none", 
  col.ind="black", col.ind.sup="blue", col.quali="magenta", label=c("ind", 
  "ind.sup", "quali"),new.plot=TRUE, title=""))
print(plot.PCA(res, axes=c(1, 2), choix="var", new.plot=TRUE, 
  col.var="black", col.quanti.sup="blue", label=c("var", "quanti.sup"), 
  lim.cos2.var=0, title=""))
summary(res, nb.dec = 3, nbelements=10, nbind = 10, ncp = 3, file="")
remove(Dataset.PCA)

