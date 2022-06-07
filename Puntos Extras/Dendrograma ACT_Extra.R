# Dendrograma
# Cargamos librerias
install.packages("cluster.datasets")
library("cluster.datasets")

data(animal.cluster.trees)

# Cambiamos el nombre de la matriz
ACT=animal.cluster.trees
head(ACT)

# Calculo de la matriz de distancia de Mahalonobis
dist.ACT<-dist(ACT[,2:6])

# Convertir los resultados del Calculo de la distancia a una matriz de datos y me indique 3 digitos.
round(as.matrix(dist.ACT)[1:6, 1:6],3)

# Calculo del dendrograma
dend.ACT<-as.dendrogram(hclust(dist.ACT))

# Generacion del dendrograma
plot(dend.ACT)

# Agregar etiquetas al Grafico
ACT.nombres=ACT
rownames(ACT.nombres)= ACT.nombres$name
ACT.nombres=ACT.nombres[,-1]

# Construimos de nuevo el Grafico
plot(as.dendrogram(hclust(dist(ACT.nombres))))

#  Modificar el dendrograma
install.packages("dendextend")
library(dendextend)

# Guardar las etiquetas en un objeto "L"
L=labels(dend.ACT)
labels(dend.ACT)=ACT$name[L]

# Cambiar el tamaño de las etiquetas
dend.ACT %>%
  set(what="labels_col", "red") %>% #Colores etiqueta
  set(what="labels_cex", 0.8) %>%
  plot(main="Dendrograma de Grupo de Animales")

# Dendograma de Circulo
install.packages("circlize")
library("circlize")

circlize_dendrogram(dend.ACT,labels_track_height=NA,
                    dend_track_height=0.1)
