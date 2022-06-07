# Cargamos librerias
install.packages("cluster.datasets")
library(cluster.datasets)
library("cluster")

# data set 
library(readxl)
spotify <- read_excel("spotify.xlsx")

# Cambiamos el nombre de la matriz
M=spotify

# Exploración base de datos
dim(M)

# Nombre de las Variables 
names(M)

# Tipo de Variables
str(M)

# Calculo de la matríz de distancia de Mahalonobis
dist.M<-dist(M[,2:6])

# Convertir los resultados del Calculo de la distancia a una matríz de datos y me indique 3 digitos.
round(as.matrix(dist.M)[1:6, 1:6],3)

# Calculo del dendrograma
dend.M<-as.dendrogram(hclust(dist.M))
dend.M

# Dendrograma
library(dendextend)

# Guardar las etiquetas en un objeto "L"
L=labels(dend.M)
labels(dend.M)=M$Artist.Name[L]

#Dendrograma con las nuevas etiquetas
install.packages("factoextra")
install.packages("ggplot2")
library(factoextra)
library(ggplot2)
fviz_dend(dend.M, 
          k = 3,cex=0.55,border=2:10,k_colors = c("darkcyan","magenta3","orangered2")) +
  labs(title = "Dendrograma Top 50 de Artistas en Spotify 2019",
       subtitle = "Dividido en 3 Grupos") 
