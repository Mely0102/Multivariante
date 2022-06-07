#1.Replica

# Cargar la matriz de datos.

X<-as.data.frame(state.x77)

#-------------------------------------
#     Transformacion de datos
#-------------------------------------

#1.- Transformacion de las variables x1,x3 y x8
# con la funcion de logaritmo.

X[,1]<-log(X[,1])
colnames(X)[1]<-"Log-Population"

X[,3]<-log(X[,3])
colnames(X)[3]<-"Log-Illiteracy"

X[,8]<-log(X[,8])
colnames(X)[8]<-"Log-Area"

#---------------------------------
#    Metodo k-means
#---------------------------------

#1.- Separacion de filas y columnas.

dim(X)
n<-dim(X)[1]
p<-dim(X[2])

# 2.- Estandarizacion univariante.
X.s<-scale(X)

# 3.- Algoritmo k-medias (3 grupos)
# cantidad de subconjuntos aleatorios que se escogen
# para realizar los calculos de algoritmo.
Kmeans.3<-kmeans(X.s, 3, nstart=25)

# centroides
Kmeans.3$centers

# cluster de pertenencia
Kmeans.3$cluster


# 4.- SCDG
SCDG<-sum(Kmeans.3$withinss)


# 5.- Clusters
cl.kmeans<-Kmeans.3$cluster


# 6.- Scatter plot con la division de grupos
# obtenidos (se utiliza la matriz de datos centrados).
col.cluster<-c("blue", "red", "green")[cl.kmeans]
pairs(X.s, col=col.cluster, main="k-means", pch=19)

#-----------------------------------------
#  Visualizacion con las dos componentes principales
#-----------------------------------------

library(cluster) 

clusplot(X.s, cl.kmeans, 
         main="Dos primeras componentes principales")

text(princomp(X.s)$score[,1:2],
     labels=rownames(X.s), pos=1, col="blue")

#-------------------------------------
#  Silhouette
#--------------------------------------
# Representacion grafica de la eficacia de
# clasificacion de una observacion dentro de un
# grupo.

# 1.- Generacion de los calculos
dist.Euc<-dist(X.s, method = "euclidean")
Sil.kmeans<-silhouette(cl.kmeans, dist.Euc)

#2.- Generacion del grafico
plot(Sil.kmeans, main="Silhouette for k-means", 
     col="blue")

________________________________________________________________________________

# 2. Cambiar el Número de clusters

# Cargar la matriz de datos.
#aqui se cnsieran las medianas
#busca k objetos representativos

X<-as.data.frame(state.x77)

#     Transformacion de datos


#1.- Transformacion de las variables x1,x3 y x8
# con la funcion de logaritmo.

X[,1]<-log(X[,1])
colnames(X)[1]<-"Log-Population"

X[,3]<-log(X[,3])
colnames(X)[3]<-"Log-Illiteracy"

X[,8]<-log(X[,8])
colnames(X)[8]<-"Log-Area"


#    Metodo k-means

#1.- Separacion de filas y columnas.

dim(X)
n<-dim(X)[1]
p<-dim(X[2])

# 2.- Estandarizacion univariante.
X.s<-scale(X)

# 3.- Algoritmo k-medias (5 grupos)
# nstar es cantidad de subconjuntos aleatorios que se escogen
# para realizar los calculos de algoritmo.
# el 3 es el nmero de clouster o de agrpupaciones, en este caso se utilizan 3
Kmeans.5<-kmeans(X.s, 5, nstart=25)

# centroides
Kmeans.5$centers

# cluster de pertenencia
Kmeans.5$cluster


# 4.- SCDG
#hasta aqui llego el minimo de scdg la idea es llegar a 0 
SCDG<-sum(Kmeans.5$withinss)


# 5.- Clusters
cl.kmeans<-Kmeans.5$cluster


# 6.- Scatter plot con la division de grupos
# obtenidos (se utiliza la matriz de datos centrados)
col.cluster<-c("dodgerblue2", "deeppink", "olivedrab1","plum3","tomato3")[cl.kmeans]
pairs(X.s, col=col.cluster, main="k-means", pch=19)

#  Visualización con las dos componentes principales

library(cluster) 

clusplot(X.s, cl.kmeans, 
         main="Dos primeras componentes principales")

text(princomp(X.s)$score[,1:2],
     labels=rownames(X.s), pos=1, col="magenta3")


#  Visualizacion con las dos componentes principales

library(cluster) 

clusplot(X.s, cl.kmeans, 
         main="Dos primeras componentes principales")

text(princomp(X.s)$score[,1:2],
     labels=rownames(X.s), pos=1, col="maroon1")

# de aqui se puede tomar la descicio para aumentar el numero de clousters
#  Silhouette

# Representacion grafica de la eficacia de
# clasificacion de una observacion dentro de un
# grupo.

# 1.- Generacion de los calculos
dist.Euc<-dist(X.s, method = "euclidean")
#el cl.kmeans es dode se se encuentran los closters 
Sil.kmeans<-silhouette(cl.kmeans, dist.Euc)

#2.- Generacion del grafico
#los ultimos numeros de la derecha son la probabilidad si es bajo es decir que la clasificacion es baja 
plot(Sil.kmeans, main="Silhouette for k-means", 
     col="turquoise2")

## Análisis:
#Se utilizo un nuevo numero de clousters en este caso fueron 5, y se disminuyo significativamente la suma de cuadrados dentro del grupo pero la probailidad de agrupamiento es muy baja para la mayoria de los grupos, el unico mas significativo es 3 y 4 no es probabilidad, es el ancho de silhouet el promedio de siluedt debe ser alto, en este caso es de 0.27 por lo que se debe busacar otro numero de clousters 
#Se da como consejo bajar el numero de clouster a 4 y volver a replicar el codigo 

