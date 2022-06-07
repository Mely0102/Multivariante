# 2.- abrir libreria 
library(datos)
# 3.- eleccion de matriz de datos 
m<-data.frame(datos::fiel)
# 4.- exploracion base de datos
dim(m)
install.packages("datos")
##Tipo de Variable
str(m)
head(m)# el head se utiliza par copiar el nombre de la variable que no quieres 
# 5.- filtara las variables quedarse con las cuantitativas
m[c("erupciones","espera")]<-NULL # se eliminan las cualitativas 
str(m) # se vuelve a visualizar para asegurarse que se removieron  
# 6.- Se definen n (numero de observaciones) y p (9 variables de atmosfera)
dim(m)

n<-dim(m)[1]
p<-dim(m)[2]

# 7.- Generacion de un scatterplot
# de las variables originales
pairs(m,col="indianred4", pch=19, 
      main="Variables originales")

# 8.- Obtencion de los componentes principales
# con base en la matriz de covarianza muestral

mu<-colMeans(m)
mu
s<-cov(m)
s
# la variable nube_baja cuso valores NA por tanto se elimina 
m[c("nube_baja")]<-NULL  
str(m)
# se vuelve a re definir 
dim(m)

n<-dim(m)[1]
p<-dim(m)[2]

# Obtencion de los componentes principales nuevamente 
# con base en la matriz de covarianza muestral

mu<-colMeans(m)
s<-cov(m)
s
# 9.- Obtencion de los componentes principales 
# con base a la matriz de covarianza muestral
es<-eigen(s)
es

# 10.- Matriz de auto-valores
eigen.val<-es$values
eigen.val
# 11.- Matriz de auto-vectores
eigen.vec<-es$vectors
eigen.vec
# Proporcion de variabilidad para cada vector
pro.val<-eigen.val/sum(eigen.val)
pro.val
# Proporcion de variabilidad acumulada
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)
pro.var.acum
#----------------------------------
# Obtencion de los componentes principales con
# base en la matriz de correlaciones muestrales
#---------------------------------------

R<-cor(m)
eR<-eigen(R)
eR

# Obtencion de auto-valores
eigen.val<-eR$values
eigen.val
# Obtencion de auto-vectores
eigen.vec<-eR$vectors
eigen.vec
# Proporcion de variablidad
pro.var<-eigen.val/sum(eigen.val)
pro.var
# Proporcion de variabilidad acumulada
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)
pro.var.acum
# Media de los auto-valores
mean(eigen.val)

#---------------------------
# Obtencion de los coeficientes (nuevas variables)
# 
#--------------------------

# 1.- Centrar los datos con respecto a la media
ones<-matrix(rep(1,n),nrow=n, ncol=1)
ones
# 2.- Construccion de la matriz centrada
X.cen<-as.matrix(m)-ones%*%mu
X.cen

# 3.- Construccion de la matriz diagonal de las 
# varianzas
Dx<-diag(diag(s))
Dx

# 4.- Construccion de la matriz centrada multiplicada
# por Dx^1/2

Y<-X.cen%*%solve(Dx)^(1/2)
Y #datos normalizados

# 5.- Construccion de los coeficientes o scores
# eigen.vec matriz de autovectores
scores<-Y%*%eigen.vec

# Nombramos las columnas PC1...PC8
colnames(scores)<-c("PC1","PC2")

# visualizamos
scores

\pagebreak
# Generacion del grafico de los scores
pairs(scores, main="scores", col="lightpink3", pch=19)



