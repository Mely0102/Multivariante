# ANALISIS DISCRIMINANTE LINEA

# Se carga y descarga la paqueteria
library(MASS)

# Se cargan los datos state.x77
JC<-as.data.frame(state.x77)

# Se define la matriz de datos y la variable
x<-JC[,1:4]
y<-JC[,5]

# Definir n y p
n<-nrow(x)
p<-ncol(x)

# Se aplica el Análisis discriminante lineal (LDA)
# Cross validation (cv): clasificacion optima
lda.state.x77<-lda(y~.,data=x, CV=TRUE)

# lda.state.x77$class contiene las clasificaciones hechas por CV usando LDA.
lda.state.x77$class

# Creación de la tabla de clasificaciones buenas y malas
table.lda<-table(y,lda.state.x77$class)
table.lda

# Proporción de errores
mis.lda<- n-sum(y==lda.state.x77$class)
mis.lda/n

# scater plot
# Buenas clasificaciones en negro y malas en rojo
col.lda.state.x77<-c("indianred1","black")[1*(y==lda.state.x77$class)+1]
pairs(x,main="Buena Clasificación (negro), Mala Clasificación (rojo)",
      pch=19,col=col.lda.state.x77)

# Probabilidad de pertenencia a uno de los tres grupos
lda.state.x77$posterior

# Gráfico de probabilidades

plot(1:n, lda.state.x77$posterior[,1],
     main="Probabilidades a Posterior",
     pch=20, col="black",
     xlab="Número de Observaciones", ylab="Probabilidades")
points(1:n,lda.state.x77$posterior[,2],
       pch=20, col="blue")
points(1:n,lda.state.x77$posterior[,3],
       pch=20, col="red")
