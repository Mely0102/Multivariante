# Descargar Librerias
#install.packages("psych")
library(psych)
#install.packages("polycor")
library("polycor")
#install.packages("ggcorrplot")
library("ggcorrplot")


O<-data.frame(Bechtoldt)

#Extraccion de datos
O<-Bechtoldt

# Exploracion de la matriz
##Dimension
dim(O)

# Tipo de variables
str(O)

# Nombre de las variables
colnames(O)

# Creacion de la matriz de datos se incluten las variables  1 a la 17 y las primeras 15 obervaciones
x1<-Bechtoldt[1:17,1:15]

# Matriz de correlaciones
R<-hetcor(x1)$correlations

# Grafico de correlaciones
ggcorrplot(R,type="lower",hc.order=TRUE)

# Factorizacion de la matriz de correlaciones

# Se utliza la prueba de esfericidad de Bartlett

p_Bartlett<-cortest.bartlett(R)

# Vizualizar del p-valor
p_Bartlett$p.value

# H0:Las variables estan correlacionadas
# Ha:Las variables no estan correlacionadas

KMO (R)

Extracion de factores 
minres: minimo de residuos
mle: max de verosimilitudes
paf:ejes principales
alpha: alfa
minchi: minimos cuadrados
mirank:rango minimo

modelo1<-fa(R,nfactor=3,rotate = "none",fm="mle")
modelo2<-fa(R,nfactor=3,rotate = "none",fm="minres")

# Extraer el resultados de las comunidalidades
Encontrar la proporcion varianza explicada. Se interpreta de tal forma que el numero cercanos a 1, el factor explica mejor la variable.

C1<-sort(modelo1$communality,decreasing = TRUE)
C2<-sort(modelo2$communality,decreasing = TRUE)

head(cbind(C1,C2))

# Extraccion de Unicidades
La unicidad es el cuadrado del coeficiente del factor unico y se expresa como la porcion de la varianza explicada por el factor unico. Quiere decir que no se puede explicar por otros.

u1<-sort(modelo1$uniquenesses,decreasing = TRUE)
u2<-sort(modelo2$uniquenesses,decreasing = TRUE)

head(cbind(u1,u2))

# Rotacion de la matriz
install.packages("GPArotation")
library(GPArotation)

rot<-c("None","Varimax","Quartimax","Promax")
bi_mod<-function(tipo){
  biplot.psych(fa(x1,nfactors = 2,fm="minres",rotate = tipo),col = c(2,3,4),pch = c(21,18),group = bfi[,"gender"])
}
sapply(rot,bi_mod)

# Interpretacion
Para esto se utiliza un grafico de arbol

modelo_varimax<-fa(R,nfactors = 5, rotate = "varimax",fm="minres")
fa.diagram(modelo_varimax)

# Visualizacion de la matriz cargada
print(modelo_varimax$loadings,cut=0)