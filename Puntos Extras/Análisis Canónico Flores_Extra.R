#Analisis Canonico


# Instalar paqueterias
install.packages("tidyverse",dependencies = T)
library(tidyverse)


# Preparación de la matriz

# Importar la matriz de datos.
G<-data.frame(datos::flores)

# Exploracion de la matriz

dim(G)
colnames(G)
str(G)
anyNA(G)

# Escalamiento de la matriz

# Generacion de variables X
X <- G %>% 
  select(Largo.Sepalo,Ancho.Sepalo ) %>%
  scale()
head(X)
G$Largo.Sepalo
# Generacion de variables Y
Y <- G %>%
  select(Largo.Petalo,Ancho.Petalo) %>%
  scale()
head(Y)

#----------------------------------
# Analisis canonico con un par de variables
#----------------------------------

# Libreria
install.packages("CCA")
library("CCA")

# Analisis
ac<-cancor(X,Y)

# Visualizacion de la matriz X
ac$xcoef

# Visualizacion de la matriz Y
ac$ycoef

# Visualizacion de la correlacion canonica
ac$cor

# Obtencion de la matriz de variables canonicas
# Se obtiene multiplicando los coeficientes por
# cada una de las variables (X1 y Y1)
ac1_X <- as.matrix(X) %*% ac$xcoef[, 1]
ac1_Y <- as.matrix(Y) %*% ac$ycoef[, 1]

# Visualizacion de los primeros 20 datos
ac1_X[1:20,]
ac1_Y[1:20,]

# Correlacion canonica entre variable X1 y Y1
cor(ac1_X,ac1_Y)

# Verificacion de la correlacion canonica
assertthat::are_equal(ac$cor[1], 
                      cor(ac1_X,ac1_Y)[1])


# Analisis canonico con dos pares de variables

# Calculo de las variables X2 y Y2
ac2_X <- as.matrix(X) %*% ac$xcoef[, 2]
ac2_Y <- as.matrix(Y) %*% ac$ycoef[, 2]

# Agregamos las variables generadas a la matriz
# original de penguins

ac_df <- G %>% 
  mutate(ac1_X=ac1_X,
         ac1_Y=ac1_Y,
         ac2_X=ac2_X,
         ac2_Y=ac2_Y)

# Visualizacion de los nombres de las variables
colnames(ac_df)

# Generacion del grafico scater plot para la
# visualizacion de X1 y Y1
ac_df %>% 
  ggplot(aes(x=ac1_X,y=ac1_Y))+
  geom_point(color="indianred1")

# Generacion de un boxplot
ac_df %>% 
  ggplot(aes(x=Especie,y=ac1_X, color=Especie))+
  geom_boxplot(width=0.5)+
  geom_jitter(width=0.15)+
  ggtitle("Variable Canónica X1 contra Especie")

# Interpretación: se observa una correlacion entre
# la variable canónica X1 y la variable latente Especie

ac_df %>% 
  ggplot(aes(x=Especie,y=ac1_Y, color=Especie))+
  geom_boxplot(width=0.5)+
  geom_jitter(width=0.15)+
  ggtitle("Variable Canónica Y1 contra Especie")

ac_df %>% 
  ggplot(aes(x=ac1_X,y=ac1_Y, color=Especie))+
  geom_point()+
  ggtitle("Variable Canónica X1 contra Y1")

