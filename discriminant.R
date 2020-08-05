library(readxl)
library(rattle)
library(MASS)
library(ggplot2)
library(rgl)

rm(list = ls())

#Establecer el directorio en la ubicación del archivo 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Importar base, cambiar nombres y establecer Y como factor
db <- read_xlsx("Discriminant.xlsx")
names(db) <- c("mun", "Y", "pop", "eco", "pov")
db$Y <- factor(db$Y, levels = unique(db$Y))

#Análisis de Discriminante Lineal
db.lda <- lda(Y ~ pop + eco + pov, data = db)

#Valores estimados
db.lda.pred <- predict(db.lda, newdata = db[c(3,4,5)])

#Explorar resultados del analisis discriminante
db.lda

#Aregar a la base original los coeficientes para cada función discriminante y los valores estimados
db <- cbind(db, db.lda.pred$x)
db$Y_hat <- db.lda.pred$class

#Gráficas de ubicación de los grupos en las funciones discrminantes
plot3d( 
  x=db$LD1, y=db$LD3, z=db$LD2, 
  col = db$Y, 
  type = 's', 
  radius = .1,
  xlab="LD1", ylab="LD3", zlab="LD2",
  colkey = TRUE)

plot3d( 
  x=db$LD1, y=db$LD3, z=db$LD2, 
  col = db$Y_hat, 
  type = 's', 
  radius = .1,
  xlab="LD1", ylab="LD3", zlab="LD2",
  colkey = TRUE)

#Otra forma de verlas en 2D
ggplot(db, aes(LD1, LD2, size = LD3)) + geom_point(aes(color = Y), alpha = 0.5) +  scale_size_continuous(range = c(0.1, 10)) + theme_bw()
ggplot(db, aes(LD1, LD2, size = LD3)) + geom_point(aes(color = Y_hat), alpha = 0.5) +  scale_size_continuous(range = c(0.1, 10)) + theme_bw()

#Tabla de confusión
table(db$Y, db.lda.pred$class, dnn = c("Observed Y", "Predicted Y"))

#Poder del modelo
lma <- mean(db.lda.pred$class == db$Y)
