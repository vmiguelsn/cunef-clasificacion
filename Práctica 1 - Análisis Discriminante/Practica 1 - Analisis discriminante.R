###TAREA 1 ANÁLISIS DISCRIMINANTE-IRIS DATASET###

library(openxlsx)
library(dplyr)
library(reader)
library(MASS)
library(car)

iris <- read.xlsx("iris.xlsx", rowNames = T)
View(iris)

summary(iris) #Las observaciones son factores o string por lo que debemos pasarlas a numeric y para ello es mejor abrir el dataset como xlsx
str(iris)
iris$sepalo.longitud


#Vamos a coger la columna id como primera identificador y quitamos una variables haciendola id de filas y 
#Eliminamos la primera variable, pues es el orden de los individuos de la muestra

scatterplotMatrix(iris[1:4]) #Hace una matriz cuadrada donde la diagonal ppal calcula la función de densidad estimada de las variables explicativas o predictoras


library(knitr)
kable(head(iris, n = 3))

library(ggpubr)

plot1 <- ggplot(data = iris, aes(x = sepalo.longitud)) +
  geom_density(aes(colour = especies)) + theme_bw()
plot2 <- ggplot(data = iris, aes(x = sepalo.grosor)) +
  geom_density(aes(colour = especies)) + theme_bw()
plot3 <- ggplot(data = iris, aes(x = petalo.longitud)) +
  geom_density(aes(colour = especies)) + theme_bw()
plot4 <- ggplot(data = iris, aes(x = petalo.grosor)) +
  geom_density(aes(colour = especies)) + theme_bw()

# la función grid.arrange del paquete grid.extra permite ordenar
# graficos de ggplot2

ggarrange(plot1, plot2, plot3, plot4, common.legend = TRUE, legend = "bottom")


pairs(x = iris[, -5], col = c("firebrick", "green3", "blue")[iris$especies],
      pch = 20)
#Como vemos, Las variables Petal.Lenght y Petal.Width son las dos variables 
#con más potencial para poder separar entre clases. 
#Sin embargo, están altamente correlacionadas, por lo que la información que aportan es en gran medida redundante. 


#representación mediante histograma de cada variable para cada especie 
par(mfcol = c(3, 4))
for (k in 1:4) {
  j0 <- names(iris)[k]
  x0 <- seq(min(iris[, k]), max(iris[, k]), le = 50)
  for (i in 1:3) {
    i0 <- levels(iris$Species)[i]
    x <- iris[iris$Species == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste("especie", i0),
         xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}

#Como no se dispone de información sobre la abundancia relativa de las especies a nivel poblacional, 
#se considera como probabilidad previa de cada especie el número de observaciones de la especie entre el número de observaciones totales.
#0.3333
50/150

#Contraste de normalidad Shapiro-Wilk para cada variable en cada especie
#Λ de Wilks 
#Los vectores de medias de las variables discriminantes deben ser diferentes en ambas poblaciones. 
#Si fueran iguales, se pondría en duda la capacidad discriminante de las variables clasificadoras 
library(reshape2)
library(knitr)
library(dplyr)
datos_organizados <- melt(iris, value.name = "valor")
kable(datos_organizados %>% group_by(especies, variable) %>% summarise(p_value_Shapiro.test = round(shapiro.test(valor)$p.value,5)))

#Normalidad multivariante
library(MVN)
royston_test <- mvn(data = iris[,-5], mvnTest = "royston", multivariatePlot = "qq")

#Cálculo de la función discriminante
## LDA: proporciones de clase del conjunto de entrenamiento empleadas como probabilidades a priori

zlin=lda(especies~.,iris)
# Matriz de Confusión: tabla de contingencia entre la asignación real y la asignación por LDA, fusiona la realidad con la predicción, 
#por ejemplo asignar individuos de un grupo a otro

modelo_lda <- lda(especies ~ sepalo.grosor + sepalo.longitud + petalo.longitud +
                    petalo.grosor, data = iris)
modelo_lda


#Evaluación de los errores de clasificación
#PREDICCION

predicciones <- predict(object = modelo_lda, newdata = iris[, -5])
tabla <- table(iris$especies, predicciones$class, dnn = c("Clase real", "Clase predicha"))
tabla
training_error <- mean(iris$especies != predicciones$class) * 100
paste("trainig_error =", training_error, "%")

plot(modelo_lda, col = as.integer(iris$especies))

plot(modelo_lda, dimen = 1, type = "b")

#Solo 3 de las 150 predicciones que ha realizado el modelo han sido erróneas. 
#El trainig error es muy bajo (2%), lo que apunta a que el modelo es bueno. 
#Sin embargo, para validarlo es necesario un nuevo set de datos con el que calcular el test error o recurrir a validación cruzada. 

z$counts # Este es el número total de observaciones por grupo

#Estos son el número total de individuos asignados a cada especie en la muestra de adiestramiento.
z$svd 

#Poder discriminante de cada función
z$N #The number of observations used.