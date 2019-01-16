
library(readxl)
library(tibble)
library(caTools)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
ruta <- 'datos.xlsx'

datos <- read_xlsx(ruta)


colnames(datos) <- c('Hogar','AyudaFamilias','RentaMenos16','VacacionesOutdoor',
                     'CapacidadAfrontar', 'TVColor', 'Ordenador','LlegarFinMes', 'RegimenTenencia',
                     'Miembros','RentaTotalAnterior',
                     'HogarPobreza', 'Region', 'EdadMayor', 'HorasSemanales', 'Mayores16','SexoMayor','ActMayor')


#### Tratamiento de variables ######

#Quitamos la columna de Renta pues incurririamos en un problema circular como usted bien ha explicado
datos <- datos[,c(-1, -6, -11, -13, -3, -17)]
str(datos)

datos$HogarPobreza <- as.factor(as.character(datos$HogarPobreza))


datos$VacacionesOutdoor <- as.factor(datos$VacacionesOutdoor)
datos$CapacidadAfrontar <- as.factor(datos$CapacidadAfrontar)
datos$Ordenador <- as.factor(datos$Ordenador)
datos$LlegarFinMes <- as.factor(datos$LlegarFinMes)
datos$RegimenTenencia <- as.factor(datos$RegimenTenencia)
datos$Miembros <- as.numeric(datos$Miembros)
datos$SexoMayor <- as.factor(datos$SexoMayor)
datos$ActMayor <- as.factor(datos$ActMayor)

str(datos)
datos <- as.data.frame(datos)
#### Modelo de Regresion Lineal #####
str(datos)
size <- floor(nrow(datos) * 0.6)

set.seed(123)
train_ind <- sample(seq_len(nrow(datos)), size = size)
training.set <- datos[train_ind, ]
test.set <- datos[-train_ind, ]

modelo01 <- glm(HogarPobreza ~ ., family = 'binomial'(link = 'logit'), data = training.set)
summary(modelo01)
exp(coef(modelo01))

anova(modelo01, test = "Chisq")

#Now the results are consistent, and no longer dependent 
#on their level of aggregation (tabulation). I have notified the maintainer of the pscl package. Maybe he has some interest.
library(pscl)
pR2(modelo01, 4)

str(test.set)
str(training.set)

fitted.results <- predict(modelo01, newdata = test.set, type = 'response')
fitted.results <- ifelse(fitted.results > 0.68, 1,0)

logit.perf <- table(test.set$HogarPobreza, fitted.results, dnn = c("Actual", "Predicted"))
logit.perf

misClasificError <- mean(fitted.results != test.set$HogarPobreza)
print(paste('Accuracy',1-misClasificError))

#ARBOLES DE Decision
#datos recoge todoe l data set que comprende 477 observaciones y 12 variables
set.seed(123)
train <- sample(nrow(datos), 0.6*nrow(datos)) #Para el train nos quedaremos con el 70% de la sobservaciones
datos.train<- datos[train,]# 286 observaciones y 12 variables

#Lo anterior muestra el esquema de nuestro árbol de clasificación. Cada inciso nos indica un nodo y 
#la regla de clasificación que le corresponde. Siguiendo estos nodos, 
#podemos llegar a las hojas del árbol, que corresponde a la clasificación de nuestros datos.

datos.test <- datos[-train,]# 191 observaciones y 12 variables
datos$HogarPobreza<- factor(datos$HogarPobreza, levels=c(0,1), labels=c("No en riesgo de pobreza", "Sí en riesgo de pobreza"))
table(datos.train$HogarPobreza)# 210 no están en riesgo de pobreza y 123 que sí lo están
table(datos.test$HogarPobreza)# 81 no están en riesgo de pobreza y 63 sí que lo están

library(rpart)
arbol <- rpart(HogarPobreza ~ ., data=datos.train, method="class",parms=list(split="information"))
plot(arbol, uniform = TRUE, branch=0.4, compress=FALSE)
text(arbol, use.n = TRUE, cex = 0.75, all=TRUE)
print(arbol)
##Lo anterior muestra el esquema de nuestro árbol de clasificación. Cada inciso nos indica un nodo y la regla de clasificación que le corresponde. Siguiendo estos nodos, podemos llegar a las hojas del árbol, que corresponde a la clasificación de nuestros datos.
#Todo lo anterior resulta mucho más claro si lo visualizamos, así que creamos una gráfica usando nuestro modelo con la función  rpart.plot() de rpart.plot.

plot(as.party(arbol))


summary(arbol)
rpart.plot(arbol)
arbol$cptable #esta es la tabla de complejidad paramétrica en donde tendremos los erroresde validación cruzada, tendremos que ver cuál de ellos es el que 
#minimiza el error de cross-validation
plotcp(arbol)
printcp(arbol)
plot(arbol)
text(arbol, use.n = TRUE, cex = 0.75, all=TRUE)
arbol$cptable[which.min(arbol$cptable[,"xerror"]),"CP"]# el error que minimiza la validación cruzada es cp=0.01
arbol.podado = prune(arbol, cp = 0.01)
arbol.podado

plot(arbol.podado, uniform = TRUE, branch=0.4, compress=FALSE)
text(arbol.podado, use.n = TRUE, cex = 0.75, all=TRUE)

#En estos gráficos, cada uno de los rectángulos representa un nodo de nuestro árbol, con su regla de clasificación.
#Cada nodo está coloreado de acuerdo a la categoría mayoritaria entre los datos que agrupa. Esta es la categoría que ha predicho el modelo para ese grupo.
#Dentro del rectángulo de cada nodo se nos muestra qué proporción de casos pertenecen a cada categoría y la proporción del total de datos que han sido
#agrupados allí. Por ejemplo, el rectángulo en el extremo inferior izquierdo de la gráfica tiene 94% de casos en el tipo 1, y 4% en los tipos 2 y 3, que representan 39% de todos los datos.
#Estas proporciones nos dan una idea de la precisión de nuestro modelo al hacer predicciones. De este modo, las reglas que conducen al rectángulo que acabamos
#de mencionar nos dan un 92% de clasificaciones correctas. En contraste, el tercer rectángulo, de izquierda a derecha, de color gris, tuvo sólo 62% de clasificaciones correctas.
#Además, podemos sentirnos contentos de que dos de las hojas de nuestro árbol de clasificación han logrado un 100% de clasificaciones correctas, para los vinos de tipo 2 y 3.
#Pero, por supuesto, necesitamos ser más sistemáticos para indagar qué tan bien hace predicciones nuestro modelo.
#Usamos la función precict() con nuestro set de prueba para generar un vector con los valores predichos por el modelo que hemos entrenado, especificamos el parámetro type = "class".

library(rpart.plot)

prp(arbol.podado, type = 2, extra = 104,fallen.leaves = TRUE, main="Decision Tree")
arbol.pred <- predict(arbol.podado, datos.test, type="class")

arbol.perf <- table(datos.test$HogarPobreza, arbol.pred,dnn=c("Actual", "Predicted"))
kable(arbol.perf)
rpart.plot(arbol.podado)
rpart.plot(arbol.podado, box.palette="GnBu",branch.lty=3, shadow.col="gray", nn=TRUE,main="Árbol podado de clasificación usando rpart.plot")


library(partykit)

plot(as.party(arbol.podado))

#El paequete party proporciona árboles de regresión no paramétrixa para respuestas nominales, ordinales, numéricas, censuradas o multivariantes
#El crecmimiento del árbol se basa en reglas estadísticas de parada, de forma que no se hace necesaria la poda


arbol.party1 = ctree(HogarPobreza ~ ., datos.train)
plot(arbol.party1, main="Árbol de inferencia condicional para los Hogares en riesgo de pobreza")
ctree.pred <- predict(arbol.party1, datos.test, type="response")
ctree.perf <- table(datos.test$HogarPobreza, ctree.pred, dnn=c("Actual", "Predicted"))
ctree.perf

ctree.pred <- predict(fit.ctree, gender.validate, type="response")
ctree.perf <- table(gender.validate$Gender, ctree.pred,
                    dnn=c("Actual", "Predicted"))
ctree.perf
plot(ctree.perf, main="Conditional Inference Tree")





















###########################################################
#########Prueba de arbol 2, pero esta vez mas sencillo ##
##########################################################3

set.seed(4578)
train <- sample(nrow(datos), 0.7*nrow(datos)) #Para el train nos quedaremos con el 70% de la sobservaciones
datos.train<- datos[train,]# 286 observaciones y 12 variables

#Lo anterior muestra el esquema de nuestro árbol de clasificación. Cada inciso nos indica un nodo y 
#la regla de clasificación que le corresponde. Siguiendo estos nodos, 
#podemos llegar a las hojas del árbol, que corresponde a la clasificación de nuestros datos.

datos.test <- datos[-train,]# 191 observaciones y 12 variables
datos$HogarPobreza<- factor(datos$HogarPobreza, levels=c(0,1), labels=c("No en riesgo de pobreza", "Sí en riesgo de pobreza"))
table(datos.train$HogarPobreza)# 210 no están en riesgo de pobreza y 123 que sí lo están
table(datos.test$HogarPobreza)# 81 no están en riesgo de pobreza y 63 sí que lo están

library(rpart)
arbol <- rpart(HogarPobreza ~ ., data=datos.train, method="class",parms=list(split="information"))
plot(arbol, uniform = TRUE, branch=0.4, compress=FALSE)
text(arbol, use.n = TRUE, cex = 0.75, all=TRUE)
print(arbol)
##Lo anterior muestra el esquema de nuestro árbol de clasificación. Cada inciso nos indica un nodo y la regla de clasificación que le corresponde. Siguiendo estos nodos, podemos llegar a las hojas del árbol, que corresponde a la clasificación de nuestros datos.
#Todo lo anterior resulta mucho más claro si lo visualizamos, así que creamos una gráfica usando nuestro modelo con la función  rpart.plot() de rpart.plot.


summary(arbol)
rpart.plot(arbol)
arbol$cptable #esta es la tabla de complejidad paramétrica en donde tendremos los erroresde validación cruzada, tendremos que ver cuál de ellos es el que 
#minimiza el error de cross-validation
plotcp(arbol)
printcp(arbol)
plot(arbol)
text(arbol, use.n = TRUE, cex = 0.75, all=TRUE)
arbol$cptable[which.min(arbol$cptable[,"xerror"]),"CP"]# el error que minimiza la validación cruzada es cp=0.01
arbol.podado = prune(arbol, cp = 0.01)
arbol.podado

plot(arbol.podado, uniform = TRUE, branch=0.4, compress=FALSE)
text(arbol.podado, use.n = TRUE, cex = 0.75, all=TRUE)

#En estos gráficos, cada uno de los rectángulos representa un nodo de nuestro árbol, con su regla de clasificación.
#Cada nodo está coloreado de acuerdo a la categoría mayoritaria entre los datos que agrupa. Esta es la categoría que ha predicho el modelo para ese grupo.
#Dentro del rectángulo de cada nodo se nos muestra qué proporción de casos pertenecen a cada categoría y la proporción del total de datos que han sido
#agrupados allí. Por ejemplo, el rectángulo en el extremo inferior izquierdo de la gráfica tiene 94% de casos en el tipo 1, y 4% en los tipos 2 y 3, que representan 39% de todos los datos.
#Estas proporciones nos dan una idea de la precisión de nuestro modelo al hacer predicciones. De este modo, las reglas que conducen al rectángulo que acabamos
#de mencionar nos dan un 92% de clasificaciones correctas. En contraste, el tercer rectángulo, de izquierda a derecha, de color gris, tuvo sólo 62% de clasificaciones correctas.
#Además, podemos sentirnos contentos de que dos de las hojas de nuestro árbol de clasificación han logrado un 100% de clasificaciones correctas, para los vinos de tipo 2 y 3.
#Pero, por supuesto, necesitamos ser más sistemáticos para indagar qué tan bien hace predicciones nuestro modelo.
#Usamos la función precict() con nuestro set de prueba para generar un vector con los valores predichos por el modelo que hemos entrenado, especificamos el parámetro type = "class".

library(rpart.plot)

prp(arbol.podado, type = 2, extra = 104,fallen.leaves = TRUE, main="Decision Tree")
arbol.pred <- predict(arbol.podado, datos.test, type="class")

arbol.perf <- table(datos.test$HogarPobreza, arbol.pred,dnn=c("Actual", "Predicted"))
arbol.perf
rpart.plot(arbol.podado)
rpart.plot(arbol.podado, box.palette="GnBu",branch.lty=3, shadow.col="gray", nn=TRUE,main="Árbol de clasificación usando rpart.plot")


library(partykit)

plot(as.party(arbol.podado))

#El paequete party proporciona árboles de regresión no paramétrixa para respuestas nominales, ordinales, numéricas, censuradas o multivariantes
#El crecmimiento del árbol se basa en reglas estadísticas de parada, de forma que no se hace necesaria la poda


arbol.party1 = ctree(HogarPobreza ~ ., datos.train)
plot(arbol.party1, main="Árbol de inferencia condicional para los Hogares en riesgo de pobreza")
ctree.pred <- predict(arbol.party1, datos.test, type="response")
ctree.perf <- table(datos.test$HogarPobreza, ctree.pred, dnn=c("Actual", "Predicted"))
kable(ctree.perf)

