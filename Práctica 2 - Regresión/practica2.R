##### PRÁCTICA 2.- REGRESIÓN LOGÍSTICA - TÉCNICAS DE CLASIFICACIÓN ######

library(readxl)
library(tibble)
library(caTools)

ruta <- 'pobreza.xlsx'

datos <- read_xlsx(ruta)


colnames(datos) <- c('Hogar','AyudaFamilias','RentaMenos16','VacacionesOutdoor',
                     'CapacidadAfrontar', 'TVColor', 'Ordenador','LlegarFinMes', 'RegimenTenencia',
                     'Miembros','RentaTotalAnterior',
                     'HogarPobreza', 'Region', 'EdadMayor', 'HorasSemanales', 'Mayores16','SexoMayor','ActMayor')

View(datos)


#### Tratamiento de variables ######

#Quitamos la columna de Renta pues incurririamos en un problema circular como usted bien ha explicado
datos <- datos[,c(-1, -6, -11, -13, -3, -17)]

datos$HogarPobreza <- as.factor(as.character(datos$HogarPobreza))


datos$VacacionesOutdoor <- as.factor(datos$VacacionesOutdoor)
datos$CapacidadAfrontar <- as.factor(datos$CapacidadAfrontar)
datos$TVColor <- as.factor(datos$TVColor)
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
pR2(modelo01, 4)

str(test.set)
str(training.set)

fitted.results <- predict(modelo01, newdata = test.set, type = 'response')
fitted.results <- ifelse(fitted.results > 0.68, 1,0)

logit.perf <- table(test.set$HogarPobreza, fitted.results, dnn = c("Actual", "Predicted"))
logit.perf

misClasificError <- mean(fitted.results != test.set$HogarPobreza)
print(paste('Accuracy',1-misClasificError))
