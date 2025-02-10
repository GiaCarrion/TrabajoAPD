#==================================================================#
#                    Preprocesamiento de datos                     #
#==================================================================#

# Eliminar columnas irrelevantes
#-------------------------------------------------------------------
data1 <- data.frame(data[, -c(1, 2, 3)], row.names = data[, 2])
head(data1, 10)

# Verificar si hay valores faltantes
#-------------------------------------------------------------------
sum(is.na(data1))
colSums(is.na(data1))

#==================================================================#
#              Transformación de variables categóricas             #
#==================================================================#

# Asignar niveles a variables categóricas ordenadas
#-------------------------------------------------------------------
unique(data1$Card.Type)
data1$Card.Type <- ordered(data$Card.Type, levels = c("SILVER", "GOLD",
                                                      "PLATINUM", "DIAMOND"))
levels(data1$Card.Type)
summary(data1$Card.Type)

# Verificación de otras variables categóricas
#-------------------------------------------------------------------
str(data1)
data1$Gender <- as.factor(data1$Gender)
data1$Geography <- as.factor(data1$Geography)

#==================================================================#
#              Análisis visual de valores atípicos                 #
#==================================================================#

# Visualizar valores atípicos en variables númericas
#-------------------------------------------------------------------
boxplot(data1$CreditScore, main = "Boxplot de CreditScore", col = "purple")
hist(data1$CreditScore, main = "Histograma de CreditScore", col = "purple")

boxplot(data1$Age, main = "Boxplot de edad", col = "lightgreen")
hist(data1$Age, main = "Histograma de edad", col = "lightgreen")

boxplot(data1$Tenure, main = "Boxplot de permanencia", col = "lightblue")
hist(data1$Tenure, main = "Histograma de permanencia", col = "lightblue")

boxplot(data1$Balance, main = "Boxplot de balance", col = "pink")
hist(data1$Balance, main = "Histograma de balance", col = "pink")

boxplot(data1$EstimatedSalary, main = "Boxplot del salario", col = "yellow")
hist(data1$EstimatedSalary, main = "Histograma del salario", col = "yellow")

boxplot(data1$Point.Earned, main = "Boxplot de puntaje", col = "orange")
hist(data1$Point.Earned, main = "Histograma de puntaje", col = "orange")

#==================================================================#
#                    Transformación de variables                   #
#==================================================================#

# Corregir valores atípicos mediante transformaciones
#-------------------------------------------------------------------
data2 <- data1

data2$CreditScore <- (data2$CreditScore)^2 # Transformación cuadrática
data2$Age <- log(data2$Age) # Transformación logarítmica

#==================================================================#
#                  Normalización de variables numéricas            #
#==================================================================#

# Escalar variables numéricas
#-------------------------------------------------------------------
data3 <- data2

variables_numericas <- c("CreditScore", "Age", "Tenure", "Balance",
                         "EstimatedSalary", "NumOfProducts",
                         "Satisfaction.Score", "Point.Earned")
data3[variables_numericas] <- scale(data3[variables_numericas])

# Ver resumen de las variables escaladas
#-------------------------------------------------------------------
summary(data3[variables_numericas])

#==================================================================#
#                   Análisis de correlación                        #
#==================================================================#

# Calcular la matriz de correlación
#-------------------------------------------------------------------
correlation_matrix <- cor(data3[variables_numericas])

# Visualizar la matriz de correlación
#-------------------------------------------------------------------
print(correlation_matrix)

#==================================================================#
#                     Codificación de variables                    #
#==================================================================#

# Aplicar One-Hot Encoding a las variables categóricas
#-------------------------------------------------------------------
category_encoder <- dummyVars("~ .", data = data3)
data4 <- data.frame(predict(category_encoder, newdata = data3))

# Resumen final del dataset procesado
#-------------------------------------------------------------------
summary(data4)

#==================================================================#
#                  División de datos en train y test               #
#==================================================================#

set.seed(100)
index <- createDataPartition(data4$Exited, p = 0.7, list = FALSE)

train_data <- data4[index, ]
dim(train_data)

test_data <- data4[-index, ]
dim(test_data)

# Verificar la distribución de la variable objetivo
#-------------------------------------------------------------------
print(table(trainData$Exited))
