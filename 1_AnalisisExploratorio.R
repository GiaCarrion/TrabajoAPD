# Librerias
library(caret)

# Lee el archivo
data <- read.csv("Customer-Churn-Records.csv")
data <- data.frame(data)

# Análisis inicial
str(data)  # Ver estructura y tipos de datos
summary(data)  # Resumen estadístico de cada variable
head(data)  # Primeras filas del dataset

# Eliminar columnas irrelevantes
data1 <- data.frame(data[, -c(1, 2, 3)], row.names = data[, 2])
head(data1, 10)

# Verificar si hay valores faltantes
sum(is.na(data1))
colSums(is.na(data1))

# Asignar niveles a variables categóricas ordenadas
unique(data1$Card.Type)
data1$Card.Type <- ordered(data$Card.Type, levels = c("SILVER", "GOLD",
                                                      "PLATINUM", "DIAMOND"))
levels(data1$Card.Type)
summary(data1$Card.Type)

# Verificación de otras variables categóricas
str(data1)
data1$Gender <- as.factor(data1$Gender)
data1$Geography <- as.factor(data1$Geography)

# Escalar variables numéricas en el dataset codificado y Corregir valores atípicos
data2 <- data1

numerical_features <- c("CreditScore", "Age", "Balance", "EstimatedSalary")
data2[numerical_features] <- scale(data2[numerical_features])

summary(data2[numerical_features])

data3 <- data2

boxplot(data1$CreditScore, main = "Boxplot de CreditScore")
hist(data2$CreditScore, main = "Histograma de CreditScore")

boxplot(data1$Age, main = "Boxplot de Edad", col = "lightgreen")
hist(data2$Age, main = "Histograma de CreditScore")

boxplot(data1$Balance, main = "Boxplot de Balance", col = "#e992cf")
hist(data2$Balance, main = "Histograma de CreditScore")

boxplot(data1$EstimatedSalary, main = "Boxplot de Salario", col = "#f1eb8e")
hist(data2$EstimatedSalary, main = "Histograma de CreditScore")

# One-Hot Encoding
category_encoder <- dummyVars("~ .", data = data1)
data2 <- data.frame(predict(category_encoder, newdata = data1))
