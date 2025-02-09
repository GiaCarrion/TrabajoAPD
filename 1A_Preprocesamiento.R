# Eliminar columnas irrelevantes
data1 <- data.frame(data[, -c(1, 2, 3)], row.names = data[, 2])
head(data1, 10)

# Verificar si hay valores faltantes
sum(is.na(data1))
colSums(is.na(data1))

# Asignar niveles a variables categóricas ordenadas
data1$Card.Type <- ordered(data$Card.Type, levels = c("SILVER", "GOLD",
                                                      "PLATINUM", "DIAMOND"))

levels(data1$Card.Type)
summary(data1$Card.Type)

# Dividir el dataset en entrenamiento y prueba
set.seed(100)
indice <- createDataPartition(data1$Exited, p = 0.7, list = FALSE)

data_train <- data1[indice, ]
dim(data_train)

data_test  <- data1[-indice, ]
dim(data_test)