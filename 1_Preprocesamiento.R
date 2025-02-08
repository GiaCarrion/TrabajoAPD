# Lee el archivo
data <- read.csv("Customer-Churn-Records.csv")
data <- data.frame(data)

# Muestra las primeras filas del archivo
head(data)

summary(data)
str(data)