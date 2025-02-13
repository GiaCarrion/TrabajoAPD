#==================================================================#
#                   Cargando la base de datos                      #
#==================================================================#

# Lee el archivo
#-------------------------------------------------------------------
data <- read.csv("Customer-Churn-Records.csv")
data <- data.frame(data)

# Análisis inicial
#-------------------------------------------------------------------
str(data)  # Ver estructura y tipos de datos
summary(data)  # Resumen estadístico de cada variable
head(data)  # Primeras filas del dataset

#==================================================================#
#                 Preparación del dataset para gráficos            #
#==================================================================#

# Crear una copia del dataset original para gráficos
#-------------------------------------------------------------------
data_grafico <- data

# Convertir la variable 'Exited' a factor con etiquetas
#-------------------------------------------------------------------
data_grafico$Exited <- factor(data_grafico$Exited, levels = c(0, 1),
                              labels = c("No", "Sí"))

# Crear una nueva columna para los rangos de edad
#-------------------------------------------------------------------
data_grafico$AgeGroup <- cut(data_grafico$Age,
                             breaks = c(0, 20, 30, 40, 50, 60, 70, Inf),
                             labels = c("0-20", "21-30", "31-40", "41-50",
                                        "51-60", "61-70", "70+"),
                             right = FALSE)

# Verificar la distribución por rangos de edad
#-------------------------------------------------------------------
table(data_grafico$AgeGroup)

#==================================================================#
#                        Visualización de Datos                    #
#==================================================================#

# Gráfico de barras para mostrar el abandono general
#-------------------------------------------------------------------
bar_chart_general <- ggplot(data_grafico, aes(x = factor(Exited))) +
  geom_bar(aes(fill = factor(Exited))) +
  geom_text(stat = "count", aes(label = ..count..), 
            position = position_stack(vjust = 1), 
            size = 6, fontface = "bold", color = "black") +  
  scale_fill_manual(values = c("#377eb8", "#ff7f00")) +
  labs(title = "Distribución de Abandono de Clientes",
       x = "Abandono",
       y = "Cantidad") +
  theme_minimal()

ggplotly(bar_chart_general)

# Gráfico de barras para mostrar el abandono por género
#-------------------------------------------------------------------
bar_chart_gender <- ggplot(data_grafico, aes(x = Gender, fill = factor(Exited))) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..), 
            position = position_dodge(width = 0.9),
            vjust = 1.5, size = 6, fontface = "bold", color = "black") +  
  scale_fill_manual(values = c("#377eb8", "#ff7f00")) +
  labs(title = "Distribución de Abandono de Clientes por Género",
       x = "Género",
       y = "Cantidad de Clientes",
       fill = "Abandono") +
  theme_minimal()

ggplotly(bar_chart_gender)

# Gráfico de barras para mostrar el abandono por país
#-------------------------------------------------------------------
bar_chart_country <- ggplot(data_grafico, aes(x = Geography, fill = Exited)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..), 
            position = position_dodge(width = 0.9),
            vjust = 1.5, size = 6, fontface = "bold", color = "black") +
  scale_fill_manual(values = c("#377eb8", "#ff7f00")) +
  labs(title = "Distribución de Abandono de Clientes por País",
       x = "País",
       y = "Cantidad de Clientes",
       fill = "Abandono") +
  theme_minimal()

ggplotly(bar_chart_country)

# Gráfico de barras para mostrar el abandono por rango de edad
#-------------------------------------------------------------------
age_bar_chart <- ggplot(data_grafico, aes(x = AgeGroup, fill = Exited)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..), 
            position = position_dodge(width = 0.9),
            vjust = 1.5, size = 4, fontface = "bold", color = "black") +
  scale_fill_manual(values = c("#377eb8", "#ff7f00")) +
  labs(title = "Distribución de Abandono por Rango de Edad",
       x = "Rango de Edad",
       y = "Cantidad de Clientes",
       fill = "Abandono") +
  theme_minimal()

ggplotly(age_bar_chart)
