#==================================================================#
#         Entrenamiento del modelo de SVM                          #
#==================================================================#

# Cargar librerías necesarias
library(e1071)      # Para el modelo SVM
library(caret)      # Para evaluación del modelo
library(dplyr)      # Para manipulación de datos

# Función para entrenar y evaluar un modelo SVM con ajuste de hiperparámetros
#-------------------------------------------------------------------
entrenar_y_evaluar_svm <- function(train_data, test_data, caso) {
  cat("\nEvaluando el caso: ", caso, "\n")

  # Verificar la distribución de clases en el conjunto de entrenamiento
  cat("\nDistribución de clases en entrenamiento:\n")
  print(table(train_data$Exited))

  # Convertir la variable objetivo a factor
  train_data$Exited <- as.factor(train_data$Exited)
  test_data$Exited <- as.factor(test_data$Exited)

  # Verificar que ambas clases están en el conjunto de entrenamiento
  if (length(unique(train_data$Exited)) < 2) {
    cat("\nError: Solo hay una clase en los datos de entrenamiento. No se puede entrenar SVM.\n")
    return(NULL)
  }

  # Separar variables predictoras y variable objetivo
  x <- train_data[, -which(names(train_data) == "Exited")]
  y <- train_data$Exited

  # Optimización de hiperparámetros usando `tune()`
  tune_result <- tune(svm, Exited ~ ., data = train_data, kernel = "radial",
                      ranges = list(cost = c(0.1, 1, 10, 100),
                                    gamma = c(0.01, 0.1, 1)))
  
  # Mejor modelo encontrado
  mejor_modelo <- tune_result$best.model

  # Hacer predicciones en el conjunto de prueba
  predicciones <- predict(mejor_modelo, newdata = test_data)

  # Convertir predicciones y etiquetas reales en factores con los mismos niveles
  niveles_clases <- levels(test_data$Exited)
  predicciones <- factor(predicciones, levels = niveles_clases)
  test_data$Exited <- factor(test_data$Exited, levels = niveles_clases)

  # Evaluar el modelo con la matriz de confusión
  cat("\nMatriz de Confusión - ", caso, ":\n")
  print(confusionMatrix(predicciones, test_data$Exited))

  # Retornar el mejor modelo entrenado para análisis adicional
  return(mejor_modelo)
}

#-------------------------------------------------------------------
#          Entrenamiento con diferentes conjuntos de datos
#-------------------------------------------------------------------

# Datos originales
modelo_svm_original <- entrenar_y_evaluar_svm(train_original, test_original, "Datos Originales con Complain")
modelo_svm_original_nc <- entrenar_y_evaluar_svm(train_original_no_complain, test_original_no_complain, "Datos Originales sin Complain")

# Datos balanceados con SMOTE
modelo_svm_smote <- entrenar_y_evaluar_svm(train_smote, test_original, "Datos SMOTE con Complain")
modelo_svm_smote_nc <- entrenar_y_evaluar_svm(train_smote_no_complain, test_original_no_complain, "Datos SMOTE sin Complain")

# Datos balanceados con Tomek Links
modelo_svm_tomek <- entrenar_y_evaluar_svm(train_tomek, test_original, "Datos Tomek Links con Complain")
modelo_svm_tomek_nc <- entrenar_y_evaluar_svm(train_tomek_no_complain, test_original_no_complain, "Datos Tomek Links sin Complain")
