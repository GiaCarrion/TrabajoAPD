#==================================================================#
#         Entrenamiento del modelo de Árbol de decisión            #
#==================================================================#

# Función para entrenar y evaluar un modelo de árbol de decisión
#-------------------------------------------------------------------
entrenar_y_evaluar_arbol <- function(train_data, test_data, caso) {
  cat("\nEvaluando el caso: ", caso, "\n")

  # Entrenar el modelo de árbol de decisión
  modelo_arbol <- rpart(Exited ~ ., data = train_data,
                        method = "class", cp = 0, minbucket = 1)

  # Visualizar el árbol (opcional)
  rpart.plot(modelo_arbol, digits = -1, type = 2, extra = 101,
             cex = 0.7, nn = TRUE, main = paste("Árbol -", caso))

  # Hacer predicciones en el conjunto de prueba
  predicciones <- predict(modelo_arbol, newdata = test_data, type = "class")

  # Evaluar el modelo con la matriz de confusión
  cat("\nMatriz de Confusión - ", caso, ":\n")
  print(confusionMatrix(predicciones, as.factor(test_data$Exited)))
}

# Caso 1: Datos originales con Complain
#-------------------------------------------------------------------
entrenar_y_evaluar_arbol(train_original, 
                         test_original, "Datos Originales con Complain")

# Caso 2: Datos originales sin Complain
#-------------------------------------------------------------------
entrenar_y_evaluar_arbol(train_original_no_complain,
                         test_original_no_complain,
                         "Datos Originales sin Complain")

# Caso 3: Datos SMOTE con Complain
#-------------------------------------------------------------------
entrenar_y_evaluar_arbol(train_smote, test_original, "Datos SMOTE con Complain")

# Caso 4: Datos SMOTE sin Complain
#-------------------------------------------------------------------
entrenar_y_evaluar_arbol(train_smote_no_complain,
                         test_original_no_complain,
                         "Datos SMOTE sin Complain")

# Caso 5: Datos Tomek Links con Complain
#-------------------------------------------------------------------
entrenar_y_evaluar_arbol(train_tomek, test_original,
                         "Datos Tomek Links con Complain")

# Caso 6: Datos Tomek Links sin Complain
#-------------------------------------------------------------------
entrenar_y_evaluar_arbol(train_tomek_no_complain,
                         test_original_no_complain, 
                         "Datos Tomek Links sin Complain")