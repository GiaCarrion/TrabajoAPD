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

  # Retornar las predicciones y el modelo
  return(list(modelo = modelo_arbol, predicciones = predicciones))
}

#==================================================================#
#         Guardar modelos y predicciones en variables              #
#==================================================================#

# Caso 1: Datos originales con Complain
res_original_complain <- entrenar_y_evaluar_arbol(train_original, test_original, "Datos Originales con Complain")

# Caso 2: Datos originales sin Complain
res_original_no_complain <- entrenar_y_evaluar_arbol(train_original_no_complain, test_original_no_complain, "Datos Originales sin Complain")

# Caso 3: Datos SMOTE con Complain
res_smote_complain <- entrenar_y_evaluar_arbol(train_smote, test_original, "Datos SMOTE con Complain")

# Caso 4: Datos SMOTE sin Complain
res_smote_no_complain <- entrenar_y_evaluar_arbol(train_smote_no_complain, test_original_no_complain, "Datos SMOTE sin Complain")

# Caso 5: Datos Tomek Links con Complain
res_tomek_complain <- entrenar_y_evaluar_arbol(train_tomek, test_original, "Datos Tomek Links con Complain")

# Caso 6: Datos Tomek Links sin Complain
res_tomek_no_complain <- entrenar_y_evaluar_arbol(train_tomek_no_complain, test_original_no_complain, "Datos Tomek Links sin Complain")

#==================================================================#
#         Extraer predicciones para uso en gráficos                #
#==================================================================#

predicciones_original_complain <- res_original_complain$predicciones
predicciones_original_no_complain <- res_original_no_complain$predicciones

predicciones_smote_complain <- res_smote_complain$predicciones
predicciones_smote_no_complain <- res_smote_no_complain$predicciones

predicciones_tomek_complain <- res_tomek_complain$predicciones
predicciones_tomek_no_complain <- res_tomek_no_complain$predicciones


# Función para graficar matriz de confusión
plot_confusion_matrix <- function(conf_matrix, title) {
  cm_df <- as.data.frame(conf_matrix$table)
  colnames(cm_df) <- c("Referencia", "Predicción", "Frecuencia")

  ggplot(cm_df, aes(x = Referencia, y = Predicción, fill = Frecuencia)) +
    geom_tile() +
    geom_text(aes(label = Frecuencia), size = 8) +
    scale_fill_gradient(low = "#fae6e6", high = "red") +
    theme_minimal() +
    labs(title = title, fill = "Frecuencia") +
    coord_fixed() +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      axis.title.x = element_text(size = 18, face = "bold"),
      axis.title.y = element_text(size = 18, face = "bold"),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14)
    )
}

# Crear matrices de confusión con las predicciones
cm_original_complain <- confusionMatrix(predicciones_original_complain, as.factor(test_original$Exited))
cm_original_no_complain <- confusionMatrix(predicciones_original_no_complain, as.factor(test_original_no_complain$Exited))

cm_smote_complain <- confusionMatrix(predicciones_smote_complain, as.factor(test_original$Exited))
cm_smote_no_complain <- confusionMatrix(predicciones_smote_no_complain, as.factor(test_original_no_complain$Exited))

cm_tomek_complain <- confusionMatrix(predicciones_tomek_complain, as.factor(test_original$Exited))
cm_tomek_no_complain <- confusionMatrix(predicciones_tomek_no_complain, as.factor(test_original_no_complain$Exited))

# Graficar cada matriz de confusión
plot_confusion_matrix(cm_original_complain, "Matriz de Confusión - Datos Originales con Complain")
plot_confusion_matrix(cm_original_no_complain, "Matriz de Confusión - Datos Originales sin Complain")
plot_confusion_matrix(cm_smote_complain, "Matriz de Confusión - Datos SMOTE con Complain")
plot_confusion_matrix(cm_smote_no_complain, "Matriz de Confusión - Datos SMOTE sin Complain")
plot_confusion_matrix(cm_tomek_complain, "Matriz de Confusión - Datos Tomek Links con Complain")
plot_confusion_matrix(cm_tomek_no_complain, "Matriz de Confusión - Datos Tomek Links sin Complain")

# Función para graficar curvas ROC
plot_roc_curve <- function(modelo, test_data, title) {
  probas <- predict(modelo, test_data, type = "prob")[,2]  # Obtener probabilidades de la clase positiva
  roc_curve <- roc(test_data$Exited, probas)

  ggroc(roc_curve, color = "blue", size = 1.5) +
    theme_minimal() +
    labs(title = title, x = "1 - Especificidad", y = "Sensibilidad") +
    geom_abline(linetype = "dashed", color = "red")  # Línea diagonal de referencia
}

# Generar Curvas ROC para cada caso
plot_roc_curve(res_original_complain$modelo, test_original, "Curva ROC - Datos Originales con Complain")
plot_roc_curve(res_original_no_complain$modelo, test_original_no_complain, "Curva ROC - Datos Originales sin Complain")
plot_roc_curve(res_smote_complain$modelo, test_original, "Curva ROC - Datos SMOTE con Complain")
plot_roc_curve(res_smote_no_complain$modelo, test_original_no_complain, "Curva ROC - Datos SMOTE sin Complain")
plot_roc_curve(res_tomek_complain$modelo, test_original, "Curva ROC - Datos Tomek Links con Complain")
plot_roc_curve(res_tomek_no_complain$modelo, test_original_no_complain, "Curva ROC - Datos Tomek Links sin Complain")
