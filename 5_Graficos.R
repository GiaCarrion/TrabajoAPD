#==================================================================#
#         Matriz de confución            #
#==================================================================#

# Función para graficar matriz de confusión con menos espacio
plot_confusion_matrix <- function(conf_matrix, title) {
  cm_df <- as.data.frame(conf_matrix$table)
  colnames(cm_df) <- c("Referencia", "Predicción", "Frecuencia")

  ggplot(cm_df, aes(x = Referencia, y = Predicción, fill = Frecuencia)) +
    geom_tile() +
    geom_text(aes(label = Frecuencia), size = 6) +
    scale_fill_gradient(low = "white", high = "red") +
    theme_minimal() +
    labs(title = title, fill = "Frecuencia") +
    coord_fixed() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      legend.position = "none",  # Ocultar la leyenda para reducir espacio
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")  # Reducir márgenes
    )
}

# Crear matrices de confusión
cm_original_complain <- confusionMatrix(predicciones_original_complain, as.factor(test_original$Exited))
cm_original_no_complain <- confusionMatrix(predicciones_original_no_complain, as.factor(test_original_no_complain$Exited))
cm_smote_complain <- confusionMatrix(predicciones_smote_complain, as.factor(test_original$Exited))
cm_smote_no_complain <- confusionMatrix(predicciones_smote_no_complain, as.factor(test_original_no_complain$Exited))

# Generar gráficos sin leyenda para reducir espacio
plot1 <- plot_confusion_matrix(cm_original_complain, "Original con Complain")
plot2 <- plot_confusion_matrix(cm_original_no_complain, "Original sin Complain")
plot3 <- plot_confusion_matrix(cm_smote_complain, "SMOTE con Complain")
plot4 <- plot_confusion_matrix(cm_smote_no_complain, "SMOTE sin Complain")

# Combinar gráficos con menos espacio entre filas y columnas
grid.arrange(plot1, plot2, plot3, plot4, 
             ncol = 2, nrow = 2, 
             heights = c(1, 1), widths = c(1, 1),  # Ajustar proporciones
             layout_matrix = matrix(c(1, 2, 3, 4), ncol = 2))

#==================================================================#
#         Curva ROC            #
#==================================================================#

# Función para calcular y almacenar curvas ROC
calcular_roc <- function(modelo, test_data, modelo_nombre) {
  # Obtener probabilidades en lugar de clases
  probas <- predict(modelo, test_data, type = "prob")[,2]  # Probabilidad de la clase positiva (1)
  
  # Calcular curva ROC
  roc_curve <- roc(test_data$Exited, probas)
  
  # Devolver datos estructurados
  data.frame(
    FPR = 1 - roc_curve$specificities,  # Falsa tasa de positivos (1 - Especificidad)
    TPR = roc_curve$sensitivities,  # Verdadera tasa de positivos (Sensibilidad)
    Modelo = modelo_nombre  # Nombre del modelo
  )
}

# Calcular curvas ROC para cada modelo
roc_arbol_original <- calcular_roc(res_original_no_complain, test_original_no_complain, "Árbol - Original")
roc_arbol_smote <- calcular_roc(res_smote_no_complain, test_original_no_complain, "Árbol - SMOTE")
#roc_arbol_tomek <- calcular_roc(modelo_arbol_tomek_nc, test_original_no_complain, "Árbol - Tomek")

roc_svm_original <- calcular_roc(modelo_svm_original_nc, test_original_no_complain, "SVM - Original")
roc_svm_smote <- calcular_roc(modelo_svm_smote_nc, test_original_no_complain, "SVM - SMOTE")
#roc_svm_tomek <- calcular_roc(modelo_svm_tomek_nc, test_original_no_complain, "SVM - Tomek")

# Unir todos los datos en un solo dataframe
roc_data <- rbind(roc_arbol_original, roc_arbol_smote, roc_arbol_tomek,
                  roc_svm_original, roc_svm_smote, roc_svm_tomek)

# Crear la curva ROC con colores diferenciados

# Crear la curva ROC con colores diferenciados
ggplot(roc_data, aes(x = FPR, y = TPR, color = Modelo)) +
  geom_line(size = 1.2) +  # Líneas gruesas para mayor visibilidad
  geom_abline(linetype = "dashed", color = "grey") +  # Línea diagonal referencia
  labs(title = "Curva ROC - Comparación Modelos",
       x = "1 - Especificidad (FPR)",
       y = "Sensibilidad (TPR)",
       color = "Modelos") +
  theme_minimal() +
  scale_color_manual(values = c("Árbol - Original" = "blue",
                                "Árbol - SMOTE" = "green",
                                "SVM - Original" = "purple",
                                "SVM - SMOTE" = "orange"))

#ggplot(roc_data, aes(x = FPR, y = TPR, color = Modelo)) +
#  geom_line(size = 1.2) +  # Líneas gruesas para mayor visibilidad
#  geom_abline(linetype = "dashed", color = "grey") +  # Línea diagonal referencia
#  labs(title = "Curva ROC - Comparación Modelos",
#       x = "1 - Especificidad (FPR)",
#       y = "Sensibilidad (TPR)",
#       color = "Modelos") +
#  theme_minimal() +
#  scale_color_manual(values = c("Árbol - Original" = "blue",
#                                "Árbol - SMOTE" = "green",
#                                "Árbol - Tomek" = "red",
#                                "SVM - Original" = "purple",
#                                "SVM - SMOTE" = "orange",
#                                "SVM - Tomek" = "brown"))
