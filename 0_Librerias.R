#------------------------------------------------------------------#
#                       Cargando librerías                         #
#------------------------------------------------------------------#
paquetes <- c(
  "pROC", "ggplot2", "reshape", "ggpubr", "reshape2", "dplyr",
  "caret", "rpart", "rpart.plot", "purrr", "precrec",
  "ROSE", "ROCR", "readr", "caret", "plotly", "FactoMineR",
  "factoextra", "ggpubr", "PerformanceAnalytics", "corrplot",
  "rgl", "corrplot", "GGally", "randomForest", "DMwR2",
  "e1071", "knitr", "gridExtra"
)

# Verifica si hay paquetes faltantes
#-------------------------------------------------------------------
lib_ins <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]

# Instala los paquetes faltantes
#-------------------------------------------------------------------
if (length(lib_ins) > 0) {
  for (paquete in lib_ins) {
    cat("Instalando paquete:", paquete, "...\n")
    tryCatch(
      install.packages(paquete),
      error = function(e) {
        cat("Error al instalar", paquete, ":", e$message, "\n")
      }
    )
  }
} else {
  print("Todas las librerías ya están instaladas")
}

# Cargar librerías
#-------------------------------------------------------------------
for (p in paquetes) {
  tryCatch(
    {
      library(p, character.only = TRUE)
      cat("Librería cargada exitosamente:", p, "\n")
    },
    error = function(e) {
      cat("Error al cargar la librería", p, ":", e$message, "\n")
    }
  )
}
