# Script de R: Análisis Predictivo con Random Forest (Gasto por Visitante)

# Instalar si es necesario: install.packages("randomForest")
library(randomForest)
library(dplyr)
library(ggplot2)

# La tabla de entrada desde Power BI es la tabla integrada 'dataset' 
# (Histórico_Visitas + Playas + CodigoPostal)

# 1. Preparación de los datos: Convertir variables a Factor y seleccionar.
dataset_clean <- dataset %>%
  select(Gasto_Promedio_€, Num_Visitantes, Temp_C, Tipo_Arena_Sim) %>%
  na.omit()

# Convertir la variable categórica a factor
dataset_clean$Tipo_Arena_Sim <- as.factor(dataset_clean$Tipo_Arena_Sim)

# 2. División Entrenamiento (80%) y Prueba (20%)
set.seed(123)
indices <- sample(nrow(dataset_clean), size = floor(0.8 * nrow(dataset_clean)))
train_data <- dataset_clean[indices, ]
test_data <- dataset_clean[-indices, ]

# 3. Entrenar el modelo Random Forest
# Predecir Gasto_Promedio_€ usando visitantes, temperatura y tipo de arena.
rf_model <- randomForest(Gasto_Promedio_€ ~ Num_Visitantes + Temp_C + Tipo_Arena_Sim,
                         data = train_data,
                         ntree = 100) # ntree reducido para un entorno de práctica

# 4. Realizar Predicciones
test_data$Prediccion_Gasto <- predict(rf_model, newdata = test_data)

# 5. Generar la gráfica de relación (Predicción vs. Valor Real)
# El resultado de este script es la tabla 'final_output' para Power BI
final_output <- test_data %>%
  select(Gasto_Promedio_€, Prediccion_Gasto)

# Visualización para el Dashboard (comparativa Real vs. Predicción)
ggplot(final_output, aes(x = Gasto_Promedio_€, y = Prediccion_Gasto)) +
  geom_point(alpha = 0.6, color = "#0078D4") +
  geom_smooth(method = "lm", col = "red", se = FALSE) + # Línea de tendencia
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") + # Línea Ideal (Y=X)
  labs(
    title = "Random Forest: Gasto Real vs. Gasto Predicho",
    subtitle = "La línea discontinua es el escenario ideal (Real = Predicho)",
    x = "Gasto Promedio Real (€)",
    y = "Gasto Promedio Predicho (€)"
  ) +
  theme_bw()