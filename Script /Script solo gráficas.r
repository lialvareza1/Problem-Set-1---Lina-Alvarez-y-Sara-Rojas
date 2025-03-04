# Universidad de Los Andes - Problem Set 1
# Big Data y Machine Learning
# Lina María Álvarez Ardila y Sara Rocío Rojas
# 03/03/25

# 📊 Gráficas

datos_18 %>%
  filter(y_salary_m >= 10000L & y_salary_m <= 4676136L) %>%
  ggplot() +
  aes(x = y_salary_m) +
  geom_histogram(bins = 30L, fill = "#112446") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribución de Salarios en Bogotá",
       x = "Salario Mensual (COP)", 
       y = "Frecuencia") +
  theme_minimal()

# 📊 Gráfica: Distribución de salarios por género
ggplot(datos_18, aes(x = as.factor(sex), y = y_salary_m, fill = as.factor(sex))) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribución de Salarios por Género",
       x = "Género (1 = Hombre, 2 = Mujer)", 
       y = "Salario Mensual (COP)") +
  theme_minimal()

# 📊 Gráfica: Salario por nivel educativo
ggplot(salario_educacion, aes(x = reorder(as.factor(maxeduclevel), mean_wage), y = mean_wage, fill = as.factor(maxeduclevel))) +
  geom_bar(stat = "identity") +
  labs(title = "Salario Promedio según Nivel Educativo",
       x = "Nivel Educativo", 
       y = "Salario Promedio (COP)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 📊 Gráfica: Salario promedio por grupo de edad
ggplot(salario_edad, aes(x = reorder(edad, media_sedad), y = media_sedad, fill = edad)) +
  geom_bar(stat = "identity") +
  labs(title = "Salario Promedio según Grupo de Edad",
       x = "Grupo de Edad", 
       y = "Salario Promedio (COP)") +
  theme_minimal()

# 📊 Gráfica: Salario por estrato socioeconómico
ggplot(salario_estrato, aes(x = as.factor(estrato1), y = media_sestrato, fill = as.factor(estrato1))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Salario Promedio según Estrato Socioeconómico",
       x = "Estrato", 
       y = "Salario Promedio (COP)") +
  theme_minimal()

# 📊 Gráfica: Salario en trabajos formales vs informales
ggplot(salario_informal, aes(x = as.factor(informal), y = media_sinf, fill = as.factor(informal))) +
  geom_bar(stat = "identity") +
  labs(title = "Salario Promedio: Trabajo Formal vs. Informal",
       x = "Formalidad (0 = Formal, 1 = Informal)", 
       y = "Salario Promedio (COP)") +
  theme_minimal()

# 📊 Gráfica: Perfil de salario por edad
ggplot(datos_18, aes(x = age, y = log_salary)) +
  geom_point(alpha = 0.3) +  # Puntos individuales (dispersión)
  geom_line(data = edad_pred, aes(x = age, y = log_salary_pred), color = "blue", size = 1.2) +
  geom_vline(xintercept = edad_pico, linetype = "dashed", color = "red") +
  labs(title = "Perfil de Salario por Edad",
       x = "Edad",
       y = "Log(Salario)") +
  theme_minimal()

# 📊 Gráfica: Perfil de salario por edad y género
ggplot(datos_18, aes(x = age, y = log_salary, color = as.factor(sex))) +
  geom_point(alpha = 0.3) +  # Puntos individuales (dispersión)
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE, size = 1.2) +  # Línea suavizada con intervalo de confianza
  labs(title = "Perfil de Salario por Edad y Género con Intervalos de Confianza",
       x = "Edad",
       y = "Log(Salario)",
       color = "Género") +
  scale_color_manual(labels = c("Hombres", "Mujeres"), values = c("blue", "red")) +
  theme_minimal()

# 📊 Gráfica: Muestra de entrenamiento y prueba
ggplot(split_data, aes(x = Split, y = Count)) +
  geom_bar(stat = "identity", fill = "pink3", width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%\n(observaciones=", Count, ")")), 
            vjust = -0.5, color = "grey3", size = 4) +
  labs(title = "Muestra de entrenamiento y prueba",
       y = "",
       x = "") +
  theme_light() +
  ylim(0, max(split_data$Count) * 1.3)

# 📊 Gráfica: Ajuste de predicciones de modelos
ggplot(resultados, aes(x = reales)) +
  geom_point(aes(y = pred_simple, color = "Predicción Simple"), alpha = 0.5) +
  geom_point(aes(y = pred_controles, color = "Predicción con Controles"), alpha = 0.5) +
  geom_point(aes(y = pred_edad, color = "Predicción con Edad"), alpha = 0.5) +
  geom_point(aes(y = pred_edad_modif, color = "Predicción con Edad Modificada"), alpha = 0.5) +
  geom_point(aes(y = pred_total, color = "Predicción Total"), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  scale_color_manual(name = "Modelo de Predicción", 
                     values = c("Predicción Simple" = "blue",
                                "Predicción con Controles" = "red",
                                "Predicción con Edad" = "green",
                                "Predicción con Edad Modificada" = "purple",
                                "Predicción Total horas trabajadas" = "yellow")) +
  labs(x = "Valores Reales", 
       y = "Predicciones", 
       title = "Ajuste de Predicciones de los Modelos OLS y FWL") +
  theme_minimal() +
  theme(legend.position = "top")
