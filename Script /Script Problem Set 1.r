# Universidad de Los Andes - Problem Set 1
# Big Data y Machine Learning
# Lina María Álvarez Ardila y Sara Rocío Rojas
# 03/03/25

# Cargar librerías necesarias
library(pacman)
p_load(rio, tidyverse, skimr, visdat, corrplot, stargazer, readr, readxl, 
       ggplot2, boot, caret) 

# Cargar datos desde Excel
data <- read_excel("C:/Users/yoyol/OneDrive/Escritorio/Universidad/Noveno semestre UNIANDES/Big Data and Machine Learning/Sesión 1/Taller 1/GEIH Datos.xlsx",
                   na = c("NA", "Faltante", ""))

# Ver la estructura de los datos
View(data)

# Manejo de valores faltantes
na_por_columna <- colSums(is.na(data))
print(na_por_columna)

# Filtrar mayores de 18 años y ocupados, transformar variables
datos_18 <- data %>%
  filter(age > 18, ocu == 1) %>%
  mutate(y_salary_m = as.numeric(y_salary_m)) %>%
  filter(!is.na(y_salary_m), y_salary_m > 0) %>%
  mutate(maxeduclevel = as.numeric(as.character(maxeduclevel)))

# Seleccionar variables relevantes
variables_relevantes <- datos_18 %>%
  select(y_salary_m, age, maxeduclevel)

# Resumen estadístico del salario
summary(datos_18$y_salary_m)

# Calcular métricas de variabilidad
variabilidad <- variables_relevantes %>%
  summarise(
    Varianza_Salario = var(y_salary_m, na.rm = TRUE),
    Rango_Salario = max(y_salary_m, na.rm = TRUE) - min(y_salary_m, na.rm = TRUE),
    CV_Salario = sd(y_salary_m, na.rm = TRUE) / mean(y_salary_m, na.rm = TRUE),  
    Varianza_Edad = var(age, na.rm = TRUE),
    Rango_Edad = max(age, na.rm = TRUE) - min(age, na.rm = TRUE),
    CV_Edad = sd(age, na.rm = TRUE) / mean(age, na.rm = TRUE),
    Varianza_Educacion = var(maxeduclevel, na.rm = TRUE),
    Rango_Educacion = max(maxeduclevel, na.rm = TRUE) - min(maxeduclevel, na.rm = TRUE),
    CV_Educacion = sd(maxeduclevel, na.rm = TRUE) / mean(maxeduclevel, na.rm = TRUE)
  )

# Mostrar resultados de variabilidad
print(variabilidad)

# Graficar distribución del salario
ggplot(datos_18, aes(x = y_salary_m)) +
  geom_histogram(fill = "blue", bins = 50, alpha = 0.7) +
  labs(title = "Distribución del Salario", x = "Salario Mensual (COP)", y = "Frecuencia") +
  theme_minimal()

# Boxplot del salario
ggplot(datos_18, aes(y = y_salary_m)) +
  geom_boxplot(fill = "red", alpha = 0.5) +
  labs(title = "Boxplot del Salario", y = "Salario Mensual (COP)") +
  theme_minimal()

# Dispersión Salario vs Edad
ggplot(datos_18, aes(x = age, y = y_salary_m)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Dispersión Salario vs. Edad", x = "Edad", y = "Salario Mensual") +
  theme_minimal()

# Estadísticas descriptivas generales
summary(datos_18)

# Estadísticas descriptivas del salario
summary_salarios <- datos_18  %>%
  summarize(
    media_salario = mean(y_salary_m, na.rm = TRUE),
    mediana_salario = median(y_salary_m, na.rm = TRUE),
    sd_salario = sd(y_salary_m, na.rm = TRUE),
    min_salario = min(y_salary_m, na.rm = TRUE),
    max_salario = max(y_salary_m, na.rm = TRUE)
  )
print(summary_salarios)

# Estadísticas del salario por género
salario_genero <- datos_18 %>%
  group_by(sex) %>%
  summarize(
    media_ssexo = mean(y_salary_m, na.rm = TRUE),
    mediana_ssexo = median(y_salary_m, na.rm = TRUE),
    sd_ssexo = sd(y_salary_m, na.rm = TRUE),
    count = n()
  )
print(salario_genero)

# Estadísticas del salario por estrato
salario_estrato <- datos_18 %>%
  group_by(estrato1) %>%
  summarize(
    media_sestrato = mean(y_salary_m, na.rm = TRUE),
    mediana_sestrato = median(y_salary_m, na.rm = TRUE),
    sd_sestrato = sd(y_salary_m, na.rm = TRUE),
    count = n()
  )
print(salario_estrato)

# Categorizar edad en grupos
datos_18 <- datos_18 %>%
  mutate(edad = case_when(
    age < 25 ~ "18-24",
    age >= 25 & age < 35 ~ "25-34",
    age >= 35 & age < 45 ~ "35-44",
    age >= 45 & age < 55 ~ "45-54",
    age >= 55 ~ "55+"
  ))

# Estadísticas del salario por edad
salario_edad <- datos_18 %>%
  group_by(edad) %>%
  summarize(
    media_sedad = mean(y_salary_m, na.rm = TRUE),
    mediana_sedad = median(y_salary_m, na.rm = TRUE),
    sd_sedad = sd(y_salary_m, na.rm = TRUE),
    count = n()
  )
print(salario_edad)

# Regresión cuadrática: salario vs edad
modelo_edad <- lm(log(y_salary_m) ~ age + I(age^2), data = datos_18)
summary(modelo_edad)

# Encontrar edad pico en el salario
edad_pico <- -coef(modelo_edad)["age"] / (2 * coef(modelo_edad)["I(age^2)"])
print(paste("La edad pico es aproximadamente:", round(edad_pico, 2)))

# Separar datos en entrenamiento y prueba (70%-30%)
set.seed(3498)   
Partición <- createDataPartition(y = datos_18$y_salary_m, p = .70, list = FALSE)
Entrenamiento <- datos_18 %>% filter(row_number() %in% Partición)
Prueba  <- datos_18 %>% filter(!row_number() %in% Partición)

# Comparación de modelos de predicción usando RMSE
modelo_simple_predicción <- lm(log_salary ~ dummy_sex, data = Entrenamiento)
predicciones_simples <- predict(modelo_simple_predicción, newdata = Prueba)
RMSE_simple <- RMSE(predicciones_simples, Prueba$log_salary)

modelo_controles <- lm(log_salary ~ dummy_sex + totalhoursworked +
                         maxeduclevel + microempresa + oficio + p6210s1 + p6426 +
                         p6920 + relab + age + p6050, data = Entrenamiento)
predicciones_controles <- predict(modelo_controles, newdata = Prueba)
RMSE_controles <- RMSE(predicciones_controles, Prueba$log_salary)

# Comparación de RMSE
RMSE_comparación <- data.frame("Simple" = RMSE_simple, 
                               "Con controles" = RMSE_controles)
print(RMSE_comparación)

# LOOCV para validar modelos
Método <- trainControl(method = "LOOCV")
modelo_edadpico_2 <- train(log_salary ~ age + age_sq, data = datos_18, method = 'lm', trControl = Método)
Calificación_1 <- RMSE(modelo_edadpico_2$pred$pred, datos_18$log_salary)

modelo_controles_2 <- train(log_salary ~ dummy_sex + totalhoursworked + 
                              maxeduclevel + microempresa + oficio + p6210s1 + p6426 + 
                              p6920 + relab + age + p6050, data = datos_18, 
                            method = 'lm', trControl = Método, na.action = na.exclude)
Calificación_2 <- RMSE(modelo_controles_2$pred$pred, datos_18$log_salary)

# Mostrar resultados finales de RMSE con LOOCV
print(data.frame(Calificación_1, Calificación_2))

