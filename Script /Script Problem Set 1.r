# Universidad de Los Andes - Problem Set 1
# Big Data y Machine Learning
# Lina María Álvarez Ardila y Sara Rocío Rojas
# 03/03/25


# Cargar librerías necesarias
library(pacman)
p_load(rio, 
       tidyverse, 
       skimr, 
       visdat, 
       corrplot, 
       stargazer,
       readr,
       readxl,
       ggplot2,
       boot, 
       caret) 

# Cargar datos desde Excel
data = read_excel("C:/Users/yoyol/OneDrive/Escritorio/Universidad/Noveno semestre UNIANDES/Big Data and Machine Learning/Sesión 1/Taller 1/GEIH Datos.xlsx",
                   na = c("NA", "Faltante", ""))


#Manejo de valores faltantes:
na_por_columna <- colSums(is.na(data))
print(na_por_columna)

# Filtrar datos y transformar variables: Eliminar NA´s, tomar mayores de 18
#años y las personas ocupadas.

datos_18 = data %>%
  filter(age > 18 , ocu == 1 ) %>%
  mutate(y_salary_m = as.numeric(y_salary_m)) %>% 
  filter( !is.na(y_salary_m), y_salary_m > 0 )%>%
  mutate(maxeduclevel = as.numeric(as.character(maxeduclevel)))

#Variables relevantes 
variables_relevantes <- datos_18 %>%
  select(y_salary_m, age, maxeduclevel)

summary(datos_18$y_salary_m)

#Estadísticas descriptivas generales

summary(datos_18)

#Descriptivas del salario 

summary_salarios = datos_18  %>%
  summarize(
    media_salario = mean(y_salary_m, na.rm = TRUE),
    mediana_salario = median(y_salary_m, na.rm = TRUE),
    sd_salario = sd(y_salary_m, na.rm = TRUE),
    min_salario = min(y_salary_m, na.rm = TRUE),
    max_salario = max(y_salary_m, na.rm = TRUE)
  )

print(summary_salarios)

#Salario según el género 

salario_genero = datos_18 %>%
  group_by(sex) %>%
  summarize(
    media_ssexo = mean(y_salary_m,na.rm = TRUE ),
    mediana_ssexo = median(y_salary_m,na.rm = TRUE),
    sd_ssexo = sd(y_salary_m,na.rm = TRUE),
    count = n()
  )
print(salario_genero)

#Salario por estrato 
salario_estrato = datos_18  %>%
  group_by(estrato1)%>%
  summarize(
    media_sestrato = mean(y_salary_m,na.rm = TRUE ),
    mediana_sestrato = median(y_salary_m,na.rm = TRUE),
    sd_sestrato = sd(y_salary_m,na.rm = TRUE),
    count = n()
  )
print(salario_estrato)  

#Agrupamos por educación
salario_educacion <- datos_18 %>%
  group_by(maxeduclevel) %>%
  summarize(
    mean_wage = mean(y_salary_m, na.rm = TRUE),
    median_wage = median(y_salary_m, na.rm = TRUE),
    sd_wage = sd(y_salary_m, na.rm = TRUE),
    count = n()
  )

print(salario_educacion)

#Agregamos una columna que categorice la edad
datos_18 <- datos_18 %>%
  mutate(edad = case_when(
    age < 25 ~ "18-24",
    age >= 25 & age < 35 ~ "25-34",
    age >= 35 & age < 45 ~ "35-44",
    age >= 45 & age < 55 ~ "45-54",
    age >= 55 ~ "55+"
  ))


#Agrupalos el salario por edad 
salario_edad <- datos_18 %>%
  group_by(edad) %>%
  summarize(
    media_sedad = mean(y_salary_m, na.rm = TRUE),
    mediana_sedad = median(y_salary_m, na.rm = TRUE),
    sd_sedad = sd(y_salary_m, na.rm = TRUE),
    count = n()
  )

print(salario_edad)

#Agrupamos por regímenes de seguridad social en salud en el que está afiliado
salario_afiliacions <- datos_18 %>%
  group_by(p6100) %>%
  summarize(
    media_sas = mean(y_salary_m, na.rm = TRUE),
    mediana_sas = median(y_salary_m, na.rm = TRUE),
    sd_sas = sd(y_salary_m, na.rm = TRUE),
    count = n()
  )

print(salario_afiliacions)

#Agrupamos según el oficio 
salario_oficio <- datos_18 %>%
  group_by(oficio) %>%
  summarize(
    media_sof = mean(y_salary_m, na.rm = TRUE),
    mediana_sof = median(y_salary_m, na.rm = TRUE),
    sd_of = sd(y_salary_m, na.rm = TRUE),
    count = n()
  )

print(salario_oficio)

#Agrupamos según informalidad laboral 

salario_informal <- datos_18 %>%
  group_by(informal) %>%
  summarize(
    media_sinf = mean(y_salary_m, na.rm = TRUE),
    mediana_sinf = median(y_salary_m, na.rm = TRUE),
    sd_inf = sd(y_salary_m, na.rm = TRUE),
    count = n()
  )

print(salario_informal)

# 📊 Gráficas

# 📊 Gráfica 1: Distribución del Salario en bogotá 
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
# 📊 Gráfica 2: Distribución del Salario por género 
ggplot(datos_18, aes(x = as.factor(sex), y = y_salary_m, fill = as.factor(sex))) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribución de Salarios por Género",
       x = "Género (1 = Hombre, 2 = Mujer)", 
       y = "Salario Mensual (COP)") +
  theme_minimal()

# 📊 Gráfica 3: Distribución del Salario según nivel educativo 
ggplot(salario_educacion, aes(x = reorder(as.factor(maxeduclevel), mean_wage), y = mean_wage, fill = as.factor(maxeduclevel))) +
  geom_bar(stat = "identity") +
  labs(title = "Salario Promedio según Nivel Educativo",
       x = "Nivel Educativo", 
       y = "Salario Promedio (COP)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
colnames(salario_edad)

# 📊 Gráfica 4: 
ggplot(salario_edad, aes(x = reorder(edad, media_sedad), y = media_sedad, fill = edad)) +
  geom_bar(stat = "identity") +
  labs(title = "Salario Promedio según Grupo de Edad",
       x = "Grupo de Edad", 
       y = "Salario Promedio (COP)") +
  theme_minimal()

# 📊 Gráfica 5: 
ggplot(salario_estrato, aes(x = as.factor(estrato1), y = media_sestrato, fill = as.factor(estrato1))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Salario Promedio según Estrato Socioeconómico",
       x = "Estrato", 
       y = "Salario Promedio (COP)") +
  theme_minimal()

# 📊 Gráfica 6:
ggplot(salario_informal, aes(x = as.factor(informal), y = media_sinf, fill = as.factor(informal))) +
  geom_bar(stat = "identity") +
  labs(title = "Salario Promedio: Trabajo Formal vs. Informal",
       x = "Formalidad (0 = Formal, 1 = Informal)", 
       y = "Salario Promedio (COP)") +
  theme_minimal()


# Punto #3:

class(datos_18$age)
str(datos_18$age)  
unique(datos_18$age)  # Ver valores únicos en edad
datos_18 <- datos_18 %>%
  mutate(log_salary = log(y_salary_m),
         age_sq = (age)^2)  

# Estimar el modelo cuadrático
modelo_edad <- lm(log_salary ~ age + age_sq, data = datos_18)

# Mostrar los resultados

summary(modelo_edad)
edad_pico <- -coef(modelo_edad)["age"] / (2 * coef(modelo_edad)["age_sq"])
print(paste("La edad pico es aproximadamente:", round(edad_pico, 2)))

#Bootstraping 

# Crear datos para la curva de predicción
edad_pred <- data.frame(age = seq(min(datos_18$age), max(datos_18$age), length.out = 100))
edad_pred$age_sq <- edad_pred$age^2
edad_pred$log_salary_pred <- predict(modelo_edad, newdata = edad_pred)

# 📊 Gráfica 7: Perfil de salario por edad 
ggplot(datos_18, aes(x = age, y = log_salary)) +
  geom_point(alpha = 0.3) +  # Puntos individuales (dispersión)
  geom_line(data = edad_pred, aes(x = age, y = log_salary_pred), color = "blue", size = 1.2) +
  geom_vline(xintercept = edad_pico, linetype = "dashed", color = "red") +
  labs(title = "Perfil de Salario por Edad",
       x = "Edad",
       y = "Log(Salario)") +
  theme_minimal()


# Función para reestimar el modelo y calcular la edad pico
boot_fn <- function(data, index) {
  modelo <- lm(log_salary ~ age + age_sq, data = data[index, ])
  edad_pico <- -coef(modelo)["age"] / (2 * coef(modelo)["age_sq"])
  return(edad_pico)
}

# Aplicar bootstrap con 1000 repeticiones
set.seed(123)
boot_edad_pico <- boot(datos_18, boot_fn, R = 1000)

# Intervalo de confianza al 95%
ci_edad_pico <- boot.ci(boot_edad_pico, type = "perc")
print(ci_edad_pico)


#Punto $4:

#Dummy mujer 

datos_18 <- datos_18 %>%
  mutate(dummy_sex = ifelse(sex == 1, 0, 1))

modelosimplebrechagenero = lm(log_salary ~ dummy_sex, data = datos_18)
summary(modelosimplebrechagenero)

#Variables de carasteristicas laborales: 

datos_control_brecha = datos_18 %>% 
  select(totalhoursworked, maxeduclevel, 
         microempresa, oficio, p6210s1, p6426, p6920, relab, age, p6050)

#Brecha de género con controles 

#FWL 

modelo_completo = lm(log_salary ~ dummy_sex + totalhoursworked +
                       maxeduclevel + microempresa + oficio + p6210s1 + 
                       p6426 + p6920 + relab + age + p6050,
                     data = datos_18)
resultados1=summary(modelo_completo)
summary(modelo_completo)

# Paso 2: Regresión de salario sobre controles

modelo_salario_controles <- lm(log_salary ~ totalhoursworked +
                                 maxeduclevel + microempresa + oficio + 
                                 p6210s1 + p6426 + p6920 + relab + age + p6050,
                               data = datos_18)
resid_salario_controles <- resid(modelo_salario_controles)

# Paso 2: Regresión de sexo sobre controles

modelo_sexo_controles <- lm(dummy_sex ~ totalhoursworked +
                              maxeduclevel + microempresa + oficio + p6210s1 +
                              p6426 + p6920 + relab + age + p6050,
                            data = datos_18)
resid_sexo_controles <- resid(modelo_sexo_controles)

# Paso 3: Regresión de los residuales de salario sobre los residuales de sexo

modelo_fwl <- lm(resid_salario_controles ~ resid_sexo_controles)
summary(modelo_fwl)

stargazer(modelo_completo, modelo_fwl,
          type = "latex",
          title = "Comparación: Modelo Completo vs FWL",
          column.labels = c("Modelo Completo", "Modelo FWL"),
          dep.var.labels = "y",
          covariate.labels = c("Sexo", "totalhoursworked", 
                                 "maxeduclevel", "microempresa", "oficio", 
                               "p6210s1", "p6426", "p6920", "relab", "age", "p6050"),
          omit.stat = c("f", "ser"),
          notes = "En el modelo FWL sólo aparece el coeficiente de 'x', pues los controles han sido parcializados.")


#FWL y bootstrapping 

eta_fn<-function(data,index){
  
  coef( lm(resid_salario_controles ~ resid_sexo_controles,
          data = datos_18, subset = index))[2] 
}

set.seed(123)
boot_resultados = boot(datos_18, eta_fn, R = 1000)
boot.ci(boot_resultados, type = "perc")

# 📊 Gráfica 8:
ggplot(datos_18, aes(x = age, y = log_salary, color = as.factor(sex))) +
  geom_point(alpha = 0.3) +  # Puntos individuales (dispersión)
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE, size = 1.2) +  # Línea suavizada con intervalo de confianza
  labs(title = "Perfil de Salario por Edad y Género con Intervalos de Confianza",
       x = "Edad",
       y = "Log(Salario)",
       color = "Género") +
  scale_color_manual(labels = c("Hombres", "Mujeres"), values = c("blue", "red")) +
  theme_minimal()

#Punto #5: Predicción 

#Separar la muestra de entrenamiento y testeo
set.seed(3498)   

Partición <- createDataPartition(
  y = datos_18$y_salary_m,  
  p = .70, 
  list = FALSE
)
Entrenamiento <- datos_18 %>% 
  filter(row_number() %in% Partición)

Prueba  <- datos_18 %>% 
  filter(!row_number() %in% Partición)

split_data <- data.frame(
  Split = factor(c("Entrenamiento", "Prueba")),
  Count = c(nrow(Entrenamiento), nrow(Prueba)),
  Percentage = c(nrow(Entrenamiento)/nrow(datos_18)*100, nrow(Prueba)/nrow(datos_18)*100)
)

# 📊 Gráfica 9: Distribución de la muestra
ggplot(split_data, aes(x = Split, y = Count)) +
  geom_bar(stat = "identity", fill = "pink3", width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%\n(observaciones=", Count, ")")), 
            vjust = -0.5, color = "grey3", size = 4) +
  labs(title = "Muestra de entrenamiento y prueba",
       y = "",
       x = "") +
  theme_light() +
  ylim(0, max(split_data$Count) * 1.3) 


#Modelos de predicción 

#RMSE modelos 

#1.
modelo_simple_predicción <- lm(log_salary ~ dummy_sex, data = Entrenamiento)
predicciones_simples <- predict(modelo_simple_predicción, newdata = Prueba)
RMSE_simple<- RMSE(predicciones_simples, Prueba$log_salary)

#2.

modelo_controles = lm(log_salary ~ dummy_sex + totalhoursworked +
                       maxeduclevel + microempresa + oficio + p6210s1 + p6426 +
                         p6920 + relab + age + p6050,
                     data = Entrenamiento)
predicciones_controles = predict(modelo_controles, newdata = Prueba)
RMSE_controles<- RMSE(predicciones_controles, Prueba$log_salary)

#3. 
modelo_edad_predicción <- lm(log_salary ~ age + age_sq, data = Entrenamiento)
predicciones_edad_pico = predict(modelo_edad_predicción, newdata = Prueba)
RMSE_edadpico<- RMSE(predicciones_edad_pico, Prueba$log_salary)

#4.
modelo_totalhours_modif = lm(log_salary ~ (totalhoursworked^2) +
                               totalhoursworked,
                         data = Entrenamiento)
predicciones_totalhours_modif = predict(modelo_totalhours_modif, newdata = Prueba)
RMSE_hours<- RMSE(predicciones_totalhours_modif, Prueba$log_salary)

#5. 

modelo_age_modif = lm(log_salary ~ (age^2) + (age^3),
                             data = Entrenamiento)
predicciones_age_modif = predict(modelo_age_modif, newdata = Prueba)
RMSE_age<- RMSE(predicciones_age_modif, Prueba$log_salary)


#Grafico de comparación
resultados <- data.frame(
  reales = Prueba$log_salary,
  pred_simple = predicciones_simples,
  pred_controles = predicciones_controles,
  pred_edad = predicciones_edad_pico,
  pred_edad_modif = predicciones_age_modif,
  pred_total = predicciones_totalhours_modif)


# 📊 Gráfica 10: Valores reales contra las predicciones de cada modelo
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


#Comparación de los metodos de predicción: 

RMSE_comparación = data.frame("Simple" = RMSE_simple, "Edad pico"=RMSE_edadpico, 
                              "Edad modificado" = RMSE_age, "Con controles"=RMSE_controles,
                              "Total horas trabajadas"=RMSE_hours)
max(RMSE_comparación)

stargazer(RMSE_comparación, type = "latex", summary = FALSE, 
          title = "Comparación de RMSE entre Modelos", 
          digits = 2)

#LOOCV: 

Método <- trainControl(
  method = "LOOCV")

modelo_edadpico_2 <- train(log_salary ~ age + age_sq,
                  data = datos_18,
                  method = 'lm', 
                  trControl= Método)
Calificación_1 <-RMSE(modelo_edadpico_2$pred$pred, datos_18$log_salary)


summary(datos_18$log_salary)

modelo_controles_2 <- train(log_salary ~ dummy_sex + totalhoursworked +
                              maxeduclevel + microempresa + oficio + p6210s1 + p6426 +
                              p6920 + relab + age + p6050,
                            data = datos_18,
                            method = 'lm', 
                            trControl = Método,
                            na.action = na.exclude)  # Asegurar que 'Método' está definido
Calificación_2 <-RMSE(modelo_controles_2$pred$pred, datos_18$log_salary)

rmse_values <- data.frame(
  Métrica = c("RMSE LOOCV - Modelo 1", "RMSE LOOCV - Modelo 2"),
  Valor = c(0.734670358393534, 0.585670044204214)
)

stargazer(rmse_values, summary = FALSE, rownames = FALSE, 
          title = "Comparación de RMSE con LOOCV", 
          label = "tab:rmse_loocv",
          digits = 6)


