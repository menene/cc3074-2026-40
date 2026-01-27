############################################################
# ANÁLISIS EXPLORATORIO - NORMALIDAD
# Dataset: Churn_Modelling.csv
############################################################

# 1. Cargar datos -------------------------------------------

#install.packages("nortest")

library(nortest)

churn <- read.csv("Churn_Modelling.csv")

# Vista general
str(churn)
summary(churn)
nrow(churn)

# 2. Clasificación de variables -----------------------------

clasificacion_variables <- data.frame(
  Variable = c(
    "RowNumber",
    "CustomerId",
    "Surname",
    "CreditScore",
    "Geography",
    "Gender",
    "Age",
    "Tenure",
    "Balance",
    "NumOfProducts",
    "HasCrCard",
    "IsActiveMember",
    "EstimatedSalary",
    "Exited"
  ),
  Tipo = c(
    "Cualitativa categórica (ID)",
    "Cualitativa categórica (ID)",
    "Cualitativa categórica",
    "Cuantitativa discreta",
    "Cualitativa categórica",
    "Cualitativa categórica",
    "Cuantitativa continua",
    "Cuantitativa discreta",
    "Cuantitativa continua",
    "Cuantitativa discreta",
    "Cualitativa categórica (binaria)",
    "Cualitativa categórica (binaria)",
    "Cuantitativa continua",
    "Cualitativa categórica (binaria)"
  ),
  stringsAsFactors = FALSE
)

clasificacion_variables

# 3. Histogramas -----------------------------

hist(churn$Age)
hist(churn$Balance)
hist(churn$EstimatedSalary)
hist(churn$CreditScore)
hist(churn$Tenure)
hist(churn$NumOfProducts)

# 4. Boxplots -----------------------------

boxplot(churn$Age)
boxplot(churn$Balance)
boxplot(churn$EstimatedSalary)
boxplot(churn$CreditScore)
boxplot(churn$Tenure)
boxplot(churn$NumOfProducts)

# 6. Separación de varibles numericas -----------------------

cuant_discretas <- c(
  "CreditScore",
  "Tenure",
  "NumOfProducts"
)

cuant_continuas <- c(
  "Age",
  "Balance",
  "EstimatedSalary"
)

cuantitativas <- c(cuant_discretas, cuant_continuas)

datos_cuant <- churn[, cuantitativas]

datos_cuant

# 7. Pruebas de normalidad: Shapiro-Wilk --------------------

# Solo funciona para muestras pequeñas

# seed para reproducibilidad
set.seed(314159)
n_total <- nrow(churn)
n_train <- 5000
indices_train <- sample(1:n_total, size = n_train, replace = FALSE)

churn_sample <- churn[indices_train, ]

datos_cuant_sample <- churn_sample[, cuantitativas]

# Hipótesis:
# H0: La variable sigue una distribución normal
# H1: La variable NO sigue una distribución normal

shapiro_results <- lapply(datos_cuant_sample, shapiro.test)

# Mostrar resultados
shapiro_results

shapiro_results$Age

# p-value > 0.05 → No se rechaza H0 (normalidad)
# p-value ≤ 0.05 → Se rechaza H0 (normalidad)

# PRUEBA KS -----------------------------------------------

# Hipótesis:
# H0: La variable sigue una distribución normal
# H1: La variable NO sigue una distribución normal

ks_results <- lapply(datos_cuant, function(x) ks.test(x, "pnorm", mean(x), sd(x)))


# Mostrar resultados
ks_results

# Ejemplo
ks_results$Age

# p-value > 0.05 → No se rechaza H0
# p-value ≤ 0.05 → Se rechaza H0

# PRUEBA LILLIEFORS ---------------------------------------

# Hipótesis:
# H0: La variable sigue una distribución normal
# H1: La variable NO sigue una distribución normal

lillie_results <- lapply(datos_cuant, lillie.test)

# Mostrar resultados
lillie_results

# Ejemplo
lillie_results$Age

# p-value > 0.05 → No se rechaza H0
# p-value ≤ 0.05 → Se rechaza H0