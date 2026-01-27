############################################################
# ANÁLISIS EXPLORATORIO
# Dataset: Titanic-Dataset.csv
############################################################

# 1. Cargar librerías
library(tidyverse)

# 2. Cargar datos
titanic <- read.csv("Titanic-Dataset.csv")

# Vista general del dataset
str(titanic)
summary(titanic)


# ANÁLISIS UNIVARIADO

# Univariado - Variable Cualitativa: Sexo
table(titanic$Sex)
prop.table(table(titanic$Sex))

ggplot(titanic, aes(x = Sex)) +
  geom_bar() +
  labs(title = "Distribución de Sexo",
       x = "Sexo",
       y = "Frecuencia")

# Univariado - Variable Cuantitativa Continua: Edad
summary(titanic$Age)
sd(titanic$Age, na.rm = TRUE)
IQR(titanic$Age, na.rm = TRUE)

ggplot(titanic, aes(x = Age)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribución de Edad",
       x = "Edad",
       y = "Frecuencia")

# Univariado - Outliers: Fare
ggplot(titanic, aes(y = Fare)) +
  geom_boxplot() +
  labs(title = "Boxplot de Fare",
       y = "Tarifa")

# ANÁLISIS BIVARIADO

# Cualitativa vs Cuantitativa: Edad según Supervivencia
ggplot(titanic, aes(x = factor(Survived), y = Age)) +
  geom_boxplot() +
  labs(title = "Edad según Supervivencia",
       x = "Sobrevivió (0 = No, 1 = Sí)",
       y = "Edad")

# Cuantitativa vs Cuantitativa: Edad vs Fare
ggplot(titanic, aes(x = Age, y = Fare)) +
  geom_point(alpha = 0.5) +
  labs(title = "Relación entre Edad y Tarifa",
       x = "Edad",
       y = "Tarifa")

# Cualitativa vs Cualitativa: Sexo vs Supervivencia
tabla <- table(titanic$Sex, titanic$Survived)
tabla
prop.table(tabla, margin = 1)

ggplot(titanic, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  labs(title = "Supervivencia según Sexo",
       x = "Sexo",
       y = "Proporción",
       fill = "Sobrevivió")

# Valores faltantes
colSums(is.na(titanic))
