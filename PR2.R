# Tipología y Ciclo de Vida de los Datos: Práctica 2
# Miembros: Sergio Frutos e Inés Ripoll
# Fecha: 27/05/2024

library(tidyverse)
library(readr)
library(corrplot)
library(relaimpo)
library(dplyr)
library(factoextra)
library(car)
library(modelsummary)
library(FSA)

# Cargar el dataset
df <- read_csv("world-happiness-report-2021.csv")
colnames(df)


# EJERCICIO 2

# Seleccionar solo variables numéricas (excepto redundantes como "Explained by")
numeric_df <- dplyr::select(df,
                            `Ladder score`,
                            `Logged GDP per capita`,
                            `Social support`,
                            `Healthy life expectancy`,
                            `Freedom to make life choices`,
                            Generosity,
                            `Perceptions of corruption`
)
colnames(numeric_df)

# Matriz de correlación
cor_matrix <- cor(numeric_df, use = "complete.obs")
round(cor_matrix[, "Ladder score"], 3)

# Visualizar correlaciones con Ladder score
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8,
         addCoef.col = "black", tl.col = "black", number.cex = 0.7)

# Modelo de regresión lineal
modelo_pre <- lm(`Ladder score` ~ ., data = numeric_df)
summary(modelo_pre)

# Análisis de importancia relativa de cada variable
calc.relimp(modelo_pre, type = "lmg", rela = TRUE)

# Resumen de los datos seleccionados
summary(numeric_df)


# EJERCICIO 3

# 3.1
# Seleccionar las variables relevantes
df_selected <- df %>%
  dplyr::select(`Ladder score`,
         `Logged GDP per capita`,
         `Social support`,
         `Healthy life expectancy`,
         `Freedom to make life choices`,
         `Perceptions of corruption`)

# Verificar valores NA
colSums(is.na(df_selected))

# Verificar cantidad de ceros
sapply(df_selected, function(x) sum(x == 0, na.rm = TRUE))

# 3.2
str(df_selected)

# 3.3
# Boxplots para detectar outliers visualmente
df_selected %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Valor") %>%
  ggplot(aes(x = "", y = Valor)) +
  geom_boxplot(fill = "skyblue") +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Boxplots por variable", x = "", y = "Valor")
 
# 3.4
# Estandarización (normalización o escalado)
df_scaled <- scale(df_selected)

# Comprobación de duplicados
sum(duplicated(df_selected))


# EJERCICIO 4

# 4.1
# Modelo supervisado: regresión lineal
modelo_post <- lm(`Ladder score` ~ ., data = df_selected)
summary(modelo_post)

# Modelos con nombres
modelos <- list(
  "Antes de limpieza" = modelo_pre,
  "Después de limpieza" = modelo_post
)

modelsummary(modelos, statistic = "p.value", stars = TRUE)


# Modelo no supervisado: Análisis de clusters (K-means)
# Escalar los datos (muy importante para clustering). 
# Se ha hecho en el proceso de limpieza del dataset (apartado 3)

# Escogemos 3 clusters 
set.seed(123)
kmeans_model <- kmeans(df_scaled, centers = 3, nstart = 25)

# Agregar el cluster al dataframe
df$cluster <- as.factor(kmeans_model$cluster)

# Visualizar los clusters con un gráfico PCA
fviz_cluster(kmeans_model, data = df_scaled, geom = "point",
             ellipse.type = "convex", palette = "jco",
             main = "Clusters de países según indicadores de felicidad")

# 4.2
# Volvemos a añadir la columan "Regional indicator"
df_selected <- df %>%
  dplyr::select(`Regional indicator`,
                `Ladder score`,
                `Logged GDP per capita`,
                `Social support`,
                `Healthy life expectancy`,
                `Freedom to make life choices`,
                `Perceptions of corruption`)

# Comprobar normalidad por grupo (Shapiro-Wilk)
shapiro_results <- df_selected %>%
  group_by(`Regional indicator`) %>%
  summarise(p_value = shapiro.test(`Ladder score`)$p.value)

print(shapiro_results)

# Comprobar homocedasticidad (Levene Test)
levene_result <- leveneTest(`Ladder score` ~ `Regional indicator`, data = df_selected)
print(levene_result)

# Selección y aplicación de la prueba
if(all(shapiro_results$p_value > 0.05) & levene_result$`Pr(>F)`[1] > 0.05) {
  # Se cumplen supuestos: realizar ANOVA
  anova_result <- aov(`Ladder score` ~ `Regional indicator`, data = df_selected)
  summary(anova_result)
} else {
  # No se cumplen supuestos: test no paramétrico Kruskal-Wallis
  kruskal_result <- kruskal.test(`Ladder score` ~ `Regional indicator`, data = df_selected)
  print(kruskal_result)
}


# Test de Dunn con corrección de Bonferroni
dunn_result <- dunnTest(`Ladder score` ~ `Regional indicator`, data = df_selected, method = "bonferroni")
print(dunn_result)