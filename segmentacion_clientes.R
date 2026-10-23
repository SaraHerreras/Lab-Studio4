# Segmentacion de clientes del Mall Customers Dataset (plantilla)
# Completa cada TODO siguiendo los pasos del enunciado.

# Paquetes necesarios
library(ggplot2)
library(cluster)

# 1. Carga del dataset (Mall_Customers.csv esta junto a este script)
# TODO: importa el CSV con read.csv() y examina las primeras filas con head()
file1 <- "Mall_Customers.csv"
mall <- read.csv(file1)
head(mall)

# 2. Exploracion y limpieza de datos
# TODO: dim(), str() y summary()
# TODO: renombra las columnas para facilitar su manejo
# TODO: codifica Genre como variable numerica (1 masculino, 0 femenino)
# TODO: normaliza Annual Income y Spending Score con scale()
dim(mall)
str(mall)
summary(mall)

mall <- mall %>% mutate(Genre = case_when(Genre == "Male" ~ 1, Genre == "Female" ~ 0))
mall <- mall %>% scale(Annual_Income, Spending_Score)

# 3. Exploracion de variables (opcional)
# TODO: histogramas / diagramas de caja de Age, Annual_Income y Spending_Score

# 4. Entrenamiento de modelos de clustering
## 4.1 K-Means
# TODO: calcula tot.withinss para k = 2..6 y dibuja el metodo del codo
# TODO: entrena kmeans() con el k optimo (usa set.seed para reproducibilidad)
#       y asigna los clusters al dataset
for (i in 1:31) {
  # fijamos semilla
  set.seed(i)
  for (j in 2:6) {
    # ajustamos un K-Means con 2 segmentos
    out1 <- kmeans(mall, 
                  centers=j)
    tot  <- out1$withinss

    fviz_nbclust(mall, kmeans, method = "wss") +
      geom_vline(xintercept = 4, linetype = 2)
    
    }
  
}

## 4.2 Clustering jerarquico
# TODO: matriz de distancias euclidianas con dist()
# TODO: hclust() con method = "ward.D" y dibuja el dendrograma
# TODO: corta el dendrograma con cutree() y asigna los clusters al dataset
mat_dist <-dist(mall,method = "euclidean")
mat_hclust <-hclust(mat_dist, method="ward.D")
options(repr.plot.width=15, repr.plot.height=8)
plot(mat_hclust, hang=-1)
cluster <- cutree(mat_hclust, k=2)

# 5. Evaluacion de modelos
# TODO: silhouette() para cada modelo, promedio de silueta y comparacion
ss <- silhouette(cluster, dist(mall,method = "euclidean"))
mean_ss <-mean(ss[, 3])

sk <- silhouette(out1, dist(mall,method = "euclidean"))
mean_sk <-mean(sk[, 3])

# 6. Analisis descriptivo de segmentos
# TODO: aggregate() con las medias de Age, Annual_Income y Spending_Score por cluster
# TODO: interpreta los perfiles (p. ej. ingresos altos y gasto bajo...)
c <-select(Age, Annual_Income, Spending_Score)
aggregate(c,cluster, mean)
# 7. Visualizacion
# TODO: geom_point() de ggplot2 coloreando por cluster (K-Means y jerarquico)
out1 %>%
  ggplot(aes(Age, col=cluster)) + 
  geom_point()


cluster %>%
  ggplot(aes(Age, col=cluster)) + 
  geom_point()
