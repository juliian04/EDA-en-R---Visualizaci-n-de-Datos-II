# Análisis Exploratorio de Datos (EDA) en R

# Estadísticas de los estudiantes de Educación Superior
# inscritos en el Instituto Tecnológico de las Américas (ITLA) 2006 – 2025

# Este conjunto de datos contiene las estadísticas de los estudiantes de Educación Superior
# inscritos en el Instituto Tecnológico de las Américas (ITLA) 2006 - 2025,
# en el cual se puede encontrar los estudiantes inscritos por cuatrimestre en las distintas carreras. 

# Este análisis exploratorio de datos (EDA) tiene como objetivo comprender
# el comportamiento de la matrícula en carreras del área tecnológica,
# analizando su distribución, evolución en el tiempo y diferencias por sexo.

# 1- Carga del Dataset

df_carreras <- read.csv("Estadisticas-Inscritos-Educacion-Superior-2006-2025.csv")

# Visualizar primeras filas
head(df_carreras)

# 2- Exploración del Dataset

# Nombres de columnas
colnames(df_carreras)

# Estructura de los datos
str(df_carreras)

# Resumen estadístico
summary(df_carreras)

# Se observa la presencia de valores variados en las variables numéricas,
# lo que sugiere la existencia de carreras con diferentes niveles de matrícula.

# 3- Limpieza del Dataset

# Se renombran las columnas para facilitar el manejo de los datos
colnames(df_carreras) <- c("Carrera", "Femenino", "Masculino", "Cuatrimestre", "Ano")

# Se crea una nueva variable con el total de estudiantes
df_carreras$Total <- df_carreras$Femenino + df_carreras$Masculino

# Verificación
head(df_carreras)

# 4- Análisis Exploratorio de Datos (EDA)

# 4.1 Análisis de relación entre variables

# Visualización general de las variables
pairs(df_carreras[, c("Femenino", "Masculino", "Total")])

# Se analiza de forma más específica la relación entre las variables numéricas del dataset

plot(df_carreras$Femenino, df_carreras$Total,
     col = "pink",
     pch = 16,
     main = "Relación entre estudiantes y total por sexo",
     xlab = "Cantidad por sexo",
     ylab = "Total de estudiantes")

points(df_carreras$Masculino, df_carreras$Total,
       col = "blue",
       pch = 16)

legend("topleft",
       legend = c("Femenino", "Masculino"),
       col = c("pink", "blue"),
       pch = 16)

# Se observa una relación directa entre la cantidad de estudiantes femeninos y masculinos
# con el total de estudiantes, evidenciando que a medida que aumenta cualquiera de estas variables,
# el total también incrementa. Esto confirma la dependencia entre las variables analizadas.

# 4.2 Histograma: Distribucion total de estudiantes

# Se analiza la distribución de la cantidad total de estudiantes

hist(df_carreras$Total,
     main = "Distribución de estudiantes por carrera",
     xlab = "Cantidad de estudiantes",
     col = "steelblue")

# La mayoría de las carreras presentan una cantidad baja de estudiantes,
# mientras que pocas concentran valores más altos, lo que indica una distribución desigual.

# 4.3 Distribución por sexo

# Totales por sexo
totales_sexo <- colSums(df_carreras[, c("Femenino", "Masculino")])

# Convertir a porcentaje
porcentaje <- round(totales_sexo / sum(totales_sexo) * 100, 1)

# Gráfico de barras
barplot(totales_sexo,
        col = c("#FF69B4", "#1E90FF"),
        main = "Distribución de estudiantes por sexo",
        ylab = "Cantidad de estudiantes",
        ylim = c(0, max(totales_sexo) * 1.2))

# Agregar etiquetas
text(x = c(0.7, 1.9),
     y = totales_sexo,
     label = paste0(porcentaje, "%"),
     pos = 3,
     cex = 1.2,
     col = "black")

# Se observa que la participación masculina es significativamente mayor que la femenina,
# representando aproximadamente el 78% del total de estudiantes, mientras que las mujeres
# constituyen cerca del 22%, evidenciando una diferencia notable en la matrícula por sexo.

# 4.4 Evolución en el tiempo

# Se analiza la evolución de la matrícula a lo largo del tiempo

totales_por_ano <- aggregate(Total ~ Ano, data = df_carreras, sum)

plot(totales_por_ano$Ano, totales_por_ano$Total,
     type = "o",
     col = "darkgreen",
     main = "Evolución de estudiantes en el tiempo",
     xlab = "Año",
     ylab = "Total de estudiantes")

# Se observa una tendencia creciente en la cantidad de estudiantes a lo largo del tiempo,
# especialmente en los años más recientes.

# 4.5 Evolución por Sexo

# Agrupar por año y sexo
fem <- aggregate(Femenino ~ Ano, data = df_carreras, sum)
masc <- aggregate(Masculino ~ Ano, data = df_carreras, sum)

# Gráfico
plot(fem$Ano, fem$Femenino,
     type = "o",
     col = "pink",
     ylim = c(0, max(masc$Masculino)),
     main = "Evolución de estudiantes por sexo",
     xlab = "Año",
     ylab = "Cantidad")

lines(masc$Ano, masc$Masculino,
      type = "o",
      col = "blue")

legend("topleft",
       legend = c("Femenino", "Masculino"),
       col = c("pink", "blue"),
       lty = 1)

# Se observa la evolución de la participación por sexo a lo largo del tiempo,
# permitiendo comparar el crecimiento de estudiantes femeninos y masculinos.

# 4.6 Análisis por carrera

# Total por carrera
total_carrera <- aggregate(Total ~ Carrera, data = df_carreras, sum)

# Ordenar de mayor a menor
total_carrera <- total_carrera[order(-total_carrera$Total), ]

# top 10
top10 <- head(total_carrera, 10)

par(mar = c(15, 8, 4, 2))

barplot(top10$Total,
        names.arg = top10$Carrera,
        las = 2,
        col = "darkorange",
        main = "Top 10 carreras con mayor cantidad de estudiantes",
        ylim = c(0, max(top10$Total) * 1.2),
        cex.names = 0.8)

# Se identifican las carreras con mayor cantidad de estudiantes,
# destacándose aquellas con mayor demanda dentro del dataset.

# 5- Conclusión

# El análisis exploratorio permitió identificar que la mayoría de las carreras presentan
# una baja cantidad de estudiantes, mientras que unas pocas concentran valores significativamente más altos,
# evidenciando una distribución desigual en la matrícula.

# Asimismo, se observó que la participación masculina es considerablemente mayor que la femenina.
# Este comportamiento puede estar relacionado con la naturaleza de las carreras analizadas,
# principalmente orientadas al área de informática y tecnología, las cuales históricamente
# han sido cursadas en mayor proporción por hombres.

# A lo largo del tiempo, se evidenció una tendencia creciente en la cantidad total de estudiantes,
# especialmente en los años más recientes, lo que indica un aumento sostenido en la matrícula.

# Finalmente, el análisis de relaciones entre variables confirmó una dependencia directa entre
# la cantidad de estudiantes por sexo y el total, así como la concentración de la demanda en ciertas carreras,
# destacándose aquellas vinculadas al ámbito tecnológico.