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

# 3- Limpieza del Dataset

# Se renombran las columnas para facilitar el manejo de los datos
colnames(df_carreras) <- c("Carrera", "Femenino", "Masculino", "Cuatrimestre", "Ano")

# Se crea una nueva variable con el total de estudiantes
df_carreras$Total <- df_carreras$Femenino + df_carreras$Masculino

# Verificación
head(df_carreras)

# 4- Análisis Exploratorio de Datos (EDA)

# 4.1 Histograma: Distribucion total de estudiantes

# Se analiza la distribución de la cantidad total de estudiantes

hist(df_carreras$Total,
     main = "Distribución de estudiantes por carrera",
     xlab = "Cantidad de estudiantes",
     col = "steelblue")

# La mayoría de las carreras presentan una cantidad baja de estudiantes,
# mientras que pocas concentran valores más altos, lo que indica una distribución desigual.

# 4.2 Boxplot: Comparación por sexo

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

# 4.3 Evolución en el tiempo

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

# 5- Conclusión

# El análisis exploratorio permitió identificar que la mayoría de las carreras presentan
# una cantidad baja de estudiantes, mientras que unas pocas concentran valores más altos.
# También se observaron diferencias en la distribución de estudiantes femeninos entre las carreras.
# Finalmente, se evidenció un crecimiento sostenido en la cantidad de estudiantes a lo largo del tiempo.