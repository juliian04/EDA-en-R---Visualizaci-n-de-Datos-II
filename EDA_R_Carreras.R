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

# Se analiza la distribución de estudiantes femeninos por carrera

boxplot(Femenino ~ Carrera, data = df_carreras,
        las = 2,
        col = "pink",
        main = "Distribución de estudiantes femeninos por carrera")

# Se observan diferencias entre las carreras en la cantidad de estudiantes femeninos,
# así como la presencia de valores atípicos en algunas de ellas.

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