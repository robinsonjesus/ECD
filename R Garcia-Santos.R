# Materia: Fundamentos de Análisis de datos
# Script para el Trabajo Práctico n°1
# Tomás García
# Germán Santos

data = read.csv("Datos trabajo 1.csv")

# Ejercicio 1.a.
# Reemplazo los datos faltantes con NA por asignación directa.

# reemplazo 999,999 (valores faltantes) por NA
data[data == "999,99"] = "NA"
# renombro calorías por calorias, sin tilde
names(data)[3] <- "Calorias"

# Ejercicio1.b.

par(mfrow=c(4,1))


data$Grasas_sat <- as.numeric(data$Grasas_sat)
data$Alcohol <- as.numeric(data$Alcohol)
data$Calorias <- as.numeric(data$Calorias)

# Grasas Saturadas
boxplot(data$Grasas_sat,
        main = "Grasas Saturadas",
        xlab = "Consumo",
        col = "red",
        border = "black",
        horizontal = TRUE,
        notch = TRUE)

# Alcohol
boxplot(data$Alcohol,
        main = "Alcohol",
        xlab = "Consumo",
        col = "blue",
        border = "black",
        horizontal = TRUE,
        notch = TRUE)

# Calorías
boxplot(data$Calorias,
        main = "Calorias",
        xlab = "Consumo",
        col = "green",
        border = "black",
        horizontal = TRUE,
        notch = TRUE)

# Calorías sin outliers
boxplot(data$Calorias,
        main = "Calorias sin outliers",
        xlab = "Consumo",
        col = "green",
        border = "black",
        horizontal = TRUE,
        notch = TRUE,
        outline=FALSE)
		
# Características de las variables

# Variable Grasas Saturadas

mean(data$Grasas_sat, na.rm=TRUE)
max(data$Grasas_sat, na.rm=TRUE)
min(data$Grasas_sat, na.rm=TRUE)
quantile(data$Grasas_sat, na.rm=TRUE)
IQR(data$Grasas_sat, na.rm=TRUE)

# Variable Alcohol

mean(data$Alcohol, na.rm=TRUE)
max(data$Alcohol, na.rm=TRUE)
min(data$Alcohol, na.rm=TRUE)
quantile(data$Alcohol, na.rm=TRUE)
IQR(data$Alcohol, na.rm=TRUE)

# Variable Calorias

mean(data$Calorias, na.rm=TRUE)
max(data$Calorias, na.rm=TRUE)
min(data$Calorias, na.rm=TRUE)
quantile(data$Calorias, na.rm=TRUE)
IQR(data$Calorias, na.rm=TRUE)

# Variable Calorias sin outliers

cal_out <- data$Calorias[!data$Calorias %in% boxplot.stats(data$Calorias)$out]

mean(cal_out, na.rm=TRUE)
max(cal_out, na.rm=TRUE)
min(cal_out, na.rm=TRUE)
quantile(cal_out, na.rm=TRUE)
IQR(cal_out, na.rm=TRUE)

# Ejercicio 1 c

# Agrego una columna con el string CATE1 o CATE3 de acuerdo al valor de la columna Calorias
data$Alc_rank <- ifelse((data$Calorias <= 1.700), "CATE 1", "CATE 3")

# Creo 2 nuevos dataframe basados en estas categorías
cate1 = data[data$Alc_rank == "CATE 1", ]
cate3 = data[data$Alc_rank == "CATE 3", ]

# Variable Alcohol para quienes consumen menos de 1700 calorías

mean(cate1$Alcohol, na.rm=TRUE)
max(cate1$Alcohol, na.rm=TRUE)
min(cate1$Alcohol, na.rm=TRUE)
quantile(cate1$Alcohol, na.rm=TRUE)
IQR(cate1$Alcohol, na.rm=TRUE)

# Variable Alcohol para quienes consumen más de 1700 calorías

mean(cate3$Alcohol, na.rm=TRUE)
max(cate3$Alcohol, na.rm=TRUE)
min(cate3$Alcohol, na.rm=TRUE)
quantile(cate3$Alcohol, na.rm=TRUE)
IQR(cate3$Alcohol, na.rm=TRUE)
