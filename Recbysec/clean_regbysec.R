library(tidyverse)
library(dplyr)

registros <- list.files(path = 'D:/mcsalvadorg/onedrive/Documentos/GIT_HUB/Records in R/Recbysec', pattern = 'csv')
data_registros <- map(registros, read_csv)
combinar_registros <- bind_rows (data_registros)
head(combinar_registros, 30)
summary (combinar_registros)
porcentaje_nulos <- colMeans(is.na(combinar_registros))

write.csv(registros_consolidados, "registros_consolidados.csv")

registros_consolidados<- select(combinar_registros, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28, -29, -30, -31, -32, -33, -34, -35, -36)


#renombrar columnas
nombre_columnas <- c("col1",
                     "col2",
                     "col3",
                     "col4",
                     "col5",
                     "col6",
                     "col7",
                     "col8",
                     "col9",
                     "col10",
                     "col11",
                     "col12",
                     "col13",
                     "col14",
                     "col15",
                     "col16",
                     "col17",
                     "col18"
                     )
colnames(registros_consolidados) <- nombre_columnas


registros_consolidados <- registros_consolidados[!(registros_consolidados$col1 == 'MU'),]
registros_consolidados <- registros_consolidados[!(registros_consolidados$col1 == 'Date'),]

registros_consolidados <- registros_consolidados %>% mutate(col1 = as.Date(col1, format = "%m/%d/%Y"))
registros_consolidados <- registros_consolidados %>%  mutate(col2 = as.POSIXct(col2, format = "%H:%M:%S"))

nombre_columnas <- c("Date",
                     "Time",
                     "Depth",
                     "Block_Ht",
                     "Bit_Depth",
                     "ROP_1ft",
                     "ROP_1min",
                     "Hookload",
                     "Pump_Pressure",
                     "Surface_Torque",
                     "Rotary_Speed",
                     "Pump_1_SPM",
                     "Pump_2_SPM",
                     "Pit_Total",
                     "Flow_In",
                     "Flow_Out",
                     "Temp_In",
                     "Temp_Out")

colnames(registros_consolidados) <- nombre_columnas



parametros <- c(
  
  "Depth",
  "Block_Ht",
  "Bit_Depth",
  "ROP_1ft",
  "ROP_1min",
  "Hookload",
  "Pump_Pressure",
  "Surface_Torque",
  "Rotary_Speed",
  "Pump_1_SPM",
  "Pump_2_SPM",
  "Pit_Total",
  "Flow_In",
  "Flow_Out",
  "Temp_In",
  "Temp_Out")

# Calculando los límites
Q1 <- quantile(registros_consolidados$ROP_1min, 0.25)
Q3 <- quantile(registros_consolidados$ROP_1min, 0.75)
IQR <- IQR(registros_consolidados$ROP_1min)
limite_inferior <- Q1 - 1.5*IQR
limite_superior <- Q3 + 1.5*IQR

# Identificando los outliers
outliers <- registros_consolidados[registros_consolidados$ROP_1min < limite_inferior | registros_consolidados$ROP_1min > limite_superior,]
write.csv(outliers, "outliers_ROP_1min.csv")
rm(outliers)

library(ggplot2)
# Crear el boxplot con ggplot2
ggplot(registros_consolidados, 
       aes(y = Depth, Date)) +
  geom_boxplot() +
  ggtitle("Outliers by Depth")

boxplot_stats(registros_consolidados$Depth)$out

ggplot(outliers_depth, aes(x = Depth, y = Date) +
  geom_boxplot() +
  labs(x = "Grupo", y = "Valor") +
  ggtitle("Comparación de valores por grupo"))

write.csv(registros_consolidados, "registros_limpios.csv")
