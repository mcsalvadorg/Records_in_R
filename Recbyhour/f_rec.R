library(tidyverse)
library(dplyr)
library(readxl)
library(fs)
library(lubridate)
library(tidyr)


## CARGA DE ARCHIVOS###

registros <- list.files(path = 'D:/mcsalvadorg/onedrive/Documentos/GIT_HUB/Records in R/Recbyhour', pattern = 'xlsx')
indice<-12


bitacora12 <-map_df(registros[indice], read_excel, .id = registros[indice] )
#FORMATO DE COLUMNAS#
nombrefila<-colnames(bitacora12[1,1])
bitacora12[,1]<-nombrefila
colnames(bitacora12)<-c("ARCHIVO", "ETAPA", "FECHA_INICIO", "FECHA_FIN", "HORA_INICIO", "HORA_FIN", "FECHA")

rm(fecha_hora, hora, hora_minuto, minutos)
fecha_hora<-ymd_hms(bitacora12$"HORA FIN")
hora <- hour(fecha_hora)
minutos <- minute(fecha_hora)
hora_minuto <- paste(hora, minutos, sep = ":")

hora_minuto[24]<- "0:00"
bitacora12$"HORA FIN"<-hora_minuto


reemplazo<-which(bitacora12$"HORA FIN" == 0)
bitacora12$"HORA FIN"[reemplazo]<-"00:00"
bitacora12$"HORA FIN" <- str_trim(bitacora12$"HORA FIN")
bitacora12$FECHA<-paste(bitacora12$`FECHA FIN`, bitacora12$`HORA FIN`)

####### CONSOLIDA LISTA ####

#lista <- NULL
#agregalista <- NULL
bitacora<-"bitacora12"
ultimafecha<-max(bitacora12$FECHA)
nombrecol<-colnames(bitacora12)
etapa<-as.character(bitacora12[2,2])

palabra_a_extraer <- c("C2","C1")
texto<-nombrecol[1]
coincidencia_TIPOC <- str_c(palabra_a_extraer, collapse = "|")
tieneC <- str_subset(texto, coincidencia_TIPOC)
tipoC <- str_extract(tieneC, coincidencia_TIPOC)


lista<-c(bitacora,etapa,tipoC,ultimafecha)
agregalista<- rbind(agregalista,lista)

nombre_columnas <- c("Bitacora",
                     "Etapa",
                     "Corrida",
                     "Fecha")

colnames(agregalista) <- nombre_columnas
agregalista<-data.frame(agregalista)

#bitacora2$HORA_INICIO<-as.character(bitacora2$HORA_INICIO)


####FUNCION ELEGIR MAS RECIENTE####
funcion_mas_recientes<-function(lista_de_bitacoras){
                        load("D:/mcsalvadorg/onedrive/Documentos/GIT_HUB/Records in R/Recbyhour/W_records.RData")
                        mas_recientes<-lista_de_bitacoras %>% group_by(Etapa,Corrida) %>% slice_head(n = 1)
                        listarecientes<-mas_recientes$Bitacora
                        df<-paste0(listarecientes)
                        df_list<-mget(df)
                        recientes_consolidados<-NULL
                        recientes_consolidados<-do.call(rbind, df_list)
                        return(recientes_consolidados)
}

recientes<-funcion_mas_recientes(agregalista)

lista_de_bitacoras<-agregalista

mas_recientes<-lista_de_bitacoras %>% group_by(Etapa,Corrida) %>% slice_head(n = 1)
listarecientes<-mas_recientes$Bitacora
df<-paste0(listarecientes)
df_list<-mget(df)
recientes_consolidados<-NULL
recientes_consolidados<-do.call(rbind, df_list)
return(recientes_consolidados)




rm(bitacora,ultimafecha,nombrecol,palabra_a_extraer,texto,coincidencia_TIPOC,tieneC,tipoC,etapa)

bitacora1 <- bitacora %>%  mutate("HORA INICIO" = as.POSIXct("HORA INICIO", format = "%H:%M"))



bitacora1 %>% mutate(bitacora1, date=strptime(bitacora1$FECHA, format="%Y-%m-%d %H:%M:%S"))


combinar_registros <- bind_rows (data_registros)
head(combinar_registros, 30)
summary (combinar_registros)
porcentaje_nulos <- colMeans(is.na(combinar_registros))

write.csv(registros_consolidados, "registros_consolidados.csv")


datadir_ls(regexp = "xlsx")%>%
  map(read_excel)