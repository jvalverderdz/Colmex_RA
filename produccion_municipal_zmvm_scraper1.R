#finanzas_zmvm_scraper.R
#Descripción: Script para scrapear los datos de finanzas municipales de los municipios de la ZMVM
#A partir de los archivos de csv de las finanzas municipales de cada año, y alojarlos en un excel de resultados
#@Author: Javier Valverde
#Versión: 2.0
#Actualización: Esta versión agrega información del Distrito Federal (a nivel agregado, no por delegaciones)
rm(list = ls())

library(readxl)
library(writexl)
library(openxlsx)
library(dplyr)
library(foreign)

#Establecer directorio
setwd("D:/Javier/Documents/Colmex - CEDUA/Alejandra Trejo Nieto/Produccion Municipal ZMVM")

#Importar catálogo de municipios de la ZMVM, conservamos solo Edomex e Hidalgo
zmvm <- read_xlsx("Catalogo_ZMVM.xlsx")
cdmx <- read_xlsx("Catalogo_Alcaldias.xlsx")

zmvm <- rbind(zmvm, cdmx)


#Importar base de producción bruta
pbm <- read_xlsx("Consulta_CE_04-14.xlsx", sheet = "Hoja1")

pbm$ID_ENTIDAD <- as.numeric(substr(pbm$ENTIDAD,1,2))
pbm$ID_MUN <- as.character(substr(pbm$MUNICIPIO,1,3))

pbm$ID_MUN <- paste0(pbm$ID_ENTIDAD, pbm$ID_MUN)

pbm$ID_MUN[pbm$ID_ENTIDAD == 9 & is.na(pbm$MUNICIPIO)] <- 9001

pbm$ENTIDAD <- substr(pbm$ENTIDAD, 4, nchar(pbm$ENTIDAD))
pbm$MUNICIPIO <- substr(pbm$MUNICIPIO, 5, nchar(pbm$MUNICIPIO))

no_zmvm <- numeric()
for(n in 1:nrow(pbm)){
  if(!(pbm$ID_MUN[[n]] %in% zmvm$ID_MUN | pbm$ID_MUN[[n]] %in% cdmx$ID_MUN)) no_zmvm <- append(no_zmvm, n)
}
pbm <- pbm[-no_zmvm,]


for(year in c(2004, 2009, 2014)){
  pbm <- rbind(pbm, c("YEAR" = year, "ENTIDAD" = "Total Zona Metropolitana", "MUNICIPIO" = NA,
                      "PBT" = sum(as.numeric(pbm$PBT[pbm$YEAR == year])),
                      "ID_ENTIDAD" = NA, "ID_MUN" = NA))
}



#===================DEFLACTACIÓN========================
inpc <- read.csv("INPC_Base2018-1.csv")
inpc <- inpc[c(1:30),c(6,7)]
names(inpc) <- c("year", "indice")
inpc$year <- as.numeric(inpc$year)


for(n in 1:nrow(pbm)){
  pbm$PBT[n] <- (as.numeric(pbm$PBT[[n]])/inpc$indice[inpc$year == pbm$YEAR[[n]]])*100
}

pbm$PBT <- as.numeric(pbm$PBT)
pbm$YEAR <- as.numeric(pbm$YEAR)
pbm$ID_ENTIDAD <- as.numeric(pbm$ID_ENTIDAD)
pbm$ID_MUN <- as.numeric(pbm$ID_MUN)

pbm <- pbm[,c(1, 5, 6, 2, 3, 4)]

names(pbm) <- c("Año", "ID_ENTIDAD", "ID_MUNICIPIO", "ENTIDAD", "MUNICIPIO", "Producción Bruta Total")

#Exportar dataframes a excel
print("Guardando archivo de Excel...")

write_xlsx(pbm,"produccion_bruta_zmvm.xlsx")