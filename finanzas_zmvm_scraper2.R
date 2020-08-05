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
setwd("C:/Users/guard/Dropbox/BAses finanzas locales")

#Importar catálogo de municipios de la ZMVM, conservamos solo Edomex e Hidalgo
zmvm <- read_xlsx("Catalogo_ZMVM.xlsx")
cdmx <- read_xlsx("Catalogo_Alcaldias.xlsx")

#Se elimina la observación de DF y se agregan las de las delegaciones
zmvm <- zmvm[-1,]
zmvm <- rbind(zmvm, cdmx)

pob <- read.dbf("C:/Mapa Digital 6/Proyecto basico de informacion 2017/EIC2015/01ITER_2015_MUNICIPAL_Poblacion.dbf")
econ <- read.dbf("C:/Mapa Digital 6/Proyecto basico de informacion 2017/EIC2015/07ITER_2015_MUNICIPAL_CaracteristicasEconomicas.dbf")

pob$ID_MUN <- as.numeric(paste0(as.numeric(pob$CVE_ENT), pob$CVE_MPIO))
econ$ID_MUN <- as.numeric(pob$CVEGEOM)

pob_tot <- c()
pobreza_tot <- c()
pobreza_rel <- c()
)
for(i in 1:nrow(zmvm)){
  pob_tot[i] <- pob$POB_001[[pob$ID_MUN == zmvm$ID_MUN[[i]], "POB_001"]]
  pobreza_tot[i] <- 
}



#Exportar dataframes a excel
print("Guardando archivo de Excel...")

write_xlsx(data.frame(),"efipem_municipal.xlsx")
wb <- loadWorkbook("efipem_municipal.xlsx")
addWorksheet(wb, "Ingresos")
addWorksheet(wb, "Egresos")
removeWorksheet(wb, "Sheet1")

writeData(wb, "Ingresos", output_ingresos)
writeData(wb, "Egresos", output_egresos)

saveWorkbook(wb, "efipem_municipal.xlsx", overwrite = TRUE)

print("Proceso de generación de bases terminado :)")