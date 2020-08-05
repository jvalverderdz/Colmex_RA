#mapa_agua_11.R
#Versión: 1.1
#Autor: Javier Valverde
#Insumos: Cuadros (coberturas recolecciòn de basura, disponibilidad y cobertura de agua).xls
#Productos: ZMVM_Agua1.png
#Descripción: Este mapa toma información del Excel y genera un mapa de porcentaje de acceso al agua
#             en la ZMVM, con la lista de los municipios y delegaciones

#Limpiar environment
rm(list =ls())

#Importar librerías
library(ggplot2)
library(readxl)
library(ggrepel)
library(dplyr)
library(formattable)
library(sp)
library(sf)
library(tmap)
library(tmaptools)
library(extrafont)
library(gridExtra)
library(gtable)
library(sysfonts)

#Importar fuentes y cargar opciones
loadfonts(device = "win")
options(digits = 2)

#Importar el archivo shp y quedarnos solo con lo útil
mex <- st_read("C:/Mapa Digital 6/Proyecto basico de informacion/marco geoestadistico nacional 2010/municipal.shp", stringsAsFactors = FALSE)
mex <- mex[,c("CVEGEO", "NOM_ENT", "geometry")]
  
#Establecer directorio e Importar datos
setwd("C:/Users/guard/Dropbox/BAses finanzas locales")
agua <- read_xls("Cuadros (coberturas recolecciòn de basura, disponibilidad y cobertura de agua).xls", sheet = "Dotación de agua", range = "C7:O86")

#Quedarnos solo con lo importante, renombrar variables y eliminar observaciones nulas  
agua <- agua[,c("Clave", "Delegaciones y municipios", "Diaria...13")]
names(agua) <- c("CVEGEO", "NOM_MUN", "Diaria")
agua <- agua[-c(1:3),]

#Quedarnos con los registros del archivo .shp de la ZMVM  
mex <- mex[mex$CVEGEO %in% agua$CVEGEO,]

#Juntar los archivos
mex_agua <- inner_join(mex, agua, by = "CVEGEO")

#Generar la variable de las categorías a mapear
mex_agua$grupo <- case_when(
  mex_agua$Diaria >=0 & mex_agua$Diaria < 20 ~ 5,
  mex_agua$Diaria >=20 & mex_agua$Diaria < 40 ~ 4,
  mex_agua$Diaria >=40 & mex_agua$Diaria < 60 ~ 3,
  mex_agua$Diaria >=60 & mex_agua$Diaria < 70 ~ 2,
  mex_agua$Diaria >=70 & mex_agua$Diaria <= 100 ~ 1
)
mex_agua$grupo <- factor(mex_agua$grupo, levels = c("1", "2", "3", "4", "5"))


#Generar las variables de los centroides de los shapes de los municipios para más adelante poner las etiquetas de CVEGEO
centroids <- st_coordinates(st_centroid(mex_agua))
mex_agua$centroids_X <- as.data.frame(centroids)$X
mex_agua$centroids_Y <- as.data.frame(centroids)$Y

#Generar archivo ggplot
map <- ggplot(mex_agua) + theme_void() +
  geom_sf(aes(fill = grupo), size = 0.0001, color = "gray75")+
  geom_text(data = mex_agua, aes(x = centroids_X, y = centroids_Y), size = 2,  label = mex_agua$CVEGEO) +
  scale_fill_manual(values = c("#575757", "#777777", "#9B9B9B", "#B9B9B9", "#DEDEDE"),
                    labels = c("70.00-100.00", "60.00-69.99", "40.00-59.99", "20.00-39.99", "0.00-19.99"))+
  theme(legend.position = "left",
        text = element_text(family="Times New Roman")) +
  guides(fill = guide_legend(title = "Percentages"))

#Generar dataframes de los nombres de los municipios para crear un dataframe que será mostrado junto al mapa
#(Se generan dos, cada uno con la mitad de las observaciones, porque se mostrarán en cuatro columnas)
muns_df1 <- agua[c(1:38),c("CVEGEO", "NOM_MUN")]
muns_df2 <- agua[c(39:76),c("CVEGEO","NOM_MUN")]
names(muns_df2) <- c("CVEGEO2", "NOM_MUN2")

#Juntar ambos dataframes
muns_df <- cbind(muns_df1, muns_df2)

#Establecer las opciones de diseño de la tabla
tg_theme <- ttheme_minimal(core = list(fg_params=list(cex = 0.7, hjust=0, x=0.1, fontfamily="Times New Roman"),
                                       padding=unit(c(3.5,1.2), "mm")))

#Establecer el título de la tabla y sus opciones de diseño
tg_title <- textGrob(expression(bold(underline("Boroughs and Municipalities"))), gp=gpar(fontsize=14, fontfamily="Times New Roman"))

#Generar la tabla de municipios y sus CVEGEO
muns_tg <- tableGrob(muns_df, theme = tg_theme, rows = NULL, cols=NULL)

#Agregar el título
muns_tg <- gtable_add_rows(
  muns_tg, 
  heights = grobHeight(tg_title) + unit(5,"mm"),
  pos = 0)
muns_tg <- gtable_add_grob(muns_tg, tg_title, 1, 1, 1, ncol(muns_tg))

#Generar el grid con mapa y tabla de municipios
output_map <- arrangeGrob(map, muns_tg, ncol=2)
output_map

#Guardar el mapa y la tabla en un png
ggsave(file="ZMVM_Agua1.png", output_map, width = 32, height = 20, units = "cm")