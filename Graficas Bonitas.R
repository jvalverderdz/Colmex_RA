library(readxl)
library(ggplot2)
library(sysfonts)
library(extrafont)
library(ggrepel)
library(scales)

loadfonts(device = "win")


#===================Grafica de Cuadro 1.4 IMI por entidad y estrato socioeconomico=======
c14 <- read_excel("CEDUA/Julieta Perez Amador/Cuadro1.4.xlsx")
names(c14) <- c("Entidad", "Total", "Muy.Bajo", "Bajo", "Medio.Alto")
c14$Entidad <- factor(c14$Entidad, levels = unique(c14$Entidad))

cols <- c("Total" = "grey7", "Medio a Alto" = "green4", "Bajo" = "darkblue", "Muy Bajo" = "orangered2")
shapes <- c("Medio a Alto" = 24, "Bajo" = 18, "Muy Bajo" = 25)

ggplot(data = c14, aes(x = Entidad, group = 1)) +
  geom_point(aes(y = Medio.Alto, shape = "Medio a Alto"), size = 4, fill = "green4", colour = "green4") +
  geom_point(aes(y = Bajo, shape = "Bajo"), size = 5, fill = "darkblue", colour = "darkblue") +
  geom_point(aes(y = Muy.Bajo, shape = "Muy Bajo"), size = 4,  fill = "orangered2", colour = "orangered2") +
  geom_line(aes(y = Total, colour = "Total"), size = 1) +
  ylab("Tasa") +
  scale_color_manual(name = "Total", values = cols) +
  scale_shape_manual(name = "Estrato", values = shapes) +
  ggtitle("Tasas de matrimonio infantil por entidad federativa y estrato socioeconómico del hogar") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

library(readxl)
library(ggplot2)

c14 <- read_excel("CEDUA/Julieta Perez Amador/Cuadro1.4.xlsx")
names(c14) <- c("Entidad", "Total", "Muy.Bajo", "Bajo", "Medio.Alto")
c14$Entidad <- factor(c14$Entidad, levels = unique(c14$Entidad))

cols <- c("Total" = "grey7", "Medio a Alto" = "green4", "Bajo" = "darkblue", "Muy Bajo" = "orangered2")
shapes <- c("Medio a Alto" = 24, "Bajo" = 18, "Muy Bajo" = 25)

ggplot(data = c14, aes(x = Entidad, group = 1)) +
  geom_line(aes(y = Total, size = "Total")) +
  geom_point(aes(y = Medio.Alto, shape = "Medio a Alto", fill = "Medio a Alto", colour = "Medio a Alto"), size = 4) +
  geom_point(aes(y = Bajo, shape = "Bajo", fill = "Bajo", colour = "Bajo"), size = 5) +
  geom_point(aes(y = Muy.Bajo, shape = "Muy Bajo", fill = "Muy Bajo", colour = "Muy Bajo"), size = 4) +
  ylab("Tasa") +
  scale_size_manual(name = "Total", values = 1.5) +
  scale_shape_manual(name = "Estrato", values = shapes) +
  scale_color_manual(name = "Estrato", values = cols) +
  scale_fill_manual(name = "Estrato", values = cols) +
  ggtitle("Tasas de matrimonio infantil por entidad federativa y estrato socioeconómico del hogar") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(size = "") +
  theme(text = element_text(colour = "black", family = "Baskerville Old Face", face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5)) + guides(size = guide_legend(title = NULL))




#==================Grafica 1.5 IMI segun % de preparatoria y estrato medio===========
a <- read_excel("Colmex - CEDUA/Julieta Perez Amador/PreparatoriaEM_Todos.xlsx")
b <- read_excel("Colmex - CEDUA/Julieta Perez Amador/PreparatoriaEM_Tops.xlsx")



ggplot(b, aes(x = Preparatoria, y = Estrato.Medio)) +
  geom_label_repel(aes(label = Entidad, fill = TMI), size = 4, point.padding = NA, label.size = 0) +
  scale_fill_gradient(low = "green3",  high = "red1", space = "Lab" ) +
  theme_bw() + ylab("Estrato Medio") +
  ggtitle("Porcentaje de mujeres de 20-24 años de edad que se unieron antes de cumplir 18 años,
          completaron al menos la preparatoria y pertencen al estrato socioeconómico medio o alto
          en las 5 entidades con mayor y menor tasa de matrimonio infantil") +
  theme(text = element_text(colour = "black", family = "Baskerville Old Face", face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5)) + guides(size = guide_legend(title = NULL))

ggplot(a, aes(x = Preparatoria, y = Estrato.Medio)) +
  geom_label_repel(aes(label = Entidad, fill = TMI), force = 0.1, size = 4, point.padding = NA, label.size = 0) +
  scale_fill_gradient(low = "green3",  high = "red1", space = "Lab" ) +
  theme_bw() + ylab("Estrato Medio") +
  ggtitle("Porcentaje de mujeres de 20-24 años de edad que se unieron antes de cumplir 18 años,
          completaron al menos la preparatoria y pertencen al estrato socioeconómico medio o alto") +
  theme(text = element_text(colour = "black", family = "Baskerville Old Face", face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5)) + guides(size = guide_legend(title = NULL))

#Graficas colorblind
ggplot(b, aes(x = Preparatoria, y = Estrato.Medio)) +
  geom_point(aes(size = TMI), alpha = 1.8) +
  scale_colour_viridis_d() +
  scale_size(range = c(min(a$TMI)/2, max(a$TMI)/2)) +
  theme_bw() + ylab("Estrato Medio") +
  geom_label_repel(aes(label = Entidad), size = 3, point.padding = NA, force = 0.1) +
  ggtitle("Porcentaje de mujeres de 20-24 años de edad que se unieron antes de cumplir 18 años,
          completaron al menos la preparatoria y pertencen al estrato socioeconómico medio o alto
          en las 5 entidades con mayor y menor tasa de matrimonio infantil") +
  theme(text = element_text(colour = "black", family = "Baskerville Old Face", face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(a, aes(x = Preparatoria, y = Estrato.Medio)) +
  geom_point(aes(size = TMI), alpha = 1.7) +
  scale_colour_viridis_d() +
  scale_size(range = c(min(a$TMI)/2, max(a$TMI)/2)) +
  theme_bw() + ylab("Estrato Medio") +
  geom_label_repel(aes(label = Entidad), size = 3, point.padding = NA, force = 0.1) +
  ggtitle("Porcentaje de mujeres de 20-24 años de edad que se unieron antes de cumplir 18 años,
          completaron al menos la preparatoria y pertencen al estrato socioeconómico medio o alto") +
  theme(text = element_text(colour = "black", family = "Baskerville Old Face", face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5))


#====Grafica 1.6 Niveles por cohorte

c <- read_excel("Colmex - CEDUA/Julieta Perez Amador/NivelesCohorte.xlsx")
names(c) <- c("edades", "s60_64", "s65_69", "s70_74", "s75_79", "s80_84", "s85_89","s90_94")

ggplot(c, aes(x = edades)) +
  geom_area(aes(y = s60_64), alpha = 1, fill = "gray50") +
  geom_area(aes(y = s65_69), alpha = 1, fill = "gold2") +  
  geom_area(aes(y = s70_74), alpha = 1, fill = "cyan2") +
  geom_area(aes(y = s75_79), alpha = 1, fill = "blue2") +
  geom_area(aes(y = s80_84), alpha = 1, fill = "maroon1") +
  geom_area(aes(y = s85_89), alpha = 1, fill = "yellow2") +
  geom_area(aes(y = s90_94), alpha = 1, fill = "red3")
  
  
  
  