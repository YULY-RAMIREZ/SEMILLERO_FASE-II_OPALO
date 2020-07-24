rm(list = ls()) #Limpiar el espacio de trabajo

install.packages("readxl")
install.packages("xlsx")

library(readxl)
library(xlsx)

install.packages("fclust")  
install.packages("factoextra") 
install.packages("cluster") 
library(fclust)            # Clustering fuzzy
library(factoextra)        # Extraer y visualizar resultados de Análisis Multivariante
library(cluster)           # Análisis de agrupamiento

install.packages("grid")   # Para construir gráficas en paneles
install.packages("gridExtra")
install.packages("ggplot2")
library(ggplot2)
library(gridExtra)          # construir matriz de gráficas


## Librerías para la georreferenciación de la información 
install.packages("rgdal") # Leer shp archivos, Tranformar datos espaciales
install.packages("rgeos") #Provee la interfaz Geometry Engin _Open source ('GEOUS')
install.packages("sp")    #Para datos espaciales
library(sp)                # Clases y métodos para datos espaciales
library(rgdal)              
library(rgeos)             


setwd(" ") ##Configurar la dirección en donde se desea guardar la información
# 1. CARGAR DATOS ----------------------------------------------------------------------------------

load(file.choose()) #### cargar datos.R

colnames(datos)

sub_datos <- datos[,c(7:31,36:42)]

# ESTANDARIZACIÓN DE LOS DATOS ------------------------------------------------------------------------
datos_stand <- scale(sub_datos)
class(datos_stand)

grafica <- fviz_pca_ind(prcomp(datos_stand), axes = c(1,2), 
                        title = "Análisis de Componentes Principales",
                        label = "all", invisible = "none", labelsize = 4,
                        col.ind = "cos2",
                        geom = c("point","text"), pointsize = 2) +
  theme_gray() +
  scale_color_gradient2(low="gray", mid="blue",
                        high="red", midpoint=0.6)


grafica

ggsave("grafica_PCA2010.png")  # Guardar la gráfica

# Clustering Fuzzy con el parametro fucificador m=2---------------------------------------------

set.seed(123)
t <- proc.time()
indice <- data.frame(matrix(NA, ncol = 5, nrow = 10))
colnames(indice) <- c("Cluster", "Indice_PC", "Indice_PE", "Indice_XB", "Indice_SIL.F")
for (k in 2:11) { 
  f <- FKM(sub_datos, k = k, m = 2, stand = 1, RS = 100)
  pc <- PC(f$U)
  pe <- PE(f$U)
  xb <- XB(f$Xca, f$U, f$H, f$m)
  sf <- SIL.F(f$Xca, f$U, alpha = 1)
  indice[k-1,1] <- k
  indice[k-1,2] <- pc
  indice[k-1,3] <- pe
  indice[k-1,4] <- xb
  indice[k-1,5] <- sf
}

proc.time()-t

write.csv(indice, file = "indice.csv")
save(indice, file = "indice.R")

# Gráficas de  los indices -----------------------------------------

indicePC_vs_cluster <- ggplot(indice, aes(x = indice$Cluster, y = indice$Indice_PC)) + 
  geom_line() + geom_point() +
  xlab("Grupo") +
  ylab("PC") +
  ggtitle("Grupo vs Indice_PC") +
  theme(axis.text=element_text(size = 7)) +
  scale_x_continuous(breaks = 2:11) +
  theme_gray()


indicePE_vs_Cluster <- ggplot(indice, aes(x = indice$Cluster, y = indice$Indice_PE)) + 
  geom_line() + geom_point(size =2) +
  xlab("Grupo") +
  ylab("PE") +
  ggtitle("Grupo vs Indice_PE") +
  theme(axis.text=element_text(size = 7)) +
  scale_x_continuous(breaks = seq(2,11,1)) +
  theme_gray()


indiceXB_vs_Cluster <- ggplot(indice, aes(x = indice$Cluster, y = indice$Indice_XB)) + 
  geom_line() + geom_point(size =2) +
  xlab("Grupo") +
  ylab("XB") +
  ggtitle("Grupo vs Indice_XB") +
  theme(axis.text=element_text(size = 7)) +
  scale_x_continuous(breaks = seq(2,11,1)) +
  theme_gray()

indiceSILF_vs_Cluster <- ggplot(indice, aes(x = indice$Cluster, y = indice$Indice_SIL.F)) + 
  geom_line() + geom_point(size =2) +
  xlab("Grupo") +
  ylab("SIL.F") +
  ggtitle("Grupo vs Indice_SIL.F") +
  theme(axis.text=element_text(size = 7)) +
  scale_x_continuous(breaks = seq(2,11,1)) +
  theme_gray()


grafica_indice <- grid.arrange(indicePC_vs_cluster, indicePE_vs_Cluster, indiceXB_vs_Cluster, indiceSILF_vs_Cluster,
                               top = "INDICES DE VALIDACIÓN AGRUPACIÓN ALGORITMO K-MEDIAS DIFUSO")


ggsave("grafica_indice.png", plot = grafica_indice,  width = 8.82, height = 6.51)


#-----------------------------------------------------------------------------------------------------------

# Agrupar los datos estandarizados según el mejor índice
set.seed(1234)
agrupamiento2 <- FKM(sub_datos, k = 2, m = 2, stand = 1, RS = 100)

# Tamaño del cluster 
cl.size(agrupamiento2$U)

# Agrupar con k = 3 y m =2
set.seed(1234)
agrupamiento3 <- FKM(sub_datos, k = 3, m = 2, stand = 1, RS = 100)
cl.size(agrupamiento3$U)

# Agrupar con k = 4 y m =2
set.seed(1234)
agrupamiento4 <- FKM(sub_datos, k = 4, m = 2, stand = 1, RS = 100)
cl.size(agrupamiento4$U)


save(agrupamiento4, file = "agrupamiento4.R")

# Asignación de cluster y grado de pertenencia de los municipios a los cluster
asignacion_cluster2 <- agrupamiento2$clus # Si se conforman dos grupos
asignacion_cluster3 <- agrupamiento3$clus # Si se conforman tres grupos
asignacion_cluster4 <- agrupamiento4$clus # Si se conforman cuatro grupos


#  INSPECCIÓN VISUAL DE LOS DATOS DESPUÉS DE DEFINIR EL NÚMERO DE GRUPOS -----------------------------------------------------------------------

N_datos_stand2 <- as.data.frame(cbind(datos_stand, asignacion_cluster2))

grafica_cluster2 <- fviz_pca_ind(prcomp(N_datos_stand2[,1:32]), axes = c(1,2), 
                                 title = "Visualización de dos agrupaciones",
                                 subtitle = "Dengue año 2010",
                                 habillage = N_datos_stand2$Cluster,
                                 addEllipses=TRUE, ellipse.level=0.95,
                                 label = "all", 
                                 geom = c("point","text"), pointsize = 2, legend = "right") +
  theme_gray()  +
  scale_color_brewer(palette="Set1")

grafica_cluster2

ggsave("grafica_Cluster2010.png", plot = grafica_cluster2)

#  INSPECCIÓN VISUAL DE LOS DATOS DESPUÉS DE DEFINIR EL NÚMERO DE GRUPOS -----------------------------------------------------------------------

N_datos_stand3 <- as.data.frame(cbind(datos_stand, asignacion_cluster3))

grafica_cluster3 <- fviz_pca_ind(prcomp(N_datos_stand3[,1:32]), axes = c(1,2), 
                                 title = "Visualización de tres agrupaciones",
                                 subtitle = "Dengue año 2010",
                                 habillage = N_datos_stand3$Cluster,
                                 addEllipses=TRUE, ellipse.level=0.95,
                                 label = "none", 
                                 geom = "point", pointsize = 2, legend = "right") +
  theme_gray()  +
  scale_color_brewer(palette="Set1") 


grafica_cluster3


#  INSPECCIÓN VISUAL DE LOS DATOS DESPUÉS DE DEFINIR EL NÚMERO DE GRUPOS -----------------------------------------------------------------------

N_datos_stand4 <- as.data.frame(cbind(datos_stand, asignacion_cluster4))


grafica_cluster4 <- fviz_pca_ind(prcomp(N_datos_stand4[,1:32]), axes = c(1,2), 
                                 title = "Visualización de cuatro agrupaciones",
                                 subtitle = "Dengue año 2010",
                                 habillage = N_datos_stand4$Cluster,
                                 addEllipses=TRUE, ellipse.level=0.95,
                                 label = "none", 
                                 geom = "point", pointsize = 2, legend = "right") +
  theme_gray()  +
  scale_color_brewer(palette="Set1") 


grafica_cluster4


grafica_red <- grid.arrange(grafica_cluster2, grafica_cluster3, grafica_cluster4, ncol = 2, nrow = 2)

ggsave("grafica_red.png", plot = grafica_red, width = 9.2, height= 5.1)

#-----------------------------------------------------------------------------------------------------------

# Características de la clusterización con tres clusters y fucificador = 2 ----------------

# Grado de pertenencia a los cluster
pertenencia <- agrupamiento3$U


# Identificar los centroides estandarizados
agrupamiento3$H

# Identificar los centroides utilizando las medidas de unidades generales
centros <- agrupamiento3$Hraw <- Hraw(agrupamiento3$X, agrupamiento3$H)

##Incluir Cluster en los datos iniciales (datos2010)
N_datos <- cbind(datos, asignacion_cluster3)
colnames(N_datos)[length(N_datos)] <- "grado pertenencia"

# Mezclar N_datos con datos adicionales --------------------
otros_datos <- as.data.frame(read_xlsx("/codigos.xlsx", sheet ="datos_municipios"))
colnames(otros_datos) <- c("Código", "Municipio", "Altitud.min", "Altitud.max", "Altitud.media", "Habitantes", "Núcleo provincial")
N2_datos <- merge(N_datos, otros_datos, by = "Código")

rownames(N2_datos) <-  N2_datos[,395]

N2_datos <- N2_datos[,-395]


#-----------------------------------------------------------------------------------------------------------

# Asociación de los cluster al mapa ######

sant<- readOGR(file.choose()) # Abrir el archivo MGN_Municipio.shp
names(sant)[9] <- "Código"

# Mezclar los datos de sant con N2_datos---------------------------------

datos_sant <- merge(sant, N2_datos, by ="Código")

# datos de los cluster para representarlos en el mapa-------------------------------------------------------
datos_cluster <- datos_sant[,c(1,2,5,8,14,16,17,404,405,408,410)]
cluster1 <- datos_cluster[datos_cluster$Cluster==1,]
cluster2<- datos_cluster[datos_cluster$Cluster==2,]
cluster3<- datos_cluster[datos_cluster$Cluster==3,]


writeOGR(cluster1, dsn = "/Grupo1.kml", layer = 'Grupo1', driver = "KML", dataset_options = c("NameField=OBJECTID"))

writeOGR(cluster2,dsn = "/Grupo2.kml", layer = 'Grupo2', driver = "KML", dataset_options = c("NameField=OBJECTID"))

writeOGR(cluster3,dsn = "/Grupo3.kml", layer = 'Grupo3', driver = "KML", dataset_options = c("NameField=OBJECTID"))

#-----------------------------------------------------------------------------------------------------------


# Guardar clusters con características para georreferenciar
Cluster1 <- subset(datos_sant@data, Cluster == 1)
Cluster2 <- subset(datos_sant@data, Cluster == 2)
Cluster3 <- subset(datos_sant@data, Cluster == 3)
save(Cluster1, Cluster2, Cluster3, file = "Cluster.R")

# Guardar grupos en archivo csv
grupo1 <- subset(N2_datos, Cluster == 1)
grupo2 <- subset(N2_datos, Cluster == 2)
grupo3 <- subset(N2_datos, Cluster == 3)


##### Guardar archivos generados

write.csv(centros, file = "centros.csv")
write.csv(pertenencia, file = "pertenencia.csv")

write.csv(grupo1, file = "Cluster1_datos.csv")
write.csv(grupo2, file = "Cluster2_datos.csv")
write.csv(grupo3, file = "Cluster3_datos.csv")
