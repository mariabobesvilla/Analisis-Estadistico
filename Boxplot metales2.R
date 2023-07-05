install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library("dplyr")
install.packages("tidyr") 
library("tidyr")
install.packages("patchwork")
library("patchwork")

setwd("C:/Users/Lenovo/OneDrive/Escritorio/TFG/R")

metalesMusculo <- read.csv("2022-09-01_Cuantif_Musculo_AsCdCrCuHgNiPbZn_LM_CBF .csv",header=TRUE, sep=";", dec = ",",row.names = "Sample.Name")
metalesHigado <- read.csv("2022-09-01_Cuantif_Higado_AsCdCrCuHgNiPbZn_LM_CBF - copia.csv",header=TRUE, sep=";", dec = ",",row.names = "Sample.Name")

metalesMusc <- metalesMusculo [, c(1,3,5,7,9,11,13,15) ]
columnasMusculo <- ggplot(metalesMusculo, aes(x=0, y=Cr, fill=Cr)) + 
  geom_col()
columnasMusculo 
metalesHig <- metalesHigado [, c(1,3,5,7,9,11,13,15) ]
columnasHigado <- geom_col(metalesHig)
columnasHigado


#MUSCULO

CrMusculo<- ggplot(metalesMusculo, aes(x=0, y=Cr, fill=Cr)) + 
            geom_boxplot() +
  scale_y_continuous(name = "Concentración en ppb") +  
  scale_x_discrete(name = "Cr") +        
  ggtitle("Concentración de Cr en músculo") +   
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.25))

 NiMusculo <- ggplot(metalesMusculo, aes(x=0, y=Ni, fill=Ni)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Concentración en ppb") +  
  scale_x_discrete(name = "Ni") +        
  ggtitle("Concentración de Ni en músculo") +   
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.25))
NiMusculo

CuMusculo <- ggplot(metalesMusculo, aes(x=0, y=Cu, fill=Cu)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Concentración en ppb") +  
  scale_x_discrete(name = "Cu") +        
  ggtitle("Concentración de Cu en músculo") +   
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.25))
CuMusculo

ZnMusculo <- ggplot(metalesMusculo, aes(x=0, y=Zn, fill=Zn)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Concentración en ppb") +  
  scale_x_discrete(name = "Zn") +        
  ggtitle("Concentración de Zn en músculo") +   
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.25))
ZnMusculo

AsMusculo <- ggplot(metalesMusculo, aes(x=0, y=As, fill=As)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Concentración en ppb") +  
  scale_x_discrete(name = "As") +        
  ggtitle("Concentración de As en músculo") +   
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.25))
AsMusculo 

CdMusculo <- ggplot(metalesMusculo, aes(x=0, y=Cd, fill=Cd)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Concentración en ppb") +  
  scale_x_discrete(name = "Cd") +        
  ggtitle("Concentración de Cd en músculo") +   
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.25))
CdMusculo

HgMusculo <- ggplot(metalesMusculo, aes(x=0, y=Hg, fill=Hg)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Concentración en ppb") +  
  scale_x_discrete(name = "Hg") +        
  ggtitle("Concentración de Hg en músculo") +   
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.25))
HgMusculo

PbMusculo <- ggplot(metalesMusculo, aes(x=0, y=Pb, fill=Pb)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Concentración en ppb") +  
  scale_x_discrete(name = "Pb") +        
  ggtitle("Concentración de Pb en músculo") +   
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.25))
PbMusculo

plotfinalMusculo <- (CrMusculo + NiMusculo) / (CuMusculo + ZnMusculo) /
  (AsMusculo + CdMusculo) / (HgMusculo + PbMusculo)
plotfinalMusculo

plotfinalMusculo <- (CrMusculo + NiMusculo) / (CuMusculo + ZnMusculo) /
  (AsMusculo + CdMusculo) / (HgMusculo + PbMusculo)
plotfinalMusculo

#medias y rango

CrMusc <- metalesMusculo [, c(1) ]
media.CrMusculo <- mean(CrMusc)
rango.CrMusculo <- range(CrMusc)

NiMusc <- metalesMusculo [, c(3) ]
media.NiMusculo <- mean(NiMusc)
rango.NiMusculo <- range(NiMusc)

CuMusc <- metalesMusculo [, c(5) ]
media.CuMusculo <- mean(CuMusc)
rango.CuMusculo <- range(CuMusc)

ZnMusc <- metalesMusculo [, c(7) ]
media.ZnMusculo <- mean(ZnMusc)
rango.ZnMusculo <- range(ZnMusc)

AsMusc <- metalesMusculo [, c(9) ]
media.AsMusculo <- mean(AsMusc)
rango.AsMusculo <- range(AsMusc)

CdMusc <- metalesMusculo [, c(11) ]
media.CdMusculo <- mean(CdMusc)
rango.CdMusculo <- range(CdMusc)

HgMusc <- metalesMusculo [, c(13) ]
media.HgMusculo <- mean(HgMusc)
rango.HgMusculo <- range(HgMusc)

PbMusc <- metalesMusculo [, c(15) ]
media.PbMusculo <- mean(PbMusc)
rango.PbMusculo <- range(PbMusc)



#HIGADO

CrHigado <- ggplot(metalesHigado, aes(x=0, y=Cr, fill=Cr)) +
geom_boxplot() +
  scale_y_continuous(name = "Concentración en ppb") +  
  scale_x_discrete(name = "Cr") +        
  ggtitle("Concentración de Cr en hígado") +   
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.25))
CrHigado

NiHigado <- ggplot(metalesHigado, aes(x=0, y=Ni, fill=Ni)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Concentración en ppb") +  
  scale_x_discrete(name = "Ni") +        
  ggtitle("Concentración de Ni en hígado") +   
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.25))
NiHigado

CuHigado <- ggplot(metalesHigado, aes(x=0, y=Cu, fill=Cu)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Concentración en ppb") +  
  scale_x_discrete(name = "Cu") +        
  ggtitle("Concentración de Cu en hígado") +   
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.25))
CuHigado

ZnHigado <- ggplot(metalesHigado, aes(x=0, y=Zn, fill=Zn)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Concentración en ppb") +  
  scale_x_discrete(name = "Zn") +        
  ggtitle("Concentración de Zn en hígado") +   
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.25))
ZnHigado

AsHigado <- ggplot(metalesHigado, aes(x=0, y=As, fill=As)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Concentración en ppb") +  
  scale_x_discrete(name = "As") +        
  ggtitle("Concentración de As en hígado") +   
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.25))
AsHigado

CdHigado <- ggplot(metalesMusculo, aes(x=0, y=Cd, fill=Cd)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Concentración en ppb") +  
  scale_x_discrete(name = "Cd") +        
  ggtitle("Concentración de Cd en hígado") +   
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.25))
CdHigado

HgHigado <- ggplot(metalesMusculo, aes(x=0, y=Hg, fill=Hg)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Concentración en ppb") +  
  scale_x_discrete(name = "Hg") +        
  ggtitle("Concentración de Hg en hígado") +   
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.25))
HgHigado

PbHigado <- ggplot(metalesHigado, aes(x=0, y=Pb, fill=Pb)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Concentración en ppb") +  
  scale_x_discrete(name = "Pb") +        
  ggtitle("Concentración de Pb en hígado") +   
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.25))
PbHigado 

plotfinalHigado <- (CrHigado  + NiHigado ) / (CuHigado  + ZnHigado) /
  (AsHigado  + CdHigado ) / (HgHigado  + PbHigado )
plotfinalHigado 

plotfinalHigado  <- (CrMusculo + NiMusculo) / (CuMusculo + ZnMusculo) /
  (AsHigado  + CdHigado ) / (HgHigado  + PbHigado )
plotfinalHigado 

#medias y rango

CrHig <- metalesHigado [, c(1) ]
media.CrH <- mean(CrHig)
rango.CrH <- range(CrHig)

NiHig <- metalesHigado [, c(3) ]
media.NiH <- mean(NiHig)
rango.NiH <- range(NiHig)

CuHig <- metalesHigado [, c(5) ]
media.CuH <- mean(CuHig)
rango.CuH <- range(CuHig)

ZnHig <- metalesHigado [, c(7) ]
media.ZnH <- mean(ZnHig)
rango.ZnH <- range(ZnHig)

AsHig <- metalesHigado [, c(9) ]
media.AsH <- mean(AsHig)
rango.AsH <- range(AsHig)

CrHig <- metalesHigado [, c(11) ]
media.CrH <- mean(CrHig)
rango.CrH <- range(CrHig)

HgHig <- metalesHigado [, c(13) ]
media.HgH <- mean(HgHig)
rango.HgH <- range(HgHig)

PbHig <- metalesHigado [, c(15) ]
media.PbH <- mean(PbHig)
rango.PbH <- range(PbHig)
