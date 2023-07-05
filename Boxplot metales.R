install.packages("tidyverse")
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library("dplyr")
install.packages("tidyr") 
library("dplyr")
setwd("C:/Users/Lenovo/OneDrive/Escritorio/TFG/R")
getwd()
metalesHigado <- read.csv("2022-09-01_Cuantif_Higado_AsCdCrCuHgNiPbZn_LM_CBF - copia.csv",header=TRUE, sep=";", dec = ",",row.names = "Sample.Name")
metalesMusculo <- read.csv("2022-09-01_Cuantif_Musculo_AsCdCrCuHgNiPbZn_LM_CBF .csv",header=TRUE, sep=";", dec = ",",row.names = "Sample.Name")
metalesMusc <- metalesMusculo [, c(1,3,5,7,9,11,13,15) ]

metalesMusculoTidy <- metalesMusc %>% 
  pivot_longer(
    cols = "Cr.Conc....ppb..":"Pb.Conc....ppb..", 
    names_to = "metales", 
    values_to = "concentracionppb"
  )

boxplotMusc <- ggplot(metalesMusculoTidy , aes(x= metales, y= concentracionppb)) +
              geom_boxplot ()  
boxplotMusc 
