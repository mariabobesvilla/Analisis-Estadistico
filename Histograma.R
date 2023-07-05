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
install.packages("gridExtra")
library("gridExtra")

setwd("C:/Users/Lenovo/OneDrive/Escritorio/TFG/R")

metalesMusculo <- read.csv("2022-09-01_Cuantif_Musculo_AsCdCrCuHgNiPbZn_LM_CBF .csv",header=TRUE, sep=";", dec = ",",row.names = "Sample.Name")

histCr <- hist(metalesMusculo$Cr,freq=FALSE,col="lightcyan",
               main="Distribución de las muestras de Cr",xlab="Concentración ppb",ylab="Frecuencia") +
  curve(dnorm(x,mean=mean(metalesMusculo$Cr),sd=sd(metalesMusculo$Cr)), from=0,to=350, 
        add=TRUE, col="blue", lwd=2)
histNi <- hist(metalesMusculo$Ni,freq=FALSE,col="lightcyan",
               main="Distribución de las muestras de Ni",xlab="Concentración ppb",ylab="Frecuencia") +
  curve(dnorm(x,mean=mean(metalesMusculo$Ni),sd=sd(metalesMusculo$Ni)), from=0,to=1000, 
        add=TRUE, col="blue", lwd=2)
histCu <- hist(metalesMusculo$Cu,freq=FALSE,col="lightcyan",
               main="Distribución de las muestras de Cu",xlab="Concentración ppb",ylab="Frecuencia") +
  curve(dnorm(x,mean=mean(metalesMusculo$Cu),sd=sd(metalesMusculo$Cu)), from=0,to=800, 
        add=TRUE, col="blue", lwd=2)
histZn <- hist(metalesMusculo$Zn,freq=FALSE,col="lightcyan",
               main="Distribución de las muestras de Zn",xlab="Concentración ppb",ylab="Frecuencia") +
  curve(dnorm(x,mean=mean(metalesMusculo$Zn),sd=sd(metalesMusculo$Zn)), from=1500,to=4000, 
        add=TRUE, col="blue", lwd=2)
histAs <- hist(metalesMusculo$As,freq=FALSE,col="lightcyan",
               main="Distribución de las muestras de As",xlab="Concentración ppb",ylab="Frecuencia") +
  curve(dnorm(x,mean=mean(metalesMusculo$As),sd=sd(metalesMusculo$As)), from=500,to=3000, 
        add=TRUE, col="blue", lwd=2)
histCd <- hist(metalesMusculo$Cd,freq=FALSE,col="lightcyan" ,
               main="Distribución de las muestras de Cd",xlab="Concentración ppb",ylab="Frecuencia") +
  curve(dnorm(x,mean=mean(metalesMusculo$Cd),sd=sd(metalesMusculo$Cd)), from=0,to=20, 
        add=TRUE, col="blue", lwd=2)
histHg <- hist(metalesMusculo$Hg,freq=FALSE,col="lightcyan",
               main="Distribución de las muestras de Hg",xlab="Concentración ppb",ylab="Frecuencia") +
  curve(dnorm(x,mean=mean(metalesMusculo$Hg),sd=sd(metalesMusculo$Hg)), from=0,to=400, 
        add=TRUE, col="blue", lwd=2)
histPb <- hist(metalesMusculo$Pb,freq=FALSE,col="lightcyan",
               main="Distribución de las muestras de Pb",xlab="Concentración ppb",ylab="Frecuencia") +
  curve(dnorm(x,mean=mean(metalesMusculo$Pb),sd=sd(metalesMusculo$Pb)), from=0,to=20, 
        add=TRUE, col="blue", lwd=2)

require(gridExtra)
grid.arrange(histCr,histNi,histCu,histZn,histAs,histCd,histHg,histPb)
