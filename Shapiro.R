setwd("C:/Users/Lenovo/OneDrive/Escritorio/TFG/R")

TODO <- read.delim("MetalesPERMANOVA.txt",row.names="Sample", dec="," ,stringsAsFactors=FALSE)
MUSCULO <- TODO [, c(1,2,3,4,5,6,7,8) ]
as.numeric(MUSCULO)
HIGADO <-TODO [, c(9,10,11,12,13,14,15,16) ]

shapiro.test(TODO$Cr.Musculo)
