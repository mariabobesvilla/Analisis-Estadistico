if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("edgeR")
force = TRUE
library(edgeR)

setwd("C:/Users/Lenovo/OneDrive/Escritorio/TFG/R")

x <- read.delim("header.loci.counts_bien_filterconMuscMax750.txt",row.names="loci_id", stringsAsFactors=FALSE)
maximosAs <- read.table("MaximosAs.txt",header=TRUE,row.names = "sample")
tablaLoci <- select (x,-MpC_02_M01, -MpC_06_M01, -MpC_07_M02, -MpC_10_M02, -MpC_13_M01,-MpC_16_M01, -MpC_22_M01, -MpC_23_M01, -MpC_24_M01,-MpC_25_M01,-MpC_26_M01,-MpC_27_M01, -MpC_36_M01,-MpC_38_M01,-MpC_39_M01,-MpC_40_M01,-MpC_42_M02,-MpC_43_M01,-MpC_43_M02, -MpC_44_M01, -MpC_50_M01, -MpC_59_M01, -MpC_62_M02, -MpC_63_M01, -MpC_67_M01, -MpC_69_M01, -MpC_70_M01, -MpC_71_M01, -MpC_75_M01, -MpC_76_M01, -MpC_84_M01,-MpC_86_M01, -MpC_88_M01, -MpC_89_M01, -MpC_90_M01, -MpC_92_M01, -MpC_93_M01, -MpC_94_M01,-MpC_95_M01, -MpC_96_M01, -MpC_97_M01, -MpC_99_M01)
y <- DGEList(counts=tablaLoci ,group = t(maximosAs))
keep <- filterByExpr(y,maximosAs)   
y <- y[keep,,keep.lib.sizes=FALSE]
y <- calcNormFactors(y) 
y<-estimateDisp(y)
fit <- glmQLFit(y)
qlf <- glmQLFTest(fit)

topTags(qlf)
FDR<- p.adjust(qlf$table$PValue, method="BH")
sum(FDR < 0.05)
sum(FDR >= 0.05)
summary(decideTests(qlf))

plotMD(qlf)
