if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("edgeR")
force = TRUE
library(edgeR)

setwd("C:/Users/Lenovo/OneDrive/Escritorio/TFG/R")
getwd()

x <- read.delim("header.loci.counts_bien_filterconMuscMax750.txt",row.names="loci_id", stringsAsFactors=FALSE)
gruposHg <- read.table("GruposHg.txt",header=TRUE, row.names="sample")
tablaLoci <- select(x, -MpC_01_M01, -MpC_22_M01,-MpC_42_M02,-MpC_43_M02, -MpC_50_M01, -MpC_51_M01, -MpC_59_M01, -MpC_62_M02, -MpC_67_M01, -MpC_70_M01, -MpC_73_M01, -MpC_77_M01, -MpC_78_M01, -MpC_82_M01, -MpC_86_M01, -MpC_88_M01, -MpC_94_M01)
y <- DGEList(counts=tablaLoci ,group = t(gruposHg))
keep <- filterByExpr(y,gruposHg)   
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
plotBCV(y)
plotMDS(y)
write.table(qlf$table,"significLOCI.txt")
rownames(design) <-colnames(y)
fit <- glmFit(y)
lrt <- glmLRT(fit)
topTags(lrt)
FDR<- p.adjust(lrt$table$PValue, method="BH")
sum(FDR < 0.05)
summary(decideTests(lrt))
lrt
plotMD(lrt)
