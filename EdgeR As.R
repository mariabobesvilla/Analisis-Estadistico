if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("edgeR")
force = TRUE
library(edgeR)

setwd("C:/Users/Lenovo/OneDrive/Escritorio/TFG/R")
getwd()



x <- read.delim("header.loci.counts_bien_filterconMuscMax750.txt",row.names="loci_id", stringsAsFactors=FALSE)
gruposAs <- read.table("GruposAs.txt",header=TRUE,row.names = "sample")
tablaLoci <- select(x, -MpC_06_M01, -MpC_07_M02, -MpC_10_M02, -MpC_22_M01, -MpC_23_M01, -MpC_25_M01,-MpC_26_M01,-MpC_42_M02,-MpC_43_M02, -MpC_59_M01, -MpC_62_M02, -MpC_63_M01, -MpC_67_M01, -MpC_71_M01, -MpC_86_M01, -MpC_88_M01, -MpC_89_M01, -MpC_90_M01, -MpC_92_M01, -MpC_95_M01, -MpC_99_M01)
y <- DGEList(counts=tablaLoci ,group = t(gruposAs))
keep <- filterByExpr(y,gruposAs)   
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