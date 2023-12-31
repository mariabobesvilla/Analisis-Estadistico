setwd("C:/Users/Lenovo/OneDrive/Escritorio/TFG/R")

x <- read.delim("header.loci.counts_bien_filterconMuscMax750.txt",row.names="loci_id", stringsAsFactors=FALSE)
gruposPb <- read.table("GruposPb.txt",header=TRUE,row.names = "sample")
Tablaloci <- select(x, -MpC_09_M01, -MpC_22_M01, -MpC_24_M01,-MpC_26_M01,-MpC_34_M01,-MpC_40_M01, -MpC_41_M01, -MpC_42_M01, -MpC_42_M02,-MpC_43_M02, -MpC_44_M01, -MpC_50_M01, -MpC_62_M02, -MpC_63_M01, -MpC_66_M01, -MpC_67_M01, -MpC_68_M01, -MpC_70_M01, -MpC_73_M01, -MpC_75_M01, -MpC_84_M01, -MpC_85_M01,-MpC_86_M01, -MpC_88_M01,  -MpC_92_M01, -MpC_96_M01, -MpC_99_M01)
y <- DGEList(counts=Tablaloci ,group = t(gruposPb))
keep <- filterByExpr(y,gruposPb)   
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