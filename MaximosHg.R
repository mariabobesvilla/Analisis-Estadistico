setwd("C:/Users/Lenovo/OneDrive/Escritorio/TFG/R")

x <- read.delim("header.loci.counts_bien_filterconMuscMax750.txt",row.names="loci_id", stringsAsFactors=FALSE)
maximosHg <- read.table("MaximosHg.txt",header=TRUE,row.names = "sample")
tablaLoci <- select (x,-MpC_01_M01, -MpC_02_M01, -MpC_07_M02, -MpC_13_M01,-MpC_17_M01, -MpC_22_M01, -MpC_24_M01,-MpC_26_M01,-MpC_27_M01, -MpC_34_M01, -MpC_36_M01,-MpC_38_M01, -MpC_40_M01,-MpC_42_M02, -MpC_43_M02, -MpC_44_M01, -MpC_50_M01, -MpC_51_M01, -MpC_59_M01, -MpC_62_M02, -MpC_63_M01,-MpC_66_M01, -MpC_67_M01, -MpC_68_M01, -MpC_70_M01, -MpC_71_M01, -MpC_73_M01, -MpC_76_M01, -MpC_77_M01, -MpC_78_M01, -MpC_79_M01, -MpC_80_M01,  -MpC_82_M01,-MpC_84_M01,-MpC_85_M01, -MpC_86_M01, -MpC_88_M01, -MpC_90_M01, -MpC_93_M01, -MpC_94_M01, -MpC_98_M01, -MpC_99_M01)
y <- DGEList(counts=tablaLoci ,group = t(maximosHg))
keep <- filterByExpr(y,maximosHg)   
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