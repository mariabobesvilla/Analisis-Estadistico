setwd("C:/Users/Lenovo/OneDrive/Escritorio/TFG/R")

x <- read.delim("loci_countsLvsM_750Max.txt",row.names="loci_id", stringsAsFactors=FALSE)
LvsM <- read.table("groupsLvsM.txt",header=TRUE,row.names = "loci_id")
tablaLoci <- select (x, -MpC_07_M02, -MpC_09_M01, -MpC_13_M01,-MpC_17_M01, -MpC_22_M01, -MpC_25_M01,-MpC_27_M01, -MpC_34_M01, -MpC_40_M01,-MpC_41_M01, -MpC_42_M01, -MpC_43_M02,  -MpC_59_M01, -MpC_62_M02, -MpC_67_M01, -MpC_68_M01, -MpC_69_M01, -MpC_70_M01, -MpC_71_M01, -MpC_73_M01,-MpC_74_M01, -MpC_76_M01, -MpC_77_M01, -MpC_75_M01, -MpC_78_M01, -MpC_79_M01, -MpC_80_M01, -MpC_81_M01,-MpC_82_M01, -MpC_84_M01,-MpC_85_M01, -MpC_86_M01, -MpC_88_M01, -MpC_89_M01,-MpC_90_M01, -MpC_92_M01, -MpC_94_M01,-MpC_95_M01, -MpC_96_M01, -MpC_97_M01,-MpC_98_M01,-MpC_99_M01)
y <- DGEList(counts=x ,group = t(LvsM))
keep <- filterByExpr(y,LvsM)   
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

