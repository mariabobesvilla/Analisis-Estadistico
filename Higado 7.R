library("FactoMineR")
library("factoextra")

setwd("C:/Users/Lenovo/OneDrive/Escritorio/TFG/R")
Higado7 <- read.table("Higado7.txt",sep = "\t", dec= "," ,header=TRUE,row.names = "Sample")
higado7 <- Higado7[, c(5,6,7,8) ]


PCA(higado7 , scale.unit = TRUE, ncp = 5, graph = TRUE)
res.pca <- PCA(higado7 , graph = FALSE)
res.pca
get_eigenvalue(res.pca)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
var <- get_pca_var(res.pca)
var

fviz_pca_var(res.pca, col.var = "black")

library(corrplot)

corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(res.pca, choice = "var", axes = 1:2)
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)
fviz_pca_ind(res.pca, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)

iris.pca <- PCA(higado7, graph = FALSE)

fviz_pca_ind(iris.pca,
             geom.ind = "point",
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,
             legend.title = "Groups"
)

fviz_pca_biplot(res.pca, repel = TRUE, labelsize = 3,
                col.var = "#2E9FDF", arrowsize = 0.9,
                col.ind = "#696969")

metal_pca<-prcomp(higado7,center=TRUE,scale=TRUE)
summary(metal_pca)
metal_transform=as.data.frame(-metal_pca$x[,1:2])

fviz_nbclust(metal_transform, kmeans, method = 'wss')
fviz_nbclust(metal_transform, kmeans, method = 'silhouette')
fviz_nbclust(metal_transform, kmeans, method = 'gap_stat')

k = 3
kmeans_metal = kmeans(metal_transform, centers = k, nstart = 50)
fviz_cluster(kmeans_metal, data = metal_transform)
kmeanDISC<-kmeans_metal$cluster
groups<-kmeans_metal$cluster

groups<-as.data.frame(groups)
treatments<-factor(groups)
groups


x <- read.delim("loci_countsLvsM_750Max.txt",row.names="loci_id", stringsAsFactors=FALSE)
Higado7Grupos <- read.table("Higado7Grupos.txt",header=TRUE,row.names = "sample")
tablaloci <- select (x, -MpC_23_M01,	-MpC_55_M01,	-MpC_70_M01,	-MpC_84_M01,	-MpC_89_M01,	-MpC_94_M01,	-MpC_95_M01)
y <- DGEList(counts=tablaloci ,group = t(Higado7Grupos))
keep <- filterByExpr(y,Higado7Grupos)   
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
plotMDS(y)
