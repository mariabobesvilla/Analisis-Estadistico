install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
setwd("C:/Users/Lenovo/OneDrive/Escritorio/TFG/R")

MetalesFulton <- read.csv("MetalesFultonMicroplasticos.csv",header=TRUE, sep=";", dec = ",",row.names = "Sample")
view(MetalesFulton)
MetalesToxicos <- MetalesFulton [, c(1,6,7,8,9) ]
view(MetalesToxicos)

PCA(MetalesToxicos, scale.unit = TRUE, ncp = 5, graph = TRUE)
res.pca <- PCA(MetalesToxicos, graph = FALSE)
res.pca
get_eigenvalue(res.pca)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
var <- get_pca_var(res.pca)
var               
fviz_pca_var(res.pca, col.var = "black")
install.packages("corrplot")
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

iris.pca <- PCA(MetalesToxicos, graph = FALSE)

fviz_pca_ind(iris.pca,
             geom.ind = "point",
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,
             legend.title = "Groups"
)

fviz_pca_biplot(res.pca, repel = TRUE, labelsize = 3,
                col.var = "#2E9FDF", arrowsize = 0.9,
                col.ind = "#696969")

metal_pca<-prcomp(MetalesToxicos,center=TRUE,scale=TRUE)
summary(metal_pca)
metal_transform=as.data.frame(-metal_pca$x[,1:2])

fviz_nbclust(metal_transform, kmeans, method = 'wss')
fviz_nbclust(metal_transform, kmeans, method = 'silhouette')
fviz_nbclust(metal_transform, kmeans, method = 'gap_stat')

k = 2
kmeans_metal = kmeans(metal_transform, centers = k, nstart = 50)
fviz_cluster(kmeans_metal, data = metal_transform)
kmeanDISC<-kmeans_metal$cluster
groups<-kmeans_metal$cluster
groups<-as.data.frame(groups)
treatments<-factor(groups)
groups
write.table(kmeanDISC,"disc2kOnlyMetals.txt")
write.table(groups, "groups2_remove.txt")
groups3<-read.table("groups3k.txt",header=TRUE,row.names = "samples")
groups3<-read.table("disc2kOnlyMetals.txt",header=TRUE,row.names = "samples")
groups3<-read.table("groups3k_nocentre.txt",header=TRUE,row.names = "samples")
groups<-groups3

install.packages("magrittr")
library("magrittr")
install.packages("dplyr")
library("dplyr")
install.packages("ggpubr")
library("ggpubr")

mds<-MetalesFulton %>%
  dist()%>%
  cmdscale()%>%
  as_tibble()

install.packages("sm")
library(sm)
mds
colnames(mds) <- c("Dim.1", "Dim.2")
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(MetalesFulton),
          size = 1,
          repel = TRUE)
clust <- kmeans(mds, 2)$cluster %>%
  as.factor()

mds <- mds %>%
  mutate(groups = clust)

ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(MetalesFulton),
          palette = "jco",
          color = "groups",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

#EdgeR

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("edgeR")
force = TRUE
library(edgeR)

x <- read.delim("header.loci.counts_bien_filterconMuscMax750.txt",row.names="loci_id", stringsAsFactors=FALSE)
Metales4 <- read.table("2Grupos4Metales.txt",header=TRUE,row.names = "sample")
tablaLoci <- select (x, -MpC_34_M01, -MpC_40_M01,-MpC_36_M01, -MpC_42_M02, -MpC_43_M02,  -MpC_69_M01, -MpC_62_M02, -MpC_38_M01, -MpC_39_M01)
y <- DGEList(counts=tablaLoci ,group = t(Metales4))
keep <- filterByExpr(y,Metales4)   
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


