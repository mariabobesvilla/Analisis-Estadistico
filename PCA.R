install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
setwd("C:/Users/Lenovo/OneDrive/Escritorio/TFG/R")

#PCA
MetalesFulton <- read.csv("MetalesFultonMicroplasticos.csv",header=TRUE, sep=";", dec = ",",row.names = "Sample")
PCA(MetalesFulton, scale.unit = TRUE, ncp = 5, graph = TRUE)
res.pca <- PCA(MetalesFulton, graph = FALSE)
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

iris.pca <- PCA(metal, graph = FALSE)

fviz_pca_ind(iris.pca,
             geom.ind = "point",
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,
             legend.title = "Groups"
)

fviz_pca_biplot(res.pca, repel = TRUE, labelsize = 3,
                col.var = "#2E9FDF", arrowsize = 0.9,
                col.ind = "#696969")

#PCA con clusters para hacer los grupos
metal_pca<-prcomp(MetalesFulton,center=TRUE,scale=TRUE)
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
write.table(kmeanDISC,"disc2kOnlyMetals.txt")
write.table(groups, "groups2_remove.txt")
groups3<-read.table("groups3k.txt",header=TRUE,row.names = "samples")
groups3<-read.table("disc2kOnlyMetals.txt",header=TRUE,row.names = "samples")
groups3<-read.table("groups3k_nocentre.txt",header=TRUE,row.names = "samples")
groups<-groups3

#MDS
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

