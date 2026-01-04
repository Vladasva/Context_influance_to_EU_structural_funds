library(Hmisc)

sprend<- read.csv("/Users/vladasverkelis/Documents/Doktorantūra/Straipsnis_1/Context_influance_to_EU_structural_funds/Data/Context/all_2020.csv")

# character variables are converted to R factors
class(sprend)


# R package factoextra provides some easy-to-use functions to extract and visualize the output of multivariate data analyses,
# including 'PCA' (Principal Component Analysis), 'FAMD' (Factor Analysis of Mixed Data), 'MFA' (Multiple Factor Analysis) 
# and 'HMFA' (Hierarchical Multiple Factor Analysis) functions from different R packages.
# It contains also functions for simplifying some clustering analysis steps and provides 'ggplot2' - based elegant data visualization.
 
library(factoextra)
set.seed(123)

# dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
# x	- numeric matrix, data frame or "dist" object  (dmatrix - a symmetric dissimilarity matrix (n x n), specified instead of dist, which can be more efficient)
# method=  the distance measure:  "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski", p= power of the Minkowski distance. 
##########################################
require("cluster")
# sil <- silhouette(sprend[,3], dist(sprend[,-3], method="euclidian"))
# arba
# Pastaba: pozymiai income ir educ  statndartizuoti SAS programoje. 
sil <- silhouette(sprend$cluster, dist(sprend[c("ROL", "GE", "COC", "RQ", "VAA", "CPI", "FOP", "JI", "IUI", "PR", "DI", "DBP", "DBD", "SSB", "DWCP", "PMSI", "PT", "EC", "cluster")], method="euclidian"))
# dist(sprend[c("x", "y")], method="euclidian")


sil

fviz_silhouette(sil)

obj <- list(data = sprend[c("ROL", "GE", "COC", "RQ", "VAA", "CPI", "FOP", "JI", "IUI", "PR", "DI", "DBP", "DBD", "SSB", "DWCP", "PMSI", "PT", "EC", "cluster")], cluster = sprend$cluster)
str(obj)   
obj$data
obj$cluster

fviz_cluster(obj, stand=FALSE,  ellipse.type = "norm")+
  theme_minimal()

fviz_cluster(obj, stand=FALSE,  ellipse.type = "convex")+
  theme_minimal()
fviz_cluster(obj, stand=FALSE,  ellipse.type = "euclidian")+
  theme_minimal()


############################################################################
#  fviz_nbclust: Dertermining and Visualizing the Optimal Number of Clusters
############################################################################
set.seed(123)



# Optimal number of clusters in the data
# ++++++++++++++++++++++++++++++++++++++
# Examples are provided only for kmeans, but
# you can also use cluster::pam (for pam) or
#  hcut (for hierarchical clustering)

### Elbow method (look at the knee)
# Elbow method for kmeans
fviz_nbclust(sprend[c("ROL", "GE", "COC", "RQ", "VAA", "CPI", "FOP", "JI", "IUI", "PR", "DI", "DBP", "DBD", "SSB", "DWCP", "PMSI", "PT", "EC", "cluster")], kmeans, method = "wss", k.max = 7) +
  geom_vline(xintercept = 3, linetype = 2)

# Average silhouette for kmeans
fviz_nbclust(sprend[c("ROL", "GE", "COC", "RQ", "VAA", "CPI", "FOP", "JI", "IUI", "PR", "DI", "DBP", "DBD", "SSB", "DWCP", "PMSI", "PT", "EC", "cluster")], kmeans, method = "silhouette", k.max = 7)

### Gap statistic
library(cluster)
set.seed(123)
# Compute gap statistic for kmeans
# K.max -the maximum number of clusters to consider, must be at least two.
# B - integer, number of Monte Carlo (?bootstrap?) samples.
# we used B = 10 for demo. Recommended value is ~500


gap_stat <- clusGap(sprend[c("ROL", "GE", "COC", "RQ", "VAA", "CPI", "FOP", "JI", "IUI", "PR", "DI", "DBP", "DBD", "SSB", "DWCP", "PMSI", "PT", "EC", "cluster")], FUN = kmeans, nstart = 25,
                    K.max = 7, B = 1000)

# method=?  character string indicating how the ?optimal? number of clusters k^,

# is computed from the gap statistics (and their standard deviations), 
# or more generally how the location k^ of the maximum of f[k] should be determined.

# method= "globalmax" simply corresponds to the global maximum, i.e., is which.max(f)
# method= "firstmax"  gives the location of the first local maximum.
# 


print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

# Gap statistic for hierarchical clustering
gap_stat <- clusGap(sprend[c("ROL", "GE", "COC", "RQ", "VAA", "CPI", "FOP", "JI", "IUI", "PR", "DI", "DBP", "DBD", "SSB", "DWCP", "PMSI", "PT", "EC", "cluster")], FUN = hcut, K.max = 7, B = 1000)
fviz_gap_stat(gap_stat)























