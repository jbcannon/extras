devtools::install_github('bi0m3trics/spanner')
library(lidR)
library(sf)
library(spanner)

las = readLAS('unseg.laz')
writeLAS(las, 'C:/Users/jeffery.cannon/OneDrive - Joseph W. Jones Ecological Research Center/Desktop/unseg2.laz', index=TRUE)

boles = lidR::filter_poi(las, Intensity>40000)
tree_locs = get_raster_eigen_treelocs(boles, res=0.1, pt_spacing=0.025,
                                               dens_threshold = 0, eigen_threshold = 0.9,
                                               grid_slice_min=1.2, grid_slice_max=1.6,
                                               minimum_polygon_area = pi*0.005^2) #down to 1 cm dbh

las_ids = spanner::segment_graph(boles, tree_locs,
                                 k = 50,
                                 distance.threshold = 0.3,
                                 subsample.graph = 0.15, 
                                 return.dense = TRUE)

# LAS colored by tree ID is all mixed up. 
plot(las_ids, color='treeID', backend='lidRviewer')

# ---> Plot LAS slice with ids
las_ids_slice = filter_poi(las_ids, Z > 1.2 & Z < 1.6)
cols=sample(paste0(rainbow(max(las_ids_slice$treeID)), '11')); cols= c(NA, cols)
plot(Y~X, data = las_ids_slice@data, pch=16, cex=0.4, col=cols[las_ids_slice$treeID+1], asp=1)

# overlay tree centroid locations and IDS from segmented las
ids = lapply(unique(las_ids$treeID), function(i) apply(subset(las_ids_slice@data, treeID==i)[, c('X', 'Y')],2,mean))
ids= as.data.frame(do.call(rbind, ids))
ids$treeID = unique(las_ids$treeID)
ids = subset(ids, treeID != 0 )
text(Y~X, data= ids, labels=treeID, cex=0.7)
#some of these are out in no-mans land because the actual points are spread among multiple trees

#Now overlay the locations from tree_locs in RED
text(Y~X, data=tree_locs, labels=TreeID, col='red', offset=0.25, cex=0.7,pos=4)


