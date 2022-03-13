setwd('C:/Users/jeffery.cannon/Documents/GitHub/extras/spanner_troubleshoot/')
devtools::install_github('jbcannon/spanner')

library(lidR)
library(sf)
library(spanner)

las = readLAS('unseg2.laz')

boles = lidR::filter_poi(las, Intensity>40000)
tree_locs = get_raster_eigen_treelocs(boles, res=0.1, pt_spacing=0.025,
                                               dens_threshold = 0, eigen_threshold = 0.9,
                                               grid_slice_min=1.2, grid_slice_max=1.6,
                                               minimum_polygon_area = pi*0.005^2, output_location = tempdir()) #down to 1 cm dbh

las_ids = spanner::segment_graph(las, tree_locs,
                                 k = 50,
                                 distance.threshold = 0.3,
                                 subsample.graph = 0.15, 
                                 return.dense = TRUE)

#grab a slice & view it
las_ids_slice = filter_poi(las_ids, Z > 1 & Z < 2 & treeID != 0 & Intensity > 40000)
plot(las_ids_slice, color='treeID', backend='lidRviewer')

#convert treelocs to spatial
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

tree_locs_sp = st_as_sf(tree_locs, coords = c('X', 'Y'))

plot(tree_locs_sp$geometry, pch=16, cex=tree_locs_sp$Radius*10)
text(st_coordinates(tree_locs_sp), labels=tree_locs_sp$TreeID, col='red', cex=0.7)

plot(las_ids_slice, color='treeID')

sapply(1:nrow(tree_locs_sp),
       function(i) getmode(clip_roi(las_ids_slice,
                                    tree_locs_sp[i,],
                                    radius = tree_locs_sp[i,]$Radius*1.1)$treeID))



# ---> Plot LAS slice with ids
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


