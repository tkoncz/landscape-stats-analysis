source("global.R")

# global params ----
resolution <- 1000
num_clusters <- c(2:16)

# read data ----
ascii <- readData(file.path("data", "ascii"), refetch = FALSE) %>%
    convertVariableColsToFactor_()

ascii_aggr_to_resolution <- aggregateDtToResolution(ascii)

# create clusters based on k-modes ----
kmodes_output <- map(num_clusters, ~{
    kmodes(as.matrix(ascii_aggr_to_resolution[, 3:12]), .x, iter.max = 10, weighted = FALSE)
}) %>% set_names(num_clusters)

# add clusters to the sample data ----
ascii_aggr_to_resolution_w_clusters <- copy(ascii_aggr_to_resolution)

iwalk(kmodes_output, ~{
    ascii_aggr_to_resolution_w_clusters[, paste0("cluster_", .y)  := .x[["cluster"]]]
})

# plot on map ----
plotClustersOnMap(ascii_aggr_to_resolution_w_clusters)

# plot elbow chart for within cluster distances ----
plotKModesElbowChart(kmodes_output, preferred_num_cluster = 7)
