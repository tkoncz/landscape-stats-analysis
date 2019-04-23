plotClustersOnMap <- function(ascii_w_clusters) {
    cluster_plot_map <- ascii_w_clusters[, !getVariableCols()] %>%
        melt(
            id.vars = c("x", "y"),
            variable.name = "num_cluster",
            value.name = "assigned_cluster"
        ) %>%
        .[, num_cluster := as.integer(gsub("cluster_", "", num_cluster))] %>%
        ggplot(aes(x = x, y = y, color = as.factor(assigned_cluster))) +
        geom_point() +
        viridis::scale_color_viridis(discrete = TRUE) +
        scale_x_continuous(label = scales::comma) +
        scale_y_continuous(label = scales::comma) +
        facet_wrap(.~num_cluster, ncol = 5) +
        labs(title = glue("K-modes Results With Different Number of Clusters")) +
        theme_minimal() +
        theme(legend.position = "none")

    ggsave(
        filename = file.path("figures", "kmodes_on_map.png"),
        plot = cluster_plot_map,
        width = 16, height = 12
    )

    cluster_plot_map
}
