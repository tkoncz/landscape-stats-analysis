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

plotKModesElbowChart <- function(kmodes_output, preferred_num_cluster = NA_integer_) {
    within_cluster_distances <- imap(kmodes_output, ~{
        data.table(
            num_cluster = as.integer(.y),
            sum_within_cluster_distance = sum(.x[["withindiff"]])
        )
    }) %>% rbindlist()

    elbow_plot <- within_cluster_distances %>%
        ggplot(aes(x = num_cluster, y = sum_within_cluster_distance)) +
            geom_vline(xintercept = preferred_num_cluster, color = "red") +
            geom_point() +
            geom_line() +
            scale_y_continuous(
                labels = scales::comma, breaks = scales::pretty_breaks(5)
            ) +
            scale_x_continuous(
                labels = 0:within_cluster_distances[, max(num_cluster)],
                breaks = 0:within_cluster_distances[, max(num_cluster)]
            ) +
            expand_limits(x = 0) +
            labs(
                title    = "Elbow Chart for Different Number of Clusters",
                subtitle = "Based on the K-modes Clustering Method",
                x        = "Number of Clusters",
                y        = "Sum of Within Cluster Distances"
            ) +
            theme_minimal() +
            theme(
                panel.grid.minor = element_blank()
            )

    ggsave(
        filename = file.path("figures", "kmodes_elbow_chart.png"),
        plot = elbow_plot,
        width = 8, height = 6
    )

    elbow_plot
}
