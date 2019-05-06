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

plotShareOfVariableValuesAcrossClusters <- function(ascii_w_clusters,
                                                    preferred_num_cluster) {
    cluster_col   <- paste0("cluster_", preferred_num_cluster)
    share_of_variable_values_in_clusters <- prepareDataForShareOfVariableValuesAcrossClustersPlot(
        ascii_w_clusters, cluster_col, preferred_num_cluster
    )

    p <- share_of_variable_values_in_clusters %>%
        ggplot(aes_string(x = cluster_col, y = "share_in_cluster", fill = "variable_value")) +
            geom_col() +
            facet_grid(. ~ variable_name) +
            viridis::scale_fill_viridis(discrete = TRUE) +
            scale_y_continuous(breaks = c(0, .25, .50, .75, 1)) +
            coord_flip() +
            labs(
                title = "Share of Variable Values Across Clusters",
                x = "Assigned Cluster",
                y = "Share in Cluster"
            ) +
            theme_minimal() +
            theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.y = element_blank(),
                legend.position = "none",
                axis.text.x = element_text(angle = 45, hjust = 1)
            )

    ggsave(
        filename = file.path("figures", "share_of_variable_values_across_clusters.png"),
        plot = p,
        width = 12, height = 9
    )

    p
}

prepareDataForShareOfVariableValuesAcrossClustersPlot <- function(ascii_w_clusters,
                                                                  cluster_col,
                                                                  preferred_num_cluster) {
    variable_cols <- getVariableCols()
    map(variable_cols, ~{
        ascii_w_clusters %>%
            .[, .N, by = c(.x, cluster_col)] %>%
            .[, .(get(.x), N / sum(N)), by = c(cluster_col)] %>%
            setnames(c(cluster_col, .x, "share_in_cluster")) %>%
            melt(
                id.vars = c(cluster_col, "share_in_cluster"),
                variable.name = "variable_name",
                value.name = "variable_value",
                value.factor = TRUE
            ) %>%
            .[, (cluster_col) := factor(get(cluster_col), levels = preferred_num_cluster:1)] %>%
            .[, variable_value := as.factor(variable_value)]
    }) %>% rbindlist()
}
