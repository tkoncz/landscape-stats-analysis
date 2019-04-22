source("global.R")

# global params ----
resolution <- 1000
num_clusters <- c(2, 3, 5, 10)

# read data ----
ascii <- readData(file.path("data", "ascii"), refetch = FALSE) %>%
    convertVariableColsToFactor_()

variable_cols <- c(
    "diffuz", "fb_valtozatossag", "felszinboritas",
    "kozet", "kozponti", "magassag",
    "nyitottsag", "szegely", "talaj", "termeszetesseg"
)

ascii_sample <- ascii %>% copy() %>%
    .[, `:=`(
        x_ = round(x / resolution) * resolution,
        y_ = round(y / resolution) * resolution
    )] %>%
    .[,
        lapply(.SD, modeest::mlv, na.rm = TRUE, method = "mfv"),
        by = c("x_", "y_"),
        .SDcols = variable_cols
    ]
# https://stackoverflow.com/questions/5016085/calculating-the-mode-for-nominal-as-well-as-continuous-variables-in-r

# create clusters based on k-modes ----
kmodes_output <- map(num_clusters, ~{
    kmodes(as.matrix(ascii_sample[, 3:12]), .x, iter.max = 10, weighted = FALSE)
})

# add clusters to the sample data ----
ascii_sample_w_clusters <- copy(ascii_sample)

walk2(kmodes_output, num_clusters, ~{
    ascii_sample_w_clusters[, paste0("cluster_", .y)  := .x[["cluster"]]]
})

# plot on map ----
coordinate_borders <- ascii[, .(
    min_x = min(x), max_x = max(x), min_y = min(y), max_y = max(y)
)]

cluster_plots <- map(num_clusters, ~{
    ascii_sample_w_clusters %>%
        .[, c("x", "y", paste0("cluster_", .x)), with = FALSE] %>%
        setnames(old =  paste0("cluster_", .x), new = "assigned_cluster") %>%
        ggplot(aes(x = x, y = y, color = as.factor(assigned_cluster))) +
        geom_point(shape = 18, alpha = 0.5) +
        viridis::scale_color_viridis(discrete = TRUE) +
        scale_x_continuous(label = scales::comma) +
        scale_y_continuous(label = scales::comma) +
        expand_limits(
            x = c(coordinate_borders[["min_x"]], coordinate_borders[["max_x"]]),
            y = c(coordinate_borders[["min_y"]], coordinate_borders[["max_y"]])
        ) +
        labs(title = glue("K-modes results with {.x} clusters")) +
        theme_minimal() +
        theme(legend.position = "none")
})

ggsave(
    filename = file.path("figures", "kmodes.png"),
    plot = patchwork::wrap_plots(cluster_plots[1:2], ncol = 2),
    width = 16, height = 12
)
