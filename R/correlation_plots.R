plotContigencyTable <- function(summary_dt,
                                variable1, variable2,
                                association_statistics,
                                show_rates = FALSE) {
    contingency_plot <- summary_dt  %>%
        ggplot(aes_string(x = variable1, y = variable2)) +
        geom_tile(aes(fill = observed_expected_count_diff)) +
        scale_fill_gradient(low = "#ddd6f3", high = "#faaca8")

    if (show_rates == TRUE) {
        contingency_plot <- contingency_plot +
            geom_text(aes(label = scales::percent(observed_rate)), fontface = "bold") +
            geom_text(
                aes(label = glue("({scales::percent(expected_rate)})")),
                vjust = 1.8, color = "red", fontface = "bold"
            )
    } else {
        contingency_plot <- contingency_plot +
            geom_text(aes(label = scales::comma(observed_count)), fontface = "bold") +
            geom_text(
                aes(label = glue("({scales::comma(round(expected_count))})")),
                vjust = 1.8, color = "red", fontface = "bold"
            )
    }

    contingency_plot <- contingency_plot +
        labs(
            title = bquote(Contingency~table~between~variables~bold(.(variable1))~and~bold(.(variable2))),
            subtitle = createSubtitleFromAssociationStats(association_statistics)
        ) +
        theme_minimal() +
        theme(
            legend.position = "none",
            panel.grid = element_blank(),
            axis.ticks = element_blank()
        )

    ggsave(
        filename = file.path(
            "figures", createFileNameForSaving(variable1, variable2, show_rates)
        ),
        plot = contingency_plot,
        width = 9, height = 9
    )

    contingency_plot
}

createSubtitleFromAssociationStats <- function(association_statistics) {
    pearson_chi_squared       <- scales::comma(association_statistics[["pearson_chi_squared"]])
    p_value                   <- format(association_statistics[["p_value"]], nsmall = 3)
    pearson_contingency_coeff <- round(association_statistics[["pearson_contingency_coeff"]], digits = 3)
    cramers_v                 <- round(association_statistics[["cramers_v"]], digits = 4)

    paste(
        glue("Pearson's X-squared: {pearson_chi_squared}, p-value: {p_value}"),
        glue("Pearson's Contingency Coeff.: {pearson_contingency_coeff}, Cramer's V: {cramers_v}"),
        sep = "\n"
    )
}

createFileNameForSaving <- function(variable1, variable2, show_rates) {
    file_name <- glue("contingency_table_{variable1}_{variable2}")
    if (show_rates == TRUE) {
        file_name <- paste0(file_name, "_with_rates.png")
    } else {
        file_name <- paste0(file_name, "_with_absolute_numbers.png")
    }

    file_name
}

plotCramersV <- function(association_statistics) {
    p <- association_statistics %>%
        ggplot(aes(x = variable1, y = variable2)) +
            geom_tile(aes(fill = cramers_v)) +
            geom_text(aes(label = round(cramers_v,2))) +
            scale_fill_gradientn(
                colours = c("white", "#faaca8"), limits = c(0, 1),
                na.value = "white"
            ) +
            labs(
                title = "Heatmap of Cramer's V for all variable pairs",
                x = "",
                y = ""
            ) +
            theme_minimal() +
            theme(
                panel.grid = element_blank(),
                axis.ticks = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.text.y = element_text(hjust = 0)
            )


    ggsave(
        filename = file.path("figures", "cramers_v_heatmap.png"),
        plot = p, width = 10, height = 8
    )

    p
}
