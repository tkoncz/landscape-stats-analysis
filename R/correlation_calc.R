getAssociationStatistics <- function(contingency_table, variable1, variable2) {
    association_statistics <- assocstats(contingency_table)

    data.table(
        variable1 = variable1,
        variable2 = variable2,
        pearson_chi_squared = association_statistics[["chisq_tests"]][2],
        p_value = association_statistics[["chisq_tests"]][6],
        pearson_contingency_coeff = association_statistics[["contingency"]],
        cramers_v = association_statistics[["cramer"]]
    )
}

createSummaryDtForContingencyPlot <- function(contingency_table, variable1, variable2) {
    chisq_test <- chisq.test(contingency_table)
    observed <- chisq_test$observed %>%
        as.table() %>%
        as.data.table() %>%
        setnames(c(variable1, variable2, "observed_count"))

    expected <- chisq_test$expected %>%
        as.table() %>%
        as.data.table() %>%
        setnames(c(variable1, variable2, "expected_count"))

    summary_dt <- merge(
        observed, expected,
        all = TRUE, by = c(variable1, variable2)
    ) %>%
        .[, observed_rate := observed_count / sum(observed_count)] %>%
        .[, expected_rate := expected_count / sum(expected_count)] %>%
        .[, observed_expected_count_diff := abs(observed_count - expected_count)]

    summary_dt
}

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
