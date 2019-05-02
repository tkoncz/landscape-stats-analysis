calculateExpectedFrequency <- function(freq1, freq2, total) {
    freq1 / total * freq2
}

calculateExpectedFrequencyRate <- function(freq1, freq2, total) {
    freq1 / total * freq2 / total
}

createSummaryDtForContingencyPlot <- function(dt, variable1, variable2) {
    dt %>%
        .[, .(frequency = .N), by = c(variable1, variable2)] %>%
        .[, (paste0("frequency_", variable1)) := sum(frequency), by = variable1] %>%
        .[, (paste0("frequency_", variable2)) := sum(frequency), by = variable2] %>%
        .[, `:=`(
            total_rows = sum(frequency),
            frequency_rate = frequency / sum(frequency)
        )] %>%
        .[, expected_frequency := calculateExpectedFrequency(
            get(paste0("frequency_", variable1)),
            get(paste0("frequency_", variable2)),
            total_rows
        )] %>%
        .[, expected_frequency_rate := calculateExpectedFrequencyRate(
            get(paste0("frequency_", variable1)),
            get(paste0("frequency_", variable2)),
            total_rows
        )] %>%
        .[, frequency_to_expected_frequency := frequency / expected_frequency] %>%
        .[, c(variable1, variable2, "frequency", "expected_frequency",
              "frequency_rate", "expected_frequency_rate",
              "frequency_to_expected_frequency"),
            with = FALSE
        ]
}

plotContigencyTable <- function(summary_dt,
                                variable1, variable2,
                                association_statistics,
                                show_rates = FALSE) {
    contingency_plot <- summary_dt %>%
        .[, frequency_to_expected_frequency_capped := pmin(
            frequency_to_expected_frequency, 2
        )] %>%
        ggplot(aes_string(x = variable1, y = variable2)) +
        geom_tile(aes(fill = frequency_to_expected_frequency_capped)) +
        scale_fill_gradientn(colours = rev(terrain.colors(10)))

    if (show_rates == TRUE) {
        contingency_plot <- contingency_plot +
            geom_text(aes(label = scales::percent(frequency_rate)), fontface = "bold") +
            geom_text(
                aes(label = glue("({scales::percent(expected_frequency_rate)})")),
                vjust = 1.8, color = "red", fontface = "bold"
            )
    } else {
        contingency_plot <- contingency_plot +
            geom_text(aes(label = scales::comma(frequency)), fontface = "bold") +
            geom_text(
                aes(label = glue("({scales::comma(round(expected_frequency))})")),
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
            legend.position = "bottom",
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
    pearson_chi_squared <- scales::comma(association_statistics$chisq_tests[2])
    pearson_p_value     <- format(association_statistics$chisq_tests[6], nsmall = 3)
    pearson_contingency_coeff <- round(association_statistics$contingency, digits = 3)
    cramer_v                  <- round(association_statistics$cramer, digits = 4)

    paste(
        glue("Pearson's X-squared: {pearson_chi_squared}, p-value: {pearson_p_value}"),
        glue("Pearson's Contingency Coeff.: {pearson_contingency_coeff}, Cramer's V: {cramer_v}"),
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
