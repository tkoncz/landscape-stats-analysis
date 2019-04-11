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
                                chi_square_test,
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
            subtitle = createSubtitleFromChiSquareTest(chi_square_test)
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

createSubtitleFromChiSquareTest <- function(chi_square_test) {
    chi_squared <- scales::comma(chi_square_test[["statistic"]])
    p_value     <- format(chi_square_test[["p.value"]], nsmall = 6)

    glue("Chi-squared: {chi_squared}, p-value: {p_value}")
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
