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

plotContigencyTable <- function(summary_dt, variable1, variable2, show_rates = FALSE) {
    p <- summary_dt %>%
        .[, frequency_to_expected_frequency_capped := pmin(
            frequency_to_expected_frequency, 2
        )] %>% 
        ggplot(aes_string(x = variable1, y = variable2)) +
        geom_tile(aes(fill = frequency_to_expected_frequency_capped)) +
        scale_fill_gradientn(colours = rev(terrain.colors(10)))

    if (show_rates == TRUE) {
        p <- p + 
            geom_text(aes(label = scales::percent(frequency_rate)), fontface = "bold") +
            geom_text(
                aes(label = glue("({scales::percent(expected_frequency_rate)})")),
                vjust = 1.8, color = "red", fontface = "bold"
            )
    } else {
        p <- p + 
            geom_text(aes(label = scales::comma(frequency)), fontface = "bold") +
            geom_text(
                aes(label = glue("({scales::comma(round(expected_frequency))})")),
                vjust = 1.8, color = "red", fontface = "bold"
            )
    }
    
    p <- p +
        labs(
            title = bquote(Contingency~table~between~variables~bold(.(variable1))~and~bold(.(variable2))),
            subtitle = "Chi-squared: " ## TODO
        ) +
        theme_minimal() +
        theme(
            legend.position = "bottom"
        )

    ggsave(
        filename = file.path("figures", glue(
            "contingency_table_{variable1}_{variable2}_show_rates_is_{show_rates}.png"
        )),
        plot = p,
        width = 9, height = 9
    )

    p
}