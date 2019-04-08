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
        .[, c(variable1, variable2, "frequency", "expected_frequency",
              "frequency_rate", "expected_frequency_rate"), with = FALSE]
}

plotContigencyTable <- function(summary_dt, variable1, variable2) {
    p <- summary_dt %>%
        ggplot(aes_string(x = variable1, y = variable2, fill = "frequency_rate")) +
        geom_tile() +
        geom_text(aes(label = scales::comma(frequency))) +
        geom_text(
            aes(label = glue("({scales::comma(round(expected_frequency))})")),
            vjust = 1.8, color = "red"
        ) +
        labs(
            title = bquote(Contingency~table~between~variables~bold(.(variable1))~and~bold(.(variable2))),
            subtitle = "Chi-squared: ", ## TODO
            caption = ""
        ) +
        scale_fill_gradient(low = "white", high = "steelblue", guide = FALSE) + ## TODO
        # scale_colour_manual(
        #     values = c(frequency = "black", expected_frequency = "red"),
        #     name = NULL,
        #     labels = c("frequency", "(expected_frequency)")
        # ) + 
        theme_minimal() +
        theme(
            legend.position = "bottom"
        )

    ggsave(
        filename = file.path("figures", glue("contingency_table_{variable1}_{variable2}.png")),
        plot = p,
        width = 9, height = 9
    )

    p
}