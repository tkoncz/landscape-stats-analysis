source("global.R")

ascii <- readData(file.path("data", "ascii"), refetch = FALSE) %>%
    convertVariableColsToFactor_()

variable_combinations <- combn(names(ascii)[c(-1, -2)], 2, simplify = FALSE)

walk(variable_combinations, ~{
    association_statistics <- assocstats(table(ascii[[.x[[1]]]], ascii[[.x[[2]]]]))

    summary_table <- createSummaryDtForContingencyPlot(ascii, .x[[1]], .x[[2]])
    plotContigencyTable(summary_table, .x[[1]], .x[[2]], association_statistics, show_rates = TRUE)
    plotContigencyTable(summary_table, .x[[1]], .x[[2]], association_statistics, show_rates = FALSE)
})
