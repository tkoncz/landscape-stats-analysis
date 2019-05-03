source("global.R")

ascii <- readData(file.path("data", "ascii"), refetch = FALSE) %>%
    convertVariableColsToFactor_()

variable_combinations <- combn(names(ascii)[c(-1, -2)], 2, simplify = FALSE)

association_statistics <- map(variable_combinations, ~{
    contingency_table <- table(ascii[[.x[[1]]]], ascii[[.x[[2]]]], useNA = "always")
    association_statistics <- getAssociationStatistics(
        contingency_table, variable1 = .x[[1]], variable2 = .x[[2]]
    )

    summary_table <- createSummaryDtForContingencyPlot(
        contingency_table, variable1 = .x[[1]], variable2 = .x[[2]]
    )
    plotContigencyTable(summary_table, .x[[1]], .x[[2]], association_statistics, show_rates = TRUE)
    plotContigencyTable(summary_table, .x[[1]], .x[[2]], association_statistics, show_rates = FALSE)

    association_statistics
}) %>% rbindlist()

association_statistics %>%
    rbind(data.table(variable1 = "diffuz", variable2 = "diffuz"), fill = TRUE) %>%
    plotCramersV()
