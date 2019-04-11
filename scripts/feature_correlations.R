source("global.R")

ascii <- readData(file.path("data", "ascii"), refetch = FALSE) %>%
    convertVariableColsToFactor_()

variable_combinations <- combn(names(ascii)[c(-1, -2)], 2, simplify = FALSE)

map(variable_combinations, ~{
    chi_square_test <- chisq.test(ascii[[.x[[1]]]], ascii[[.x[[2]]]])
    summary_table <- createSummaryDtForContingencyPlot(ascii, .x[[1]], .x[[2]])

    summary_table %>% plotContigencyTable(.x[[1]], .x[[2]], chi_square_test, show_rates = TRUE)
    summary_table %>% plotContigencyTable(.x[[1]], .x[[2]], chi_square_test, show_rates = FALSE)
})
