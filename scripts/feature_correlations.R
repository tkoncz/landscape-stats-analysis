source("global.R")

ascii <- readData(file.path("data", "ascii"), refetch = FALSE) %>% 
    convertVariableColsToFactor_()

variable_combinations <- combn(names(ascii)[c(-1, -2)], 2, simplify = FALSE)

map(variable_combinations, ~{
    createSummaryDtForContingencyPlot(ascii, .x[[1]], .x[[2]]) %>%
        plotContigencyTable(.x[[1]], .x[[2]], show_rates = TRUE)
})
    
ascii[, chisq.test(diffuz, fb_valtozatossag)]
