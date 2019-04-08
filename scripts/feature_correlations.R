source("global.R")

ascii <- readData(file.path("data", "ascii"), refetch = FALSE)
convertVariableColsToFactor_(ascii)

map(combn(names(ascii)[c(-1, -2)], 2, simplify = FALSE)[1], ~{
    createSummaryDtForContingencyPlot(ascii, .x[[1]], .x[[2]]) %>%
        plotContigencyTable(.x[[1]], .x[[2]])    
})

ascii[, chisq.test(diffuz, fb_valtozatossag)]