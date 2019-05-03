getAssociationStatistics <- function(contingency_table, variable1, variable2) {
    association_statistics <- assocstats(contingency_table)

    data.table(
        variable1                 = variable1,
        variable2                 = variable2,
        pearson_chi_squared       = association_statistics[["chisq_tests"]][2],
        p_value                   = association_statistics[["chisq_tests"]][6],
        pearson_contingency_coeff = association_statistics[["contingency"]],
        cramers_v                 = association_statistics[["cramer"]]
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
