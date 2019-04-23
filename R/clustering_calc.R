aggregateDtToResolution <- function(dt) {
    variable_cols <- getVariableCols()

    dt[, lapply(.SD, clacModeOfCategoricalVector),
        by = .(
            x = round(x / resolution) * resolution,
            y = round(y / resolution) * resolution
        ),
        .SDcols = variable_cols
    ]
}

getVariableCols <- function() {
    c(
        "diffuz",         "fb_valtozatossag", "felszinboritas",
        "kozet",          "kozponti",         "magassag",
        "nyitottsag",     "szegely",          "talaj",
        "termeszetesseg"
    )
}

clacModeOfCategoricalVector <- function(x) {
    names(sort(table(x), decreasing = TRUE))[[1]]
}
