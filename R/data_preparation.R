readSampleData <- function(refetch = FALSE) {
    file_name <- file.path("data", "sample_data.csv")

    if(!refetch & file.exists(file_name)) {
        message("reading from .csv")
        sample_data <- fread(file_name)
    } else {
        message("reading from .ascii files")
        sample_data <- getSampleDataFromASCII()
        fwrite(sample_data, file_name)
    }

    sample_data
}

getSampleDataFromASCII <- function() {
    magassag <- raster(file.path("data", "ascii", "magassag.asc")) %>% 
        as.data.frame(xy = TRUE) %>% 
        as.data.table()

    message("Magassag:")
    print(magassag[, .N, by = is.na(magassag)])

    talaj <- raster(file.path("data", "ascii", "talaj.asc")) %>% 
        as.data.frame(xy = TRUE) %>% 
        as.data.table()

    message("Talaj:")
    print(talaj[, .N, by = is.na(talaj)])

    sample_data <- merge(talaj, magassag, by = c("x", "y"), all = TRUE) %>%
        .[!(is.na(talaj) & is.na(magassag))]
}