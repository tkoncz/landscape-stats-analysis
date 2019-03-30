readData <- function(ascii_folder, refetch = FALSE) {
    file_name <- file.path(ascii_folder, "merged_data_from_asciis.csv")

    if(!refetch && file.exists(file_name)) {
        message("reading from .csv")
        ascii_data <- fread(file_name)
    } else {
        message("reading from .ascii files")
        ascii_data <- getDataFromASCII(ascii_folder)
        fwrite(ascii_data, file_name)
    }

    ascii_data
}

getDataFromASCII <- function(ascii_folder) {
    ascii_files <- list.files(
        path = ascii_folder, pattern = "\\.asc$", full.names = TRUE
    )

    attribute_list <- map(ascii_files, ~{
        current_variable <- gsub(".*/(.*)\\.asc$", "\\1", .x)
        dt <- raster(.x) %>% 
            as.data.frame(xy = TRUE) %>% 
            as.data.table()

        message(glue("{Sys.time()}, {.x}: "))
        print(dt[, .(
            row_count = scales::comma(.N), 
            NA_count  = scales::comma(sum(is.na(get(current_variable))))
        )])

        dt[!is.na(get(current_variable))]
    })

    reduce(attribute_list, merge, by = c("x", "y"), all = TRUE)
}