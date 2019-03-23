suppressPackageStartupMessages(library("data.table"))
suppressPackageStartupMessages(library("magrittr"))
suppressPackageStartupMessages(library("raster"))
suppressPackageStartupMessages(library("glue"))
suppressPackageStartupMessages(library("ggplot2"))

invisible(sapply(list.files("R", full.names = TRUE), source))