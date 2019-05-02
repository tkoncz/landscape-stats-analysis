# general packages
suppressPackageStartupMessages(library("data.table"))
suppressPackageStartupMessages(library("magrittr"))
suppressPackageStartupMessages(library("purrr"))
suppressPackageStartupMessages(library("glue"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("patchwork"))

# special packages for this project
suppressPackageStartupMessages(library("raster"))
suppressPackageStartupMessages(library("klaR"))
suppressPackageStartupMessages(library("vcd"))


invisible(sapply(list.files("R", full.names = TRUE), source))
