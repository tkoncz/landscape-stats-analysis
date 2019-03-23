source("global.R") 

# Mapping Tables ----
MAGASSAG_DICT <- data.table(
    "Magasság Kód" = c(1, 2, 3, 4),
    "Magasság Kategória név" = c(
        "Síkság", "Magas teraszsíkság", "Dombság és hegységperem", "Középhegység"
    ),
    "Tengerszint feletti magasság" = c("-115", "115-230", "230-350", "350-")
)

TALAJ_DICT <- data.table(
    "Talaj Kód" = c(1, 2, 3, 4, 5, 6, 7, 0),
    "Talaj Kategória név" = c(
        "Váztalajok (Homoktalajok)", "Kőzethatású talajok", "Barna erdőtalajok",
        "Mezőségi talajok", "Szikes talajok", "Réti- és öntéstalajok",
        "Láptalajok", "Nincs adat (vízfelület, Budapest belterülete)"
    )
)

# Read landscape data ----
raw_sample <- readSampleData(refetch = FALSE)

# Add info from mapping tables ---- 
sample_w_categories <- merge(
    raw_sample, MAGASSAG_DICT, 
    by.x = "magassag", by.y = "Magasság Kód", all.x = TRUE
) %>%
    merge(
        TALAJ_DICT, by.x = "talaj", by.y = "Talaj Kód", all.x = TRUE
    )