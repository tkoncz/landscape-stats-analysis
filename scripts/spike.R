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
ascii <- readData(file.path("data", "ascii"), refetch = FALSE)

# Add info from mapping tables ---- 
ascii_w_categories <- merge(
    ascii, MAGASSAG_DICT, 
    by.x = "magassag", by.y = "Magasság Kód", all.x = TRUE
) %>%
    merge(
        TALAJ_DICT, by.x = "talaj", by.y = "Talaj Kód", all.x = TRUE
    )

# Plot sample of data
sample_size <- 20000
seed <- 93

set.seed(seed)
ascii_w_categories %>%
    .[sample(.N, sample_size)] %>%
    .[, .(x, y, `Magasság Kategória név`, `Talaj Kategória név`)] %>%
    melt(id.vars = c("x", "y")) %>%  
    ggplot(aes(x = x, y = y, color = as.factor(value))) +
    geom_point() +
    facet_grid(.~variable) +
    scale_x_continuous(label = scales::comma) +
    scale_y_continuous(label = scales::comma) +
    labs(
        title = glue("Landscape categories based on a subsample of {scales::comma(sample_size)}")
    ) +
    theme_minimal()

ggsave(
    file.path("figures", glue("landscape_seed_{seed}_n_{sample_size}.png")),
    width = 16, height = 9
)