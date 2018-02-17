library(tidyverse)
library(rvest)

# geography
cantons <- "https://en.wikipedia.org/wiki/Cantons_of_Costa_Rica" %>%
    read_html() %>%
    html_nodes('.wikitable') %>%
    html_table() %>%
    first() %>% 
    filter(!Name == "Costa Rica")

names(cantons) <- c("cantón", "provincia",
                    "pop11", "pop00", "pop_change",
                    "area", "pop_density", "incorporation")

canton_matches <- rep("\\1", length(cantons$cantón))
names(canton_matches) <- paste0(".*(", cantons$cantón, ").*")

province_matches <- cantons %>% 
    pull(provincia) %>%
    unique() %>% 
    paste0(".*(", ., ").*") %>% 
    setNames(rep("\\1", length(.)), .)

start_time <- Sys.time()
cleaned_texts <- cleaned_texts %>% 
    mutate(canton = str_replace_all(resumen, canton_matches),
           canton = if_else(canton != resumen, canton, NA_character_))
stop_time <- Sys.time()
stop_time - start_time # <30 min

start_time <- Sys.time()
cleaned_texts <- cleaned_texts %>% 
    mutate(provincia = str_replace_all(resumen, province_matches),
           provincia = if_else(provincia != resumen, provincia, NA_character_))
stop_time <- Sys.time()
stop_time - start_time # <3 min