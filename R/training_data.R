library(tidyverse)
library(stringr)
library(rvest)

# get training file --------
if (!file.exists("data-raw/protestas.csv")) {
    prot <- read_csv("http://www.protestas.iis.ucr.ac.cr/protestas.csv")
    write_csv(prot, "data-raw/protestas.csv")
}

# reformat variable names
prot0 <- read_csv("data-raw/protestas.csv")
names(prot0) <- names(prot0) %>%
    tolower() %>% 
    make.names() %>% 
    str_replace_all("\\.", "_")

# identify mass protests
mass_protests <- c("Actos sobre la propiedad",
                   "Bloqueo",
                   "Huelga",
                   "Huelga de hambre",
                   "Marcha",
                   "Mitin o concentración",
                   "Paro",
                   "Toma de propiedad")

prot <- prot0 %>%
    mutate(mass = if_else(tipo_de_protesta %in% mass_protests, 
                          true = 1,
                          false = 0),
           sub_tipo_de_protesta = if_else(is.na(sub_tipo_de_protesta), "ninguno", sub_tipo_de_protesta),
           mass = if_else(str_detect(resumen, "bloque\\B") &
                              !str_detect(resumen, "(Uber|importaciones|FIV)") &
                              sub_tipo_de_protesta!="Amenaza",
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "se concentra"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "((realizan?|inician?) una?|(declaran?|mantienen?) en) (paro|huelga)"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "mantienen? ocupando"),
                          true = 1,
                          false = mass),
           prot_number = min_rank(resumen)) %>% 
    group_by(resumen) %>% 
    mutate(mass = max(mass),
           prot_entry = row_number(resumen)) %>% 
    ungroup() %>% 
    filter(prot_entry==1) %>% 
    select(-prot_entry)



