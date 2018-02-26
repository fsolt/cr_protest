library(tidyverse)
library(rvest)

# get Costa Rica file --------
prot_cr <- read_csv("https://www.protestas.iis.ucr.ac.cr/protestas/bd/costa_rica.csv")
write_csv(prot, "data-raw/protestas_costa_rica.csv")

# Nicaragua file
prot_nic <- read_csv("https://www.protestas.iis.ucr.ac.cr/protestas/bd/nicaragua.csv")
write_csv(prot_nic, "data-raw/protestas_nicaragua.csv")


format_iis_dataset <- function(filepath) {
    mass_protests <- c("Actos sobre la propiedad",
                       "Bloqueo",
                       "Huelga",
                       "Huelga de hambre",
                       "Marcha",
                       "Mitin o concentración",
                       "Paro",
                       "Toma de propiedad")
    
    iis0 <- read_csv(filepath, 
                     col_types = cols(.default = col_character(),
                                     ID = col_integer(),
                                     Fecha = col_date(format = "")))
    names(iis0) <- names(iis0) %>%
        tolower() %>% 
        str_replace_all(" ", "_")
    
    iis <- iis0 %>%
        mutate(mass = as.numeric(tipo_de_protesta %in% mass_protests),
               sub_tipo_de_protesta = if_else(is.na(sub_tipo_de_protesta),
                                              "ninguno", 
                                              sub_tipo_de_protesta)) %>% 
        group_by(fecha, resumen) %>% 
        mutate(mass = max(mass),
               prot_entry = row_number(resumen)) %>% 
        ungroup() %>% 
        filter(prot_entry==1) %>% 
        mutate(resumen5 = word(resumen, 1, 5)) %>% 
        group_by(fecha, cantón, demanda, resumen5) %>% 
        mutate(mass = max(mass),
               prot_entry = row_number(resumen)) %>% 
        ungroup() %>% 
        filter(prot_entry==1) %>% 
        select(-prot_entry, -resumen5)
    
    return(iis)
}

iis_cr <- format_iis_dataset("data-raw/protestas_costa_rica.csv")
iis_nic <- format_iis_dataset("data-raw/protestas_nicaragua.csv")

load("data/hand_checked.rda")

all_coded_cr <- iis_cr %>% 
    select(resumen, mass) %>% 
    bind_rows(hand_checked)

all_coded <- iis_cr %>% 
    bind_rows(iis_nic) %>% # add IIS Nicaragua observations
    select(resumen, mass) %>% 
    bind_rows(hand_checked)

save(iis_cr, iis_nic, hand_checked, all_coded_cr, all_coded, file = "data/training_data.rda")
