library(tidyverse)
library(rvest)

# get training file --------
if (!file.exists("data-raw/protestas.csv")) {
    prot <- read_csv("http://www.protestas.iis.ucr.ac.cr/protestas.csv")
    write_csv(prot, "data-raw/protestas.csv")
}

# Nicaragua file
if (!file.exists("data-raw/protestas_nicaragua.csv")) {
    prot_nic <- read_csv("https://www.protestas.iis.ucr.ac.cr/protestas.csv?search%5Bpais%5D=nicaragua")
    write_csv(prot_nic, "data-raw/protestas_nicaragua.csv")
}

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
        make.names() %>% 
        str_replace_all("\\.", "_")
    
    iis <- iis0 %>%
        mutate(mass = as.numeric(tipo_de_protesta %in% mass_protests),
               sub_tipo_de_protesta = if_else(is.na(sub_tipo_de_protesta),
                                              "ninguno", 
                                              sub_tipo_de_protesta)) %>% 
        group_by(fecha, cantón, demanda) %>% 
        mutate(mass = max(mass),
               prot_entry = row_number(resumen)) %>% 
        ungroup() %>% 
        filter(prot_entry==1) %>% 
        select(-prot_entry)
    
    return(iis)
}

iis <- format_iis_dataset("data-raw/protestas.csv")
iis_nic <- format_iis_dataset("data-raw/protestas_nicaragua.csv")

load("data/hand_checked.rda")

all_coded_cr <- iis %>% 
    select(resumen, mass) %>% 
    bind_rows(hand_checked)

all_coded <- iis %>% 
    bind_rows(iis_nic) %>% # add IIS Nicaragua observations
    select(resumen, mass) %>% 
    bind_rows(hand_checked)
