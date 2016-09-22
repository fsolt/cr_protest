library(tidyverse)
library(stringr)
library(rvest)

# get training file --------
if (!file.exists("data-raw/protestas.csv")) {
    prot <- read_csv("http://www.protestas.iis.ucr.ac.cr/protestas.csv")
    write_csv(prot, "data-raw/protestas.csv")
}

# reformat variable names
prot <- read_csv("data-raw/protestas.csv")
names(prot) <- names(prot) %>%
    tolower() %>% 
    make.names() %>% 
    str_replace_all("\\.", "_")

# identify mass protests
mass_protests <- c("Actos sobre la propiedad",
                   "Bloqueo",
                   "Huelga",
                   "Marcha",
                   "Mitin o concentración",
                   "Paro",
                   "Toma de propiedad")

prot <- prot %>%
    mutate(mass = if_else(tipo_de_protesta %in% mass_protests, 
                          true = 1,
                          false = 0),
           mass = if_else(str_detect(resumen, "bloque\\B") &
                              !str_detect(resumen, "(Uber|importaciones|FIV)"),
                          true = 1,
                          false = mass))

prot <- prot %>% 
    mutate(bloqueo = (tipo_de_protesta=="Bloqueo") %>% as.numeric(),
           bloqueo2 = str_detect(resumen, "bloque\\B") &
               !str_detect(resumen, "(Uber|importaciones|FIV)"))

# 