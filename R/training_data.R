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
           mass = if_else(str_detect(resumen, "(?<!no )bloque\\B") &
                              !str_detect(resumen, "(Uber|importaciones|FIV)") &
                              sub_tipo_de_protesta!="Amenaza",
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "se concentra|[Cc]on una concentración"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "((realizan?|inician?) una?|(declaran?|mantienen?) en) (paro|huelga)"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "mantienen? ocupando"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "[Ll]uego de la manifestación"),
                          true = 1,
                          false = mass),
           prot_number = min_rank(resumen)) %>% 
    group_by(resumen) %>% 
    mutate(mass = max(mass),
           prot_entry = row_number(resumen)) %>% 
    ungroup() %>% 
    filter(prot_entry==1) %>% 
    select(-prot_entry)

prot4 <- cleaned_texts %>% 
    filter(file=="2011_04.txt") %>% 
    mutate(mass = if_else(str_detect(resumen, "(?<!no )bloque\\B") &
                              !str_detect(resumen, "[Aa]menaz"),
                          true = 1,
                          false = 0),
           mass = if_else(str_detect(resumen, "se concentra|[Cc]on una (concentración)"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "((realizan?|inician?) una?|(declaran?|mantienen?) en|[Cc]on una?) (paro|huelga)"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "mantienen? (ocupando|cerrado)"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "realizan? (el|la|una?) (manifestación|marcha|paro|huelga)"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "[Ll]uego de la manifestación"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "protestan? (frente|antes|en las afueras)"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "manifestación convocada días atrás"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "marchan?\\b"),
                          true = 1,
                          false = mass))

prot5 <- cleaned_texts %>% 
    filter(file=="2012_05.txt") %>% 
    mutate(mass = if_else(str_detect(resumen, "(?<!no )bloque\\B") &
                              !str_detect(resumen, "[Aa]menaz"),
                          true = 1,
                          false = 0),
           mass = if_else(str_detect(resumen, "se concentra|[Cc]on una (concentración)"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "((realizan?|inician?) una?|(declaran?|mantienen?) en|[Cc]on una?) (paro|huelga)"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "mantienen? (ocupando|cerrado)"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "realizan? (el|la|una?) (manifestación|marcha|paro|huelga)"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "[Ll]uego de la manifestación"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "protestan? (frente|antes|en las afueras)"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "manifestación convocada días atrás"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "marchan?\\b"),
                          true = 1,
                          false = mass))

