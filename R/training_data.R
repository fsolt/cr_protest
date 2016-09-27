library(tidyverse)
library(stringr)
library(rvest)

# get training file --------
if (!file.exists("data-raw/protestas.csv")) {
    prot <- read_csv("http://www.protestas.iis.ucr.ac.cr/protestas.csv")
    write_csv(prot, "data-raw/protestas.csv")
}

# reformat variable names
iis0 <- read_csv("data-raw/protestas.csv")
names(iis0) <- names(iis0) %>%
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

iis <- iis0 %>%
    mutate(mass = if_else(tipo_de_protesta %in% mass_protests, 
                          true = 1,
                          false = 0),
           sub_tipo_de_protesta = if_else(is.na(sub_tipo_de_protesta),
                                          "ninguno", 
                                          sub_tipo_de_protesta)) %>% 
    mutate(mass = if_else(str_detect(resumen, "((?<!no )bloque\\B)|(cierre de calles)|tortuguismo") &
                              !str_detect(resumen, "[Aa]menaz|[Aa]nuncian?\\b"),
                          true = 1,
                          false = 0),
           mass = if_else(str_detect(resumen, "(se concentran?)|([Cc]on una) concentración"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "((realizan?|inician?) una?|(declaran?|encuentran?) en|[Cc]on una?|(La|El)|este día se espera el|mantienen? (el|la|en)|cumple el \\b.*\\b día de) (paro|huelga)"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "el paro (de labores )?realizado"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "(mantienen? (ocupando|cerrado))|(se amarran?\\b)|(impiden? el acceso)"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "([Rr]ealizan?|[Dd]urante) (el|la|una?) (manifestación|marcha|paro|huelga|actividad pública)"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "(([Ll]uego de la)|([Cc]on una)|(realizan?)( una?)?) (manifestación|mitin|caravana)"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "protestan? (frente|antes|en las afueras|en las calles)"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "realización de una manifestación|manifestación convocada días atrás|se presentan en las oficinas"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "marchan?\\b|toman? las calles"),
                          true = 1,
                          false = mass),
           mass = if_else(str_detect(resumen, "cierran? (el portón|los portones)"),
                          true = 1,
                          false = mass)) %>% 
    group_by(resumen) %>% 
    mutate(mass = max(mass),
           prot_entry = row_number(resumen)) %>% 
    ungroup() %>% 
    filter(prot_entry==1) %>% 
    select(-prot_entry)

hand_checked <- c("2008_01.txt", 
                  "2009_02.txt", 
                  "2010_03.txt",
                  "2011_04.txt",
                  "2012_05.txt",
                  "2013_06.txt",
                  "2014_07.txt") %>% 
    map_df(function(m) {
        cleaned_texts %>% 
            filter(file==m) %>% 
            mutate(mass = if_else(str_detect(resumen, "((?<!no )bloque\\B)|(cierre de calles)|tortuguismo") &
                                      !str_detect(resumen, "[Aa]menaz|[Aa]nuncian?\\b"),
                                  true = 1,
                                  false = 0),
                   mass = if_else(str_detect(resumen, "(se concentran?)|([Cc]on una) concentración"),
                                  true = 1,
                                  false = mass),
                   mass = if_else(str_detect(resumen, "((realizan?|inician?) una?|(declaran?|encuentran?) en|[Cc]on una?|(La|El)|este día se espera el|mantienen? (el|la|en)|cumple el \\b.*\\b día de) (paro|huelga)"),
                                  true = 1,
                                  false = mass),
                   mass = if_else(str_detect(resumen, "(el paro (de labores )?realizado)|(paralizan? sus labores)"),
                                  true = 1,
                                  false = mass),
                   mass = if_else(str_detect(resumen, "(mantienen? (ocupando|cerrado))|(se amarran?\\b)|(impiden? el acceso)"),
                                  true = 1,
                                  false = mass),
                   mass = if_else(str_detect(resumen, "([Rr]ealizan?|[Dd]urante) (el|la|una?) (manifestación|marcha|paro|huelga|actividad pública)"),
                                  true = 1,
                                  false = mass),
                   mass = if_else(str_detect(resumen, "(([Ll]uego de la)|([Cc]on una)|(realizan?)( una?)?) (manifestación|mitin|caravana)"),
                                  true = 1,
                                  false = mass),
                   mass = if_else(str_detect(resumen, "(protestan?|manifiestan?) (frente|antes|en las afueras|en las calles)"),
                                  true = 1,
                                  false = mass),
                   mass = if_else(str_detect(resumen, "realización de una manifestación|manifestación convocada días atrás|se presentan? (en las oficinas|al edificio)"),
                                  true = 1,
                                  false = mass),
                   mass = if_else(str_detect(resumen, "marchan?\\b|toman? las calles"),
                                  true = 1,
                                  false = mass),
                   mass = if_else(str_detect(resumen, "cierran? (el portón|los portones)"),
                                  true = 1,
                                  false = mass))
    })

hand_checked2 <- list(iis, hand_checked) %>% 
    map_df(function(df) {
        df %>% 
            select(resumen, mass)
    })