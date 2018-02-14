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
        group_by(resumen) %>% 
        mutate(mass = max(mass),
               prot_entry = row_number(resumen)) %>% 
        ungroup() %>% 
        filter(prot_entry==1) %>% 
        select(-prot_entry)
    
    return(iis)
}

iis <- format_iis_dataset("data-raw/protestas.csv")
iis_nic <- format_iis_dataset("data-raw/protestas_nicaragua.csv")

load("data/cleaned_texts.rda")
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
            mutate(mass = if_else(str_detect(resumen, "((?<!no )bloque\\B)|(cierre de calles)|tortuguismo|(mantienen? un corte de carretera)") &
                                      !str_detect(resumen, "[Aa]menazan?\\b|[Aa]nuncian?\\b|protestará|finqueros|terratenientes"),
                                  true = 1,
                                  false = 0),
                   mass = if_else(str_detect(resumen, "(se concentran?)|([Cc]on una concentración)|(concentración realizada)|([Uu]n grupo de .*(protestan?\\b|se han? declarado|manifiestan?))"),
                                  true = 1,
                                  false = mass),
                   mass = if_else(str_detect(resumen, "(((realizan?|inician?) una?)|((declaran?\\b|encuentran?\\b)[^\\.]*( (en|una?))?)|([Cc]on una?)|(La|El)|(este día se espera el)|(mantienen? (el|la|en))|([Cc]umplen? (((el )?\\b.*\\b días?)|(((la )?\\b.*\\b semanas?)))( más)? de)|([Cc]ontinúa|[Pp]ersiste|[Mm]ientras) (el|la)) (paro|huelga)"),
                                  true = 1,
                                  false = mass),
                   mass = if_else(str_detect(resumen, "(el paro (de labores )?realizado)|(paralizan?\\b)|(encuentran?( parcialmente)? paralizad)"),
                                  true = 1,
                                  false = mass),
                   mass = if_else(str_detect(resumen, "(mantienen? (ocupando|cerrado))|(se amarran?\\b)|(se encadenan?\\b)|((permanecen?|continúan?) encadenado)|(impiden? el acceso)|(organizan un .?cordón humano.?)") &
                                      !str_detect(resumen, "terratenientes"),
                                  true = 1,
                                  false = mass),
                   mass = if_else(str_detect(resumen, "([Rr]ealizan?|[Dd]urante|organizan?|efectúan) (el|la|una?) (manifestación|marcha|paro|huelga|mitin|actividad pública)"),
                                  true = 1,
                                  false = mass),
                   mass = if_else(str_detect(resumen, "(([Ll]uego de la)|([Cc]on una)|(realizan?)( una?)?) (manifestación|mitin|caravana)"),
                                  true = 1,
                                  false = mass),
                   mass = if_else(str_detect(resumen, "(protestan?|manifiestan?|(suspender las protestas)) (frente|antes|en las afueras|en las calles|durante|en el campus)"),
                                  true = 1,
                                  false = mass),
                   mass = if_else(str_detect(resumen, "realización de una manifestación|manifestación convocada días atrás|se presentan? (en las oficinas|al edificio|a la Asamblea Legislativa)"),
                                  true = 1,
                                  false = mass),
                   mass = if_else(str_detect(resumen, "(?<!(anuncian? la|una))((marchan?\\b)|(toman? las calles))"),
                                  true = 1,
                                  false = mass),
                   mass = if_else(str_detect(resumen, "(cierran? (el portón|los portones))|(toman las nuevas instalaciones)"),
                                  true = 1,
                                  false = mass))
    })

all_coded_cr <- iis %>% 
    select(resumen, mass) %>% 
    bind_rows(hand_checked)

all_coded <- iis %>% 
    bind_rows(iis_nic) %>% # add IIS Nicaragua observations
    select(resumen, mass) %>% 
    bind_rows(hand_checked)
