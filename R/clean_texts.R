library(tidyverse)

# Load text files--------
dir.create("data-raw/texts_clean", showWarnings = FALSE) # Make texts_clean directory if it doesn't already exist

all_text_files <- data_frame(file = list.files("data-raw/texts"),
                             year = str_extract(file, "\\d{4}"),
                             month = str_extract(file, "(?<=_)\\d{2}"))

text_file_names <- all_text_files %>% 
    pull(file)

texts <- text_file_names %>% 
    str_c("data-raw/texts/", .) %>%
    map(function(x) {
        t0 <- readBin(x, file.info(x)$size, what="raw") # Work around to account for embedded nuls in text files
        t <- rawToChar(t0[t0!="00"])
        return(t)
    })

# Clean
días <- c("Domingo", "Lunes", "Martes", "Mi[ée]rcoles", "Jueves", "Viernes", "S[áa]bado") 
meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

clean_text <- function(t) {
    t %>% 
        # two-year-old pre-stringr code; sorry it's (extra-)hard to read 
        # Add extra line breaks around days
        gsub(paste0("\\n((?:", paste(días, collapse="|"), ")[[:blank:]]*[0-9]{1,2})\\n"),
             "\n\n\\1\n\n",  ., ignore.case=T) %>%
        # Add extra line breaks around months
        gsub(paste0("\\n((?:", paste(meses, collapse="|"), ")[[:blank:]]*(de [0-9]{4})?)\\n"),
             "\n\n\n\\1\n\n\n",  ., ignore.case=T) %>% 
        # Add extra line break after "Glosario de Siglas"
        gsub("Glosario de Siglas", "Glosario de Siglas\n", ., ignore.case=T) %>% 
        # Omit line breaks between sentences within paragraphs
        gsub("([^\n]{70,}\\.)\\n([[:upper:]]|“)", "\\1 \\2", .) %>% 
        # Omit lines with just page numbers
        gsub("\\n+\\s*\\d+\\s*\\n+", "\\\n", .) %>% 
        # Omit lines with headers or cite-to footnote
        gsub("\\n[^\n]*(Cronolog|OSAL|Osal|IIS)[^\n]*\\n", "\\\n", .) %>% 
        str_replace_all("\\n(\\s*Protesta\\s*)?Social[^\n]*\\n\\s*http://iis.ucr.ac.cr/[^\n]*\\n", "") %>% 
        # Omit line breaks within sentences
        gsub("([][:alpha:]),;:”%&\"])[[:blank:]]*\\n+\\s*([[:alnum:](“[«\"$])", "\\1 \\2", .) %>% 
        # Omit line breaks after numbers within sentences
        gsub("([[:digit:]])[[:blank:]]*\\n+\\s*([[:lower:]])", "\\1 \\2", .) %>%
        # Omit line breaks before numbers within sentences
        gsub("\\.\\n+([[:digit:]]*)[[:blank:]]*\\s*([[:lower:]])", ". \\1 \\2", .) %>%
        # Omit line breaks within words
        gsub("([[:alpha:][:digit:]])\\s*\\-\\s*\\n\\s*([[:alpha:][:digit:]])", "\\1\\2", .) %>% 
        # Re-split month-day-date lines     
        str_replace_all(str_c("\\n\\s*(", str_c(meses, collapse="|"),
                              ")\\s*((", str_c(días, collapse="|"),
                              ")\\s*\\d{1,2})\\s*\\n"),
                        "\n\n\\1\n\n\n\\2\n\n")
}

día_pattern <- días %>% 
    str_c(collapse="|") %>% 
    str_c("(?<=^\\s{0,100}\\w{0,10}\\s{0,100})((",.,")\\s*\\d{1,2})|(Glosario de Siglas)")

mes_pattern <- meses %>% 
    str_c(collapse="|") %>% 
    str_c("^\\s*((",.,")\\s*)")

cleaned_texts <- map2_df(texts, text_file_names, function(ts, t_f) {
    clean_text(ts) %>%
        str_split("\\n") %>%
        first() %>% 
        data_frame(file = t_f,
                   yyyy = str_extract(file, "\\d{4}"),
                   mm = str_extract(file, "(?<=_)\\d{2}"),
                   resumen = .,
                   día_fecha = ifelse(str_detect(resumen, regex(día_pattern, ignore_case = TRUE)),
                                      str_extract(resumen, regex(día_pattern, ignore_case = TRUE)),
                                      NA_character_),
                   mes = ifelse(str_detect(resumen, regex(mes_pattern, ignore_case = TRUE)),
                                str_extract(resumen, regex(mes_pattern, ignore_case = TRUE)) %>% 
                                    tolower() %>% 
                                    str_trim(),
                                NA_character_)) %>% 
        filter(!(str_detect(resumen, "^\\s*$"))) %>% 
        left_join(tibble(mes = tolower(meses),
                         mes_mm = sprintf("%02d", 1:12)), by = "mes") %>% 
        fill(mes, mes_mm, día_fecha) %>% 
        mutate(mm = ifelse(is.na(mes_mm), mm, mes_mm), 
               día_fecha = str_trim(día_fecha)) %>%
        filter(!(is.na(día_fecha) |
                     día_fecha=="Glosario de Siglas" |
                     día_fecha==str_trim(resumen) |
                     (!is.na(mes) & mes==tolower(resumen)) |
                     str_detect(resumen, "^!") |
                     str_detect(resumen, "^\\s*Social"))) %>%
        mutate(día_fecha = ifelse(día_fecha=="Jueves13", 
                                  "Jueves 13",
                                  día_fecha)) %>%
        mutate(resumen = str_replace_all(resumen, "ﬁ", "fi"),
               resumen = str_replace(resumen, paste0("^\\s*", día_fecha), "")) %>% 
        separate(día_fecha, c("día", "dd")) %>%
        mutate(dd = sprintf("%02d", as.numeric(dd)),
               día = paste0(toupper(substring(día, 1, 1)), tolower(substring(día, 2)))) %>% 
        select(file, yyyy, mm, dd, día, resumen) %>% 
        group_by(file) %>% 
        mutate(n = row_number(),
               mm = if_else(n < 3 & dd >= 27 & as.numeric(mm) > 1, sprintf("%02d", as.numeric(mm) - 1), mm),
               yyyy_mm = paste(yyyy, mm, sep = "_")) %>% 
        select(-n) 
})

write_csv(cleaned_texts, file = "data/cleaned_texts.csv")
