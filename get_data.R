library(readr)
library(dplyr)
library(stringr)
library(rvest)
library(purrr)

cr <- read_csv("http://www.protestas.iis.ucr.ac.cr/protestas.csv")
write_csv(cr, "data-raw/protestas.csv")

cr <- read_csv("data-raw/protestas.csv")
names(cr) <- names(cr) %>%
    tolower() %>% 
    make.names() %>% 
    str_replace_all("\\.", "_")

cron <- read_html("http://www.protestas.iis.ucr.ac.cr/publicaciones/cronologias")

last_page <- cron %>% 
    html_nodes(".next a") %>%
    html_attr("href") %>% 
    str_extract("\\d+$") %>% 
    as.numeric()

all_pages <- str_c("http://www.protestas.iis.ucr.ac.cr/publicaciones/cronologias?page=",
                   1:last_page)

report_numbers <- map(all_pages, function(page) {
    read_html(page) %>%
        html_nodes(".caption .btn-primary") %>%
        html_attr("href")}) %>%
    unlist() %>% 
    str_extract("\\d+")

months <- c("enero", "febrero", "marzo", "abril",
            "mayo", "junio", "julio", "agosto",
            "septiembre", "octubre", "noviembre", "diciembre")

report_names <- map(all_pages, function(page) {
    read_html(page) %>%
        html_nodes(".caption h3") %>%
        str_extract("(?<=>)[^<]*")}) %>%
    unlist() %>% 
    str_replace("(\\w+) (\\d+)", "\\2_\\1") %>% 
    tolower()

walk2(report_numbers, report_names, function(number, name) {
    doc <- str_c("http://www.protestas.iis.ucr.ac.cr/publicaciones/", number) %>%
        read_html() %>% 
        html_node(":nth-child(6) .col-md-12") %>% 
        str_extract('\\/system[^\\"]+') %>% 
        str_c("http://www.protestas.iis.ucr.ac.cr", .)
    download.file(doc, destfile = str_c("data-raw/files/", name, ".pdf"))
})

