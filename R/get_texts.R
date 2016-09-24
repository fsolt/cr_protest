library(dplyr)
library(stringr)
library(rvest)
library(tidyr)
library(purrr)

# depends on poppler (on Macs, install via Homebrew: `brew install poppler`)

# get PDF reports -------------------
# find number of last page of reports
last_page <- read_html("http://www.protestas.iis.ucr.ac.cr/publicaciones/cronologias") %>% 
    html_nodes(".next a") %>%
    html_attr("href") %>% 
    str_extract("\\d+$") %>% 
    as.numeric()

# build list of all pages of reports
all_pages <- str_c("http://www.protestas.iis.ucr.ac.cr/publicaciones/cronologias?page=",
                   1:last_page)

# get the (arbitrary) numbers that identify reports from all pages
report_numbers <- map(all_pages, function(page) {
    read_html(page) %>%
        html_nodes(".caption .btn-primary") %>%
        html_attr("href")}) %>%
    unlist() %>% 
    str_extract("\\d+")

# get report names, formatted to year_month 
mes <- data_frame(
    mes = c("Enero", "Febrero", "Marzo", "Abril",
         "Mayo", "Junio", "Julio", "Agosto",
         "Septiembre", "Octubre", "Noviembre", "Diciembre"),
    no = sprintf("%02d", 1:12))

ifelse(is.na(idx <- match(county, pattern)), county, replacement[idx])

report_names <- map(all_pages, function(page) {
    read_html(page) %>%
        html_nodes(".caption h3") %>%
        str_extract("(?<=>)[^<]*")}) %>%
    unlist() %>% 
    as_data_frame() %>% 
    separate(value, c("mes", "year"), " ") %>% 
    left_join(mes, by = "mes") %>% 
    unite(name, year, no) %>% 
    select(name) %>% 
    unlist()

# make files directory, if it doesn't already exist
dir.create("data-raw/files", showWarnings = FALSE, recursive = TRUE) 

# download all reports as PDF files
walk2(report_numbers, report_names, function(number, name) {
    if (!file.exists(str_c("data-raw/files/", name, ".pdf"))) { # if file doesn't exist
        doc <- str_c("http://www.protestas.iis.ucr.ac.cr/publicaciones/", number) %>%
            read_html() %>% 
            html_node(":nth-child(6) .col-md-12") %>% 
            str_extract('\\/system[^\\"]+') %>% 
            str_c("http://www.protestas.iis.ucr.ac.cr", .) # get the link to it
        download.file(doc, destfile = str_c("data-raw/files/", name, ".pdf")) # and download
    }
})

# get earlier reports
old_links <- read_html("https://web.archive.org/web/20140612020851/http://www.clacso.org.ar/institucional/1h.php?idioma=esp") %>% 
    html_nodes("#item_11 div .link_osal_inverso") %>%
    html_attr("href")

old <- data_frame(
    link = old_links,
    year = str_extract(link, "\\d{4}$"),
    mes_todo = str_extract(link, "(?<=%20)[\\w-]+(?=%20\\d{4}$)"),
    mes = str_extract(link, "(?<=%20)\\w+(?=-?\\w*%20\\d{4}$)")) %>% 
    left_join(mes, by = "mes") %>% 
    mutate(name = str_c(year, "_", no))

old_names <- old$name

walk2(old_links, old_names, function(link, name) {
    if (!file.exists(str_c("data-raw/files/", name, ".pdf"))) { # if file doesn't exist
        doc <- str_c("http://web.archive.org", link) # create full link
        while (!file.exists(str_c("data-raw/files/", name, ".pdf"))) { # until file exists,
            try(download.file(doc, destfile = str_c("data-raw/files/", name, ".pdf"))) # keep trying to download
            Sys.sleep(4)
        }
    }
})

# convert to text ---------------------------------
# make texts directory, if it doesn't already exist
dir.create("data-raw/texts", showWarnings = FALSE) 

all_reports <- c(report_names, old_names)

# extract text from PDFs
walk(all_reports, function(name) {
    if (!file.exists(str_c("data-raw/texts/", name, ".txt"))) { # if text doesn't exist
        str_c("pdftotext -layout \"data-raw/files/", 
                     name, ".pdf\" \"data-raw/texts/", name,".txt\"") %>% 
            system(ignore.stderr = TRUE)
    }
})

