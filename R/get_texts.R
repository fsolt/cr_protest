library(dplyr)
library(stringr)
library(rvest)
library(tidyr)
library(purrr)

# depends on poppler (on Macs, install via Homebrew: `brew install poppler`)
# plus pcregrep, tesseract (plus dictionaries), imagemagick, parallel, and xpdf
#   1. Install MacPorts <https://www.macports.org/install.php>
#   2. In terminal: sudo port install pcre
#   3. In terminal: sudo port install tesseract
#       a. sudo port install tesseract-spa
#   4. In terminal: sudo port install imagemagick

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

# Crud-----------
# Identify files with problems and use OCR to make text files (two-year-old code)
# https://ryanfb.github.io/etc/2014/11/13/command_line_ocr_on_mac_os_x.html
system("cd \"data-raw/texts/\"; for f in *.txt; do echo \"$f\"; pcregrep -c '�' $f;  pcregrep -ci '(m\\s?a\\s?r\\s?t\\s?e\\s?s|l\\s?u\\s?n\\s?e\\s?s|b\\s?a\\s?d\\s?o|feira|\\sos\\s)' $f; done > \"crud.txt\"") # Count lines of garbage characters (and days) in each text file
crud <- data.frame(matrix(readLines("data-raw/texts/crud.txt"), ncol=3, byrow=T), stringsAsFactors = F) # Read in the counts
crud[, 2:3] <- lapply(crud[, 2:3], as.numeric) # Reformat count variable as numeric rather than string
crud <- crud[crud$X2 > 4 | crud$X3 == 0, ] # Files with more than four lines of garbage characters (or no mention of days) have problems and need OCR'd

dir.create("data-raw/files_scanned", showWarnings = FALSE) # Make files_scanned directory if it doesn't already exist
dir.create("data-raw/texts_bad", showWarnings = FALSE) # Make texts_bad directory if it doesn't already exist

# Move problematic texts from texts directory to texts_bad directory, 
# copy corresponding PDFs to files_scanned directory and then OCR them, 
# then move result to texts
lapply(crud$X1, function(i){
    ii <- gsub("txt", "pdf", i)
    system(paste0("mv data-raw/texts/", i, " data-raw/texts_bad; ",
                  "cp data-raw/files/", ii, " data-raw/files_scanned/", ii, "; ",
                  "./ocr.sh data-raw/files_scanned/", ii, "; ",
                  "mv ", i, " data-raw/texts"))
})

system("mv data-raw/texts/crud.txt data-raw/texts_bad/crud.txt")   # Move file with count of garbage characters out of Texts directory
