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

report_names <- map(all_pages, function(page) {
    read_html(page) %>%
        html_nodes(".caption h3") %>%
        str_extract("(?<=>)[^<]*")}) %>%
    unlist() 
    
report_names_short <- report_names %>%
    as_tibble() %>% 
    separate(value, c("mes", "year"), " ") %>% 
    left_join(mes, by = "mes") %>% 
    unite(name, year, no) %>% 
    select(name) %>% 
    unlist()

# make files directory, if it doesn't already exist
dir.create("data-raw/files", showWarnings = FALSE, recursive = TRUE) 

# download all reports as PDF files
walk2(report_numbers, report_names_short, function(number, name) {
    if (!file.exists(str_c("data-raw/files/", name, ".pdf"))) { # if file doesn't exist
        doc <- str_c("http://www.protestas.iis.ucr.ac.cr/publicaciones/", number) %>%
            read_html() %>% 
            html_node(".embed-responsive-item") %>% # Note to self: get the CSS selector using Firefox Developer Edition's Inspector (find the iframe, get contextual menu, "Copy CSS Selector")
            str_extract('\\/system[^\\"]+') %>% 
            str_c("http://www.protestas.iis.ucr.ac.cr", .) # get the link to it
        download.file(doc, destfile = str_c("data-raw/files/", name, ".pdf")) # and download
    }
})

# quick-and-dirty backup, for if (when) they recode the page html again
# pwalk(list(report_numbers, report_names, report_names_short), function(number, name, short_name) {
#     print(paste(number, name))
#     if (!file.exists(str_c("data-raw/files/", short_name, ".pdf"))) { # if file doesn't exist
#         link <- paste0("https://www.protestas.iis.ucr.ac.cr/system/publicacions/archivos/000/000/",
#                         number,
#                         "/original/IIS-UCR_Costa_Rica_Cronolog%C3%ADa_",
#                         name %>% str_replace(" ", "_"),
#                         ".pdf") # make the link to it
#         tryCatch(download.file(link, destfile = file.path("data-raw","files", paste0(short_name, ".pdf"))),
#                                error = function(e) {
#                                    tryCatch(download.file(link %>% str_replace("%C3%AD", "i"), destfile = file.path("data-raw","files", paste0(short_name, ".pdf"))),
#                                             error = function(e) {
#                                                 download.file(link %>% str_replace("Septiembre", "Setiembre"), destfile = file.path("data-raw","files", paste0(short_name, ".pdf")))
#                                             })
#                                }) # and download
#     }
# })

# get links to earlier reports from wayback machine, then download from OSAL
old_links <- read_html("https://web.archive.org/web/20140612020851/http://www.clacso.org.ar/institucional/1h.php?idioma=esp") %>% 
    html_nodes("#item_11 div .link_osal_inverso") %>%
    html_attr("href")

old <- tibble(link = old_links,
              year = str_extract(link, "\\d{4}$"),
              mes_todo = str_extract(link, "(?<=\\s)[\\w-]+(?=\\s\\d{4}$)"),
              mes = str_extract(link, "(?<=\\s)\\w+(?=-?\\w*\\s\\d{4}$)"),
              link_osal = str_replace(link, 
                                      "/web/20140612020851/http:",
                                      "https:")) %>% 
    left_join(mes, by = "mes") %>% 
    mutate(name = paste0(year, "_", no))

pwalk(old, function(link_osal, name, ...) {
    if (!file.exists(str_c("data-raw/files/", name, ".pdf"))) { # if file doesn't exist
        while (!file.exists(str_c("data-raw/files/", name, ".pdf"))) { # until file exists,
            try(download.file(link_osal, destfile = str_c("data-raw/files/", name, ".pdf"))) # keep trying to download
            Sys.sleep(4)
        }
    }
})

# convert to text ---------------------------------
# make texts directory, if it doesn't already exist
dir.create("data-raw/texts", showWarnings = FALSE) 

all_reports <- c(report_names_short, old %>% pull(name))

# extract text from PDFs
walk(all_reports, function(name) {
    if (!file.exists(str_c("data-raw/texts/", name, ".txt"))) { # if text doesn't exist
        paste0("pdftotext -layout \"data-raw/files/", 
                     name, ".pdf\" \"data-raw/texts/", name,".txt\"") %>% 
            system(ignore.stderr = TRUE)
    }
})

# Crud-----------
# Identify files with problems and use OCR to make text files
# https://ryanfb.github.io/etc/2014/11/13/command_line_ocr_on_mac_os_x.html
system("cd \"data-raw/texts/\"; for f in *.txt; do echo \"$f\"; pcregrep -c '�' $f;  pcregrep -ci '(m\\s?a\\s?r\\s?t\\s?e\\s?s|l\\s?u\\s?n\\s?e\\s?s|b\\s?a\\s?d\\s?o|feira|\\sos\\s)' $f; done > \"crud.txt\"") # Count lines of garbage characters (and days) in each text file
crud <- as_tibble(matrix(readLines("data-raw/texts/crud.txt"), ncol=3, byrow=T)) %>%  # Read in the counts
    mutate(V2 = as.numeric(V2),  # Reformat count variable as numeric rather than string
           V3 = as.numeric(V3)) %>% # Files with more than four lines of garbage characters (or no mention of days) have problems and need OCR'd
    filter((V2 > 4 | V3 == 0) & V1 != "crud.txt")

dir.create("data-raw/files_scanned", showWarnings = FALSE) # Make files_scanned directory if it doesn't already exist
dir.create("data-raw/texts_bad", showWarnings = FALSE) # Make texts_bad directory if it doesn't already exist

# Move problematic texts from texts directory to texts_bad directory, 
# copy corresponding PDFs to files_scanned directory and then OCR them, 
# then move result to texts
# consider using ocrmypdf here

walk(crud$V1, function(i) {
    ii <- gsub("txt", "pdf", i)
    system(paste0("mv data-raw/texts/", i, " data-raw/texts_bad; ",
                  "cp data-raw/files/", ii, " data-raw/files_scanned/", ii, "; ",
                  "./R/ocr.sh data-raw/files_scanned/", ii, "; ",
                  "mv ", i, " data-raw/texts")) 
})

system("mv data-raw/texts/crud.txt data-raw/texts_bad/crud.txt")   # Move file with count of garbage characters out of Texts directory
