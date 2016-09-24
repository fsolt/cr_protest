### Clean up text files
# First cut in "OSAL/OSAL2/osal_text_test.do"
dir.create("../Clean_Texts", showWarnings = FALSE) # Make Clean_Texts directory if it doesn't already exist

for (i in seq(length(all.texts))) {
    text.file <- paste0("../Texts/", all.texts[i])
    #  t <- readChar(text.file, file.info(text.file)$size) # Doesn't work because of embedded nuls in some text files (e.g., 100.txt)
    t0 <- readBin(text.file, file.info(text.file)$size, what="raw") # Work around to account for embedded nuls in text files
    t <- rawToChar(t0[t0!="00"])
    
    days <- c("Domingo", "Lunes", "Martes", "Miércoles", "Miercoles", "Jueves", "Viernes", "Sábado", "Sabado")
    days2 <- unlist(lapply(days, function(i) paste(unlist(strsplit(i, split="")), collapse=" ")))
    t2 <- gsub(paste0("\\n((?:", paste(days, collapse="|"), ")[[:blank:]]*[0-9]{1,2})\\n"), "\n\n\\1\n\n",  t, ignore.case=T) # Add extra line breaks around days
    t2 <- gsub(paste0("((?:", paste(days2, collapse="|"), ")[[:blank:]]*[0-9])([[:blank:]]*[0-9])?"), "\n\n\\1\\2\n\n",  t2, ignore.case=T) # Add extra line breaks around days
    months <- c("Enero", "Febrero", "Marzo", "Mayo", "Abril", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
    months2 <- unlist(lapply(months, function(i) paste(unlist(strsplit(i, split="")), collapse=" ")))
    t2 <- gsub(paste0("\\n((?:", paste(months, collapse="|"), ")[[:blank:]]*(de [0-9]{4})?)\\n"), "\n\n\\1\n\n",  t2, ignore.case=T) # Add extra line breaks around months
    t2 <- gsub(paste0("((?:", paste(months2, collapse="|"), ")[[:blank:]]*(de [0-9]{4})?)"), "\n\\1.\n",  t2, ignore.case=T) # Add extra line breaks around months
    t2 <- gsub("Glosario de Siglas", "Glosario de Siglas\n", t2, ignore.case=T)
    
    t2 <- gsub("([^\n]{75}\\.)\\n([[:upper:]])", "\\1 \\2", t2)   # Omit line breaks between sentences within paragraphs
    t2 <- gsub("\\n+\\s*\\d+\\s*\\n+", "\\\n", t2)   # Omit lines with just page numbers
    t2 <- gsub("\\n[^\n]*(Cronolog|OSAL|Osal)[^\n]*\\n", "\\\n", t2)  # Omit lines with headers
    t2 <- gsub("([[:alpha:]),])[[:blank:]]*\\n+\\s*([[:alnum:](“])", "\\1 \\2", t2) # Omit line breaks within sentences
    t2 <- gsub("([[:digit:]])[[:blank:]]*\\n+\\s*([[:lower:]])", "\\1 \\2", t2) # Omit line breaks at numbers within sentences
    t2 <- gsub("([[:alpha:]])\\s*\\-\\s*\\n\\s*([[:alpha:]])", "\\1\\2", t2)  # Omit line breaks within words
    
    t2 <- gsub("Glosario de Siglas", "Glosario de Siglas\n", t2, ignore.case=T)
    writeLines(t2, paste0("../Clean_Texts/", all.texts[i]))
}
