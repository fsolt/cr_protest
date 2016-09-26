#!/bin/bash

#Adapted by Frederick Solt from Ryan Baumann's code at 
#https://gist.github.com/ryanfb/f792ce839c8f26e972cf

PREFIX=$(basename "$1" .pdf)
echo "Current document: $PREFIX"
echo "Converting to TIFF . . ."
LAST_PAGE=$(($(xpdf-pdfinfo "$1"|grep '^Pages:'|awk '{print $2}') - 1))
parallel --bar -u convert -density 300 "$1"\[\{1\}\] -type Grayscale -compress lzw -background white +matte -depth 32 "${PREFIX}_page_%05d.tif" ::: $(seq 0 $LAST_PAGE)

echo "Performing OCR . . ."
BRAZIL=$(grep -c 'Brasil' ../Bad_Texts/"${PREFIX}".txt)
if [ $BRAZIL -gt 1 ]; then
  parallel --bar -u --retries 3 "tesseract -l por {} {.} pdf 2>/dev/null" ::: "${PREFIX}"_page_*.tif
else
  parallel --bar -u --retries 3 "tesseract -l spa {} {.} pdf 2>/dev/null" ::: "${PREFIX}"_page_*.tif
fi

echo "Cleaning up . . ."
cat "${PREFIX}"_page_*.txt > "${PREFIX}".txt
rm "${PREFIX}"_page_*.tif "${PREFIX}"_page_*.txt
