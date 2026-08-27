#!/bin/bash
# render each easeling xml via LO, each ref pdf via pdftoppm, trim, align, score
cd out
printf "%-26s %8s %8s\n" scenario diff_pct verdict
for x in *.xml; do
  n=${x%.xml}
  python3 ../make_xlsx.py $n.xml $n.xlsx 2>/dev/null || { echo "$n WRAP_FAIL"; continue; }
  soffice --headless --convert-to pdf --outdir . $n.xlsx > /dev/null 2>&1
  pdftoppm -png -r 96 -f 1 -l 1 $n.pdf ${n}_e 2>/dev/null
  pdftoppm -png -r 96 -f 1 -l 1 ${n}_ref.pdf ${n}_r 2>/dev/null
  python3 ../score.py ${n}_e-1.png ${n}_r-1.png $n
done
