# !/bin/bash
#part a
# How many rows are there for region 3 in the RECS 2015 data set?
#head -n 1 recs2015_public_v3.csv
cat recs2015_public_v3.csv | awk -F, '{print $2}'| grep 3 | wc -l
#method2
cat recs2015_public_v3.csv | cut -d "," -f 2 | grep 3 | wc -l
#method3
csvcut -c REGIONC recs2015_public_v3.csv| csvstat

#data set containing only: DOEID, NWEIGHT, and BRRWT1-BRRWT96.
cut -d "," -f 1,475-571 recs2015_public_v3.csv

#part 2
#loop to count and print number of observations within each region.
for d in 1 2 3 4
do
 echo $d
 cut -d "," -f 2 recs2015_public_v3.csv | grep $d | wc -l
done

#file region_division.txt (sorted list unique combi REGIONC,DIVISION
cut -d "," -f 2-3 recs2015_public_v3.csv| sort | uniq -d -u> region_division.txt
