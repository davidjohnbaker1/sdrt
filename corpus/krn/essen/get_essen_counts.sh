# Unzip Essen 
unzip europa.zip 
mkdir temp_essen
# Put all Kern in One Director
find . -name '*.krn' -exec mv {} temp_essen \;
cd temp_essen
# Count all Degs
deg -a *.krn | rid -GLId | grep -v [=r] | sortcount > ../../../../data/aggregate_data/essen_counts.tsv
# Delete Kerns 
rm *.krn
cd ..
rm -rf temp_essen
rm -rf essen 


