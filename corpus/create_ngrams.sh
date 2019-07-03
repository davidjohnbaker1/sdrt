# This script creates all grams for sdrt 

deg *.krn | rid -GLId | grep -v '[=r]'  sortcount > melosol_unograms.tsv
echo "Just did One Gram"
deg *.krn | rid -GLId | grep -v '[=r]' | context -n 2 | sortcount > melosol_bigrams.tsv
echo "Just did two Gram"
deg *.krn | rid -GLId | grep -v '[=r]' | context -n 3 | sortcount > melosol_trigrams.tsv
echo "Just did three gram"
deg *.krn | rid -GLId | grep -v '[=r]' | context -n 5 | sortcount > melosol_quintgrams.tsv
echo "Just did Five Gram"
deg *.krn | rid -GLId | grep -v '[=r]' | context -n 7 | sortcount > melosol_septgrams.tsv
echo "Just did Seven"
deg *.krn | rid -GLId | grep -v '[=r]' | context -n 9 | sortcount > melosol_nongrams.tsv
echo "Done"

