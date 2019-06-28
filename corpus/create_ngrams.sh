# This script creates all grams for sdrt 

solfa -x *.krn | rid -GLId | grep -v '[=r]'  sortcount > melosol_unograms
solfa -x *.krn | rid -GLId | grep -v '[=r]' | context -n 2 | sortcount > melosol_bigrams
solfa -x *.krn | rid -GLId | grep -v '[=r]' | context -n 3 | sortcount > melosol_trigrams
solfa -x *.krn | rid -GLId | grep -v '[=r]' | context -n 5 | sortcount > melosol_quintgrams
solfa -x *.krn | rid -GLId | grep -v '[=r]' | context -n 7 | sortcount > melosol_septgrams

