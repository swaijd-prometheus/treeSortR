tSR
===
### The following parameters exist for treeSortR

`-i` target group #1

`-j` target group #2

`-d` directory where trees are located and must begin with `~/`

`-e` minimum bootstrap threshold

`-f` query proportion

`-g` queryTT proportion

### Program use

`cd dashte/git/tSR/treeSortR/tSR`
`Rscript main.R -i "Viridiplantae" -j "Amoebozoa" -e 60 -g 65 -f 80 -d "~/Documents/DBLab/treeSortR"`

### Program output: two files are created
```
tSR_dftViridiplantae-Amoebozoa_2013Mar.15.151226.csv
tSR_Viridiplantae-Amoebozoa_2013Mar.15.151228.csv
```
### Authors
Sana Wajid [1], Developer

Cheong Xin Chan [2], Debashish Bhattacharya [1] , Advisors

1 Department of Ecology, Evolution, and Natural Resources and Institute of Marine and Coastal Science, Rutgers University, New Brunswick, NJ 08901, USA.

2 The University of Queensland, Institute for Molecular Bioscience, and ARC Centre of Excellence in Bioinformatics, Brisbane, QLD 4072, Australia.
