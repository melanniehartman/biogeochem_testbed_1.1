1             Starting year
2100          Last year
harvardforest.100   Site file name
0             Labeling type
-1            Labeling year
-1.00         Microcosm
-1             CO2 Systems
0             pH shift
-1             Soil warming
2             N input scalar option
1
0             OMAD scalar option
0             Climate scalar option
3             Initial system
G3N           Initial crop
HARVF          Initial tree

Year Month Option
1             Block #   growing_forest
1849          Last year
1             Repeats # years
1900          Output starting year
12            Output month
1.000         Output interval
F             Weather choice
harvardforest.wth   
   1   15 TREE HARVF 
   1  100 TFST
   1  349 TLST
-999 -999 X

2             Block #   clear_cut
1850          Last year
1             Repeats # years
1990          Output starting year
12            Output month
1.000         Output interval
C             Weather choice
   1   15 TREE HARVF 
   1  100 TFST
   1  122 TREM CC   
   1  122 TLST
-999 -999 X

3             Block #   planting_pasture
1851          Last year
1             Repeats # years
1990          Output starting year
12            Output month
1.000         Output interval
C             Weather choice
   1  122 CROP G3N  
   1  122 FRST
   1  122 CULT K    
   1  305 SENM
   1  349 LAST
-999 -999 X

4             Block #   pasture
1899          Last year
1             Repeats # years
1990          Output starting year
12            Output month
0.083         Output interval
C             Weather choice
   1    2 CROP G3N  
   1    2 FRST
   1  153 GRAZ GM_M 
   1  182 GRAZ GM_M 
   1  213 GRAZ GM_M 
   1  244 GRAZ GM_M 
   1  305 SENM
   1  349 LAST
-999 -999 X
5             Block #   abandon_pasture_regrow_forest
1937          Last year
1             Repeats # years
1990          Output starting year
0.083         Output month
1.000         Output interval
C             Weather choice
   1    2 CROP G3N  
   1    2 FRST
   1    2 TREE HARVF 
   1  100 TFST
   1  305 SENM
   1  335 LAST
   1  335 TLST
-999 -999 X
6             Block #   1938_hurricane
1938          Last year
1             Repeats # years
1990          Output starting year
12            Output month
0.083         Output interval
C             Weather choice
   1    2 CROP G3N  
   1    2 FRST
   1    2 TREE HARVF 
   1  100 TFST
   1  259 TREM HFHUR
   1  305 SENM
   1  335 LAST
   1  335 TLST
-999 -999 X
7             Block #   continue_forest_regrowth
1949          Last year
1             Repeats # years
1990          Output starting year
12            Output month
0.083         Output interval
C             Weather choice
   1    2 CROP G3N  
   1    2 FRST
   1    2 TREE HARVF 
   1  100 TFST
   1  305 SENM
   1  335 LAST
   1  335 TLST
-999 -999 X
8             Block #   regrow_forest_with_no_pasture
1991          Last year
1             Repeats # years
1990          Output starting year
12            Output month
0.083         Output interval
F             Weather choice
harvardforest.wth
   1    2 TREE HARVF 
   1  100 TFST
   1  349 TLST
-999 -999 X
9             Block #   growing_forest_match_weather_data_years
2100          Last year
1             Repeats # years
1990          Output starting year
1             Output month
0.083         Output interval
C             Weather choice
   1    2 TREE HARVF 
   1  100 TFST
   1  349 TLST
-999 -999 X
