# BMB: did you intend to have a comma in your filename?
# not as bad as spaces, but weird and potentially problematic/annoying

## Add tidyverse

library(tidyverse)

# Import data to environment

# BMB: the structure of your repo is (apparently) not the same
# as the structure of your project on disk? this is only going to work
# if the data are in a "neighbouring" directory to data/, whereas your repo
# only has a single directory, and no data/ subdirectory ... ?
# There are various reasonable ways to organize things, but this
# one isn't reproducible at the moment.
# Wing_Table <- read_csv("../data/NEW_CD_DGRP_Subset_Data_2019_V2.csv")

# modified so it will work from repo directory
Wing_Table <- read_csv("NEW_CD_DGRP_Subset_Data_2019_V2.csv")

# Find means of SQ_Measure and Total_Area.px

Wing_SQ_Measure_mean <- mean(Wing_Table$SQ_Measure)

Wing_Total_Area_mean <- mean(Wing_Table$TotalArea.px)

# Print both means

print(Wing_SQ_Measure_mean)
print(Wing_Total_Area_mean)

# OK. (These computations are on the light side of 'substantive', but OK ...)
# mark: 1.8
