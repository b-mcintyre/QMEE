# Add tidyverse

library(tidyverse)

# Import data to environment

Wing_Table <- read_csv("../data/NEW_CD_DGRP_Subset_Data_2019_V2.csv")

# Find means of SQ_Measure and Total_Area.px

Wing_SQ_Measure_mean <- mean(Wing_Table$SQ_Measure)

Wing_Total_Area_mean <- mean(Wing_Table$TotalArea.px)

# Print both means

print(Wing_SQ_Measure_mean)
print(Wing_Total_Area_mean)

