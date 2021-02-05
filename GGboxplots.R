# Code made in R-4.0.3
# Please read README.md for more information

library(tidyverse)
theme_set(theme_bw())

# create a wing_table
Wing_Table <- read_csv("NEW_CD_DGRP_Subset_Data_2019_V2.csv")

#convert total area in pixels to total area in mmsqr
px.mmsqr_conversion <- 0.00005375
Wing_Table_mmsqr <- Wing_Table %>%
  mutate(
    TA_mmsqr = TotalArea.px * px.mmsqr_conversion,
    TotalArea.px = NULL
  )

# box plot comparing mutant phenotypes effect on Total Area 
# regardless of genetic backgrounds
BP_mutant_TA <- ggplot(Wing_Table_mmsqr, aes(y=TA_mmsqr, x=Allele_1)) + 
  geom_boxplot() +
  labs(x="Mutant Allele", y="Total Area mmsqr")
print(BP_mutant_TA)

# box plot comparing mutant phenotypic effect on SQ measurement
# regardless of genetic background
BP_mutant_SQ <- ggplot(Wing_Table_mmsqr, aes(y=SQ_Measure, x=Allele_1)) + 
  geom_boxplot() +
  labs(x="Mutant Allele", y="Semi-Quantitative Measure")
print(BP_mutant_SQ)

BPBG_mutant_SQ <- ggplot(Wing_Table_mmsqr, aes(y=SQ_Measure, x=WT_Background)) +
  geom_boxplot() + 
  facet_wrap(~Allele_1) + 
  labs(x= "Wild Type DGRP Strain", y="Semi-Quantitative Measure") +
  scale_color_brewer(palette="Set2")

BPBG_mutant_SQ_vertlabels <- BPBG_mutant_SQ+theme(axis.text.x=element_text(angle=90))
print(BPBG_mutant_SQ_vertlabels)
#Box plots comparing the SQ measure of mutant phenotypes within each genetic background 


BPBG_mutant_TA <- ggplot(Wing_Table_mmsqr, aes(y=TA_mmsqr, x=WT_Background)) +
  geom_boxplot() + 
  facet_wrap(~Allele_1) + 
  labs(x= "Wild Type DGRP Strain", y="Total Area mmsqr") +
  scale_colour_brewer(palette="Set1")


BPBG_mutant_TA_vertlabels <- BPBG_mutant_TA+theme(axis.text.x=element_text(angle=90))
print(BPBG_mutant_TA_vertlabels)
#Box plots comparing the total area of mutant phenotypes within each genetic background 

