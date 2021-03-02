# Permutations assignment

#### Prelims ####
library("ggplot2")
library("lmPerm")
library("coin")
library("gtools")
library("tidyverse")


# using the data set located in the main branch of my repo titled "NEW_CD_DGRO_Subset_Data_2019_V2


wing_table <- read.csv("NEW_CD_DGRP_Subset_Data_2019_V2.csv")

wing_table$Allele_1 <- factor(wing_table$Allele_1)

wing_table$WT_Background <- factor(wing_table$WT_Background)

px.mmsqr_conversion <- 0.00005375

wing_table_mmsqr <- wing_table %>%
  mutate(
    TA_mmsqr = TotalArea.px * px.mmsqr_conversion,
    TotalArea.px = NULL
  )

#visualization of data
bp_TA <- ggplot(wing_table_mmsqr, aes(y=TA_mmsqr, x=WT_Background, colour=WT_Background)) +
  geom_boxplot() + 
  facet_wrap(~Allele_1) + 
  labs(x= "Wild Type DGRP Strain", y="Total Area mmsqr") +
  stat_sum(alpha=0.25) +
  scale_size(range = c(0.5,4)) +
  theme(axis.text.x = element_text(angle = 90))

print(bp_TA)

#### Null Hypothesis ####

#1)	The observed increased variability of total area of wings within a genetic background is 
#not due to mutant alleles with moderate phenotypic effect but chance. 
#Test by permutation within a genetic background by interchanging either the mutant alleles or area measurements
#and seeing if there is still an observed change in variability

#2)	The observed decreased variability of total wing areas between wild type genetic backgrounds is not due to mutant
#alleles with strong or weak phenotypic effects and is due to chance.
#Test by permutation between wild type genetic backgrounds by interchanging the mutant alleles, area measurements, 
#or wild type genetic backgrounds and testing if the variability around the mean for each wild type background 
#remains the same for each group. 

# aspects of the data I would like to scramble include the predictors Allele_1 and WT background. 


#### permutations ####

#Brute force (takes awhile)
#scrambled the predictor variable allele_1

# Run this code to ends as properly placing the scrambled RSS in a vector using pull as suggested by BB
set.seed(101)
nsim <- 9999
res1 <- numeric(nsim)
for (i in 1:nsim) {
  perm1 <- sample(nrow(wing_table_mmsqr))
  bdat1 <- transform(wing_table_mmsqr, Allele_1=Allele_1[perm1])
  res1[i] <- (bdat1
              %>% group_by(Allele_1, WT_Background)
              %>% mutate(dev=(TA_mmsqr-mean(TA_mmsqr))^2)
              %>% ungroup()
              %>% summarise(RSS=sum(dev))
              %>% pull(RSS))
}

# Running this code creates the observed RSS using pull as suggested by BB
obsgroupRSS <- wing_table_mmsqr %>%
  group_by(Allele_1, WT_Background) %>%
  mutate(dev=(TA_mmsqr-mean(TA_mmsqr))^2) %>% 
  ungroup() %>% 
  summarise(RSS=sum(dev)) %>%
  pull(RSS)

#Run this code to added the observed RSS to the vector  
res1 <- c(res1, obsgroupRSS)



# Created histogram and set the ranges and breaks if I do not it results in a histogram that has one large bar
# The observed RSS is much less than the null RSS 
hist(res1, las=1, main="", xlim = range(13500,141000), breaks = 1500)
abline(v=obsgroupRSS, col="red")

#Created a histogram, but there is one large bar example.  
hist(res1, las=1, main="")

#double the tails and have a result of 2 which seems too clean of a number. 
2*mean(res1>=obsgroupRSS)





#scrambled the predictor variable WT_Background

# Example of when I used unlist code vs pull. This code creates a list with 9999 elements.
set.seed(101)
nsim <- 9999
res2 <- numeric(nsim)
for (i in 1:nsim) {
  perm2 <- sample(nrow(wing_table_mmsqr))
  bdat2 <- transform(wing_table_mmsqr, WT_Background=WT_Background[perm2])
  res2[i] <- (bdat2
              %>% group_by(Allele_1, WT_Background)
              %>% mutate(dev=(TA_mmsqr-mean(TA_mmsqr))^2)
              %>% ungroup()
              %>% summarise(RRS=sum(dev)))
}

#created the observed groupRSS shown as a list with 1 element
obsgroupRSS <- wing_table_mmsqr %>%
  group_by(Allele_1, WT_Background) %>%
  mutate(dev=(TA_mmsqr-mean(TA_mmsqr))^2) %>% 
  ungroup() %>% 
  summarise(RSS=sum(dev))
  
#added the obsgroupRSS to create a list with 10 000 elements
res2 <- c(res2, obsgroupRSS)

#unlisted the list to allow for creation of histogram
res2<- unlist(res2, use.names = FALSE)

# Again, the observed value is far away from the calculated shuffled values 
hist(res2, las=1, main="", xlim = range(13000,29000), breaks = 500)
abline(v=obsgroupRSS, col="red")
 
2*mean(res2>=obsgrouped_means)

#### using LmPerm package #### 

Allele_1lmp<- summary(lmp(TA_mmsqr~Allele_1,data = wing_table_mmsqr))
summary(Allele_1lmp)

WT_Backgroundlmp <- summary(lmp(TA_mmsqr~WT_Background,data = wing_table_mmsqr))
summary(WT_Backgroundlmp)

#### using coin package ####

Allele_1coin <- oneway_test(TA_mmsqr~Allele_1,data=wing_table_mmsqr,distribution=approximate(nresample=9999))
summary(Allele_1coin)

WT_Backgroundcoin <- oneway_test(TA_mmsqr~WT_Background,data=wing_table_mmsqr,distribution=approximate(nresample=9999))
summary(WT_Backgroundcoin)


            
