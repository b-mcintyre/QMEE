# Permutations assignment

#### Prelims ####
library("ggplot2")
library("lmPerm")
library("coin")
library("gtools")
library("tidyverse")

wing_table <- read.csv("NEW_CD_DGRP_Subset_Data_2019_V2.csv")

wing_table$Allele_1 <- factor(wing_table$Allele_1)

wing_table$WT_Background <- factor(wing_table$WT_Background)

px.mmsqr_conversion <- 0.00005375

wing_table_mmsqr <- wing_table %>%
  mutate(
    TA_mmsqr = TotalArea.px * px.mmsqr_conversion,
    TotalArea.px = NULL
  )

wing_table_geno <- wing_table_mmsqr %>% unite(genotype, Allele_1,WT_Background) %>%
  mutate(obs=seq(n()))

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

# three aspects of the data I would like to scramble include the total wing area(can also be done for the 
# Semi-quantitative scale)
# and the predictors Allele_1 and WT background 


#### permutations ####

#Brute force (takes awhile)
# scrambled the response variable of TA_mmsqr
set.seed(101)
nsim <- 9999
res <- numeric(nsim)
for (i in 1:nsim) {
  perm <- sample(nrow(wing_table_mmsqr))
  bdat <- transform(wing_table_mmsqr, TA_mmsqr=TA_mmsqr[perm])
  res[i] <- (bdat
             %>% group_by(Allele_1, WT_Background)
             %>% summarise(TA_mmsqr=mean(TA_mmsqr))
             %>% pull(TA_mmsqr)
             %>% diff())
}

obsgrouped_means <-wing_table_mmsqr %>%
  group_by(Allele_1, WT_Background) %>%
  mutate(mean(TA_mmsqr)) %>%
  pull (TA_mmsqr)


res <- c(res,obsgrouped_means)

hist(res,las=1,main="")

2*mean(res>=obsgrouped_means)  

#scrambled the predictor variable allele_1
set.seed(101)
nsim <- 9999
res1 <- numeric(nsim)
for (i in 1:nsim) {
  perm1 <- sample(nrow(wing_table_mmsqr))
  bdat1 <- transform(wing_table_mmsqr, Allele_1=Allele_1[perm1])
  res1[i] <- (bdat
             %>% group_by(Allele_1, WT_Background)
             %>% summarise(TA_mmsqr=mean(TA_mmsqr))
             %>% pull(TA_mmsqr)
             %>% diff())
}

obsgrouped_means <-wing_table_mmsqr %>%
  group_by(Allele_1, WT_Background) %>%
  mutate(mean(TA_mmsqr)) %>%
  pull (TA_mmsqr)

res1 <- c(res1, obsgrouped_means)

hist(res1, las=1, main="")

2*mean(res1>=obsgrouped_means)

#scrambled the predictor variable WT_Background

set.seed(104)
nsim <- 9999
res2 <- numeric(nsim)
for (i in 1:nsim) {
  perm2 <- sample(nrow(wing_table_mmsqr))
  bdat2 <- transform(wing_table_mmsqr, WT_Background=WT_Background[perm2])
  res2[i] <- (bdat
              %>% group_by(Allele_1, WT_Background)
              %>% summarise(TA_mmsqr=mean(TA_mmsqr))
              %>% pull(TA_mmsqr)
              %>% diff())
}

obsgrouped_means <-wing_table_mmsqr %>%
  group_by(Allele_1, WT_Background) %>%
  mutate(mean(TA_mmsqr)) %>%
  pull (TA_mmsqr)

res2 <- c(res2, obsgrouped_means)

hist(res2, las=1, main="")

2*mean(res2>=obsgrouped_means)

# using LmPerm package 

Allele_1lmp<- summary(lmp(TA_mmsqr~Allele_1,data = wing_table_mmsqr))

WT_Backgroundlmp <- summary(lmp(TA_mmsqr~WT_Background,data = wing_table_mmsqr))

#using coin package

Allele_1coin <- oneway_test(TA_mmsqr~Allele_1,data=wing_table_mmsqr,distribution=approximate(nresample=9999))


WT_Backgroundcoin <- oneway_test(TA_mmsqr~WT_Background,data=wing_table_mmsqr,distribution=approximate(nresample=9999))



            