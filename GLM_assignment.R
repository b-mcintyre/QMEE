# GLM_assigment 

## BMB: please don't load packages you don't need. Are you going to use lmPerm, coin, gtools?
#### Prelims ####
library("lmPerm")
library("coin")
library("gtools")
library("tidyverse")
library("car")
library("emmeans")
library("ggplot2")

# Please us the data set found in my repo in the main page titled: 
# NEW_CD_DGRP_Subset_Data_2019_V2

wing_table <- read.csv("NEW_CD_DGRP_Subset_Data_2019_V2.csv")

wing_table$Allele_1 <- factor(wing_table$Allele_1, levels = c("OREw", "sd[1]", "sd[29.1]", "sd[ETX4]", 
"sd[E3]", "sd[58d]", "bx[1]", "bx[2]", "bx[3]"))

## BMB: it would be nice to write code to order "OREw", "sd*", "bx*"

wing_table$WT_Background <- factor(wing_table$WT_Background)


px.mmsqr_conversion <- 0.00005375

wing_table_mmsqr <- wing_table %>% mutate(
    TA_mmsqr = TotalArea.px * px.mmsqr_conversion,
    TotalArea.px = NULL
  )

## BMB: you do this every time.  Can you write code to do the cleaning once
## and save the clean/transformed data as a .rda/.rds file?

#### GLM & log transformed models ####
# I do not think a glm would be a good choice for my continuous variable... Used a gamma distribution with a log link
# I have also included an interact linear model where the log transformation was completed before the lm applied 
interactglm <- glm(formula=TA_mmsqr ~ WT_Background * Allele_1, 
                   family = Gamma(link = "log"),
                   data=wing_table_mmsqr)
summary(interactglm)
plot(interactglm)

## BMB: you will (soon) want to treat Allele_1 as a random effect, I think ...


# residuals vs leverage show there is not an obvious pattern, possibly larger variation residuals around the center?
# The Normal Q-Q shows increased variation from normal distribution than a lm without log transformation
# scale location is showing greater homoscedacity as the predicted value decreases, larger hooscedacity around the 
# center 
# all values appear to be within cook's distance
# I'm not noticing much of a difference between this glm and log transforming my lm

## BMB: differences between Gamma+log and log-normal model are indeed small/subtle

interactlog <-lm(log(TA_mmsqr) ~ WT_Background * Allele_1,
              data=wing_table_mmsqr)
summary(interactlog)
plot(interactlog)
# I do not notice too much of a difference between the glm and the lm models. 

interact_em <- emmeans(interactglm,~Allele_1|WT_Background)
summary(interact_em)

plot(interact_em,~Allele_1|WT_Background, comparisons = TRUE) + 
  facet_wrap(~WT_Background)
# see the wild type mutant alleles some lines have more variability than others
## BMB: I'm not sure what we get out of this plot ...

emmip(interactglm, WT_Background ~ Allele_1)
# plot showing increased variation for the moderate (sdETX4 and sdE3) alleles
# based on interacting with the DGRP backgrounds
## BMB: what's up with sd[58d]?

Anova(interactglm)
Anova(interactlog)

## BMB: a little bit boring since there are *huge* differences.
## eventually move toward making more quantitative conclusions (moving
## to mixed models will help)

## grade: 2.1
