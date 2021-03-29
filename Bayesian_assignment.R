library(dplyr)
library(readr)
library(R2jags)
library(coda)
library(broom.mixed)
library(lme4)
library(arm)

# I was receiving syntax errors that I wasn't entirely sure what I was doing wrong.
# I just don't think I understand what the for loop is running. 
# I know for my priors I believe the variation of the mutant allele will be much larger (precision lower) than the 
# wild type background 
# This uses the cleaned_wing_table.rda file on the main page of my repo.

## utility for constructing an automatically named list
named_list <- lme4:::namedList

load("cleaned_wing_table.rda")


# interact lm model to compare to the bayes model later 
interact <-lm(formula=TA_mmsqr ~ WT_Background * Allele_1,
              data=wing_table_mmsqr)
anova(interact)

summary(interact)
#### interaction model #### 

## create list of data
wingdat1 <- with(wing_table_mmsqr,
                named_list(N=nrow(wing_table_mmsqr),            ## total obs
                           n_Allele_1=length(levels(Allele_1)), ## number of categories of alleles
                           n_WT_Background=length(levels(WT_Background)), ## number of categories of backgrounds
                           WT_Background=as.numeric(WT_Background), # numeric index for categories of backgrounds
                           Allele_1=as.numeric(Allele_1),## numeric index or categories of alleles
                           TA_mmsqr))                   ## TA measurements 


# I don't think I really understand what is going in in this for loop...?
interact_mutant_background_model1<- function() {
  for (i in 1:N) {
    Allelepred[i] <- b_Allele_1[Allele_1[i]] ## predicted (counts)
    Backgroundeff[i] <- b_WT_Background[Allele_1[i]]*(WT_Background[i]) ## effect of background for group i
    pred[i] <- Allelepred[i] + Backgroundeff[i] ## parsing error here?
    TA_mmsqr ~ pred[i] 
  }
  for (i in 1:n_Allele_1) {
    b_Allele_1[i] ~ dnorm(0,0.00001)
    b_WT_Background[i] ~ dnorm(0,0.0001)
  }
}

j_intereact_mutant_background <- jags(data=wingdat1,
           inits=NULL,
           parameters=c("b_Allele_1","b_WT_Background"),
           model.file=interact_mutant_background_model1)
tidy(j_intereact_mutant_background, conf.int=TRUE, conf.method="quantile")



#### Additive ####
additive_model <- function() {
  for (i in 1:N) {
    ## Poisson model
    Allelepred[i] <- b_Allele[Allele_1[i]]       ## predicted (counts)
    Backgroundeff[i] <- b_Background*(WT_Background[i])  ## effect of "Background"
    pred[i] <- (Allelepred[i] + Backgroundeff[i]) ## same error here? does it have to do with the levels of factors?
    TA_mmsqr[i] ~ pred[i]
  }
  ## define priors in a loop
  for (i in 1:n_Allele_1) {
    b_Background[i] ~ dnorm(0,0.001)
  }
  b_Allele_1 ~ dnorm(0,0.001)
}

j1 <- jags(data=wingdat1,
           inits=NULL,
           parameters=c("b_Background","b_Allele"),
           model.file=additive_model)
tidy(j3, conf.int=TRUE, conf.method="quantile")


?jags


#Try with bayesglm 
b1 <- bayesglm(TA_mmsqr~ WT_Background|Allele_1, data = wingdat1)
#error message of arguments imply differing number of rows...

#### matrix creation method ####

#### BB example given ####
    dd <- expand.grid(f=factor(letters[1:3]), 
                      g= factor(LETTERS[1:3])); 
    model.matrix(~f+g, data=dd); model.matrix(~f*g, data=dd)

# figuring out the parts of this code
?expand.grid

dd1 <- expand.grid(f=factor(letters[1:3]),
                   g=factor(LETTERS[1:3]))
mm1 <-model.matrix(~f+g, data = dd1)
mm2 <-model.matrix(~f*g, data=dd1)


# Creation of data frame DD2 and design matrices with own data #

DD2 <- expand.grid(allele = wing_table_mmsqr$Allele_1, 
                   wtbackground = wing_table_mmsqr$WT_Background); 
model.matrix(~ allele + wtbackground, data = DD2);
model.matrix(~ allele * wtbackground, data= DD2)

# Error of second martrix unable to be created because the vector size is over 40.5 Gb 

DD2 <- expand.grid(allele = levels(wing_table_mmsqr$Allele_1),                   
                        wtbackground = levels(wing_table_mmsqr$WT_Background));
x <- model.matrix(~ allele + wtbackground, data = DD2)
y <- model.matrix(~ allele * wtbackground, data= DD2)

#pass the levels of the factor vs the whole data set

# now have design matrices
# not sure how to pass them to jags as x? checked the jags user manual, in the appendix talks about using dim
# within structure to give information on the dimensions 

interact_model <- function() {
  for (i in 1:N) {
    y[i] ~ dnorm(pred[i], tau)
    pred[i] = inprod(X[i,],beta) + inprod(Y[i,],alpha)
    Y <- model.matrix(~ allele + wtbackground, data = DD2)
    X <- model.matrix(~ allele * wtbackground, data= DD2)
    Y <- structure(x, dim(180, 28))
    X <- structure(y, dim(180,180))
    
  }
  #priors
  # I'm not sure what I would call the allele/background priors as here to tell jags to run them?
  # would I just use Allele_1/WT_Background
  # for these I believe Allele_1 would have less precision or be more variable than the WT_Background 
  # Also I would set the mean to be greater than zero as my starting point 
  # try saving beta, alpha, and tau? 
  
  tau ~ dgamma (0.001, 0.001) # precision 
beta[i] ~ dnorm (0, 0.0001)
alpha[i] ~ dnorm(0, 0.0001)
}

jagsintereact <- jags(data= DD2, 
                      inits = NULL,
                      parameters.to.save =c("beta","alpha", "tau"), 
                      model.file = interact_model)

tidy(jagsintereact, conf.int=TRUE, conf.method="quantile")



args(structure)
?structure
