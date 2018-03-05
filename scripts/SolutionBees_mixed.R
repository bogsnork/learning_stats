######################################################################
#Bees exercise
#Set the working directory and import the data
setwd("F:/Statistics/SARD11_MixedModel&GLM")
setwd("G:/@Projects/MSc/Statistics/data")
Bees <- read.table("Bees.txt", header=TRUE)
######################################################################



names(Bees); head(Bees)
#"Rawdata"   "Spobee"    "Hive"      "X"         
#"Y"         "Infection" "BeesN"   

# > str(Bees)
# 'data.frame':	72 obs. of  7 variables:
 # $ Rawdata  : num  2 4 2 2 6 0 6 2 5 10 ...
 # $ Spobee   : num  6.67 13.33 6.67 6.67 20 ...
 # $ Hive     : int  1 1 1 2 2 2 3 3 3 4 ...
 # $ X        : int  0 0 0 0 0 0 0 0 0 0 ...
 # $ Y        : num  0 0 0 91 91 91 262 262 262 353 ...
 # $ Infection: int  0 0 0 0 0 0 0 0 0 0 ...
 # $ BeesN    : int  95000 95000 95000 95000 95000 95000 85000 85000 85000 90000 ...


######################################################################
#Aim:
#Model Spobee (or LSpobee) as a function of:
#  -Infection
#  -BeesN (number of bees in a hive)
#  -Interaction between Infection and BeesN
#  -hive effect
  
  

###################################################################
#Load packages and library files
#Change the path of this:
source(file = "HighstatLibV6.R")

library(lattice)
library(nlme)  #For lme
######################################################################


  
######################################################################
#House keeping
Bees$fHive <- factor(Bees$Hive)
str(Bees)                     

#This is a cheeky one:
Bees$LSpobee <- log10(Bees$Spobee + 1)
#We should really first do the entire analysis and then
#discover that a log-transformation is needed.
#But that takes a lot of time...so..let's work with
#log transformed data!


######################################################################
#Data exploration
#This also shows why we may need a transformation
par(mfrow = c(1, 2), mar = c(3, 4, 1, 1))
dotchart(Bees$Spobee,  groups = Bees$fHive)
dotchart(Bees$LSpobee, groups = Bees$fHive)

par(mfrow = c(1, 1))
#The code below shows why we need to convert
#Infection into a 0-1 variable
dotchart(Bees$Infection)
table(Bees$Infection)

#And here is how we do it.
#Create a new vector with 0-1 for infection
Bees$Infection01  <- Bees$Infection
Bees$Infection01[Bees$Infection01 > 0] <- 1
Bees$fInfection01 <- factor(Bees$Infection01)

table(Bees$fInfection01)
#Seems to be slightly (!) better



#Relationships
#Let's have a look at the effect of infection on log-spores
boxplot(LSpobee ~ fInfection01, 
        varwidth = TRUE, data = Bees,
        xlab     = "Infection",
        ylab     = "Log of spores")

#par(mfrow=c(1,1))
#Hive effect
boxplot(LSpobee ~ fHive, varwidth = TRUE,
        data = Bees,
        ylab = "Log spores", xlab = "Hives", main = "IMPORTANT")

#Can we fit a random intercept AND slope model?
coplot(LSpobee ~ BeesN | fHive, data = Bees) #No

#What about the interaction between BeesN and fInfection01 
coplot(LSpobee ~ BeesN | fInfection01, 
       data = Bees)
       
       
#################################################################       
#END OF DATA EXPLORATION
#################################################################

#Continue with frequentist analysis



######################################################################
#Protocol for mixed modelling (3 steps).
#Based on Zuur et al. (2013). 
#Beginner's Guide to GLM and GLMM with R

#1. Based on prior knowledge of the dependency 
#   structure in the data, select the random 
#   structure (i.e. the random effects) a priori.
#2. Fit the model and investigate which covariates 
#   in the fixed part are significant or important. 
#   Alternatively, omit model selection on the 
#   covariates and adopt an information theoretic 
#   approach (specify and compare 10 – 15 models).
#3. When the optimal model has been found, present 
#   the numerical output and provide a graphic 
#   representation of the model fit.

#In step 2 you may want to consider applying a limited amount of model
#selection...e.g. drop non-significant interactions.


            
            
##################################################################
#Step 1 of the protocol
M1 <- lme(LSpobee ~ fInfection01 * BeesN,
          random =~ 1 | fHive,
          data = Bees, method = "REML")
          
summary(M1)

#Everything ok?
#Take residuals.....
#1.1 - Plot residuals versus fitted values
#1.2 - Plot residuals versus each covariate in the model
#1.3 - Plot residuals versus each covariate NOT in the model (if relevant)
#1.4 - Plot residuals versus time (if relevant)
#1.5 - Plot residuals versus spatial coordinates

#1.1
plot(M1, pch = 16, col = 1)

#1.2
#Normalized residuals in lme are of the form:
# E / sqrt(variance of the residuals) 
E1 <- resid(M1, type = "n")
boxplot(E1 ~ Bees$fHive, data = Bees)
abline(0,0)

plot(x = Bees$BeesN, y = E1)
abline(h = 0, lty = 2)

boxplot(E1 ~ fInfection01, data = Bees)
#Is the ratio of the variances bigger than 4?
#Based on Fox (2002)
tapply(E1, FUN = var, INDEX = Bees$fInfection01) ; 0.7914/0.1895


#1.3: Not relevant here

#1.4: Not relevant here

#1.5
plot(x = Bees$X, y = Bees$Y)  #But units are presented the wrong way

#
xyplot(Y ~ X , data = Bees, aspect = "iso")

#We want:
# size of dots proportional to value of residual E3
# Color of dots should reflect sign of residuals

N     <- nrow(Bees)
MyCol <- vector(length = N)
MyCol[E1 <= 0] <- 1  #use black for negative residuals
MyCol[E1 > 0]  <- 2   #use red for positive residuals



xyplot(Y ~ X, col = MyCol, pch = 16, 
       data = Bees, aspect = "iso")
#Problem....3 observations per hive...and these have the same X and Y
#Solution: add random variation to X and Y...and this is what jitter does

xyplot(jitter(Y) ~ jitter(X), col = MyCol, pch = 16, 
       data = Bees, aspect = "iso")

#Now deal with size of the dots
MyCex <- abs(E1)
xyplot(jitter(Y) ~ jitter(X), 
       col = MyCol, 
       cex = 3 * MyCex,  pch = 16, 
       data = Bees, aspect = "iso")
#Trouble at the edges?


#Results model validation:
#Looks ok (?)...no structure in residuals...go to step 2
#Is everything significant? To answer
#this question we can use three different approaches in a
#frequentist approach:
#A. Use p-values of t-statistics (or z-statistics)
#B. Ue F test
#C. Use likelihood ratio test

#Technicallity: In approaches A andf B we MUST use REML estimation,
#in approach C we must use ML estimation


#Approach A:
M2 <- lme(LSpobee ~ fInfection01 + BeesN + 
                    fInfection01 : BeesN,
          random =~ 1 | fHive, method = "REML",
          data = Bees)

summary(M2)  #And see what is significant based on p-values


#Let's do a model selection (sorry)
#Drop interaction


M2A <- lme(LSpobee ~ fInfection01 + BeesN,
          random =~ 1 | fHive, method = "REML",
          data = Bees)
summary(M2A)
#Drop BeeN


M2B <- lme(LSpobee ~ fInfection01,
          random =~ 1 | fHive, method = "REML",
          data = Bees)
summary(M2B)
####################
#End of approach A

#########THE REST IS OPTIONAL##############
#########THE REST IS OPTIONAL##############
#########THE REST IS OPTIONAL##############
#########THE REST IS OPTIONAL##############

#Approach B: F test
M3 <- lme(LSpobee ~ fInfection01 + BeesN + 
                    fInfection01 : BeesN,
          random =~ 1 | fHive, method = "REML",
          data = Bees)
anova(M3)  #But these p-values depend on the order of the covariates!

#Change the order of the main terms:
M3A <- lme(LSpobee ~ fInfection01 + BeesN,
          random =~ 1 | fHive, method = "REML",
          data = Bees)

M3B <- lme(LSpobee ~ BeesN + fInfection01,
          random =~ 1 | fHive, method = "REML",
          data = Bees)

anova(M3A)
anova(M3B)
#End of approach B
##################################################


#Approach C (likelihood ratio test) 
M4 <- lme(LSpobee ~ fInfection01 + BeesN + 
                    fInfection01 : BeesN,
          random =~ 1 | fHive, method = "ML",
          data = Bees)

M4A <- update(M4, .~. -fInfection01 : BeesN)
anova(M4, M4A); 1-pchisq(0.5470648, 1)
#Drop interaction

M5 <- lme(LSpobee ~ fInfection01 + BeesN, 
          random =~ 1 | fHive, method = "ML",
          data = Bees)
M5A <- update(M5, .~. -fInfection01 )
M5B <- update(M5, .~. -BeesN)
anova(M5, M5A)
anova(M5, M5B)
#Remove BeesN

M6 <- lme(LSpobee ~ fInfection01,
          random =~ 1 | fHive,
          method = "ML",
          data = Bees)
M6A <- update(M6, .~. -fInfection01 )
anova(M6, M6A)

#    Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#M6      1  4 132.7358 141.8425 -62.36792                        
#M6A     2  3 150.2225 157.0525 -72.11124 1 vs 2 19.48663  <.0001
#

#Or with AIC
# AIC(M4)
# AIC(M4A)
#etc....
#Finished step 2. Note....many referees will criticise this model selection.
#Why not just drop the interaction and stop at that point?



#Step 3: Presents results (REML gives better estimates of the sigmas...
#        so use REML for final presentation)
M7 <- lme(LSpobee ~ fInfection01,
          random =~ 1 | fHive,
          method = "REML",
          data = Bees)

#Just double check all the assumptions again
plot(M7)
E7 <- resid(M7, type = "n")
#Repeat all model validation graphs from step 2
#.......




#Understanding the output:
#LogSpobee_ij = intercept + beta * Infection_ij + a_i + e_ij
#a_i  ~ N(0, sigma_hive^2)
#e_ij ~ N(0, sigma^2)
#   i = 1,....., 24
#   j = 1, ...,3

summary(M7)

# 
# #Random effects:
# # Formula: ~1 | fHive
# #        (Intercept)  Residual
# #StdDev:   0.9904917 0.3373335
# #
# #Fixed effects: LSpobee ~ fInfection01
# #                 Value Std.Error DF  t-value p-value
# #(Intercept)   1.757273 0.2257216 48 7.785134       0
# #fInfection011 2.902090 0.5529028 22 5.248825       0
# 
# Fitted model: 
# LSpores = 1.757 +        a_i + e_ij   #for not infected
# LSpores = 1.757 + 2.90 + a_i + e_ij   #for infected
# 
# a_i ~  N(0, 0.99^2)
# e_ij ~ N(0, 0.3373335^2)
# 
# Intraclass correlation
# 
#                 0.9904917 ^ 2
#          -------------------------
#          0.9904917^2 + 0.3373335^2
#          
# ( 0.9904917 ^ 2)/( 0.9904917 ^ 2 + 0.3373335^2)     = 0.896066
# 
#######################################


#We will show how to skecth the fitted values in the Bayesian version
#of this exercise. 



####################################################################
#The code below contains:
#A: Information on the different components of lme output
#   For example random effects are extracted and you can use these
#   to make a histogram of them
#B: How to fit a model using lme4 and get p-values
#C: How to get an R2 from a mixed model.

#These modules are mainly self-study topics and will not be discussed in
#a course.


#########################################################
#A. What is what in nlme?
#Random effects:
RandomEffectHive <- ranef(M7)$'(Intercept)'
Hive.Numerical <- as.numeric(as.factor(Bees$fHive))
#Make a histogram of RandomEffectHive

#Y = X*beta + Z*b + eps
OUT <- cbind(Bees$LSpobee,
             fitted(M7,level = 0), #X * beta
             fitted(M7,level = 1), #X * beta + Z * b
             RandomEffectHive[Hive.Numerical],   #b
             resid(M7, level = 0),    #Z*b + eps
             resid(M7, level = 1),    #eps
             Bees$fHive,              #Hive number
             resid(M7, type = "n"),   #eps / sigma
             resid(M7)                #eps
             )

colnames(OUT) <- c("Y",
                 "X*beta",
                 "X*beta + Z*b",
                 "b",
                 "Z*b + eps",
                 "eps",
                 "Hive",
                 "eps/sigma",
                 "eps")
                 
head(OUT,10)  #First 10 rows of Z


#By defaul level = 1 in fitted() and resid()
#fitted(M7)[1:5]

# fitted(M7, level = 0)     : X * beta
# fitted(M7, level = 1)     : X * beta + Z * b    level = 1 is default
# ranef(M7)                 : b
# ranef(M7, level = 1)      : b
# resid(M7, level = 0)      : Z*b + eps
# resid(M7, level = 1)      : eps    level = 1 is default
# resid(M7)                 : eps
# resid(M7, type = "n")     : eps / sigma  
# resid(M7, type = "n")     : resid(M7)/M7$sigma






#########################################################
#B. Fit the same model using lmer from lme4 (need to be installed)
detach("package:nlme")  #nlme and lmer don't like each other
library(lme4)

#LM1 is equivalent of M1
LM1 <- lmer(LSpobee ~ fInfection01 * BeesN + (1 | fHive),
            data = Bees)

summary(LM1)

#How do you get p-values?
#Use a z-distribution
LM1Est.Param <- fixef(LM1)               #Get the betas
param.SD     <-  sqrt(diag(vcov(LM1)))   #Get the SEs
pval <- 2*pnorm(-abs(LM1Est.Param  / param.SD))  #Z distribution
cbind(LM1Est.Param,param.SD, pval)


#############################################################
#Start the protocol from scratch using lmer:

LM1 <- lmer(LSpobee ~ fInfection01 * BeesN + (1 | fHive), 
            data = Bees)
plot(LM1) #This does not work

#lmer graphical output: plot residuals vs fitted values
plot(fitted(LM1), resid(LM1))
abline(h = 0)

quartz()  #Mac command for a new window
plot(M1, col =1)
#Question....what is in resid and fitted????
#We have Y = X * beta + Z * b + epsilon
#Text below
#resid() is eps 
#fitted() is X * beta + Z * b


#In lme:
plot(M1)  #lme: These residuals are divided by sigma


#Plot residuals
E1 <- resid(LM1)
boxplot(E1 ~ Bees$fHive, data = Bees)
abline(0,0)

#Can lmer do weights options?
LM1B <- lmer(LSpobee ~ fInfection01 * BeesN + 
            (1 | fHive), 
             data = Bees,
             weights = varIdent(form = ~ 1 | fInfection01))
             
#No!


#Looks ok...no structure in residuals...go to step 2
#Step 2: Is everything significant
LM2 <- lmer(LSpobee ~ fInfection01 + BeesN + 
                      fInfection01 : BeesN + 
                      (1 | fHive), REML = FALSE,
          data = Bees)

LM2A <- update(LM2, .~. -fInfection01 : BeesN)
anova(LM2, LM2A)
#Or: 
drop1(LM2, test = "Chi")
#Compare with lme results: Exactly the same


#Here is the model selection again (assuming you want to do it)
#Drop main terms
LM3 <- lmer(LSpobee ~ fInfection01 + BeesN + (1 | fHive), REML = FALSE,
          data = Bees)
drop1(LM3, test = "Chi")
#Remove BeesN


LM4 <- lmer(LSpobee ~ fInfection01 + (1 | fHive),
          REML = FALSE,
          data = Bees) 
drop1(LM4, test = "Chi")
#Finished with model selection!

#Step 3 
LM5 <- lmer(LSpobee ~ fInfection01 + (1 | fHive),
          REML = TRUE,
          data = Bees)
summary(LM5)

#Model validation
plot(fitted(LM5), resid(LM5))
abline(h=0)

F5 <- fitted(LM5)
E5 <- resid(LM5, type = "n")
plot(x = F5, y = E5)
boxplot(E5 ~ Bees$fInfection01)
plot(y = E5, x = Bees$BeesN)
#End of protocol.







#######################################################################
#What is what in lmer?
#Y = X*beta + Z*b + eps
LM7 <- lmer(LSpobee ~ fInfection01 + (1 | fHive),
          REML = FALSE, data = Bees)

#Compare with M7 from lme (which used ML)

#lmer can only cope with level 1 residuals
resid(LM7, type = "r", level = 0)
resid(LM7, type = "r", level = 1)
resid(LM7, type = "p", level = 1)
resid(LM7, type = "p", level = 1)
#All the same

cbind(E7, resid(LM7))
#E7 are: E / sigma residuals from lme

F.LM7 <- fitted(LM7)

beta <- fixef(LM7)
X <- model.matrix(LM7)
F.LM7.manual <- X %*% beta


cbind(F.LM7, F.LM7.manual)  #Different

#These are the 24 random effects
RE.LM7 <- ranef(LM7)$fHive$'(Intercept)'

#Blow them up to 72 rows
AllRE <- RE.LM7[as.numeric(Bees$fHive)]

Z.LM7 <- cbind(F.LM7, F.LM7.manual, AllRE, 
               Bees$Hive, F.LM7.manual + AllRE )  
colnames(Z.LM7) <- c("fitted","fit manual",
                     "Hive","RE", 
                     "fit manual + RE")

head(Z.LM7)
#Conclusions:
#fitted(lmer object) equal Xb + Zb


resid(LM7)[1:5]  #Seems that these are the raw residuals (Y-Xb -Zb)
ResRawManual <- Bees$LSpobee - F.LM7.manual - AllRE
ResRawManual[1:5]
#Yes....resid(lmer object) for Gaussian model is Y - Xb - Zb
#Makes sense as there are no variance structures.



Z.LM7 <- cbind(F.LM7, F.LM7.manual, AllRE, 
               Bees$Hive, F.LM7.manual + AllRE,
               resid(LM7), ResRawManual)  
colnames(Z.LM7) <- c("fitted","fit manual",
                     "Hive","RE", 
                     "fit manual + RE", 
                     "resid", "eps")

head(Z.LM7)


#If you want to divide things by sigma, use:
Sigmas <- as.numeric(VarCorr(LM7))
Sigmas  #That didn't do any good

summary(LM7)@REmat

as.numeric(summary(LM7)@REmat[,4])
Sigmas <- as.numeric(summary(LM7)@REmat[,4])


Z.LM7 <- cbind(F.LM7, F.LM7.manual, AllRE, 
               Bees$Hive, F.LM7.manual + AllRE,
               resid(LM7), ResRawManual,
               resid(LM7)/Sigmas[2])  
colnames(Z.LM7) <- c("fitted","fit manual",
                     "Hive","RE", 
                     "fit manual + RE", 
                     "resid", "eps",
                     "resid/sigma")

head(Z.LM7)

# fitted(M7)                : X * beta 
# ranef(M7)                 : b
# resid(M7)                 : eps
# resid(M7, type = "n")     : eps




##############################################################################
#C: How to get an R2 for mixed effect models
#This is based on:
#Ronghui Xu (2003). Measuring explained variation in linear mixed effects models
#Statistics in Medicine 2003: 22_3527 - 3541


#Take the optimal model with ML
M7 <- lme(LSpobee ~ fInfection01, random =~ 1 | fHive,
          method = "ML", data = Bees)

#Fit a model with only intercept and the random intercept:
M7.0 <- lme(LSpobee ~ 1, random =~ 1 | fHive,
            method = "ML", data = Bees)

#Equation 7 in Xu (2003):
sigma2.M7   <- M7$sigma^2
sigma2.M7.0 <- M7.0$sigma^2
r2 = 1 - (sigma2.M7 / sigma2.M7.0)

#Equation 11 in Xu (2003):
E7   <- resid(M7)
E7.0 <- resid(M7.0)
R2 <- 1 - sum(E7^2)/sum(E7.0^2)

#Equation 20 in Xu (2003):
N <- nrow(Bees)
rho2 <- 1 - (sigma2.M7/sigma2.M7.0) * exp(  sum(E7^2) / (N * sigma2.M7) - sum(E7.0^2) / (N * sigma2.M7.0))


out <- cbind(r2, R2, rho2)
out

#Doesn't work here because the random intercept in M7.0 takes over all information in the fixed covariate
#Remove the random intercept from the model!


M7.00 <- gls(LSpobee ~ 1,
             method = "ML", data = Bees)

#Equation 7 in Xu (2003):
sigma2.M7.00 <- M7.00$sigma^2
r2           <- 1 - (sigma2.M7 / sigma2.M7.00)


#Equation 11 in Xu (2003):
E7.00 <- resid(M7.00)
R2 <- 1 - sum(E7^2)/sum(E7.00^2)

#Equation 20 in Xu (2003):
rho2 <- 1 - (sigma2.M7/sigma2.M7.00) * exp(  sum(E7^2) / (N * sigma2.M7) - sum(E7.00^2) / (N * sigma2.M7.00))


out.00 <- cbind(r2, R2, rho2)
out.00
#So..this is the amount of variation explained by X*beta + Z*b

#Xu: Uses ML estimation
#r2 is ok for small and large cluster size
#R2 and rho can be 10% wrong for small cluster size
#########################################################




################################################################
#Simulation study to investigate the effect of
#number of random intercepts, and number of observations
#per group on estimated parameters. See also the ruddy turnstone
#chapter in Zuur, Hilbe, Ieno (2013). Beginner's Guide to GLM and GLMM with R

a   <- 1       #intercept
b   <- 2       #slope
N   <- 1000    #Sample
X1  <- runif(N)
SigmaEps <- 0.5
Eps <- rnorm(N, mean = 0, sd = SigmaEps) 
y  <- a + b * X1 + Eps
S1 <- lm(y ~ X1)
summary(S1)

RE <- rnorm(25, mean = 0, sd = 0.5)   #a_i values....25 of them
Hive <- rep(1:25, each = 40)

data.frame(y,X1, Hive)
RE[Hive]
y1  <- a + b * X1 + RE[Hive] + Eps

fHive <- factor(Hive)
library(nlme)
S2 <- lme(y1 ~ X1,
          random =~ 1 | fHive)
summary(S2)

##################
#Now start pottering around


a   <- 1
b   <- 2
N   <- 1000
X1  <- runif(N)
SigmaEps <- 0.5
Eps  <- rnorm(N, mean = 0, sd = SigmaEps) 
RE   <- rnorm(10, mean = 0, sd = 1.5)
Hive <- rep(1:10, each = 100)
y1   <- a + b * X1 + RE[Hive] + Eps
fHive <- factor(Hive)
S2 <- lme(y1 ~ X1, random =~ 1 | fHive)
summary(S2)



#Unbalanced data
##################

a   <- 1
b   <- 2
N   <- 1000
X1  <- runif(N)
SigmaEps <- 0.5
Eps  <- rnorm(N, mean = 0, sd = SigmaEps) 
RE   <- rnorm(10, mean = 0, sd = 1.5)
Hive <- rep(1:10, each = 100)
y1   <- a + b * X1 + RE[Hive] + Eps
fHive <- factor(Hive)

MyData <- data.frame(
            y1 = y1,
            X1 = X1,
            Hive = Hive)

GroupSize <- c(1, 10, 25, 32, 99, 87, 5, 54, 7, 12)
AllHives <- unique(Hive)
AllSelectedData <- NULL
for (i in AllHives) {
	Datai <- MyData[Hive ==i, ]
	SelectedRows <- sample(1:100, GroupSize[i])
	SelectedData <- Datai[SelectedRows,]
	AllSelectedData <- rbind(AllSelectedData, SelectedData)
}
AllSelectedData

AllSelectedData$fHive <- factor(AllSelectedData$Hive)

S2 <- lme(y1 ~ X1, random =~ 1 | fHive,
          data = AllSelectedData)
summary(S2)






