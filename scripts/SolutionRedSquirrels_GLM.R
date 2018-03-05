#    Introduction to Introduction to MCMC, 
#    Linear Mixed Effects models and GLMM with R
#    Highland Statistics Ltd.
#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  


######################################################################
#Squirrel example
#Set the working directory and import the data
setwd("F:/Statistics/SARD11_MixedModel&GLM")
setwd("/home/ed/Dropbox/WORK/courses/course_ED.6F7Z1012.Stats.Res.Design/2013/labs/lab11.GLM and mixed")
SQ <- read.table(file   = "RedSquirrels.txt", 
                 header = TRUE, 
                 dec    = ".")

str(SQ)
names(SQ)
# [1] "Id"          "Squcones"    "Ntrees"     
# [4] "DBHav"       "T_heightav"  "CanopyCover"
###################################################################




###################################################################
#Load packages and library files
library(lattice)  #Needed for multi-panel graphs
#source the file: HighstatLibV6.R
source(file="HighstatLibV6.R")  
#It can be dowloaded from the course website. 
##################################################################


##################################################################
#Data exploration
#Outliers
MyVar <- c("SqCones", "Ntrees", "DBH", 
           "TreeHeight", "CanopyCover")

Mydotplot(SQ[,MyVar])
#Ouch....1 large DBH value!
#A few small Canopy values!
#A few large Ntrees values.
#  There may be a patch with large
#  DBH and NTrees value!

#Remove the value with extreme large DBH value
SQ2 <- subset(SQ, DBH < 0.6)
dim(SQ)
dim(SQ2)


#Collinearity
MyVar <- c("Ntrees", "DBH", 
           "TreeHeight", 
           "CanopyCover")

pairs(SQ2[, MyVar], 
      lower.panel = panel.cor)

#look at "variance inflation factors" - a measure of correlation between independent variables
corvif(SQ2[,MyVar]) #looking for values >4 or so
#In principle that is ok!!
#But the outliers may be doing funny things!


#Relationships
MyVar <- c("Ntrees", "DBH", 
           "TreeHeight", 
           "CanopyCover")
Myxyplot(SQ2, MyVar, "SqCones", 
         MyYlab = "Number of stripped cones")
#Yes...that point was indeed going to cause trouble!!!


#Zero inflation?
plot(table(SQ2$SqCones))
#No

#Conclusion data exploration: 
#1 outlier in DBH
#Small amount of collinearity
#Weak patterns between Y and X
#Potentially NB distribution for SqCones
#####################################################





#####################################################
#Start analysis

#Standardize independent variables as good practice
MyStd <- function(x) { (x - mean(x)) / sd(x)}

SQ2$Ntrees.std      <- MyStd(SQ2$Ntrees)
SQ2$TreeHeight.std  <- MyStd(SQ2$TreeHeight)
SQ2$CanopyCover.std <- MyStd(SQ2$CanopyCover)


######################################
#Step 1: Fit a model.
#This is the Poisson GLM
 #SqCones_i ~ Poisson(mu_i)
 #E(SqCones_i) = mu_i
 #var(SqCones_i) = mu_i
 #log(mu_i) = alpha + beta_1 * Ntrees.std_i + .... 
 #                    beta_3 * CanopyCover_std_i

M1 <- glm(SqCones ~ Ntrees.std +  TreeHeight.std + 
          CanopyCover.std,
          family = "poisson",
          data = SQ2)

summary(M1)
#Model validation
#Assess overdispersion:
#Pearson residuals
# (Y - E(Y)) / sqrt(var(Y)) = (Y - mu) / sqrt(mu)
E1 <- resid(M1, type = "pearson")
sum(E1^2) / (nrow(SQ2) - length(coef(M1)))
#[1] 13.68  #this should be close to 1
#if less than 1 you have underdispersion - LIKE LESS THAN .9 - .85
#if greater than 1 - LIKE, GREATER THAN 1.5 - you have overdispersion
#here we are very overdispersed
#the larger the overdispersion, the larger the effect on p-values in model results


#Why do we have overdispersion?
  #A. Outliers?                  ==> Remove them?
  #B. Missing covariates or interactions?  ==> Go back or..add a latent variable 
  #C. Zero inflation?            ==> ZIP ; zero inflated poisson
  #D. Large variance?            ==> NB ; negative binomial
  #E. Correlation?               ==> GLMM
  #F. Non-linear patterns        ==> GAM 
  #G. Wrong link function        ==> Change it 


#Standard steps to address some of these points:
F1 <- fitted(M1, type = "response")

par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x = F1, 
     y = E1, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(h = 0, lty = 2)
     
#A. Outliers
plot(cooks.distance(M1), #measures single data points with "high influence"
     type = "h",
     xlab = "Observation", 
     ylab = "Cook distance",
     cex.lab =  1.5)

#Still heterogeneity
plot(y = SQ2$SqCones, 
     x = F1,
     xlab = "Fitted values",
     ylab = "Observed data",
     cex.lab = 1.5,
     xlim = c(0,60), 
     ylim = c(0,60) )
abline(coef = c(0, 1), lty = 2)   

#C. Zero inflation
sum(SQ2$SqCone==0) / nrow(SQ2)  #proportion of zeros - ~25% is "officially zero-inflated"


#F. Non-linear pattenrs
MyVar <- c("Ntrees.std", "DBH", 
           "TreeHeight.std", 
           "CanopyCover.std")

SQ2$E1 <- E1    
Myxyplot(SQ2, MyVar, "E1")

     
#We cannot pinpoint any of the commen causes for
#overdispersion. The variation in the data is relatively
#large...try a different family, like negative binomial?     
###############################################




#Negative binomial GLM
library(MASS)
M2 <- glm.nb(SqCones ~ Ntrees.std +  #this is the glm function for the negative binomial distribution family
             TreeHeight.std + CanopyCover.std,
             data = SQ2)
summary(M2)

#Assess overdispersion:
E2 <- resid(M2, type = "pearson")
N  <- nrow(SQ2)
p  <- length(coef(M2)) + 1  #+1 is due to k
sum(E2^2) / (N - p)
#That's ok

#########THE REST IS OPTIONAL##############
#########THE REST IS OPTIONAL##############
#########THE REST IS OPTIONAL##############
#########THE REST IS OPTIONAL##############

###Below deswcribes the model parameters for negative binomial
##NB that R output for glm.nb in {MASS} calls is dispersion factor AND theta (instead of k as below)
#This is the model:
#SqCones_i ~ NB(mu_i, k)
#E(SqCones_i)   = mu_i
#var(SqCones_i) = mu_i + mu_i^2 / k
#               = mu_i + alpha * mu_i^2  where alpha = 1/k 
#log(mu_i)      = Intercept + beta_1 *NTrees.std_i + ...... 

#SqCones_i ~ NB(mu_i, 1.0791)
#E(SqCones_i)   = mu_i
#var(SqCones_i) = mu_i + mu_i^2 / 1.0791
#log(mu_i)      = 2.6180 +  0.3059 *NTrees.std_i + ...... 


#Step 2: Is everything significant?
drop1(M2, test = "Chi")
#Some of the terms are not significant
#Option 1: Leave the model as it is
#Option 2: Drop covariates
step(M2)  #backwards selection using AIC


#Model validation # par(mfrow = c(1,1))
F2 <- fitted(M2, type = "response")
plot(x = F2, 
     y = E2,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)
     
SQ2$E2 <- E2
Myxyplot(SQ2, MyVar, "E2")
#Can't really fault the model. The small sample size does not
#make it easy neither
###########################################################





###########################################################
#Model interpretation of the NB GLM
#Sketch the fitted values

par(mfrow = c(1,3), mar = c(5,5,2,2))
plot(x = SQ2$Ntrees.std , 
     y = SQ2$SqCones,
     xlab = "Standardized number of trees",
     ylab = "Number of stripped cones",
     cex.lab = 2,
     pch = 16, type = "n")

range(SQ2$Ntrees.std)
MyData <- data.frame(Ntrees.std = seq(-1.1, 4, length = 25),
                     TreeHeight.std  = 0,
                     CanopyCover.std = 0)
P1 <- predict(M2, newdata = MyData, type = "link", se = TRUE)
lines(x = MyData$Ntrees.std, y = exp(P1$fit), lwd = 3)
lines(x = MyData$Ntrees.std, y = exp(P1$fit + 2*P1$se.fit), lwd = 3, lty = 2)
lines(x = MyData$Ntrees.std, y = exp(P1$fit - 2*P1$se.fit), lwd = 3, lty = 2)
     

plot(x=SQ2$TreeHeight.std , y = SQ2$SqCones,
     xlab = "Standardized tree height",
     ylab = "Number of stripped cones",
     cex.lab = 2,
     pch = 16, type = "n")

range(SQ2$TreeHeight.std)
MyData <- data.frame(Ntrees.std = 0,
                     TreeHeight.std  = seq(-3, 1.5, length = 25),
                     CanopyCover.std = 0)
P1 <- predict(M2, newdata = MyData, type = "link", se = TRUE)
lines(x = MyData$TreeHeight.std, y = exp(P1$fit), lwd = 3)
lines(x = MyData$TreeHeight.std, y = exp(P1$fit + 2*P1$se.fit), lwd = 3, lty = 2)
lines(x = MyData$TreeHeight.std, y = exp(P1$fit - 2*P1$se.fit), lwd = 3, lty = 2)
     


plot(x=SQ2$CanopyCover.std , y = SQ2$SqCones,
     xlab = "Standardized canopy cover",
     ylab = "Number of stripped cones",
     cex.lab = 2,
     pch = 16, type = "n")

range(SQ2$CanopyCover.std)
MyData <- data.frame(Ntrees.std = 0,
                     TreeHeight.std  = 0,
                     CanopyCover.std = seq(-4, 1, length = 25))
P1 <- predict(M2, newdata = MyData, type = "link", se = TRUE)
lines(x = MyData$CanopyCover.std, y = exp(P1$fit), lwd = 3)
lines(x = MyData$CanopyCover.std, y = exp(P1$fit + 2*P1$se.fit), lwd = 3, lty = 2)
lines(x = MyData$CanopyCover.std, y = exp(P1$fit - 2*P1$se.fit), lwd = 3, lty = 2)
     
