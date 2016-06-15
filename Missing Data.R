# This file is the application to Gelman and Hill (ch 25 Missing data problem)
# Bing-Jie Yen


install.packages("xtable")
install.packages("mice")
install.packages("arm")
install.packages("agricolae") 
library(xtable)
library(mice)
library(agricolae)
library(lattice) #for stripplot
library(arm) #se.coef


#Download the data

## Downloading the data

setwd("d:/Dropbox/2015 WUSTL 104-1/Multilevel Modeling for Quantitative Research/Oct 29 Missing Data/HW_missing_data")
getwd()
star98.missing <- read.table("star98.missing.dat.txt",header=TRUE)
summary(star98.missing)
dim(star98.missing)
# Determine the missing data
sum(is.na(star98.missing))
md.pattern(star98.missing)
## Ploting the variables
#par(mar=c()sets the bottom, left, top and right margins respectively of the plot region in number of lines of text. 
#plot(x axis name, y axis name, pch="the symbol", col="color")
#abline( ) This function adds one or more straight lines through the current plot. 
par(mfrow=c(1,2),mar=c(3,3,3,3)) 
plot(star98.missing$SUBSIDIZED.LUNCH, star98.missing$READING.ABOVE.50, pch="+", col="blue") #plot(x axis name, y axis name)
abline(lm(star98.missing$READING.ABOVE.50~star98.missing$SUBSIDIZED.LUNCH), lwd=3)
mtext(side=1, cex=1, line=2, "District Percent Receiving Subsidized Lunch")
mtext(side=2, cex=1, line=2, "District Percent Above National Reading Median")


plot(star98.missing$PTRATIO,star98.missing$READING.ABOVE.50, pch="*", col="blue")
abline(lm(star98.missing$READING.ABOVE.50~star98.missing$PTRATIO), lwd=3)
mtext(side=1, cex=1, line=2, "District Pupil/Teacher Ratio")
mtext(side=2, cex=1, line=2, "District Percent Above National Reading Median")
mtext(side=3, cex=1.5, outer=TRUE, line=-2,"Calfornia 9th Grade by District, 1998") #side=3 is the name for the whole graph


## Assessing the incidence of missing

#sum(as.numeric(is.na(star98.missing[,1])))  it means the total number of the missing values in subsidized lunch
#dim(star98.missing)[1]  
sum(as.numeric(is.na(star98.missing[,1])))/dim(star98.missing)[1]
sum(as.numeric(is.na(star98.missing[,2])))/dim(star98.missing)[1]
sum(as.numeric(is.na(star98.missing[,3])))/dim(star98.missing)[1]
#################################
##  ANSWER: a quick examination reveals that there is a substantial amount of missing data.
##  The dataset contains three variables: (a) the % of students receiving a subsidized lunch; (b)
##  the pupil/teacher ratio; and (c) the % of students above the national reading median. For
##  each of these variables, the proportion of missing data ranges between 30% and 35% of the
##  sample, a huge amount:
####################################


##Case-wise deletion

star98.missing.omit<-na.omit(star98.missing)
dim(star98.missing.omit)[1]/dim(star98.missing)[1]

star98.sum <- round(cbind(summary(star98.missing[,1])[-7], summary(star98.missing.omit[,1]),summary(star98.missing[,2])[-7], summary(star98.missing.omit[,2]),summary(star98.missing[,3])[-7], summary(star98.missing.omit[,3])), 1)
xtable(star98.sum)
###################
####  ANSWER:To see whether there is a discernible pattern, we create a new dataset that contains complete
### cases only, and compare the summary statistics. The new dataset contains only 89 observa-
###  tions, less than a third than the original one. Yet as the table below shows, there is little
### dfference between the two datasets. This can happen for one of two reasons: (a) missing is
### completely at random; or (b) the missing values are concentrated in the same observations.
########################


##An examination of the missing pattern using the md.pattern() function reveals that the
##last scenario does not appear to be the case, as we have all kinds of combination in our
##missing data. Maybe we are lucky and the data is MCAR: because only 3 observations miis all three variables

md.pattern(star98.missing)

## Running the model
##Creating the imputations

##We then use the mice() function to create 10 imputed datasets. A quick look at the distri-bution of the imputed data (in red) suggests that there are no major problems:
star98.imp<-mice(star98.missing, m=10, seed=1)

####################
##Diagnostics######
###################
### We use the stripplot to see that the imputation data match the original data
stripplot(star98.imp, pch=20, cex=0.75) #pch= symbol sign

# Imputation method by using mice()
mod.imp<-with(star98.imp,lm(READING.ABOVE.50~SUBSIDIZED.LUNCH+PTRATIO))
summary(pool(mod.imp))

#Case-wise deletion method
mod.comp<-lm(READING.ABOVE.50~SUBSIDIZED.LUNCH+PTRATIO,data=star98.missing)
summary(mod.comp)


###########ANSWER
###Finally, we run a linear model with READING.ABOVE.50 as the response and SUBSIDIZED.LUNCH
###and PTRATIO as predictors. Comparing the results of the MI approach with that of the com-
###plete cases approach, we see that there is little difference in the coeffcients, despite the fact
###that we have a substantial amount of missing data. This is consistent with the theory that
###the data is MCAR

#################
#### Suicide attack in Israel
###################

harr<-read.table("harrison3.txt", header=TRUE)
apply(harr,2,is.na)  # call the all the missing values in columns
apply(apply(harr,2,is.na),2,sum)   #  sum up the number of all missing values
attach(harr)


######After we select the variable of interest, we see that only 3 of them (AgeofFirstAttacker,
#######FirstAttackerisFemale and TargetisMilitary) contain missing data:

### Generate a data set includes the variables we are interested in
harr2 <- data.frame(cbind(NumberKilled, NumberInjured, AgeofFirstAttacker,
                          Date, ResponsibleisMartyrs, AttackerisChallenged,
                          FirstAttackerisFemale,
                          ResponsibleisPIJ, TargetisBus, TargetisCheckpoint, DeviceisCar,
                          TargetisCafe, TargetisMilitary, ResponsibleHamas))
detach(harr)
apply(apply(harr2, 2, is.na), 2, sum)

##Generate new variable 
harr2$TotalVictims <- harr2$NumberKilled + harr2$NumberInjured
summary(harr2)
par(mfrow=c(1,3)) # how to put the graphs try par(mfrow(3,1))
hist(harr2$TotalVictims, breaks=12, main="Total Victims")
hist(harr2$NumberKilled, breaks=12, main="Killed")
hist(harr2$NumberInjured, breaks=12, main="Injured")




## Creating the imputed datasets
## Create imputations
harrs.imp<-mice(harr2, m=10, seed=1234)
stripplot(harrs.imp,pch=20, cex=0.75) #Diagonosis

##Analysis
## Number Killed
mod.imp.K<-with (harrs.imp, glm(NumberKilled~AgeofFirstAttacker+
                                ResponsibleisMartyrs+AttackerisChallenged+
                                FirstAttackerisFemale+
                                ResponsibleisPIJ+ TargetisBus+TargetisCheckpoint+DeviceisCar+
                                TargetisCafe+ TargetisMilitary, family=poisson(link="log")))
summary(pool(mod.imp.K))



mod.imp.I<-with (harrs.imp, glm(NumberInjured~AgeofFirstAttacker+
                                  ResponsibleisMartyrs+AttackerisChallenged+
                                  FirstAttackerisFemale+
                                  ResponsibleisPIJ+ TargetisBus+TargetisCheckpoint+DeviceisCar+
                                  TargetisCafe+ TargetisMilitary, family=poisson(link="log")))
summary(pool(mod.imp.I))

mod.imp.T<-with (harrs.imp, glm(TotalVictims~AgeofFirstAttacker+
                                  ResponsibleisMartyrs+AttackerisChallenged+
                                  FirstAttackerisFemale+
                                  ResponsibleisPIJ+ TargetisBus+TargetisCheckpoint+DeviceisCar+
                                  TargetisCafe+ TargetisMilitary, family=poisson(link="log")))
summary(pool(mod.imp.T))

##Case-wise deletion



model.K<-glm(NumberKilled ~ AgeofFirstAttacker + ResponsibleisMartyrs
             + AttackerisChallenged + FirstAttackerisFemale
             + ResponsibleisPIJ + TargetisBus + TargetisCheckpoint + DeviceisCar
             + TargetisCafe + TargetisMilitary, 
             family=poisson(link="log"), data=harr2)
            
summary(model.K)

model.I<-glm(NumberInjured~AgeofFirstAttacker+
                ResponsibleisMartyrs+AttackerisChallenged+
                FirstAttackerisFemale+
                ResponsibleisPIJ+ TargetisBus+TargetisCheckpoint+DeviceisCar+
                TargetisCafe+ TargetisMilitary, family=poisson(link="log"),data=harr2)
summary(model.I)


model.T<-glm(TotalVictims~AgeofFirstAttacker+
                ResponsibleisMartyrs+AttackerisChallenged+
                FirstAttackerisFemale+
                ResponsibleisPIJ+ TargetisBus+TargetisCheckpoint+DeviceisCar+
                TargetisCafe+ TargetisMilitary, family=poisson(link="log"),data=harr2)
summary(model.T)


##Creating the table to plot the coefficients

coef.table <- cbind(coef(model.K), summary(pool(mod.imp.K))[,1],
                    coef(model.I), summary(pool(mod.imp.I))[,1],
                    coef(model.T), summary(pool(mod.imp.T))[,1])
se.table <- cbind(se.coef(model.K), summary(pool(mod.imp.K))[,2],
                  se.coef(model.I), summary(pool(mod.imp.I))[,2],
                  se.coef(model.T), summary(pool(mod.imp.T))[,2])
##Creating x limts variable
maxmin.table <- cbind(apply(coef.table, 1, min),
                      apply(coef.table, 1, max), apply(se.table, 1, max))
xlimits <- cbind(maxmin.table[,1] - 1.96 * maxmin.table[,3],
                 maxmin.table[,2] + 1.96 * maxmin.table[,3])

maxmin.table <- cbind(apply(coef.table, 1, min),
                      apply(coef.table, 1, max), apply(se.table, 1, max))
xlimits <- cbind(maxmin.table[,1] - 1.96 * maxmin.table[,3],
                 maxmin.table[,2] + 1.96 * maxmin.table[,3])
xlimits[11,] <- cbind(-0.863596 - 1.96 * 1.007747, -0.135802 + 1.96 * 1.007747)

## Because there is a serious problem of multicollinearity for one of the models
par(mfrow=c(4,4), mar=c(2, 1, 3, 1))
for(i in 1:nrow(coef.table)){
  plot(x=coef.table[i,c(1,3,5)], y=c(1, 3, 5),
       xlim=range(xlimits[i,]), ylim=c(1, 6),
       yaxt="n", xlab="", ylab="",
       main=rownames(coef.table)[i],
       pch=16, col="blue", cex=1.5)
  points(x=coef.table[i,c(2,4,6)], y=c(2, 4, 6),
         pch=16, cex=1.5, col="red")
  segments(x0=coef.table[i,c(1,3,5)] - 1.96 * se.table[i,c(1,3,5)],
           x1=coef.table[i,c(1,3,5)] + 1.96 * se.table[i,c(1,3,5)],
           y0=c(1, 3, 5), y1=c(1, 3, 5), col="blue", lwd=1.5)
  segments(x0=coef.table[i,c(2,4,6)] - 1.96 * se.table[i,c(2,4,6)],
           x1=coef.table[i,c(2,4,6)] + 1.96 * se.table[i,c(2,4,6)],
           y0=c(2, 4, 6), y1=c(2, 4, 6), col="red", lwd=1.5)
  abline(v=0, lty=2, col="gray50")
  abline(h=c(2.5, 4.5), lty=3, col="gray50")
}
