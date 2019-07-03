# statistical.analysis
### Statistical Analysis of Non-Pair Matched Contingency Table Data: A Non-Technical Primer
### Additional File 1
### Ochieng Amos Okutse
### Jomo Kenyatta University of Agriculture & Technology
### This is part of a paper to be submitted to Biochemia Medica;
############################################################################################
#####Measures of Diagnostic Accuracy for a 2*2 table########################################
############################################################################################
## Scott et al. 2008, Table 1:
## A new diagnostic test was trialled on 1586 patients. Of 744 patients
## that were disease positive, 670 tested positive. Of 842 patients that 
## were disease negative, 640 tested negative. What is the likeliood
## ratio of a positive test? What is the number needed to diagnose?

library(epiR)
dat <- as.table(matrix(c(670,202,74,640), nrow = 2, byrow = TRUE))
colnames(dat) <- c("Dis+","Dis-")
rownames(dat) <- c("Test+","Test-")
rval <- epi.tests(dat, conf.level = 0.95)
print(rval) 
epistats=summary(rval)
epistats

##extracting the various diagnostic tests;
# sensitivity value
sensitivity=round(epistats$est[3], 4)
sensitivity
#specificity
specificity=round(epistats$est[4], 4)
specificity
#positive predictive value
pprev=round(epistats$est[9],4)
pprev
#negative predictive value
nprev=round(epistats$est[10],4)
nprev
#likelihood ratios
#LR+
LRpositive=round(epistats$est[11],4)
LRpositive
LRnegative=round(epistats$est[12],4)
LRnegative
#diagnostic odds ratio
diag.ratio=round(epistats$est[4],4)
diag.ratio
#diagnostic accuracy
diag.acc=round(epistats$est[5],4)
diag.acc
##this code produces a citation for the above named R package.
citation(package="epiR")

## Test sensitivity is 0.90 (95% CI 0.88 -- 0.92). Test specificity is 
## 0.76 (95% CI 0.73 -- 0.79). The likelihood ratio of a positive test 
## is 3.75 (95% CI 3.32 to 4.24). The number needed to diagnose is 
## 1.51 (95% CI 1.41 to 1.65). Around 15 persons need to be tested 
## to return 10 positive tests.



######################################################################################################################################
##### Measures of Disease Frequency##################################################################################################
#####################################################################################################################################
##Example 2 shaper et al., 1981 

## The British Regional Heart Study was a 
##cohort study of 7735 men aged 40-59 years randomly selected from genera practices 
##in 24 British towns, with the aim of identifying risk factors for ischemic heart 
##disease. Of the 7718 men who provided information on smoking status, 5899(76.4%) 
##had smoked at some stage during their lives. Over the subsequent 10 years, 650 of these 
##7718 men (8.4%) had a myocardial infraction (MI). The following were the results displayed
##in 2x2 table show the number and percentage of 
##smokers and non-smokers who developed and did not develop and MI over the 10 year period. 

tab1=as.table(matrix(c(563,5336,87,1732),nr=2,byrow=T))
rownames(tab1)=c("smoked","not smoked")
colnames(tab1)=c("yes","no")
rval1=epi.tests(tab1,conf.level=0.95)
print(rval1)
summary(rval1)
##calculating prevalence using epi.tests in epiR;

##suppose that this was a prevalence study then the prevalence will be 
true.prevalence=summary(rval1)$est[2]
true.prevalence
apparent.prev=summary(rval1)$est[1]
apparent.prev

##calculating incidence in exposed (smokers) and unexposed (non smokers);
prop.test(tab1) ##test for equality of proportions
dat2.test=prop.test(tab1)
names(dat2.test)
##incidence in the exposed and unexposed;
incidence.smokers=dat2.test$estimate[1]
incidence.smokers
incidence.nonsmokers=dat2.test$estimate[2]
incidence.nonsmokers

####incidence in smokers is 9.5% whereas in the non smokers is 4.8%; incidence in the smokers
#### is twice the incidence in the non smoking male study population.

########################################################################
######### Measures of Association/Risk of a disease#####################
########################################################################
## Example 3 Shaper et al. 1981 same illustration as in Example 2. The British Heart Cohort Study

testdat=as.table(matrix(c(563,5336,87,1732),nr=2,byrow=T))
rownames(testdat)=c("smoked","not smoked")
colnames(testdat)=c("yes","no")

riskstats=summary(epi.2by2(dat=testdat, method="cohort.count",
                   conf.level=0.95, units=100, outcome="as.columns"))

print(riskstats)

##relative risk is 1.99 
riskstats$RR.strata.wald
##Relative Risk;
##The risk of developing ischemic heart disease is 1.99 (1.60 to 2.48) times higher in 
##smokers than in non smokers 


##Odds ratio 
riskstats$OR.strata.wald
## Odds ratio:
## The odds of having ischemic heart disease for smokers is 2.100
## (95% CI 1.66 to 2.64) times greater than the odds of having ischemic heart disease
## for non-smokers.

##Risk Difference
risk.difference=riskstats$ARisk.strata.wald
risk.difference

##Risk Difference:
##The difference between the risk of 
##developing ischemic heart disease in smokers and non smokers is 4.76 (95% CI 3.52 to 5.99)


###########################################################################
############ Statistical Tests for 2 by 2 Contingency Tables##############
##########################################################################

###   1. CHI SQUARE TEST FOR INDEPENDENCE/ASSOCIATION ###

#Example 4
#To illustrate the use of χ2 test in testing independence, 
#we consider a study in which interest is in examining the association 
#between a disease and a possible risk factor for infection. 
#In this study by Rivers et al., (2001) the aim is estimating early goal directed therapy
#in the treatment of sepsis and septic shock. 
#Of the 263 patients who were allocated to either early goal directed therapy or 
#the standard treatment, 236 patients completed the study with the outcomes of interest (29). 
#The outcomes are summarized in Table 6.
dt=as.table(matrix(c(38,79,59,60),nr=2,byrow=T))
rownames(dt)=c("died","survived")
colnames(dt)=c("early goal therapy","standard treatment")

chisq.test(dt)
##interpretation;
##Using the p-value approach where p<0.05 indicates statistical significance, from the above test, 
##we reject H0 and conclude that there is an association between early diagnostic therapy and the risk of 
##sepsis and septic shock (χ2(1) = 6.4382, p= .011).

