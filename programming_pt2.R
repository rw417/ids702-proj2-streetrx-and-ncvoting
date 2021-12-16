knitr::opts_chunk$set(echo = TRUE)
# Load packages
library(ggplot2)
library(tidyverse)
library(arm)
library(pROC)
library(rms)
library(e1071)
library(caret)
require(gridExtra)
library(leaps)
library(dplyr)

### Part 2

# Import Data ####
voters <- read.delim("Data/voter_stats_20201103.txt")
voter_hist <- read.delim("Data/history_stats_20201103.txt")

# Data Prep ####

# selecting a random sample of 25 counties
sample_list <- unique(voters[c("county_desc")])
set.seed(4747)
counties <- sample_n(sample_list, 25)[,]
voter_hist <- voter_hist[is.element(voter_hist$county_desc,counties),]
voters <- voters[is.element(voters$county_desc,counties),]


# aggregating data and discarding variables
aggregated_data <- aggregate(voter_hist$total_voters, 
                             list(county_desc=voter_hist$county_desc,
                                  age= voter_hist$age, 
                                  party_cd=voter_hist$voted_party_cd, 
                                  ethnic_code=voter_hist$ethnic_code, 
                                  race_code=voter_hist$race_code, 
                                  sex_code=voter_hist$sex_code),sum)
colnames(aggregated_data)[7] <- c("turnout")


# dropping variables
# drop1 <- c("election_date","stats_type", "update_date","vtd_abbrv")
# voters <- voters[,!(names(voters) %in% drop1)]     
voters_agg <- aggregate(voters$total_voters, 
                        list(county_desc=voters$county_desc,
                             age= voters$age, 
                             party_cd=voters$party_cd, 
                             ethnic_code=voters$ethnic_code,
                             race_code=voters$race_code, 
                             sex_code=voters$sex_code),sum)
colnames(voters_agg)[7] <- c('total_voters')

# joining both datasets
new <- left_join(aggregated_data, voters_agg, by = NULL, copy = FALSE, suffix = c(".x", ".y"),)
new <- drop_na(new)
bool <- new$turnout > new$total_voters
new[which(bool), "turnout"] <- new[which(bool), "total_voters"]
colnames(new)[8] <- c("registered_count")

## Checking if we have empty 
sapply(new, function(x) sum(is.na(x)))

# remove unnecessary datasets so that it doesn't take up memory
rm(aggregated_data, voter_hist, voters, voters_agg)

## Creating a % column
new$per_turnout <- (new$turnout/new$registered_count) * 100


# Combined race and sex categories with fewer obs
# combine LIB, GRE, CST parties
small_parties <- c("LIB","GRE","CST")
new$party_cd_combined <- new$party_cd
new[is.element(new$party_cd_combined, small_parties),]$party_cd_combined <- 'LIB_GRE_CST'

# combine race
small_races <- c('A', 'M', 'P','I','O')
new$race_code_combined <- new$race_code
new[is.element(new$race_code_combined, small_races),'race_code_combined'] <- 'Other'


# factorize
new$party_cd <- as.factor(new$party_cd)
new$ethnic_code <- as.factor(new$ethnic_code)
new$race_code <- as.factor(new$race_code)
new$sex_code <- as.factor(new$sex_code)
new$party_cd_combined <- as.factor(new$party_cd_combined)
new$race_code_combined <- as.factor(new$race_code_combined)

# change baseline
new$ethnic_code <- relevel(new$ethnic_code, ref="NL")
new$race_code_combined <- relevel(new$race_code_combined, ref="W")

# EDA ####
# See each team member's individual file for the bulk of EDA, but here are some of the more interesting interactions plots:

# party by age
# pattern seems a little different for old vs young - should investigate
ggplot(new, aes(x=party_cd_combined, y=per_turnout, fill=party_cd_combined)) +
  geom_boxplot() + 
  facet_wrap(~age) +
  theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

# party by ethnic code
# patterns are different - include
ggplot(new, aes(x=party_cd_combined, y=per_turnout, fill=party_cd_combined)) +
  geom_boxplot() + 
  facet_wrap(~ethnic_code) +
  theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))


# Modeling ####
## No interactions
model1 <- glmer(cbind(turnout, registered_count-turnout)~age+party_cd_combined+ethnic_code+sex_code +race_code_combined+ (1|county_desc),
                family = binomial(link = 'logit'),data = new,
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(model1)
dotplot(ranef(model1, condVar=TRUE))$county_desc
library(broom.mixed)
tidy(model1,conf.int=TRUE,exponentiate=FALSE,effects="fixed")

## Interactions asked by Michael
# party * age, party * sex
model2 <- glmer(cbind(turnout, registered_count-turnout)~party_cd_combined*(age+sex_code)+ethnic_code+race_code_combined+(1|county_desc),
                family = binomial(link = 'logit'),data = new,
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(model2)
dotplot(ranef(model2, condVar=TRUE))$county_desc
tidy(model2,conf.int=TRUE,exponentiate=FALSE,effects="fixed")

anova(model1, model2, test="Chi-sq")

## Additional interactions
# party * age, party * sex, party * race, race * sex
model3 <- glmer(cbind(turnout, registered_count-turnout)~party_cd_combined*(age+sex_code+race_code_combined)+ethnic_code+race_code_combined:sex_code+(1|county_desc),
                family = binomial(link = 'logit'),data = new,
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(model3)
exp(summary(model3)$coef)

dotplot(ranef(model3, condVar=TRUE))$county_desc
tidy(model3,conf.int=TRUE,exponentiate=FALSE,effects="fixed")


anova(model1, model2, test="Chi-sq")
anova(model2, model3, test="Chi-sq")
AIC(model1); BIC(model1)

new$fitted_model3 <- fitted(model3)
new$resid_model3 <- residuals(model3,"resp")
ggplot(data=new, aes(x=fitted_model3,y=resid_model3)) + geom_point()


# Final fixed effects interactions
# party * age, party * sex, race * sex
model4 <- glmer(cbind(turnout, registered_count-turnout)~party_cd_combined*(age+sex_code)+ethnic_code+race_code_combined*sex_code+(1|county_desc),
                family = binomial(link = 'logit'),data = new,
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
saveRDS(model4, "model4")
anova(model3, model4, test="Chi-sq")

## plots
dotplot(ranef(model4, condVar=TRUE))$county_desc


# add in varying slopes for race | county
model5 <- glmer(cbind(turnout, registered_count-turnout)~party_cd_combined*(age+sex_code)+ethnic_code+race_code_combined*sex_code+(race_code_combined|county_desc),
                family = binomial(link = 'logit'),data = new,
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model5)

anova(model4, model5, test="Chi-sq")
AIC(model4);AIC(model5)

## plots
dotplot(ranef(model5, condVar=TRUE))$county_desc
plot_model(model5, type="diag")


## save model
saveRDS(model5, "final_model")
final_model <- readRDS("final_model")


# Model Assessment ####
# VIF
gvif(model3)
library(performance)
check_collinearity(model3)

# ROC Curve
invisible(roc(new$per_turnout,fitted(model3),plot=T,print.thres="best",legacy.axes=T,
              print.auc =T,col="red3"))

# confusion matrix
#Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(model3) >= mean(new$per_turnout), "1","0")),
#                            as.factor(arsenic$switch),positive = "1")

# Binned Plot
resid <- residuals(model3,"resp")
binnedplot(x=fitted(model3),y=resid,xlab="Pred. Probabilities",
           col.int="red4",ylab="Avg. Residuals",main="Binned Residual vs Fitted Values",col.pts="navy")




final_model <- readRDS("final_model")

# Visualizing Final Model
dotplot(ranef(final_model, condVar=TRUE))$county_desc

