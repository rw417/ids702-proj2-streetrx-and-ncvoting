## Part 2 EDA
# Prep work ####
# Load packages
library(ggplot2)
library(tidyverse)
library(arm)
library(pROC)
library(rms)
library(e1071)
library(caret)
library(gridExtra)
library(leaps)
library(lme4)
library(dplyr)
library(glmmTMB)
library(sjPlot)

### Part 2
voters <- read.delim("C:/Users/HP/GitHub/team-project-2-streetrx-and-voting-in-nc-purple-team/Data/voter_stats_20201103.txt")
voter_hist <- read.delim("C:/Users/HP/GitHub/team-project-2-streetrx-and-voting-in-nc-purple-team/Data/history_stats_20201103.txt")

# selecting a random sample of 25 counties
sample_list <- unique(voters[c("county_desc")])
set.seed(4747)
counties <- sample_n(sample_list, 25)[,]
voter_hist <- voter_hist[is.element(voter_hist$county_desc,counties),]
voters <- voters[is.element(voters$county_desc,counties),]



# aggregating data and discarding variables
aggregated_data <- aggregate(voter_hist$total_voters, list(county_desc=voter_hist$county_desc,
                                                           age= voter_hist$age, party_cd=voter_hist$party_cd, ethnic_code=voter_hist$ethnic_code,
                                                           race_code=voter_hist$race_code, sex_code=voter_hist$sex_code),sum)
colnames(aggregated_data)[7] <- c("turnout")



# dropping variables
# drop1 <- c("election_date","stats_type", "update_date","vtd_abbrv")
# voters <- voters[,!(names(voters) %in% drop1)]     
voters_agg <- aggregate(voters$total_voters, list(county_desc=voters$county_desc,
                                                  age= voters$age, party_cd=voters$party_cd, ethnic_code=voters$ethnic_code,
                                                  race_code=voters$race_code, sex_code=voters$sex_code),sum)
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


# check for NA
sapply(voting_clean, function(x) sum(is.na(x)))
sapply(voter_hist, function(x) sum(is.na(x)))
sapply(new, function(x) sum(is.na(x)))  # there is NA in total_voters

# drop na
new <- na.omit(new)
sapply(new, function(x) sum(is.na(x)))



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



 
## data validation ####
voters[(voters$precinct_abbrv=="SCC") & (voters$race_code=="W") & (voters$sex_code=="F") & (voters$age=="Age 41 - 65") & (voters$party_cd == "DEM") & (voters$ethnic_code=="NL"),]
voter_hist[(voter_hist$precinct_abbrv=="SCC") & (voter_hist$race_code=="W") & (voter_hist$sex_code=="F") & (voter_hist$age=="Age 41 - 65") & (voter_hist$party_cd == "DEM") & (voter_hist$ethnic_code=="NL"),]
new[(new$precinct_abbrv=="SCC") & (new$race_code=="W") & (new$sex_code=="F") & (new$age=="Age 41 - 65") & (new$party_cd == "DEM") & (new$ethnic_code=="NL"),]
aggregated_data[(aggregated_data$precinct_abbrv=="SCC") & (aggregated_data$race_code=="W") & (aggregated_data$sex_code=="F") & (aggregated_data$age=="Age 41 - 65") & (aggregated_data$party_cd == "DEM") & (aggregated_data$ethnic_code=="NL"),]

voters[(voters$precinct_abbrv=="091") & (voters$race_code=="W") & (voters$sex_code=="F") & (voters$age=="Age 41 - 65") & (voters$parttey_cd == "DEM") & (voters$ethnic_code=="NL"),]
voter_hist[(voter_hist$precinct_abbrv=="091") & (voter_hist$race_code=="W") & (voter_hist$sex_code=="F") & (voter_hist$age=="Age 41 - 65") & (voter_hist$party_cd == "DEM") & (voter_hist$ethnic_code=="NL"),]
new[(new$precinct_abbrv=="091") & (new$race_code=="W") & (new$sex_code=="F") & (new$age=="Age 41 - 65") & (new$party_cd == "DEM") & (new$ethnic_code=="NL"),]
aggregated_data[(aggregated_data$precinct_abbrv=="091") & (aggregated_data$race_code=="W") & (aggregated_data$sex_code=="F") & (aggregated_data$age=="Age 41 - 65") & (aggregated_data$party_cd == "DEM") & (aggregated_data$ethnic_code=="NL"),]


## Explore turnout and total voters
### county
new %>%
  filter(!is.na(registered_count) & !is.na(turnout)) %>%
  group_by(county_desc) %>%
  summarise(pct_turnout = sum(turnout)/sum(registered_count))

new %>%
  filter(!is.na(registered_count) & !is.na(turnout)) %>%
  group_by(county_desc) %>%
  summarise(sum(registered_count, rm.na=TRUE)) %>%
  print(n=Inf)

### precinct
new %>%
  filter(!is.na(registered_count) & !is.na(turnout)) %>%
  group_by(precinct_abbrv) %>%
  summarise(pct_turnout = sum(turnout)/sum(registered_count)) %>%
  filter(pct_turnout > 1)

### raw
sum(new$turnout)
sum(new$registered_count, rm.na=TRUE)
new[is.na(new$registered_count)==TRUE,]  # check for NAs




# EDA ####
## Response Variable ####
ggplot(data=new, aes(x=per_turnout)) +
  geom_histogram(color = "black", fill = "grey", bins=20)


## county ####
## different turnout rates between counties
new %>%
  group_by(county_desc) %>%
  summarise(total_turnout = sum(turnout)) %>%
  arrange(desc(total_turnout))

ggplot(new, aes(x=county_desc, y=per_turnout, fill=county_desc)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))


# county by sex
# cannot see any pattern here
ggplot(new, aes(x=county_desc, y=per_turnout, fill=county_desc)) +
  geom_boxplot() + 
  facet_wrap(~sex_code) +
  theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))


# county by party
# does seem to have some different patterns
# we may need to group the smaller parties together
new %>%
  group_by(party_cd) %>%
  summarise(sum(turnout))

sample_source <- c("DEM","REP","UNA")
ggplot(new[is.element(new$party_cd, sample_source),], aes(x=county_desc, y=per_turnout, fill=county_desc)) +
  geom_boxplot() + 
  facet_wrap(~party_cd) +
  theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))




## precinct ####
# will not pursue for now
new %>%
  group_by(precinct_abbrv) %>%
  summarise(total_turnout = sum(turnout)) %>%
  arrange(desc(total_turnout))

sample_source <- sample_n(unique(new['precinct_abbrv']), 25)[,]
ggplot(new[is.element(new$precinct_abbrv, sample_source),], aes(x=county_desc, y=per_turnout, fill=county_desc)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))




## party_cd ####
new %>%
  group_by(party_cd_combined) %>%
  summarise(mean_turnout = mean(per_turnout)) %>%
  arrange(desc(mean_turnout))


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


# party by race code
# no clear differences but may be worth to include
new %>%
  group_by(race_code_combined, party_cd_combined) %>%
  summarise(total_turnout = sum(turnout)) %>%
  #arrange((total_turnout)) %>%
  print(n=40)

new %>%
  group_by(race_code_combined) %>%
  summarise(total_turnout = sum(turnout)) %>%
  #arrange((total_turnout)) %>%
  print(n=40)

ggplot(new, aes(x=party_cd_combined, y=per_turnout, fill=party_cd_combined)) +
  geom_boxplot() + 
  facet_wrap(~race_code_combined) +
  theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))


# party by sex
# 'U' has a different pattern - include
ggplot(new, aes(x=party_cd_combined, y=per_turnout, fill=party_cd_combined)) +
  geom_boxplot() + 
  facet_wrap(~sex_code) +
  theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))




 
## age ####
# older people more likely to vote
new %>%
  group_by(age) %>%
  summarise(mean_turnout = mean(per_turnout)) %>%
  arrange(desc(mean_turnout))


# age by ethnic_code
# younger HL equally likely to vote; younger NL much less likely to vote - include
ggplot(new, aes(x=age, y=per_turnout, fill=age)) +
  geom_boxplot() + 
  facet_wrap(~ethnic_code) +
  theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))


# age by race
# patterns different across races - include
ggplot(new, aes(x=age, y=per_turnout, fill=age)) +
  geom_boxplot() + 
  facet_wrap(~race_code_combined) +
  theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))


# age by sex
# males over 65 much more likly to vote, not so much for females - might include
ggplot(new, aes(x=age, y=per_turnout, fill=age)) +
  geom_boxplot() + 
  facet_wrap(~sex_code) +
  theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))




## ethnicity ####
# HL much more likely to vote
new %>%
  group_by(ethnic_code) %>%
  summarise(mean_turnout = mean(per_turnout)) %>%
  arrange(desc(mean_turnout))

ggplot(new, aes(x=ethnic_code, y=per_turnout, fill=ethnic_code)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))


# ethnicity by race
# doesn't actually make sense - don't use
new %>%
  group_by(ethnic_code, race_code_combined) %>%
  summarise(total_turnout = sum(turnout))

ggplot(new, aes(x=ethnic_code, y=per_turnout, fill=ethnic_code)) +
  geom_boxplot() + 
  facet_wrap(~race_code_combined) +
  theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))


# ethnicity by sex
# not much difference except U - ambivalent
ggplot(new, aes(x=ethnic_code, y=per_turnout, fill=ethnic_code)) +
  geom_boxplot() + 
  facet_wrap(~sex_code) +
  theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))


## race ####
# minority groups more likely; W and B less likely
new %>%
  group_by(race_code_combined) %>%
  summarise(mean_turnout = mean(per_turnout)) %>%
  arrange(desc(mean_turnout))

new %>%
  group_by(race_code) %>%
  summarise(total_turnout = sum(turnout)) %>%
  arrange(desc(total_turnout))

ggplot(new, aes(x=race_code_combined, y=per_turnout, fill=race_code_combined)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))


# race by sex
# no difference except for U - ambivalent
ggplot(new, aes(x=race_code_combined, y=per_turnout, fill=race_code_combined)) +
  geom_boxplot() + 
  facet_wrap(~sex_code) +
  theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))




# sex ####
# M and F equally likely; U much more likely
new %>%
  group_by(sex_code) %>%
  summarise(mean_turnout = mean(per_turnout)) %>%
  arrange(desc(mean_turnout))

ggplot(new, aes(x=sex_code, y=per_turnout, fill=sex_code)) +
  geom_boxplot() +
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
setwd("C:/Users/HP/GitHub/team-project-2-streetrx-and-voting-in-nc-purple-team/EDA")
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




# Questions to ask Michael ####
# what can we do when binned plots are really bad?

# what are ways to evaluate a hierarchical logistic model?

# how to evaluate logistic models when we don't have continuous predictor variables?

# does it make sense to build confusion matrix and ROC curve for hierarchical model?

# how to check if we could use varying slopes in the EDA?

# how to interpret SE of random effects? Context?

