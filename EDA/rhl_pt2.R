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
library(lme4)
library(hglm)

### Part 2
# Importing/Cleaning Data
voters <- read.delim("Data/voter_stats_20201103.txt")
voter_hist <- read.delim("Data/history_stats_20201103.txt")

# selecting a random sample of 25 counties
sample_list <- unique(voters[c("county_desc")])
set.seed(4747)
counties <- sample_n(sample_list, 25)[,]
voter_hist <- voter_hist[is.element(voter_hist$county_desc,counties),]
voters <- voters[is.element(voters$county_desc,counties),]

aggregated_data <- aggregate(voter_hist$total_voters, 
                             list(county_desc=voter_hist$county_desc,
                                  age= voter_hist$age, 
                                  party_cd=voter_hist$voted_party_cd, 
                                  ethnic_code=voter_hist$ethnic_code, 
                                  race_code=voter_hist$race_code, 
                                  sex_code=voter_hist$sex_code),sum)
colnames(aggregated_data)[7] <- c("turnout")

voters_agg <- aggregate(voters$total_voters, 
                        list(county_desc=voters$county_desc,
                             age= voters$age, 
                             party_cd=voters$party_cd, 
                             ethnic_code=voters$ethnic_code,
                             race_code=voters$race_code, 
                             sex_code=voters$sex_code),sum)
colnames(voters_agg)[7] <- c('total_voters')

new <- left_join(aggregated_data, voters_agg, by = c('county_desc', 'age', 'party_cd', 'ethnic_code', 'race_code', 'sex_code'), copy = FALSE, suffix = c(".x", ".y"),)
new <- drop_na(new)
bool <- new$turnout > new$total_voters

new[which(bool), "turnout"] <- new[which(bool), "total_voters"]
colnames(new)[8] <- c("registered_count")
new$per_turnout <- (new$turnout/new$registered_count) * 100

## EDA
# Total number of registered voters:1,414,146, and turnout was 1,066,719 for a turnout of 75.4%
sum(new$registered_count)
sum(new$turnout)
sum(new$turnout)/sum(new$registered_count)

# Plotted turnout for each county. Definitely a difference. When looking at table, we can see that percent turnout ranges from 63% to 83%.
ggplot(new, aes(x = county_desc, y = per_turnout)) + geom_boxplot()
by_county <- aggregate(new[,c('turnout', 'registered_count')], by=list(county_desc = new$county), FUN=sum)
by_county$per_turnout <- by_county$turnout/by_county$registered_count
by_county %>% arrange(desc(per_turnout))

# Sex vs. Turnout
by_sex <- aggregate(new[,c('turnout', 'registered_count')], by=list(county = new$county, sex = new$sex_code), FUN=sum)
by_sex$per_turnout <- by_sex$turnout/by_sex$registered_count
ggplot(by_sex, aes(x=sex, y=per_turnout)) + geom_boxplot()

# By ethnicity
by_eth <- aggregate(new[,c('turnout', 'registered_count')], by=list(ethnicity = new$ethnic_code, county = new$county_desc), FUN=sum)
by_eth$per_turnout <- by_eth$turnout/by_eth$registered_count
ggplot(by_eth, aes(x=ethnicity, y=per_turnout)) + geom_boxplot()

# By age
by_age <- aggregate(new[,c('turnout', 'registered_count')], by=list(age = new$age, county = new$county_desc), FUN=sum)
by_age$per_turnout <- by_age$turnout/by_age$registered_count
ggplot(by_age, aes(x=age, y=per_turnout)) + geom_boxplot()

# By race
by_race <- aggregate(new[,c('turnout', 'registered_count')], by=list(race = new$race_code, county = new$county_desc), FUN=sum)
by_race$per_turnout <- by_race$turnout / by_race$registered_count
ggplot(by_race, aes(x=race, y=per_turnout)) + geom_boxplot()

# By party
by_party <- aggregate(new[,c('turnout', 'registered_count')], by=list(party = new$party_cd, county = new$county_desc), FUN=sum)
by_party$per_turnout <- by_party$turnout/by_party$registered_count
ggplot(by_party, aes(x=party, y=per_turnout)) + geom_boxplot()

### Interactions
## Age and everything
# Race and age
race_age <- aggregate(new[,c('turnout', 'registered_count')], by=list(age = new$age, race = new$race_code, county = new$county_desc), FUN=sum)
race_age$per_turnout <- race_age$turnout/race_age$registered_count
ggplot(race_age, aes(x = age, y = per_turnout)) + geom_boxplot() + facet_wrap(~race)

# Sex and age
sex_age <- aggregate(new[,c('turnout', 'registered_count')], by=list(age = new$age, sex = new$sex_code, county = new$county_desc), FUN=sum)
sex_age$per_turnout <- sex_age$turnout/sex_age$registered_count
ggplot(sex_age, aes(x = age, y = per_turnout)) + geom_boxplot() + facet_wrap(~sex)

# Eth and age
eth_age <- aggregate(new[,c('turnout', 'registered_count')], by=list(age = new$age, ethnicity = new$ethnic_code, county = new$county_desc), FUN=sum)
eth_age$per_turnout <- eth_age$turnout / eth_age$registered_count
ggplot(eth_age, aes(x = age, y = per_turnout)) + geom_boxplot() + facet_wrap(~ethnicity)

# Party and age
party_age <- aggregate(new[,c('turnout', 'registered_count')], by=list(age = new$age, party = new$party_cd, county = new$county_desc), FUN=sum)
party_age$per_turnout <- party_age$turnout/party_age$registered_count
ggplot(party_age, aes(x = age, y = per_turnout)) + geom_boxplot() + facet_wrap(~party)

# County and age
county_age <- aggregate(new[,c('turnout', 'registered_count')], by=list(age = new$age, county_desc = new$county_desc), FUN=sum)
county_age$per_turnout <- county_age$turnout / county_age$registered_count

ggplot(new, aes(x = age, y = per_turnout)) + geom_boxplot() + facet_wrap(~county_desc)

## Race and everything
# Race and sex
race_sex <- aggregate(new[,c('turnout', 'registered_count')], by=list(race = new$race, sex = new$sex_code, county = new$county_desc), FUN=sum)
race_sex$per_turnout <- race_sex$turnout/race_sex$registered_count
lim <- race_sex %>% filter(race %in% c('B', 'W'))
lim <- lim %>% filter(sex %in% c('M', 'F'))
ggplot(lim, aes(x=race, y = per_turnout)) + geom_boxplot() + facet_wrap(~sex)
ggplot(race_sex, aes(x=race, y = per_turnout)) + geom_boxplot() + facet_wrap(~sex)

# Race and ethnicity
race_eth <- aggregate(new[,c('turnout', 'registered_count')], by=list(race = new$race_code, eth = new$ethnic_code, county = new$county_desc), FUN=sum)
race_eth$per_turnout <- race_eth$turnout/race_eth$registered_count
ggplot(race_eth, aes(x=race, y = per_turnout)) + geom_boxplot() + facet_wrap(~eth)


# Race and party, this could be interesting.
race_party <- aggregate(new[,c('turnout', 'registered_count')], by=list(race = new$race_code, party = new$party_cd, county = new$county_desc), FUN=sum)
race_party$per_turnout <- race_party$turnout/race_party$registered_count
ggplot(race_party, aes(x=race, y = per_turnout)) + geom_boxplot() + facet_wrap(~party)

# Race and county
ggplot(new, aes(x = race_code, y = per_turnout)) + geom_boxplot() + facet_wrap(~county_desc)

## Party and everything
# Party and sex
party_sex <- aggregate(new[,c('turnout', 'registered_count')], by=list(party = new$party_cd, sex = new$sex_code, county = new$county_desc), FUN=sum)
party_sex$per_turnout <- party_sex$turnout/party_sex$registered_count
ggplot(party_sex, aes(x = party, y = per_turnout)) + geom_boxplot() + facet_wrap(~sex)

# Party and ethnicity
party_eth <- aggregate(new[,c('turnout', 'registered_count')], by=list(party = new$party_cd, eth = new$ethnic_code, county = new$county_desc), FUN=sum)
party_eth$per_turnout <- party_eth$turnout/party_eth$registered_count
ggplot(party_eth, aes(x = party, y = per_turnout)) + geom_boxplot() + facet_wrap(~eth)

lim4 <- party_eth %>% filter(party %in% c('DEM', 'REP', 'UNA'))
ggplot(lim4, aes(x = eth, y = per_turnout)) + geom_boxplot() + facet_wrap(~party)

# Party and county
ggplot(new, aes(x= party_cd, y = per_turnout)) + geom_boxplot() + facet_wrap(~county_desc)


## Sex and everything
# Sex and ethnicity
sex_eth <- aggregate(new[,c('turnout', 'registered_count')], by=list(sex = new$sex_code, eth = new$ethnic_code, county = new$county_desc), FUN=sum)
sex_eth$per_turnout <- sex_eth$turnout/sex_eth$registered_count
ggplot(sex_eth,aes(x=eth, y = per_turnout)) + geom_boxplot() + facet_wrap(~sex)

# Sex and county
ggplot(new, aes(x = sex_code, y = per_turnout)) + geom_boxplot() + facet_wrap(~county_desc)
  
### MODELING

model1 <- glmer(cbind(turnout, registered_count)~age+party_cd+ethnic_code+sex_code +race_code+population+ (1|county_desc),
               family = binomial(link = 'logit'),
               data= new)

small_races <- c('A', 'M', 'P','I','O')
new$race_code_combined <- new$race_code
new[is.element(new$race_code_combined, small_races),'race_code_combined'] <- 'Other'
model5 <- glmer(cbind(turnout, registered_count-turnout)~party*(age+sex)+ethnicity+race*sex + (race|county_desc),
                family = binomial(link = 'logit'),data = new,
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
