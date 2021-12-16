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
voters <- read.delim("Data/voter_stats_20201103.txt")
voter_hist <- read.delim("Data/history_stats_20201103.txt")

# selecting a random sample of 25 counties
sample_list <- unique(voters[c("county_desc")])
set.seed(4747)
counties <- sample_n(sample_list, 25)[,]
voter_hist <- voter_hist[is.element(voter_hist$county_desc,counties),]
voters <- voters[is.element(voters$county_desc,counties),]



# aggregating data and discarding variables
aggregated_data <- aggregate(voter_hist$total_voters, list(county_desc=voter_hist$county_desc,precinct_abbrv = voter_hist$precinct_abbrv,
                                                           age= voter_hist$age, party_cd=voter_hist$party_cd, ethnic_code=voter_hist$ethnic_code,
                                                           race_code=voter_hist$race_code, sex_code=voter_hist$sex_code),sum)
colnames(aggregated_data)[8] <- c("turnout")



# dropping variables
# drop1 <- c("election_date","stats_type", "update_date","vtd_abbrv")
# voters <- voters[,!(names(voters) %in% drop1)]     
voters_agg <- aggregate(voters$total_voters, list(county_desc=voters$county_desc,precinct_abbrv = voters$precinct_abbrv,
                                                  age= voters$age, party_cd=voters$party_cd, ethnic_code=voters$ethnic_code,
                                                  race_code=voters$race_code, sex_code=voters$sex_code),sum)
colnames(voters_agg)[8] <- c('total_voters')

# joining both datasets
new <- left_join(aggregated_data, voters_agg, by = NULL, copy = FALSE, suffix = c(".x", ".y"),)
new <- drop_na(new)
bool <- new$turnout > new$total_voters
new[which(bool), "turnout"] <- new[which(bool), "total_voters"]
colnames(new)[9] <- c("registered_count")

## Checking if we have empty 
sapply(new, function(x) sum(is.na(x)))

# remove unnecessary datasets so that it doesn't take up memory
rm(aggregated_data, voter_hist, voters, voters_agg)

## Creating a % column

new$per_turnout <- (new$turnout/new$registered_count) * 100


#trying AIC to get an idea of interactions

#NullModel<-glm(per_turnout ~ county_desc+age+party_cd+ethnic_code+race_code+sex_code, data=new,family=binomial)
#FullModel<-glm(per_turnout ~ county_desc+age+party_cd+ethnic_code+race_code+sex_code+
        #         county_desc*age+ county_desc*party_cd+county_desc*ethnic_code+county_desc*race_code+county_desc*sex_code+
            #     age*county_desc+ age*party_cd+age*ethnic_code+age*race_code+age*sex_code+
            #     party_cd*age+ party_cd*county_desc+party_cd*ethnic_code+party_cd*race_code+party_cd*sex_code+
             #    ethnic_code*age+ ethnic_code*county_desc+ethnic_code*party_cd+ethnic_code*race_code+ethnic_code*sex_code+
              #   race_code*age+ race_code*county_desc+race_code*party_cd+race_code*ethnic_code+race_code*sex_code+
              #   sex_code*age+ sex_code*county_desc+sex_code*party_cd+sex_code*race_code+sex_code*race_code
              #   , data=new, family=binomial)

#Model_stepwiseA <- step(NullModel, scope = list(upper=FullModel, lower=NullModel),direction="both",trace=0)
#Model_stepwiseA$call
data<-new
ggplot(data,aes(x=county_desc,y=per_turnout,fill=county_desc))+geom_boxplot()+labs(title= "county vs per_turnout",x="county_desc",y="per_turnout")+theme_classic()+theme(legend.position = "none")
ggplot(data,aes(x=county_desc,y=per_turnout,fill=county_desc))+geom_boxplot()+labs(title= "county vs per_turnout, facet_wrap(~age) ",x="county_desc",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~age)
ggplot(data,aes(x=county_desc,y=per_turnout,fill=county_desc))+geom_boxplot()+labs(title= "county vs per_turnout, facet_wrap(~party_cd)",x="county_desc",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~party_cd)
ggplot(data,aes(x=county_desc,y=per_turnout,fill=county_desc))+geom_boxplot()+labs(title= "county vs per_turnout, facet_wrap(~sex_code)",x="county_desc",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~sex_code)
ggplot(data,aes(x=county_desc,y=per_turnout,fill=county_desc))+geom_boxplot()+labs(title= "county vs per_turnout, facet_wrap(~race_code)",x="county_desc",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~race_code)
ggplot(data,aes(x=county_desc,y=per_turnout,fill=county_desc))+geom_boxplot()+labs(title= "county vs per_turnout, facet_wrap(~ethnic_code)",x="county_desc",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~ethnic_code)

#age
ggplot(data,aes(x=age,y=per_turnout,fill=age))+geom_boxplot()+labs(title= "age vs per_turnout",x="age",y="per_turnout")+theme_classic()+theme(legend.position = "none")
ggplot(data,aes(x=age,y=per_turnout,fill=age))+geom_boxplot()+labs(title= "age vs per_turnout, facet_wrap(~county_desc)",x="age",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~county_desc)
ggplot(data,aes(x=age,y=per_turnout,fill=age))+geom_boxplot()+labs(title= "age vs per_turnout, facet_wrap(~party_cd)",x="age",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~party_cd)
ggplot(data,aes(x=age,y=per_turnout,fill=age))+geom_boxplot()+labs(title= "age vs per_turnout, facet_wrap(~sex_code)",x="age",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~sex_code)
ggplot(data,aes(x=age,y=per_turnout,fill=age))+geom_boxplot()+labs(title= "age vs per_turnout, facet_wrap(~race_code)",x="age",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~race_code)
ggplot(data,aes(x=age,y=per_turnout,fill=age))+geom_boxplot()+labs(title= "age vs per_turnout, facet_wrap(~ethnic_code)",x="age",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~ethnic_code)

#party_cd
ggplot(data,aes(x=party_cd,y=per_turnout,fill=party_cd))+geom_boxplot()+labs(title= "party_cd vs per_turnout",x="party_cd",y="per_turnout")+theme_classic()+theme(legend.position = "none")
ggplot(data,aes(x=party_cd,y=per_turnout,fill=party_cd))+geom_boxplot()+labs(title= "party_cd vs per_turnout, facet_wrap(~age)",x="party_cd",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~age)
ggplot(data,aes(x=party_cd,y=per_turnout,fill=party_cd))+geom_boxplot()+labs(title= "party_cd vs per_turnout, facet_wrap(~county_desc)",x="party_cd",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~county_desc)
ggplot(data,aes(x=party_cd,y=per_turnout,fill=party_cd))+geom_boxplot()+labs(title= "party_cd vs per_turnout, facet_wrap(~sex_code)",x="party_cd",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~sex_code)
ggplot(data,aes(x=party_cd,y=per_turnout,fill=party_cd))+geom_boxplot()+labs(title= "party_cd vs per_turnout, facet_wrap(~race_code)",x="party_cd",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~race_code)
ggplot(data,aes(x=party_cd,y=per_turnout,fill=party_cd))+geom_boxplot()+labs(title= "party_cd vs per_turnout, facet_wrap(~ethnic_code)",x="party_cd",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~ethnic_code)


#ethnic code
ggplot(data,aes(x=ethnic_code,y=per_turnout,fill=ethnic_code))+geom_boxplot()+labs(title= "ethnic_code vs per_turnout",x="ethnic_code",y="per_turnout")+theme_classic()+theme(legend.position = "none")
ggplot(data,aes(x=ethnic_code,y=per_turnout,fill=ethnic_code))+geom_boxplot()+labs(title= "ethnic_code vs per_turnout, facet_wrap(~age)",x="ethnic_code",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~age)
ggplot(data,aes(x=ethnic_code,y=per_turnout,fill=ethnic_code))+geom_boxplot()+labs(title= "ethnic_code vs per_turnout, facet_wrap(~county_desc)",x="ethnic_code",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~county_desc)
ggplot(data,aes(x=ethnic_code,y=per_turnout,fill=ethnic_code))+geom_boxplot()+labs(title= "ethnic_code vs per_turnout, facet_wrap(~sex_code)",x="ethnic_code",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~sex_code)
ggplot(data,aes(x=ethnic_code,y=per_turnout,fill=ethnic_code))+geom_boxplot()+labs(title= "ethnic_code vs per_turnout, facet_wrap(~race_code)",x="ethnic_code",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~race_code)
ggplot(data,aes(x=ethnic_code,y=per_turnout,fill=ethnic_code))+geom_boxplot()+labs(title= "ethnic_code vs per_turnout, facet_wrap(~party_cd)",x="ethnic_code",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~party_cd)

#race_code
ggplot(data,aes(x=race_code,y=per_turnout,fill=race_code))+geom_boxplot()+labs(title= "race_code vs per_turnout",x="race_code",y="per_turnout")+theme_classic()+theme(legend.position = "none")
ggplot(data,aes(x=race_code,y=per_turnout,fill=race_code))+geom_boxplot()+labs(title= "race_code vs per_turnout, facet_wrap(~age)",x="race_code",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~age)
ggplot(data,aes(x=race_code,y=per_turnout,fill=race_code))+geom_boxplot()+labs(title= "race_code vs per_turnout, facet_wrap(~county_desc)",x="race_code",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~county_desc)
ggplot(data,aes(x=race_code,y=per_turnout,fill=race_code))+geom_boxplot()+labs(title= "race_code vs per_turnout, facet_wrap(~sex_code)",x="race_code",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~sex_code)
ggplot(data,aes(x=race_code,y=per_turnout,fill=race_code))+geom_boxplot()+labs(title= "race_code vs per_turnout, facet_wrap(~ethnic_code)",x="race_code",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~ethnic_code)
ggplot(data,aes(x=race_code,y=per_turnout,fill=race_code))+geom_boxplot()+labs(title= "race_code vs per_turnout, facet_wrap(~party_cd)",x="race_code",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~party_cd)


#sex_code
ggplot(data,aes(x=sex_code,y=per_turnout,fill=sex_code))+geom_boxplot()+labs(title= "sex_code vs per_turnout",x="sex_code",y="per_turnout")+theme_classic()+theme(legend.position = "none")
ggplot(data,aes(x=sex_code,y=per_turnout,fill=sex_code))+geom_boxplot()+labs(title= "sex_code vs per_turnout, facet_wrap(~age)",x="sex_code",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~age)
ggplot(data,aes(x=sex_code,y=per_turnout,fill=sex_code))+geom_boxplot()+labs(title= "sex_code vs per_turnout, facet_wrap(~county_desc)",x="sex_code",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~county_desc)
ggplot(data,aes(x=sex_code,y=per_turnout,fill=sex_code))+geom_boxplot()+labs(title= "sex_code vs per_turnout, facet_wrap(~ethnic_code)",x="sex_code",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~ethnic_code)
ggplot(data,aes(x=sex_code,y=per_turnout,fill=sex_code))+geom_boxplot()+labs(title= "sex_code vs per_turnout, facet_wrap(~race_code)",x="sex_code",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~race_code)
ggplot(data,aes(x=sex_code,y=per_turnout,fill=sex_code))+geom_boxplot()+labs(title= "sex_code vs per_turnout, facet_wrap(~party_cd)",x="sex_code",y="per_turnout")+theme_classic()+theme(legend.position = "none")+facet_wrap(~party_cd)


#sex_code*age

