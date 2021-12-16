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
library(lme4)
library(sjPlot)


## Part 1
# We left the variables api_temp and source in the dataset even though we
# don't intend on using it for our model

# loading the data
load("Data/streetrx.RData")
head(streetrx)

# subsetting based on our assigned drug
streetrx1 <- streetrx[streetrx$api_temp== "lorazepam",]

# taking out all predictors that we don't need
drop <- c("yq_pdate", "price_date", "city", "country","Primary_Reason", "api_temp")
streetrx1 = streetrx1[,!(names(streetrx1) %in% drop)]

# dropping missing values 
sum(is.na(streetrx1$ppm))
sapply(streetrx1, function(x) sum(is.na(x)))
# 30 rows with missing values. The only variables with missing are ppm and mgstr, so we drop.
streetrx_f <- drop_na(streetrx1)
sapply(streetrx_f, function(x) sum(is.na(x)))

## checking structure of variables
str(streetrx_f)



## EDA
# Structure of response variable, price per milligram. Wildly skewed, and almost uninterpretable.
summary(streetrx_f$ppm)
ggplot(data=streetrx_f, aes(x=ppm)) +
  geom_histogram(color = "black", fill = "grey", bins=20)

# 19 values above 100, out of 6,105 total. Definitely outliers to worry about.
sum(streetrx_f$ppm > 100)

# regraph histogram with those excluded.
temp <- streetrx_f[streetrx_f$ppm <= 50,]
ggplot(data=temp, aes(x=ppm)) +
  geom_histogram(color = "black", fill = "grey", bins=20)


# interestingly, there seems to be a grouping effect, where people round their prices to a 5 or 10 value above a certain point.
table(streetrx_f$ppm[streetrx_f$ppm >= 20])

# What if we use the log?
streetrx_f$ppml <- log(streetrx_f$ppm)
ggplot(data=streetrx_f, aes(x=ppml)) +
  geom_histogram(color = "black", fill = "grey", bins=20)

## Lets look at eaach predicter. First, the continuous predictor, strength of the dose.
# Outlier at 18 mg
summary(streetrx_f$mgstr)
table(streetrx_f$mgstr)
# Looks like it's better as a factor

streetrx_f$mgstr_f <- as.factor(streetrx_f$mgstr)
ggplot(data=streetrx_f, aes(x=mgstr, y = ppm)) + geom_boxplot()
ggplot(data=streetrx_f, aes(x=mgstr, y = ppml)) + geom_boxplot()


# It appears that the price per milligram decreases as strength increases. (we have to ignore the two highest levels)
aggregate(streetrx_f[,c('ppm')], by=list(mgstr = streetrx_f$mgstr), FUN=median)
aggregate(streetrx_f[,c('ppm')], by=list(mgstr = streetrx_f$mgstr), FUN=mean)


# Let's do by state
aggregate(streetrx_f[,c('ppm')], by=list(mgstr = streetrx_f$state), FUN=mean) %>% arrange(desc(x))
aggregate(streetrx_f[,c('ppm')], by=list(mgstr = streetrx_f$state), FUN=median) %>% arrange(desc(x))

ggplot(data=streetrx_f, aes(x=state, y = ppm)) + geom_boxplot()
# log
ggplot(data=streetrx_f, aes(x=state, y = ppml)) + geom_boxplot()
temp3 <- aggregate(streetrx_f[,c('ppml')], by=list(mgstr = streetrx_f$state), FUN=mean)
ggplot(temp3, aes(x = mgstr, y = x)) + geom_col()



# By region. This is not very different, but there are some changes.
aggregate(streetrx_f[,c('ppm')], by=list(mgstr = streetrx_f$USA_region), FUN=mean) %>% arrange(desc(x))
aggregate(streetrx_f[,c('ppm')], by=list(mgstr = streetrx_f$USA_region), FUN=median) %>% arrange(desc(x))

ggplot(data=streetrx_f, aes(x=USA_region, y = ppm)) + geom_boxplot()
# log
ggplot(data=streetrx_f, aes(x=USA_region, y = ppml)) + geom_boxplot()
aggregate(streetrx_f[,c('ppml')], by=list(mgstr = streetrx_f$USA_region), FUN=mean) %>% arrange(desc(x))


# By form. Only one form, so not useful.
aggregate(streetrx_f[,c('ppm')], by=list(mgstr = streetrx_f$form_temp), FUN=mean) %>% arrange(desc(x))

# By bulk purchase. Bulk purchase is actually more expensive?? Not if you use median, however.
aggregate(streetrx_f[,c('ppm')], by=list(bulkpurchase = streetrx_f$bulk_purchase), FUN=mean) %>% arrange(desc(x))
aggregate(streetrx_f[,c('ppm')], by=list(bulkpurchase = streetrx_f$bulk_purchase), FUN=median) %>% arrange(desc(x))

ggplot(data=streetrx_f, aes(x=bulk_purchase, y = ppm)) + geom_boxplot()
ggplot(data=streetrx_f, aes(x=bulk_purchase, y = ppml)) + geom_boxplot()

# Let's see if we can figure out source
keep = c('Personal', 'Heard it', 'Internt', 'Internet Pharmacy')
streetrx_f$source <- as.character(streetrx_f$source)

streetrx_f <- streetrx_f %>% mutate(source = ifelse(source %in% keep, source, 'Other'))
streetrx_f$source <- as.factor(streetrx_f$source)

ggplot(data=streetrx_f, aes(x=source, y = ppm)) + geom_boxplot()
ggplot(data=streetrx_f, aes(x=source, y = ppml)) + geom_boxplot()

# mostly a difference in internet pharmacy, which is a small sample size. But also, heard it is lower.
aggregate(streetrx_f[,c('ppm')], by=list(source = streetrx_f$source), FUN=mean) %>% arrange(desc(x))
aggregate(streetrx_f[,c('ppml')], by=list(source = streetrx_f$source), FUN=mean) %>% arrange(desc(x))

### INTERACTIONS
## Let's look at region
# Region and strength, not much.
ggplot(data=streetrx_f, aes(x=mgstr_f, y = ppm)) + geom_boxplot() + facet_wrap(~USA_region)
ggplot(data=streetrx_f, aes(x=mgstr_f, y = ppml)) + geom_boxplot() + facet_wrap(~USA_region)

aggregate(streetrx_f[,c('ppm')], by=list(strength = streetrx_f$mgstr, region = streetrx_f$USA_region), FUN=mean) %>% arrange(desc(x))
aggregate(streetrx_f[,c('ppml')], by=list(region = streetrx_f$USA_region, strength = streetrx_f$mgstr), FUN=mean) %>% arrange(desc(region), desc(strength))

# Region and bulk purchase
ggplot(data=streetrx_f, aes(x=bulk_purchase, y = ppm)) + geom_boxplot() + facet_wrap(~USA_region)
ggplot(data=streetrx_f, aes(x=bulk_purchase, y = ppml)) + geom_boxplot() + facet_wrap(~USA_region)

#Definitely potential for interaction here.
aggregate(streetrx_f[,c('ppm')], by=list(bulk = streetrx_f$bulk_purchase, region = streetrx_f$USA_region), FUN=mean) %>% arrange(desc(region))

# Region and source
ggplot(data=streetrx_f, aes(x=source, y = ppm)) + geom_boxplot() + facet_wrap(~USA_region)
ggplot(data=streetrx_f, aes(x=source, y = ppml)) + geom_boxplot() + facet_wrap(~USA_region)

# Can't really tell, maybe only in internet pharamcy, which has a small n
temp1 <-aggregate(streetrx_f[,c('ppml')], by=list(source = streetrx_f$source, region = streetrx_f$USA_region), FUN=mean) %>% arrange(desc(region))
ggplot(temp1, aes(x = source, y = x)) + geom_col() + facet_wrap(~region)

## Now look at state
# State and bulk. Definitely an interaction here.
state_bulk <- aggregate(streetrx_f[,c('ppml')], by=list(bulk = streetrx_f$bulk_purchase, state = streetrx_f$state), FUN=mean) %>% arrange(desc(state))
ggplot(state_bulk, aes(x = bulk, y = x)) + geom_col() + facet_wrap(~state)

# State and strength. Probably not enough data?
state_strength <- aggregate(streetrx_f[,c('ppml')], by=list(strength = streetrx_f$mgstr, state = streetrx_f$state), FUN=mean) %>% arrange(desc(state))
state_strength <- state_strength[state_strength$strength %in% c('0.5', '1', '2'),]
ggplot(state_strength, aes(x = strength, y = x)) + geom_col() + facet_wrap(~state)

## Now look at bulk and strength
bulk_strength <- aggregate(streetrx_f[,c('ppml')], by=list(bulk = streetrx_f$bulk_purchase, strength = streetrx_f$mgstr), FUN=mean)
ggplot(bulk_strength, aes(x = bulk, y = x)) + geom_col() + facet_wrap(~strength)


### Modeling!
## Fit just state
model1 <- lmer(ppml ~ (1|state) + bulk_purchase + mgstr + source, data = streetrx_f)
model2 <- lmer(ppml ~ (1|state) + (1|USA_region) + bulk_purchase + mgstr + source, data = streetrx_f)

anova(model1, model2, test="Chisq")

model3 <- lmer(ppml ~ (1|state) + bulk_purchase + mgstr_f + source, data = streetrx_f)
model4 <- lmer(ppml ~ (bulk_purchase|state) + (1|USA_region) + mgstr_f + source, data = streetrx_f)
summary(model4)
dotplot(ranef(model6, condVar=TRUE))$state
dotplot(ranef(model6, condVar=TRUE))$USA_region

model5 <- lmer(ppml ~ (mgstr_f|state) + (1|USA_region) + mgstr_f + source + source*mgstr_f + source*bulk_purchase + mgstr_f*bulk_purchase, data = streetrx_f)
model6 <- lmer(ppml ~ (mgstr_f|state) + (1|USA_region) + mgstr_f + source + bulk_purchase, data = streetrx_f)
model7 <- lmer(ppml ~ (mgstr_f|state) + (1|USA_region) + mgstr_f + source + mgstr_f*bulk_purchase, data = streetrx_f)

summary(model5)

confint.merMod(model5)
anova(model5,model6)
anova(model5, model7)
