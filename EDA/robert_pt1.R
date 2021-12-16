## Part 1 EDA

# load packages
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
library(sjstats)


  ## Part 1
# We left the variables api_temp and source in the dataset even though we
# don't intend on using it for our model

# loading the data
load("C:/Users/HP/GitHub/team-project-2-streetrx-and-voting-in-nc-purple-team/Data/streetrx.RData")
head(streetrx)

# subsetting based on our assigned drug
streetrx1 <- streetrx[streetrx$api_temp== "lorazepam",]

# taking out all predictors that we don't need
drop <- c("yq_pdate", "price_date", "city", "country","Primary_Reason")
streetrx1 = streetrx1[,!(names(streetrx1) %in% drop)]

# dropping missing values 
sum(is.na(streetrx1$ppm))
sapply(streetrx1, function(x) sum(is.na(x)))
streetrx_f <- drop_na(streetrx1)
sapply(streetrx_f, function(x) sum(is.na(x)))

## checking structure of variables
str(streetrx_f)

rm(streetrx)
rm(streetrx1)
rm(streetrx_reduced)

# Transform variables ####
streetrx_f$ppm_log <- log(streetrx_f$ppm)
streetrx_f$mgstr_factor <- as.factor(streetrx_f$mgstr)


# EDA ####
# validate cleaniness of State
streetrx_f %>%
  group_by(state) %>%
  count() %>%
  arrange(n) %>%
  print(n=Inf)

streetrx_f %>%
  group_by(state) %>%
  count() %>%
  filter(n <= 10) %>%
  print(n=Inf)


# validate cleaniness of mgstr
streetrx_f %>%
  group_by(mgstr) %>%
  count() %>%
  print(n=Inf)
  

sort(unique(streetrx_f[,'state']))

# ppm
# we should consider deleting extreme values of ppm
quantile(streetrx_f$ppm, 0.98)
head(streetrx_f$ppm[order(streetrx_f$ppm, decreasing=T)])
sd(streetrx_f$ppm)
mean(streetrx_f$ppm)


# take out ppm > 50
streetrx_reduced <- streetrx_f[streetrx_f$ppm <= 50,]
mean(streetrx_reduced$ppm)


# ppm
# should use log even if we take out extreme ppm values
ggplot(data=streetrx_f, aes(x=ppm)) +
  geom_histogram(color = "black", fill = "grey", bins=20) +
  labs(title = "Distribution of PPM", x="PPM", y="Count")

ggplot(data=streetrx_reduced, aes(x=ppm)) +
  geom_histogram(aes(y=..density..), color = "black", fill = "grey", bins=25) +
  labs(title = "Distribution of PPM-Reduced", x="PPM", y="Count")

ggplot(data=streetrx_f, aes(x=ppm_log)) +
  geom_histogram(color = "black", fill = "grey", bins=25) +
  labs(title = "Distribution of Log PPM", x="PPM", y="Count")  # much better

ggplot(data=streetrx_reduced, aes(x=ppm_log)) +
  geom_histogram(color = "black", fill = "grey", bins=25) +
  labs(title = "Distribution of Log PPM", x="PPM", y="Count")  # much better


# State
# should group by state
table(streetrx_f$state)
quantile(count(streetrx_f, state)[,2],0.1)

sample_state <- names(which(table(streetrx_f$state) > 20))
#sample_state <- sample(unique(streetrx_f$state),8,replace=F)
ggplot(streetrx_f[is.element(streetrx_f$state, sample_state),],
       aes(x=state, y=ppm_log, fill=state)) +
  geom_boxplot() +
  labs(title="Log PPM by State",
       x="State",y="Log PPM") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))
## reduced
ggplot(streetrx_reduced[is.element(streetrx_reduced$state, sample_state),],
       aes(x=state, y=ppm_log, fill=state)) +
  geom_boxplot() +
  labs(title="Log PPM by State",
       x="State",y="Log PPM") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

streetrx_f[is.element(streetrx_f$state, sample_state),]


# USA_region
# region doesn't seem to be important
# other is lower; South is lower
table(streetrx_f$USA_region)

ggplot(streetrx_f,
       aes(x=USA_region, y=ppm_log, fill=USA_region)) +
  geom_boxplot() +
  labs(title="Log PPM by USA_region",
       x="USA_region",y="Log PPM") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

ggplot(streetrx_reduced,
       aes(x=USA_region, y=ppm_log, fill=USA_region)) +
  geom_boxplot() +
  labs(title="Log PPM by USA_region",
       x="USA_region",y="Log PPM") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))


# source
# price from internet is much higher
unique(streetrx_f$source)
which(table(streetrx_f$source) > 0)

sample_source <- names(which(table(streetrx_f$source) > 5))

ggplot(streetrx_f[is.element(streetrx_f$source, sample_source),],
       aes(x=source, y=ppm_log, fill=source)) +
  geom_boxplot() +
  labs(title="Log PPM by Source",
       x="Source",y="Log PPM") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

ggplot(streetrx_reduced[is.element(streetrx_reduced$source, sample_source),],
       aes(x=source, y=ppm_log, fill=source)) +
  geom_boxplot() +
  labs(title="Log PPM by Source",
       x="Source",y="Log PPM") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

streetrx_f %>%
  group_by(source) %>%
  count() %>%
  arrange(desc(n)) %>%
  print(n=Inf)


# bulk_purchase
# non-bulk purchase is a little higher
unique(streetrx_f$bulk_purchase)
ggplot(streetrx_f,
       aes(x=bulk_purchase, y=ppm_log, fill=bulk_purchase)) +
  geom_boxplot() +
  labs(title="Log PPM by Bulk Purchase",
       x="Bulk Purchase",y="Log PPM") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

ggplot(streetrx_reduced,
       aes(x=bulk_purchase, y=ppm_log, fill=bulk_purchase)) +
  geom_boxplot() +
  labs(title="Log PPM by Bulk Purchase",
       x="Bulk Purchase",y="Log PPM") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))


# form
# same form for all
names(which(table(streetrx_f$form_temp) > 0))


# mgstr
# not interesting
# could argue that ppm for low strength is a little higher
ggplot(streetrx_f[streetrx_f$mgstr < 50,],
       aes(x=mgstr, y=ppm_log)) +
  geom_point() +
  geom_smooth(aes(x=mgstr, y=ppm_log), method=loess) +
  labs(title="Log PPM by Strength",
       x="Mg Strength",y="Log PPM") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

ggplot(streetrx_reduced[streetrx_reduced$mgstr < 5, ],
       aes(x=mgstr, y=ppm_log)) +
  geom_point() +
  geom_smooth(aes(x=mgstr, y=ppm_log), method=loess) +
  labs(title="Log PPM by Strength",
       x="Mg Strength",y="Log PPM") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))




## Interactions ####
# state by source
# pattern does change by state, but we'll run into data issues with some sources
sample_source <- names(which(table(streetrx_f$source) > 20))
sample_state <- sample(unique(streetrx_f$state),8,replace=F)
## not take out large ppm
ggplot(streetrx_f[is.element(streetrx_f$source, sample_source) & is.element(streetrx_f$state, sample_state),],
       aes(x=state, y=ppm_log, fill=state)) +
  geom_boxplot() +
  facet_wrap(~source) +
  labs(title="Log PPM by State by Source",
       x="State",y="Log PPM") + theme_classic() + 
  theme(legend.position="none")
## take out large ppm
ggplot(streetrx_reduced[is.element(streetrx_reduced$source, sample_source) & is.element(streetrx_reduced$state, sample_state),],
       aes(x=state, y=ppm_log, fill=state)) +
  geom_boxplot() + 
  facet_wrap(~source) + 
  labs(title="Log PPM by State by Source",
       x="State",y="Log PPM") + theme_classic() + 
  theme(legend.position="none")
## count by source and state
streetrx_f[is.element(streetrx_f$source, sample_source),] %>%
  group_by(source, state) %>%
  count() %>%
  arrange(n) %>%
  print(n=50)


# state by bulk purchase
# patterns do seem to change - could include
sample_state <- sample(unique(streetrx_f$state),8,replace=F)
## not take out large ppm
ggplot(streetrx_f[is.element(streetrx_f$state, sample_state),],
       aes(x=state, y=ppm_log, fill=state)) +
  geom_boxplot() +
  facet_wrap(~bulk_purchase) +
  labs(title="Log PPM by State by Bulk",
       x="Bulk Purchase",y="Log PPM") + theme_classic() + 
  theme(legend.position="none", axis.text.x = element_text(angle = 90))
## take out large ppm
ggplot(streetrx_reduced[is.element(streetrx_reduced$state, sample_state),],
       aes(x=state, y=ppm_log, fill=state)) +
  geom_boxplot() + 
  facet_wrap(~bulk_purchase) + 
  labs(title="Log PPM by State by Bulk",
       x="Bulk Purhcase",y="Log PPM") + theme_classic() + 
  theme(legend.position="none", axis.text.x = element_text(angle = 90))


# source by bulk purchase
# no change in patterns
sample_source <- names(which(table(streetrx_f$source) > 20))
## not take out large ppm
ggplot(streetrx_f[is.element(streetrx_f$source, sample_source),],
       aes(x=source, y=ppm_log, fill=source)) +
  geom_boxplot() +
  facet_wrap(~bulk_purchase) +
  labs(title="Log PPM by Source by Bulk Purchase",
       x="State",y="Log PPM") + theme_classic() + 
  theme(legend.position="none", axis.text.x = element_text(angle = 90))


# mgstr by source
# some change in patterns but not very interesting
sample_source <- names(which(table(streetrx_f$source) > 20))

ggplot(streetrx_f[(streetrx_f$mgstr < 5) & is.element(streetrx_f$source, sample_source),],
       aes(x=mgstr, y=ppm_log)) +
  geom_point() +
  facet_wrap(~source) +
  geom_smooth(aes(x=mgstr, y=ppm_log), method=loess) +
  labs(title="Log PPM by Strength",
       x="Mg Strength",y="Log PPM") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

ggplot(streetrx_f[(streetrx_f$mgstr < 5) & is.element(streetrx_f$source, sample_source),],
       aes(x=mgstr_factor, y=ppm_log)) +
  geom_boxplot() +
  facet_wrap(~source) +
  labs(title="Log PPM by Strength",
       x="Mg Strength",y="Log PPM") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))


# mgstr by bulk purchase
# not interesting
ggplot(streetrx_f[(streetrx_f$mgstr < 5),],
       aes(x=mgstr, y=ppm_log)) +
  geom_point() +
  facet_wrap(~bulk_purchase) +
  geom_smooth(aes(x=mgstr, y=ppm_log), method=loess) +
  labs(title="Log PPM by Strength",
     x="Mg Strength",y="Log PPM") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

ggplot(streetrx_f[(streetrx_f$mgstr < 5),],
       aes(x=mgstr_factor, y=ppm_log)) +
  geom_boxplot() +
  facet_wrap(~bulk_purchase) +
  labs(title="Log PPM by Strength",
       x="Mg Strength",y="Log PPM") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))


# mgstr by state
# could be interesting
sample_state <- sample(unique(streetrx_f$state),6,replace=F)

ggplot(streetrx_f[is.element(streetrx_f$state, sample_state),],
       aes(x=mgstr_factor, y=ppm_log, fill=mgstr_factor)) +
  geom_boxplot() +
  facet_wrap(~state) +
  labs(title="Log PPM by Strength",
       x="Mg Strength",y="Log PPM") + theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90))


## EDA for fun ####
streetrx %>%
  dplyr::filter(source %like% "silkroad") %>%
  count(Primary_Reason)

streetrx %>%
  #dplyr::filter(source %like% "silkroad") %>%
  count(Primary_Reason, sort=T)

streetrx %>%
  dplyr::filter(Primary_Reason %like% "5 To resell") %>%
  count(source, sort=T)


streetrx_f %>%
  filter(ppm > 20) %>%
  count(bulk_purchase)




# Modeling ####
sample_source <- names(which(table(streetrx_f$source) > 5))

model0 <- lm(ppm_log ~ source + bulk_purchase + mgstr, data=streetrx_f[is.element(streetrx_f$source, sample_source),])
summary(model0)
model0f <- lm(ppm_log ~ source + bulk_purchase + mgstr_factor, data=streetrx_f[is.element(streetrx_f$source, sample_source),])
summary(model0f)
anova(model0, model0f)  # F-test suggests that we should use mgstr_factor

model0s <- lm(ppm_log ~ source + bulk_purchase + mgstr_factor + state, data=streetrx_f[is.element(streetrx_f$source, sample_source),])
anova(model0f, model0s)  # F-test suggests that we should add state as a predictor???


model0i <- lm(ppm_log ~ source*(bulk_purchase + mgstr_factor) + bulk_purchase:mgstr_factor, data=streetrx_f[is.element(streetrx_f$source, sample_source),])
summary(model0i)
anova(model0f, model0i)  # no difference - don't use interactions

model1f <- lmer(ppm_log ~ source + bulk_purchase + mgstr_factor + (1 | state), data=streetrx_f[is.element(streetrx_f$source, sample_source),])
summary(model1f)

model2f <- lmer(ppm_log ~ source + bulk_purchase + mgstr_factor + (1 | state) + (1 | USA_region), data=streetrx_f[is.element(streetrx_f$source, sample_source),])
summary(model2f)
anova(model1f, model2f, test='Chisq')  # should not include USA_region



# should we use varying slopes?
model3f <- lmer(ppm_log ~ source + bulk_purchase + mgstr_factor + (mgstr_factor | state), data=streetrx_f[is.element(streetrx_f$source, sample_source),])
summary(model3f)  # running into singularity issues
anova(model1f, model3f, test='Chisq')

confint.merMod(model3f)
sjPlot::plot_model(model3f,type='diag')


model4f <- lmer(ppm_log ~ source + bulk_purchase + mgstr_factor + (bulk_purchase | state), data=streetrx_f[is.element(streetrx_f$source, sample_source),],
                control=lmerControl(optimizer="nloptwrap"))
summary(model4f)  # running into singularity issues
anova(model3f, model4f, test='Chisq')

p <- dotplot(ranef(model1f, condVar=TRUE), xlim = c(-0.5, 0.5))$state
p[1]


model5 <- lmer(ppm_log ~ source + bulk_purchase + (mgstr | state), data=streetrx_f[is.element(streetrx_f$source, sample_source),],
               control=lmerControl(optimizer="bobyqa"))
summary(model5)
anova(model1f, model5, test='Chisq')  # using mgstr is just way worse

model6f <- lmer(ppm_log ~ source + mgstr_factor + (bulk_purchase | state), data=streetrx_f[is.element(streetrx_f$source, sample_source),],
               control=lmerControl(optimizer="Nelder_Mead")) # singularity issue
summary(model6f)


# looks like model1f is best 
summary(model1f)
AIC(model0f); AIC(model1f)
BIC(model0f); BIC(model1f)

plot_model(model1f,type='diag')


lattice::dotplot(ranef(model1, condVar=TRUE))$state
confint(model1f, level=0.9)
dotplot

streetrx_f %>%
  group_by(mgstr_factor) %>%
  count()

streetrx_f %>%
  group_by(source) %>%
  count()

summary(model0)
summary(model0f)
summary(model0s)
anova(model0f, model0s)


## Model Assessment of model1f ####
# qq plot for normality
qqnorm(residuals(model1f))
qqline(residuals(model1f), col = "blue", lwd = 2)

# linearity
# not applicable since all predictors are factors

# independence & equal variance
ggplot(data=streetrx_f, aes(x=fitted(model1f), y=residuals(model1f))) +
  geom_point(alpha=0.7) +
  theme_classic() +
  labs(x="Fitted Values",y="Residuals")

# cook's distance
# no point with cook's distance > 0.5 - not concerning
library(HLMdiag)
model1f.infl <- hlm_influence(model=model1f, level=1)
dotplot_diag(x = model1f.infl$cooksd, cutoff = 'internal', name = "cooks.distance", modify = FALSE)
model1f.infl %>%
  arrange(desc(cooksd))

# leverage
dotplot_diag(x = model1f.infl$leverage.overall, cutoff = "internal", name = "leverage", modify = FALSE)
high_leverage <- model1f.infl %>%
  filter(leverage.overall > 0.016)

# multicollinearity
library(car)
vif(model1f)


# state-level
state.infl <- hlm_influence(model=model1f, level='state')
dotplot_diag(x = state.infl$cooksd, cutoff = "internal", name = "cooks.distance", modify = FALSE)
dotplot_diag(x = state.infl$leverage.overall, cutoff = "internal", name = "leverage", modify = FALSE)


state.infl %>%
  arrange(desc(cooksd))

streetrx_f %>%
  group_by(state) %>%
  count() %>%
  print(n=Inf)


# produce tables ####
## try with xtable
library(robustlmm)
options(xtable.comment = FALSE)
xtable::xtable(robustlmm::compare(model1f))

## try with tab_model
sjPlot::tab_model(model1f)

## try with stargazer
stargazer::stargazer(model1f)
stargazer::stargazer(summary(model1f)$coef)
stargazer::stargazer(summary(model1f)$varcor)

print(summary(model1f)$varcor)
as.data.frame(VarCorr(model1f))
stargazer::stargazer(as.data.frame(summary(model1f)$varcor))

a <- data.frame(matrix(unlist(summary(model1f)$varcor), nrow=length(summary(model1f)$varcor), byrow=TRUE))

plot(model1f, which = 1:4, 
     title = c("Fitted Values vs. Residuals", 
               "Normal Q-Q vs. Residuals", 
               "Normal Q-Q vs. Random Effects",
               "Scatterplot of Random Effects for Group \"%s\""),
     multiply.weights = FALSE)

print(model1f, ask = interactive() & length(model1f) > 1)
library(Matrix)
library(robustlmm)
plot.rlmerMod(model1f)


densityplot(model1f)

