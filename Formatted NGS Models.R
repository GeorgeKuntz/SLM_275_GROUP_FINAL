## Authors: George Kuntz, Eric Parker, Ted Brennan, Collin Garner, Mathew Seifert
## Class: SLM 275
## Date: TBD

## The purpose of the following code is to fit multiple linear
## regression models to find the impact that different stats 
## have on passer rating for Quarterbacks (QB). We run multiple 
## models with single predictors to find individual impacts
## of single predictors and then fit multiple multiple
## predictor regression models.

## Loads the necessary packages for creating and running the models
## as well as building plots
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggfortify)
library(MASS)
library(GGally)
library(lindia)
library(car)
library(leaps)
library(knitr)




## Loads in the reduced Quarterback (QB) data.
NGS <- read_excel("NGS.pass.edited.xlsx")

#################################

## Fitting smaller models

#################################

## We see that the aggressiveness of the QB is significant in determining
## the passer rating, however, it only explains about 8% ~ 9% of passer
## rating. We can also see that there may be some violations of variance 
## and linearity with the current model.
passer.lm <- lm(passer_rating ~ aggressiveness, data = NGS)
summary(passer.lm)
autoplot(passer.lm)

#################################

## We see that this is a very good model that is significant and has a 
## high adjusted r^2. Explaining 55% of passer rating. The assumption
## plots also look good

td.lm <- lm(passer_rating ~ pass_touchdowns, data = NGS)
summary(td.lm)
autoplot(td.lm)

################################

## This model is also another very good model in comparison to others. 
## it is significant and explains 38% ~ 39% of passer rating. However, 
## the scale-Location plot is a bit off violating the equal variance
## assumption.

py.lm <- lm(passer_rating ~ pass_yards, data = NGS)
summary(py.lm)
autoplot(py.lm)

################################

## Another very strong model. It is significant as well as predicting 20%
## of passer rating. The assumption plots are pretty good as well.

pa.lm <- lm(passer_rating ~ attempts, data = NGS)
summary(pa.lm)
autoplot(pa.lm)

################################

## We see that interceptions are not a significant stat when determining
## passer rating, however we may still fit it into a larger model
## there are also issues with assumptions with the model that is ironed
## out may change the outcome.
inter.lm <- lm(passer_rating ~ interceptions, data = NGS)
summary(inter.lm)
autoplot(inter.lm)

################################

## This model is not consequential at all. No where near significant
## and explains 0% of passer rating. Still may fit into a larger model
## to see the impact.

attt.lm <- lm(passer_rating ~ avg_time_to_throw, data = NGS)
summary(attt.lm)
autoplot(attt.lm)

################################

## This model does show significance and explains about 9% ~ 10% of passer
## rating. There are some issues with the scale location plot that can be 
## fixed. 

ecp.lm <- lm(passer_rating ~ expected_completion_percentage, data = NGS)
summary(ecp.lm)
autoplot(ecp.lm)

##################################

## This model is the best so far, which should be expected. It is significant
## and explains 66% of passer rating. The assumption plots are mostly okay, 
## but looking to make them better might be a good idea

cp.aov <- lm(passer_rating ~ completion_percentage, data = NGS)
summary(cp.lm)
autoplot(cp.lm)

#################################

## This model is also great at explaining passer rating. It is significant and
## explains about 52% of passer rating. The plot are mostly fine, but always 
## good to look at some adjustments

cpae.lm <- lm(passer_rating ~ completion_percentage_above_expectation, data = NGS)
summary(cpae.lm)
autoplot(cpae.lm)

#################################

## This model isn't significant, and explains 0% of passer rating.
## However, we may still fit a model with this variable.

aayts.lm <- lm(passer_rating ~ avg_air_yards_to_sticks, data = NGS)
summary(aayts.lm)
autoplot(aayts.lm)

############################

## Rankings of Predictors
## 1. completion_percentage
## 2. pass_touchdowns
## 3. completion_percentage_above_expectation
## 4. pass_yards
## 5. attempts
## 6. expected_completion_percentage
## 7. aggressiveness
## 8. interceptions
## 9. avg_time_to_throw
## 10. avg_air_yards_to_sticks

## These are ordered in the percentage (%) of passer rating that the 
## predictors explain.

############################

## Fitting Larger Models

############################

## The first model that we fit is using the top three predictors, which all 
## happen to be related to completion percentage. The model looks good,
## however we have an issue with singularities with this model. There also
## may be some assumption violations.
completion_model <- lm(passer_rating ~ completion_percentage + completion_percentage_above_expectation
              + expected_completion_percentage, data = NGS)

summary(completion_model)
autoplot(completion_model)

################

## With this model we fitted our lower ranking predictors. We can see that
## the only predictor that is significant was aggressiveness. This is
## in line with the models above. There are also some issues with
## the residuals vs. fitted plot. 

lower_ranking_model <- lm(passer_rating ~ aggressiveness + interceptions +
                          avg_time_to_throw + avg_air_yards_to_sticks
                          + expected_completion_percentage,
                          data = NGS)

summary(lower_ranking_model)
autoplot(lower_ranking_model)

###############

## This is a strong model. High adjusted r^2 explaining 89% of passer rating.
## However, the Residuals vs. Fitted plot is a bit weird, violating
## the linearity assumption

high_ranking_model <- lm(passer_rating ~ completion_percentage + pass_touchdowns +
                           completion_percentage_above_expectation + pass_yards +
                           attempts, data = NGS)

summary(high_ranking_model)
autoplot(high_ranking_model)
###############

## This model so far is the best. It explains passer rating the most and 
## is significant with almost perfect assumption plots. However, there may 
## be some significant outliers.

mixed_model <- lm(passer_rating ~ completion_percentage + completion_percentage_above_expectation
                  + aggressiveness + pass_touchdowns + pass_yards, data = NGS)

summary(mixed_model)
autoplot(mixed_model)

###############

## This model uses all predictors except expected_completion_percentage. This 
## is so we can avoid issues seen earlier with singularities.
## In this model we see that previously insignificant predictors now
## become significant, and other previously significant models
## become insignificant.The model does however appear to be the best one
## so far as it is significant and explains the largest percent of
## passer rating. However, some transformations to the model will be
## needed as some assumptions appear to be violated.

full_model <- lm(passer_rating ~ completion_percentage + completion_percentage_above_expectation
                 + aggressiveness + interceptions + avg_time_to_throw +
                   avg_air_yards_to_sticks + pass_touchdowns +
                   pass_yards + attempts, data = NGS)

summary(full_model)
autoplot(full_model)

##########################################

## Checking Assumptions of Full Models

##########################################

## For each of the full models we need to check our assumptions. We can 
## check them with our assumption plots using the autoplot function
## and perform necessary transformations on the models to fix
## assumptions.

###################################

autoplot(completion_model)

## We can see that Residuals vs. Leverage plot may be a bit weird. 
## This means that there may be some significant outliers,
## so this may not be the best model to use and predict passer rating

###################################

autoplot(lower_ranking_model)

## This model has issues in the Residual vs. Leverage plot as well as
## the Residual vs. Fitted plot also seems to violate the linearity 
## assumption

###################################

autoplot(high_ranking_model)

## The Residuals vs. Fitted plot appears to be violated.

###################################

autoplot(mixed_model)

## This model appears to satisfy all necessary assumptions we need.

###################################

autoplot(full_model)

## There definitely will need to be transformations performed on the
## assumption plots.

########################

## BOXCOX Plot Checks

########################

boxcox(completion_model)

## The boxcox has a value of about 1 in the middle so no transformation 
## is needed.

##################

boxcox(lower_ranking_model)

## The boxcox has a value of about 1 in the middle so no transformation 
## is needed.

##################

boxcox(high_ranking_model)

## A transformation by raising the response to the 1.5 looks to be necessary

##################

boxcox(mixed_model)

## A transformation by raising the response to the 1.5 looks to be necessary

##################

boxcox(full_model)

## A transformation by raising the response to the 1.5 looks to be necessary

##############################

## Fitting The Fixed Models

##############################

## As noted above three models need to have the passer_rating response
## transformed. This should fix the Residuals Vs. Fitted plot.

###################

## Fixes the residuals plot for high ranking predictors determined above.

high_ranking_model_fx <- high_ranking_model <- lm(((passer_rating)^1.5) ~ completion_percentage + pass_touchdowns +
                                                    completion_percentage_above_expectation + pass_yards +
                                                    attempts, data = NGS)

autoplot(high_ranking_model_fx)

####################

## Fixes the residuals plot for the mixed predictors model

mixed_model_fx <- lm(((passer_rating)^1.5) ~ completion_percentage + completion_percentage_above_expectation
                     + aggressiveness + pass_touchdowns + pass_yards, data = NGS)

autoplot(mixed_model_fx)

#####################

## Fixes the residuals vs. fitted plot, however a larger transformation was 
## needed. However the scale-location plot is still violated.

full_model_fx <- full_model <- lm(((passer_rating)^1.9) ~ completion_percentage +
                                    completion_percentage_above_expectation
                                  + aggressiveness + interceptions + avg_time_to_throw +
                                    avg_air_yards_to_sticks + pass_touchdowns +
                                    pass_yards + attempts, data = NGS)

autoplot(full_model_fx)

#############################

## VIF Co-linearity check

#############################

## We need to check for co-linearity.

#########################

## There is an issue with perfect co-linearity, this model might as well
## be thrown out as no concrete inferences can be made.

vif(completion_model)

##########################

## No issues with co-linearity appear as all vif values are low

vif(lower_ranking_model)

#############################

## There are definitely issues with co-linearity with the predictors
## pass_yards and attempts

vif(high_ranking_model_fx)

#############################

## No issues with co-linearity appear

vif(mixed_model_fx)

################################

## There are issues with co-linearity among the predictors completion_percentage 
## attempts, and pass_yards 

vif(full_model_fx)

##############################

## AIC and BIC Value Check

#############################


## AIC allows us to see which model best fits the data. Interesting enough
## the completion model which has major co-linearity issues is deemed to
## fit the data the best as it has the lowest AIC value.

AIC(completion_model)
AIC(lower_ranking_model)
AIC(high_ranking_model_fx)
AIC(mixed_model_fx)
AIC(full_model_fx)

## BIC allows us to determine how well the models predict outcomes from the data.
## Once again we see that the completion model has been determined to be the best.

BIC(completion_model)
BIC(lower_ranking_model)
BIC(high_ranking_model_fx)
BIC(mixed_model_fx)
BIC(full_model_fx)

###############################

## Backwards Step-wise Model

###############################

## After creating our own models and choosing the predictors
## we now want to perform a function that allows us to find the best
## model possible.

########################

## The backward step wise allows us to use the computer to make a best 
## model selection for us. It does so by choosing the model with the
## lowest AIC value. The model it picks below looks very good. However
## it does have some co-linearity issues, so we would have to remove those
## predictors from the model, we may also consider removing
## avg_time_to_throw from the model as it is insignificant.

step.pick.backward <- stats::step(full_model_fx, direction="backward")

summary(step.pick.backward)

vif(step.pick.backward)

##############################

## Forward Selection Model

##############################

## A forward selection also allows us to use the computer to make a model
## selection based off of AIC scores.

##################

## This forward selection model also has issues with co-linearity so removing 
## those predictors is a good idea.

null.fit <- lm(passer_rating ~ 1, data=NGS)

step.pick.forward <- stats::step(null.fit, scope=formula(full_model_fx), direction="forward")

summary(step.pick.forward)

vif(step.pick.forward)

##########################################

## Fix the Backward and Forward Models

##########################################

## Fixing the backward model by taking out all predictors with co-linearity
## issues and predictors that were not significant creates a great model that
## has all significant predictors, predicts 93% of passer_rating and when 
## looking at the autoplot has no violated assumptions.

backward_fix <- lm(passer_rating ~ completion_percentage + interceptions +
                     avg_air_yards_to_sticks + attempts +
                     pass_touchdowns, data = NGS)

summary(backward_fix)
autoplot(backward_fix)

#################################

## As we can see both the backward and forward predictions agree on the
## best model to use.

forward_fix <- lm(passer_rating ~ completion_percentage + pass_touchdowns +
                    interceptions + avg_air_yards_to_sticks + attempts, data = NGS)

summary(forward_fix) 
autoplot(forward_fix)

########################

## Model Comparison

########################

## Here we want to compare our models with the backwards model just to determine
## that the backwards model is indeed the best fit. We want to pick the model 
## with the largest adjusted r^2 and lowest AIC and BIC.

###########################

## Adjusted r^2: 0.9349 or 93.49%
## AIC: 617.6748
## BIC: 637.1287

summary(backward_fix)
AIC(backward_fix)
BIC(backward_fix)

#############################

## Adjusted r^2: 0.6875 or 68.75%
## AIC: 801.5185
## BIC: 812.635

summary(completion_model)
AIC(completion_model)
BIC(completion_model)

#############################

## Adjusted r^2: 0.1519 or 15.19%
## AIC: 923.2151
## BIC: 942.669

summary(lower_ranking_model)
AIC(lower_ranking_model)
BIC(lower_ranking_model)

############################

## Adjusted r^2: 0.8991 or 89.91%
## AIC: 1298.515
## BIC: 1317.969

summary(high_ranking_model_fx)
AIC(high_ranking_model_fx)
BIC(high_ranking_model_fx)

############################

## Adjusted r^2: 0.8544 or 85.44%
## AIC: 1342.139
## BIC: 1361.592

summary(mixed_model_fx)
AIC(mixed_model_fx)
BIC(mixed_model_fx)

############################

## Adjusted r^2: 0.9523 or 95.23%
## AIC: 1695.952
## BIC: 1726.523

summary(full_model_fx)
AIC(full_model_fx)
BIC(full_model_fx)

#############################

## Confidence Interval

###########################

## We want to create a confidence interval for an average QB in the NFL
## So we find the mean of the predictors for our best fit model.

mean(NGS$completion_percentage)
mean(NGS$pass_touchdowns)
mean(NGS$interceptions)
mean(NGS$avg_air_yards_to_sticks)

## Now we create a mutated data frame with our mean values

mutated_NGS <- data.frame(pass_touchdowns = 19,
                           interceptions = 9.5,
                           completion_percentage = 63,
                          avg_air_yards_to_sticks = -0.5)

## We can say that the 95% confidence interval for an average QB in the
## NFL will be between 88.35 and 89.53
predict(forward_fix, newdata = mutated_NGS, interval = "confidence")







