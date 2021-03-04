### QUESTION 1
# use the datasets 'pinelands_bees.csv' and 'land_cover.csv' to answer the question:
# Does natural land cover in the surrounding landscape predict the number of individual bees collected at a site?
# follow the analysis workflow that we learned in class, for which there are prompts below
# make sure to complete all 6 prompts
# use comment lines to briefly explain your reasoning at each step (ie, why you are doing what you are doing)
# you will turn in your code (ie, this R file) to your github repo at the end of class

## brief metadata
# the datasets you will use are 'pinelands_bees.csv'  and 'land_cover.csv'
# these are data I collected on wild bees of 117 ish species (ish because the taxonomy of some specimens is not fully sorted out) 
# data were collected at 27 study sites in the New Jersey pinelands
# all sites were in forest habitat, but sites differed in how much of the surrounding landscape was forested
# bees were collected on four days at each site between April and September 2003, with collection events organized into rounds, such that each site was sampled once in round 1 before round 2 was begun, etc
# 'pinelands_bees.csv'contains data on the bees collected, where each row is one individual bee
# 'land_cover.csv' column 'Nat1600' is the percentage of the natural land cover surrounding the site within a 1600m radius (roughly an 800 ha area), where the natural habitat is predominantly forest, with a small amount of other natural habitats such as open wetlands

#I'm turning on some packages before we start:
library(dplyr)
library(ggplot2)
library(MuMIn)
library(tidyr)

## ----------------------- 1 Get and format the data-------------------
# you will need to create a new dataframe that has the variables you need: site, land cover, number of bees collected
# you may want to use group_by, summarize, n(), left_join

#read in csvs
bee <- read.csv("pinelands_bees.csv")
land <- read.csv("land_cover.csv")

#summarize the data to get count of bees at each site
bee.count<-summarize(group_by(bee, site_name), cnt =n())

#column names were weird, so I added a new one to work as the key for left join
land$site_name <- land$ï..site_name
bee.land<- left_join(land, bee.count, key = site_name)

##-------------------------------- 2 Data picture-------------------------------
# plot the data and figure out what type of model might be best
ggplot(data = bee.land, aes(x = Nat1600, y = cnt)) +
  geom_point()
#looks pretty scattered.


##----------------------------- 3 Test model assumptions-------------------------
# you can test the assumptions of more than one type of model here if you aren't sure
# explain your reasoning for the model you choose

#first I'll just try a linear one
beemod1<-lm(cnt~Nat1600, data = bee.land)
plot(beemod1)
#Not bad. The residuals look pretty good (except a few outliers)
#the Q-Q plot also looks pretty good, until that end part
#scale-location is decent. variance doesn't change much from right to left
#resid vs. leverage looks good too (if you ignore that one point all the way in the upper right)
summary(beemod1)
#I don't really have many random effects that I could look at, so I opted for simplicity. Just a plain lm()

##------------------------------------- 4 Report and interpret results---------------------------
# make sure you interpret the following things: coefficients, p value, explanatory value of model
# state your conclusions using numbers and units
# discuss the magnitude and biological significance of your result, not just its significance
# how confident are you in this analysis and why

coefficients(beemod1)
#coefficients are:
#     Intercept: 72.598
#     Nat1600: -0.435

summary(beemod1)
#p-value is 0.02927,     nice.
#Adjusted R^2 is 0.138,  less nice
#It seems like the percent of natural land around you explains a part of the trend, but not all of it.

confint(beemod1)
#The 95% confidence intervals do not contain 0 for this model
#so I'm at least 95% confident in the analysis.

#Interpretation: if there is no natural area in your surroundings, you'll find about 72 bees (on average)
#               You'll find about 0.435 less bees for every 1% of natural land around you.

#I suppose that makes sense. If you have a lot of natural land around you, the bees will have more options.
#       They may be less concentrated at one site.
#       But, in a less natural landscape, your natural site would be a little sanctuary for them.

##------------------------ 5 Plot model results back onto the data picture-----------------------
# geom_smooth is the easy way to do this, but you can alternatively do it manually using the model output (coefficients) if you want
plot(bee.land$Nat1600,bee.land$cnt)
abline(beemod1)

##------------------------ 6  If you were to add a random effect to this model, what would it be, what would adding it accomplish?-----------------------
# please answer this question in comment lines
# you do not need to format any data here or code or run anything
# explain your reasoning for using this random effect
# you will need to go back to the original data file and metadata to answer this question
# extra credit: write out the model code for your model now including the random effect

#I would include species as a random effect. Perhaps some species are more sensitive to the forest
#     (or lack thereof) in the surrounding landscape. We should account for that variability by grouping the species
#I'll pass on the extra credit for now. Maybe I'll come back to it if I have extra time

###------------------------------------ QUESTION 2---------------------------
# The file "modSel.csv" contains simulated dataset of observations of a focal species at a series of sites.
# For each site, you have observed abundance, and measurements of environmental variables you hypothesize
# to affect the distribution of this species.
# Specifically, you hypothesize the species is more abundant in warmer, wetter regions,
# and that it prefers core over edge habitat.
# You have measured each of these in a couple ways, as mean annual and summer temperature,
# cumulative annual and summer precipitation, and distance to nearest edge and total edge within 500 m.
# Your goal here is to find the best model you can, given your hypotheses,
# to describe the distribution of this species.
# In doing so, you will also assess the more relevant measure of each environmental condition,
# and weigh their relative importance (or at least predictive power).
# For simplicity, do not consider interactions between variables.
# Please give your models interpretable names.


# Step 1. Find the best error structure/error distribution for these data.
# State your conclusion in comment lines
# (Hints: you want to assess model-error distributions, not the data distribution; these are count data.)

modsel <- read.csv("modSel.csv")

#Lets just make one model for each, then do the diagnostic plots
#assuming the different measurement types are (mostly) equally good, I'll just pick 3
mod.pois <- glm(observedAbundance ~ meanAnnualTemp + annualPrecipitation + totalEdge, data = modsel, family = poisson)
plot(mod.pois)

mod.binom <- glm(observedAbundance ~ meanAnnualTemp + annualPrecipitation + totalEdge, data = modsel, family = binomial)
#Won't run because the data is not binomial. I guess I should have known that

#Poisson looks pretty good on the diagnostic plots. And it's count data, so I'll go with that.

#--------------------------------------- Step 2: --------------------------------------
#Having determined the best error structure, determine the more effective method of measuring each variable.
# For each variable, compare methods as a pair of single-variable models (e.g., summer temp vs annual temp).
# State your conclusion in comment lines

mod.pois.annT <- glm(observedAbundance ~ meanAnnualTemp, data = modsel, family = poisson)
mod.pois.sumT <- glm(observedAbundance ~ meanSummerTemp, data = modsel, family = poisson)
AIC(mod.pois.annT)
AIC(mod.pois.sumT)      #Winner

mod.pois.annP <- glm(observedAbundance ~ annualPrecipitation, data = modsel, family = poisson)
mod.pois.sumP <- glm(observedAbundance ~ summerPrecipitation, data = modsel, family = poisson)
AIC(mod.pois.annP)
AIC(mod.pois.sumP)      #Winner

mod.pois.TE <- glm(observedAbundance ~ totalEdge, data = modsel, family = poisson)
mod.pois.D2E <- glm(observedAbundance ~ distance2edge, data = modsel, family = poisson)
AIC(mod.pois.TE)      #Chicken Dinner
AIC(mod.pois.D2E)

#The 3 winning variables are:
#     Summer Temp
#     Summer Precip
#     Total Edge

#-------------------------------- Step 3: -------------------------------
#Having determined which method of measurement for each variable is best,
# determine the most effective combination of predictors;
# run a set of competing models and create a table comparing these models to each other and to a null.
# state your conclusion in comment lines

WarmEdge <- glm(observedAbundance ~ totalEdge + meanSummerTemp, data = modsel, family = poisson)
WarmWet <- glm(observedAbundance ~ summerPrecipitation + meanSummerTemp, data = modsel, family = poisson)
WetEdge <- glm(observedAbundance ~ summerPrecipitation + totalEdge, data = modsel, family = poisson)
null.mod <- glm(observedAbundance ~ 1, data = modsel, family = poisson)
global.mod <- glm(observedAbundance ~ summerPrecipitation + totalEdge + meanSummerTemp, data = modsel, family = poisson)
mod.table <- model.sel(WarmEdge,WarmWet,WetEdge,null.mod,global.mod)

#SummerTemp + TotalEdge is the best model, with the global model as a close 2nd.


#-------------------------- Step 4: Interpret these results.-------------------------------
# Were your hypotheses supported? What is the relative importance of each predictor?
# What is your general conclusion?

confint(WarmEdge)
#both summer temp and total edge have significant effects.
#     Increased Total edge negatively effects observed abundance
#     Increased Summer temp positively effects observed abundance

#The hypothesis stating that it likes warmer climates is supported
#The hypothesis stating that it likes core habitat is supported

#Let me just double check the Precip effects using the global model, since it was a close 2nd place model
confint(global.mod)
#Nope, the confidence interval for precipitation contains 0. no good

#The hypothesis stating that it likes wetter climates is NOT supported

#CONCLUSION: This species prefers warmer climates with less edge habitat.
#     It seems to be indifferent to the amount of rainfall.

