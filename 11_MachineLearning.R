#######################################
##AEDA Machine Learning assignment  ##
##Based on scripts from Lucas 2020 ##
####################################

## If you don't have these loaded yet, here are the libraries you will need
## Load general data manipulation / plotting libraries
library(dplyr)
library(ggplot2)

# Load modeling libraries
library(caret)
library(ranger)
library(pdp)
library(traitdata)
library(kernlab)

## Load helper scripts from Lucas 2020 - these are on the Github site as ML_helpers.R
## You can download and then source from this file (change file path to the location on your computer)
source('ML_helpers.R')

set.seed(100)

#caret_Setup

folds <- createFolds(p$y, k = 5, returnTrain = TRUE)
trcntrl <- trainControl(index = folds, savePredictions = TRUE, search = 'random')

## Now let's use similar methods to those we used in the exercise to evaluate covariates of litter/clutch size in reptiles!
## Load a larger dataset for amniotes

data(amniota)

amniote_data<-amniota

names(amniote_data)
dim(amniote_data)

sum(!is.na(amniote_data$litter_or_clutch_size_n))

#The trait names should be pretty self-explanatory. "svl" = snout-vent length 

#Q1: Write some code to clean the data.
#Rename the variable of interest to "y", log transform variable of interest and remove any taxa with missing litter/clutch size data.
#Then, retain only taxa in the class Reptilia and remove any variables with no data (all NA).
amniote_data <- amniote_data %>% 
  filter(!is.na(litter_or_clutch_size_n)) %>% 
  filter(litter_or_clutch_size_n >= 1) %>% 
  mutate(y = log1p(litter_or_clutch_size_n)) %>% 
  dplyr::select(-litter_or_clutch_size_n)
reptiles <- amniote_data[amniote_data$Class=="Reptilia",]
keep<-vector(length=ncol(reptiles))
for (i in (1:ncol(reptiles))){
  x <- is.na(reptiles[,i])
  keep[i]<-(FALSE %in% x)
  print(i)
}
reptiles <- reptiles[,keep]

##Q2: Plot the distribution of log-transformed litter/clutch size in reptiles.
##Histogram or boxplot (or both if you want) are fine.
##Visualizing by order may be useful.
hist(reptiles$y)
ggplot(reptiles, aes(x = Order, y = y)) + geom_boxplot()

##Q3: Write a little more data-cleaning code!
##Impute missing data and remove taxonomic data, common name, and scientific name.
preprocesses <- preProcess(reptiles, method = 'medianImpute')
rep_impute <- predict(preprocesses, reptiles)

names(rep_impute)
# Remove taxonomic data
cols=c(7:29,31)
rep_impute_data=rep_impute[,cols]

##Q4: Visualize the distributions for the predictor variables.
##Identify which variables look like they have a highly non-normal distribution.
##Log-transform these variables and visualize again.
##Which of the four models we will fit need the input variables to be log-transformed?
names(rep_impute_data)
hist(rep_impute_data$female_maturity_d)               #Non-normal
hist(rep_impute_data$adult_body_mass_g)               #Non-normal
hist(rep_impute_data$gestation_d)               #Non-normal
hist(rep_impute_data$birth_or_hatching_weight_g)               #Non-normal
hist(rep_impute_data$egg_mass_g)               #Non-normal
hist(rep_impute_data$fledging_age_d)               #Non-normal
hist(rep_impute_data$male_maturity_d)               #Non-normal
hist(rep_impute_data$female_body_mass_g)               #Non-normal
hist(rep_impute_data$no_sex_body_mass_g)               #Non-normal
hist(rep_impute_data$egg_length_mm)               #Non-normal
hist(rep_impute_data$adult_svl_cm)               #Non-normal
hist(rep_impute_data$female_svl_cm)               #Non-normal
hist(rep_impute_data$female_svl_at_maturity_cm)               #Non-normal
hist(rep_impute_data$no_sex_svl_cm)               #Non-normal
hist(rep_impute_data$litters_or_clutches_per_y)               #Non-normal
hist(rep_impute_data$maximum_longevity_y)               #Non-normal
hist(rep_impute_data$incubation_d)               #Non-normal
hist(rep_impute_data$longevity_y)               #Non-normal
hist(rep_impute_data$inter_litter_or_interbirth_interval_y)               #Non-normal
hist(rep_impute_data$male_body_mass_g)               #Non-normal
hist(rep_impute_data$egg_width_mm)               #Non-normal
hist(rep_impute_data$male_svl_cm)               #Non-normal
hist(rep_impute_data$birth_or_hatching_svl_cm)               #Non-normal
hist(rep_impute_data$no_sex_maturity_d)               #Non-normal
#time to log transform them
hist(log(rep_impute_data$female_maturity_d))    
hist(log(rep_impute_data$adult_body_mass_g))      
hist(log(rep_impute_data$gestation_d))        
hist(log(rep_impute_data$birth_or_hatching_weight_g))           
hist(log(rep_impute_data$egg_mass_g))             
hist(log(rep_impute_data$fledging_age_d))       
hist(log(rep_impute_data$male_maturity_d))          
hist(log(rep_impute_data$female_body_mass_g))   
hist(log(rep_impute_data$no_sex_body_mass_g))       
hist(log(rep_impute_data$egg_length_mm))        
hist(log(rep_impute_data$adult_svl_cm))             
hist(log(rep_impute_data$female_svl_cm))            
hist(log(rep_impute_data$female_svl_at_maturity_cm))          
hist(log(rep_impute_data$no_sex_svl_cm))              
hist(log(rep_impute_data$litters_or_clutches_per_y))              
hist(log(rep_impute_data$maximum_longevity_y))            
hist(log(rep_impute_data$incubation_d))           
hist(log(rep_impute_data$longevity_y))  
hist(log(rep_impute_data$inter_litter_or_interbirth_interval_y))              
hist(log(rep_impute_data$male_body_mass_g))           
hist(log(rep_impute_data$egg_width_mm))         
hist(log(rep_impute_data$male_svl_cm))               
hist(log(rep_impute_data$birth_or_hatching_svl_cm))           
hist(log(rep_impute_data$no_sex_maturity_d)) 
#they look much better now

##Q5: Fit a linear model relating your response variable to some potential predictors.
##To make this similar to our model for mammals, use adult body mass, 
#age to maturity for females, incubation length, litters/clutches per year, and maximum longevity.
##Visualize model fit and get R2.
##How does this model compare to the mammal model?
folds <- createFolds(reptiles$y, k = 5, returnTrain = TRUE)
trcntrl <- trainControl(index = folds, savePredictions = TRUE, search = 'random')
apriori_formula <- y ~ adult_body_mass_g + female_maturity_d + incubation_d + litters_or_clutches_per_y + maximum_longevity_y
rep_m0_lm <- train(apriori_formula, data = rep_impute_data, method = 'lm', trControl = trcntrl, na.action = na.omit)
plotCV(rep_m0_lm)
summary(rep_m0_lm$finalModel)
#Similar variables were significant in both models, although there were some exceptions (female reproductive maturity age, for example)


##Q6: Fit an elastic net to the data. Use the same hyperparameters used for the mammal dataset.
##Visualize model fit and get maximum R2.
##Plot R2 vs lasso/ridge fraction and strength of regularization (lambda).
##Does using the elastic net improve prediction relative to the linear model for this dataset?
enet_gr <- expand.grid(lambda = 10 ^ seq(0, -4, length.out = 20), fraction = c(seq(0.01, 1, length.out = 25)))
rep_m1_enet <- train(y ~ ., data = rep_impute_data, method = 'enet', tuneGrid = enet_gr, trControl = trcntrl, na.action = na.omit)
#Cross-validation: plot observed vs predicted
plotCV(rep_m1_enet)

#elastic_net_summary - best model fit
rep_m1_enet$results$Rsquared %>% max

# Plot R2 vs regularization strength
rep_m1_enet$results %>%
  ggplot(aes(fraction, Rsquared, colour = lambda, group = factor(lambda))) +
  geom_line() +
  geom_point() + scale_color_viridis_c(trans = 'log10') + xlab('Lasso/Ridge fraction')
#Improvement from the linear model


##Q7: Fit a Gaussian process model to the data. Use the same range of sigma values used for the mammal dataset. 
##Visualize model fit and get R2.
##Plot R2 vs sigma. How does this plot compare to the plot from the mammal dataset?
##Overall, does the Gaussian process model perform better than the linear model?
gp_gr <- data.frame(sigma = c(0.01, 0.02, 0.04, 0.08, 0.16))
rep_m2_gp <- train(y ~ ., data = rep_impute_data, method = 'gaussprRadial', tuneGrid = gp_gr, trControl = trcntrl, na.action = na.omit)

## Plot R2 vs sigma (recreate figure 1b)
rep_m2_gp$results %>% ggplot(aes(sigma, Rsquared)) +
  geom_line() + geom_point() + xlab('Sigma')

#Cross-validation: plot observed vs predicted
plotCV(rep_m2_gp)

#gaussian process summary
rep_m2_gp
rep_m2_gp$results$Rsquared %>% max

##Q7: Train a random forest on the data. Note - use a lower maximum number of random predictors by setting mtry = c(2, 5, 10, 20).
##Visualize model fit and get R2.
##Plot R2 vs node size and number of random predictors.
##What does the node size selected indicate about the amount of noise in the model?
##What does the number of random predictors selected indicate about interaction depth?
rf_gr <- expand.grid(mtry = c(2, 5, 10, 20), splitrule = 'variance', min.node.size = c(5, 10, 20, 50))
rep_m3_rf <- train(y ~ ., data = rep_impute_data, method = 'ranger', tuneGrid = rf_gr, trControl = trcntrl, na.action = na.omit, importance = 'impurity', num.trees = 1000)

##Plot # of random predictors and minimum node size vs R2
rep_m3_rf$results %>%
  ggplot(aes(mtry, Rsquared, colour = factor(min.node.size), group = factor(min.node.size))) +
  geom_line() +
  geom_point() +
  labs(colour = 'min.node.size')

#Cross-validation: plot observed vs predicted
plotCV(rep_m3_rf)

# Random forest summary
rep_m3_rf
rep_m3_rf$results$Rsquared %>% max

##Q8: Overall, which model(s) perform best at predicting litter/clutch size, and which perform the worst?
##Compare this to the mammal analysis. What does this say about the universality of these methods?
compare_models(rep_m2_gp, rep_m3_rf)
compare_models(rep_m1_enet, rep_m3_rf)
compare_models(rep_m0_lm, rep_m3_rf)

#The random forest method works best, and the linear model is the worst for reptiles
#for the mammal models, they all fit pretty well. with an R^2 of ~0.60
#Depending on the data and question, all can be viable.

##Q9: Evaluate variable importance for the elastic net, gaussian process, and random forest.
##Which variable is most important across models? 
varImp(rep_m1_enet)     #Top 3: egg mass, birth weight, longevity
varImp(rep_m2_gp)       #Top 3: egg mass, birth weight, longevity
varImp(rep_m3_rf)       #Top 3: adult body mass, snout-vent length, incubation time

##Q10: Plot functional forms for the relationship between litter/clutch size and the most important variable for the Gaussian Process and the random forest models.
##How do they differ?
##What does this say about the likely relation ship between litter/clutch size and the best predictor variable?
# partial dependence plot for egg mass
partial(rep_m1_enet,pred.var = c('egg_mass_g'), plot = TRUE)
partial(rep_m2_gp, pred.var = c('egg_mass_g'), plot = TRUE)
#as, egg mass increases, so does the clutch size (but that doesn't sound correct)

# partial dependence plot for adult body mass
partial(rep_m3_rf, pred.var = c('adult_body_mass_g'), plot = TRUE)
#as adult body mass increases, so does clutch size (upt to the point where it levels off at ~1e+05g)