# Random Forest Tutorial
# October 9, 2020
# Rafi Mazor, Annie Holt, Heili Lowman

# The following script will walk through a random forest tutorial, with datasets from SMC and StreamCat databases. The dependent variable in this case will be the Algal Stream Condition Index (ASCI) state-wide.

# Step One - Load In ------------------------------------------------------

# Load packages.
library(quantregForest)
library(caret)
library(tidyverse)
library(tidymodels)
library(skimr)

# Load datasets.
asci_df <- read_csv("asci_rf_data.csv") %>%
  rename(COMID = comid)
skim(asci_df) # Examine the dataset.
str(asci_df)

ca <- read_csv("streamcat_landuse_CA_wide.csv")
skim(ca)
str(ca) # Checking to be sure COMID is numeric in both datasets.

# Bind the datasets together.
mydf <- asci_df %>%
  select(stationcode, COMID, asci) %>%
  inner_join(ca)
skim(mydf) # Examing completeness of this joined dataset.
length(unique(mydf$COMID)) # Checking for duplicates.

set.seed(1) # Every time I run the code below, it's based on the same random pull of data.
mydf2 <- mydf %>% 
  group_by(COMID) %>%
  sample_n(size = 1) %>% # Pull out only one instance of each COMID.
  ungroup()

# Important to have complete datasets for training data. For testing data, it's less critical.
# Throw out incomplete data/cases.

# Step Two - Training Data ------------------------------------------------

# Create calibration and validation splits with tidymodels initial_split() function.

mydf2_split <- mydf2 %>%
  initial_split(prop = 0.75) # splits data into training and testing set. default is 3/4ths split (but 75% training, 25% testing) but can edit this. Rafi added "Strata = State" to this for CA and AZ datasets.

# Create a training data set with the training() function

mydf2_train <- training(mydf2_split)
mydf2_test <- testing(mydf2_split)
# Examine the environment to be sure # of observations looks like the 75/25 split.

# Create a separate dataset of available COMIDS that were not used in the training dataset.

nottrain <- ca %>% # all COMIDS, sampled or not
  filter(!COMID %in% mydf2_train$COMID) # Removing sites used to train the model.

# Step Three - Kitchen Sink model -----------------------------------------

# Create finalized training dataset and include all possible variables. 
rf_dat <- mydf2_train %>%
  select(-stationcode, -COMID, -RdDensCatRp100) # Dropping variable due to incompleteness.

# Random forest -- 
# a decision tree model, using predictors to answer dichotomous questions to create nested splits.
# no pruning happens - rather, multiple trees are built (the forest) and then you are looking for consensus across trees
# training data goes down the tree and ends up in a terminal node
# if testing data goes down the same route, then this upholds our conclusions. Or, if it goes awry somehow, this allows us to look for patterns in how it goes awry.
# Many suggest that due to this structure, the model doesn't need to be additionally validated.

set.seed(2) # assures the data pulled is random, but sets it for the run below (makes outcome stable)
myrf <- randomForest(y = rf_dat$asci, # dependent variable
  x = rf_dat %>%
    select(-asci), # selecting all predictor variables
  importance = T, # how useful is a predictor in predicting values (nothing causal)
  proximity = T, 
  ntrees = 1000) # Up from 500 to 1000 trees. 

myrf # examine the results.
# % variance explained - can be negative, and if so, is likely just zero (and explains none of the variance in the dataset)

summary(myrf)
# mtry allows you to parameterize the number of splits

plot(myrf)
# model performance appears to improve most at ~100 trees

varImpPlot(myrf)
# displays which variables are most important
# helps to winnow down list of predictors
# Rafi recommends weighing left pane more
# right pane also shows how evenly things split based on the list of predictors
# values close to 0 can be dropped, but don't have to be

myrf$importance
# displays the data plotted in the plot above

predict(myrf)
# returns out of bag predictions for training data
# in the bag: every time a tree is built, it uses ~80% of the original 75% we set aside from the original dataset used to create a tree to assure random data selection
# out of bag: looking at the remaining 20% of the training data to predict, when you want to know what your model does at the training location sites

# Predict ASCI scores state-wide.
nottrain_prediction <- nottrain %>% # taking all available COMIDS, that haven't been used to train the model
  na.omit() %>% # remove NAs
  mutate(asci_predicted = predict(myrf, newdata = nottrain %>% na.omit())) # using developed model (myrf), inputting predictor variables (nottrain - which contains COMIDs and associated StreamCat data) to predict output/dependent variable (asci_predicted a.k.a. ASCI).

# rePredict ASCI scores for training data.
mydf2_train$asci_predicted <- predict(myrf) # Adds column of predicted ASCI values to training dataset.

# Creates new dataset of bound rows for both ...
ca_predictions <- bind_rows(nottrain_prediction %>%
                            mutate(Set = "Non-training"), # ... statewide COMIDs (none of which were used for training data).
                            mydf2_train %>%
                            mutate(Set = "Training")) # ... COMIDS from our training dataset (that were indeed used for training the random forest model).
# This creates the dataset that will be plotted (i.e. you're trying to create a state-wide plot of predicted ASCI scores).

# Plot the data.
plot1 <- ggplot(nottrain_prediction, aes(x = PctImp2011Ws, y = asci_predicted)) +
  geom_point(alpha = 0.1) +
  labs(x = "% Imperviousness in the Watershed in 2011",
    y = "Predicted ASCI Score") +
  theme_classic()

# Step Four - Quantile Regression model -----------------------------------

# Quantile random forest regression mode, instead of looking at the mode of trees, can compare to 10th, 50th, 90th percentiles etc.

set.seed(20)
myqrf <- quantregForest(y = rf_dat$asci, # dependent variable
              x = rf_dat %>%
                  select(-asci),
              importance = T, 
              proximity = T,
              keep.inbag=T,
              ntrees = 1000) 

predict(myqrf) # automatically presents 10th %tile, median, and 90th %tile
predict(myqrf, what=c(0.2, 0.3, 0.999)) # to print specific quantiles

plot(myqrf) # plots the results.

# Step Five - Predictor Selection -----------------------------------------

# Using caret to select the best predictors
# What are the parameters you want to use to run recursive feature elimination (rfe)?
my_ctrl <- rfeControl(functions = rfFuncs,
                      method = "cv",
                      verbose = FALSE,
                      returnResamp = "all")

# rfe = recursive feature elimination
# THIS STEP TAKES FOR-EV-ER!!!
set.seed(22)
my_rfe <- rfe(y = rf_dat$asci, # set dependent variable
              x = rf_dat %>% select(-asci), # set predictor variables
              size = 3:30, # sets how many variables are in the overall model
              rfeControl = my_ctrl) # pull in control from above

# can you make your model even simpler?
# the following will pick a model with the smallest number of predictor variables based on the tolerance ("tol") that you specify (how much less than the best are you willing to tolerate?)
my_size <- pickSizeTolerance(my_rfe$results, metric = "RMSE", tol = 1, maximize = F)
# higher tol (~10) gives you less variables
# lower tol (~1) gives you more variables - "I'm taking the simplest model that's within 1% of the best model."
pickVars(my_rfe$variables, size = my_size)

# Bring your brain to the party - you need to consider if the data you're feeding into the model in the first place makes sense in terms of predictors.

# You can also drill down into the splits using partial dependence plots.

# End of R script.