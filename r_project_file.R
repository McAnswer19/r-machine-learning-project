## Specifying the packages of interest in a vector
packages = c("dplyr", "varImp","randomForest", "ggplot2", "e1071", "caret", 
             "kknn" )

## Now load or install all that are needed. Got this block from elsewhere. 
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


# Getting the cleaned df. NOTE: if this fails, go into the session tab, and set 
# working directory to the source file location. 
life_expectancy_df <- read.csv("cleaned_life_expectancy_data.csv", header=TRUE)


# Setting the seed for replicability
set.seed(19)


# Some helper functions

# Helper function for getting the MSE. 
get_mse <- function(vec1, vec2){
  return(mean((vec1 - vec2)**2))
}

# Helper function for getting the MAE.  
get_mae <- function(vec1, vec2){
  return(mean(abs(vec1 - vec2)))
}


# Function makes a formula from a given set of variables. 
# Life Expectancy is hardcoded as the DV.
make_formula_object <- function(predictor_variables){
  
  given_formula <- as.formula(
          paste("Life_Expectancy", 
          paste(predictor_variables, collapse = " + "), 
          sep = " ~ "))
  
  return(given_formula)
  
}

# Linear Model ---------------------------------------------------------------

# Train/test split
sample_size <- floor(0.8 * nrow(life_expectancy_df))
train_ind <- sample(seq_len(nrow(life_expectancy_df)), size = sample_size)
train_df <- life_expectancy_df[train_ind, ]
test_df <- life_expectancy_df[-train_ind, ]
# Some rm()
rm(sample_size)
rm(train_ind)
                    

lm_predictor_variables <- c("Year",
                             "Adult_Mortality", 
                             "Alcohol_Consumption",            
                             "Hepatitis_B",
                             "Polio",                           
                             "Total_Expenditure",  
                             "Diphtheria",                       
                             "HIV_AIDS")   
                             

lm_formula <- make_formula_object(lm_predictor_variables)

# Making a linear model with base_r 
linear_model <- lm(lm_formula, train_df)


# Have to get the actual y-values in a very roundabout way. We only only want
# the y-values where NONE of the lm_predictor_variables are NA. So we SELECT, 
# then NA.omit, then we grab the life expectancy. Ugly, but it works. 
linear_actual_y_values <- na.omit(select(test_df,
                                  all_of(c("Life_Expectancy", 
                                           lm_predictor_variables))))$Life_Expectancy

# For the predicted, we just call na.omit. 
linear_predicted_y_values <- na.omit(as.vector(predict(linear_model, 
                                                            newdata = test_df)))


print("linear model feature importance:")
print(summary(linear_model)$coefficients)

print(c("MSE for linear model:",  get_mse(linear_actual_y_values, linear_predicted_y_values)))
  
print(c("MAE for linear model:",  get_mae(linear_actual_y_values, linear_predicted_y_values))) 

r_squared_value <- cor(linear_actual_y_values, linear_predicted_y_values)**2

print(c("r squared for linear model:", r_squared_value))
  
print("-------------------------------------------------")


# Random Forest --------------------------------------------------------------

# Train/test split
sample_size <- floor(0.8 * nrow(life_expectancy_df))
train_ind <- sample(seq_len(nrow(life_expectancy_df)), size = sample_size)
train_df <- life_expectancy_df[train_ind, ]
test_df <- life_expectancy_df[-train_ind, ]
# XXX TODO
rm(sample_size)
rm(train_ind)

# Replacing na values with -1. The reason for this is that random_forests
# models cannot handle NA at all, 
train_df[is.na(train_df)] <- -1
test_df[is.na(test_df)] <- -1



rf_predictor_variables <- c("GDP_Per_Capita", 
                            "Development_Status", 
                            "Region", 
                            "Thinness_1_19_Years",             
                            "Thinness_5_9_Years",              
                            "Income_Composition_Of_Resources", 
                            "Average_Years_of_schooling",                                                  
                            "Mean_Male_Height_cm")

rf_formula <- make_formula_object(rf_predictor_variables)

#Random Forest Modeling using default parameters
rf_model <- randomForest(rf_formula, data = train_df)


print("random forest relative feature importance:")
print(importance(rf_model, type = 2, scale = TRUE)/sum(importance(rf_model, type = 2, scale = TRUE)))


rand_forest_actual_y_values <- test_df$Life_Expectancy
rand_forest_predicted_y_values <- as.vector(predict(rf_model, newdata = test_df))


print(c("MSE for random forest: ",  get_mse(rand_forest_actual_y_values,
                                            rand_forest_predicted_y_values)))
        
print(c("MAE for random forest: ",  get_mae(rand_forest_actual_y_values, 
                                            rand_forest_predicted_y_values)))

r_squared_value <- cor(rand_forest_actual_y_values, rand_forest_predicted_y_values)**2

print(c("r squared for random forest:", r_squared_value))


print("-------------------------------------------------")
# KNN regression ---------------------------------------------------------------
sample_size <- floor(0.8 * nrow(life_expectancy_df))
train_ind <- sample(seq_len(nrow(life_expectancy_df)), size = sample_size)
train_df <- life_expectancy_df[train_ind, ]
test_df <- life_expectancy_df[-train_ind, ]
# some rm()
rm(sample_size)
rm(train_ind)

# Replacing na values with -1. The reason for this is that knn
# models cannot handle NA at all. 
train_df[is.na(train_df)] <- -1
test_df[is.na(test_df)] <- -1


knn_predictor_variables <- c("Income_Composition_Of_Resources", 
                             "Region", 
                             "Average_Years_of_schooling", 
                             "Adult_Mortality", 
                             "Alcohol_Consumption", 
                             "Diphtheria")

knn_formula <- make_formula_object(knn_predictor_variables)


# Tune the cross-validation
tc_object <- trainControl(method = 'repeatedcv', number = 5, repeats = 5)


tuneGrid <- expand.grid(kmax = 15, 
                        distance = 1:2, 
                        kernel = c("rectangular"))

knn_model <- train(knn_formula, 
                  data = train_df, 
                  method = 'kknn',
                  trControl = tc_object,
                  preProcess = c('range'),
                  tuneGrid = tuneGrid)


KNN_actual_y_values <- test_df$Life_Expectancy
KNN_predicted_y_values <- as.vector(predict(knn_model, newdata = test_df))

print(c("MSE for KNN: ",  get_mse(KNN_actual_y_values, KNN_predicted_y_values)))
print(c("MAE for KNN: ",  get_mae(KNN_actual_y_values, KNN_predicted_y_values)))

r_squared_value <- cor(KNN_actual_y_values, KNN_predicted_y_values)**2
print(c("r squared for KNN:", r_squared_value))

print("-------------------------------------------------")
# SVM regression with RBF kernel ---------------------------------------------------------------
# Train/test split
sample_size <- floor(0.8 * nrow(life_expectancy_df))
train_ind <- sample(seq_len(nrow(life_expectancy_df)), size = sample_size)
train_df <- life_expectancy_df[train_ind, ]
test_df <- life_expectancy_df[-train_ind, ]
# Some RM
rm(sample_size)
rm(train_ind)

# Replacing na values with -1. The reason for this is that svm
# models cannot handle NA at all. 
train_df[is.na(train_df)] <- -1
test_df[is.na(test_df)] <- -1



svm_predictor_variables <- c("Income_Composition_Of_Resources", 
                             "Region", 
                             "Average_Years_of_schooling", 
                             "Adult_Mortality", 
                             "Alcohol_Consumption", 
                             "Diphtheria"
)



svm_formula <- make_formula_object(svm_predictor_variables)

#Random Forest Modeling
svm_model <- svm(svm_formula, data = train_df, 
                  type = "nu-regression", kernel = "radial")

# Sequence of exponents to feed into the ranges() parameter specifying gamma
# and cost gridding. 
exponent_seq = seq(from = -2, to = 2, by = 0.5)

tc_object = tune.control(random = TRUE, sampling = "cross", cross = 5, 
                         best.model = TRUE, nrepeat = 1)

tune_results <- tune(svm, svm_formula, data = train_df, 
                        ranges = list(gamma = 10**exponent_seq, cost = 10**exponent_seq), 
                        tunecontrol = tc_object)

# Extracting the best model by the hyperparameter optimization carried out by
# the tune() function
svm_model <- tune_results$best.model


SVM_actual_y_values <- test_df$Life_Expectancy
SVM_predicted_y_values <- as.vector(predict(svm_model, newdata = test_df))

print(c("MSE for SVM: ",  get_mse(SVM_actual_y_values, SVM_predicted_y_values))) 
print(c("MAE for SVM: ",  get_mae(SVM_actual_y_values, SVM_predicted_y_values))) 

r_squared_value <- cor(SVM_actual_y_values, SVM_predicted_y_values)**2
print(c("r squared for SVM:", r_squared_value))

# All plots produced here  ---------------------------------------------------------------

# Linear_Residuals Plot:-------------------------------------------------------
linear_residuals <- linear_actual_y_values - linear_predicted_y_values
print("shapiro-wilk test results for linear:")
print(shapiro.test(linear_residuals))


# Diving by standard to scale for comparison to standard normal. 
linear_residuals <- linear_residuals/sd(linear_residuals) 


plotting_df <- data.frame(pseudo_index = c(1:length(linear_residuals)), 
                          linear_residuals = linear_residuals)


ggp_graph <- ggplot(plotting_df, aes(x = linear_residuals))


ggp_graph <- ggp_graph + geom_density(aes(size = 0.01, color = "red"), alpha = 0.5)

ggp_graph <- ggp_graph + stat_function(fun = dnorm, 
                                           args = list(mean = 0, sd = 1),
                                           aes(size = 0.01 , color = "blue"),
                                           )

ggp_graph <- ggp_graph + guides(size = "none") #To turn off the shape legend

ggp_graph <- ggp_graph + labs(x = "Standard Deviations",y = "Frequency", 
                              color = "Legend")

ggp_graph <- ggp_graph + scale_color_manual(labels = c("Standard Normal", "Residuals"), 
                                            values = c("lightblue", "red"))

ggp_graph <- ggp_graph + theme(legend.position = c(0.2, 0.8))


print(ggp_graph)



# Plot of worst performance, Linear:-------------------------------------------
plotting_df <- data.frame(actual = linear_actual_y_values, 
                          predicted = linear_predicted_y_values)

plotting_df <- arrange(plotting_df, linear_actual_y_values)
plotting_df$index <- c(1:nrow(plotting_df))

ggp_graph <- ggplot(plotting_df)


ggp_graph <- ggp_graph + geom_point(aes(x = index, y = actual, 
                                    color="DarkRed"), alpha = 0.9)

ggp_graph <- ggp_graph + geom_point(aes(x = index, y = predicted, 
                                    color="Black"), alpha = 0.3)

ggp_graph <- ggp_graph + geom_line(aes(x = index, y = predicted, 
                                    color="Black"), alpha = 0.3)

ggp_graph <- ggp_graph + scale_color_identity(name = "Model fit",
                                       breaks = c("DarkRed", "Black"),
                                       labels = c("Actual", "Predicted"),
                                       guide = "legend")

ggp_graph <- ggp_graph + labs(x = "Index (Ascending)", y = "Life Expectancy")

print(ggp_graph)


# Also getting average 95% CI gap.
lower_interval <- predict(linear_model, newdata = test_df, 
                          interval = "confidence")[, 2]

upper_interval <- predict(linear_model, newdata = test_df, 
                          interval = "confidence")[, 3]

print(c("Average linear CI gap", get_mae(lower_interval, upper_interval) ))



# Plot of best performance, KNN:-----------------------------------------------
plotting_df <- data.frame(actual = KNN_actual_y_values, 
                          predicted = KNN_predicted_y_values)

plotting_df <- arrange(plotting_df, KNN_actual_y_values)
plotting_df$index <- c(1:nrow(plotting_df))

ggp_graph <- ggplot(plotting_df)

ggp_graph <- ggp_graph + geom_point(aes(x = index, y = actual, 
                                        color="DarkRed"), alpha = 0.9)

ggp_graph <- ggp_graph + geom_point(aes(x = index, y = predicted, 
                                        color="Black"), alpha = 0.3)

ggp_graph <- ggp_graph + geom_line(aes(x = index, y = predicted, 
                                       color="Black"), alpha = 0.3)

ggp_graph <- ggp_graph + scale_color_identity(name = "Model fit",
                                              breaks = c("DarkRed", "Black"),
                                              labels = c("Actual", "Predicted"),
                                              guide = "legend")

ggp_graph <- ggp_graph + labs(x = "Index (Ascending)", y = "Life Expectancy")

print(ggp_graph)


# Boxplot for residuals of the 3 best models.
rand_forest_residuals <- rand_forest_actual_y_values - rand_forest_predicted_y_values
KNN_residuals <- KNN_actual_y_values - KNN_predicted_y_values
SVM_residuals <- SVM_actual_y_values - SVM_predicted_y_values

plotting_df <- data.frame(rand_forest_residuals = rand_forest_residuals, 
                          KNN_residuals = KNN_residuals, 
                          SVM_residuals = SVM_residuals)


ggp_graph <- ggplot(plotting_df)
ggp_graph <- ggp_graph + geom_boxplot(aes(x = 1, y = rand_forest_residuals)) 
ggp_graph <- ggp_graph + geom_boxplot(aes(x = 2, y = KNN_residuals))
ggp_graph <- ggp_graph + geom_boxplot(aes(x = 3, y = SVM_residuals))
ggp_graph <- ggp_graph + labs(y = "Residuals", x = "Models")
ggp_graph <- ggp_graph + scale_x_discrete(name ="Models", 
                     limits=c("Random Forest","KNN","SVM"))

print(ggp_graph)

# And getting the tukey HSD results for those residuals. 
anova_results <- aov(values ~ ind, stack(plotting_df))
tukey_results <- TukeyHSD(anova_results)
print(tukey_results)
