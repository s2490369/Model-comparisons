#Define packages - tidyverse contains useful packages
#ggplot2 is a graphing package
#mgcv is used for the GAM plot
#randomForest library for a random forest plot
library(tidyverse)
library(ggplot2)
library(mgcv)
library(randomForest)

#Import data file
dataset <- read_csv("data/processed/cleaned_data.csv")

#Create a data set of numerical entries from survival change (%) at 36 Months
dataset$KM36 <- as.numeric(dataset$'Kaplan-Meier survival 36m')

#Create a data set of categorical data of Gender
dataset$Gender <- factor(dataset$Gender, levels = c("Male", "Female"))

#Creates a vector which contains column names
dep_cols <- c("Deprivation quintile 1 - most deprived",
              "Deprivation quintile 2",
              "Deprivation quintile 3",
              "Deprivation quintile 4",
              "Deprivation quintile 5 - least deprived")

#We then create a subset of the data set which now contains the deprivation data numerically
dataset[dep_cols] <- lapply(dataset[dep_cols], function(x) as.numeric(as.character(x)))

#Converts that subset to a matrix for better handling
dep_matrix <- as.matrix(dataset[dep_cols])

#Initialize a numerical scale to represent each level of deprivation
dep_scores <- 1:5

#We then use a simple function which calculates the mean deprivation for each row and stores it as a new column
dataset$Dep_Avg <- apply(dep_matrix, 1, function(x) {
  sum(x * dep_scores, na.rm = TRUE) / sum(x, na.rm = TRUE)
})

#We looked at the plot between average deprivation and survival rate to see what our data looked like this is not part of the modelling
ggplot(dataset,aes(x=Dep_Avg,y=KM36)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "pink" )+
  theme_minimal()

#Now we need to do a similar thing so we can work with ethnicity, below we are trying to use proportion of white ethnicity as a factor
#Creates a vector to contain column names
ethnicity_cols <- c("Ethnicity - White","Ethnicity - Asian excl Chinese","Ethnicity - Black", "Ethnicity - Chinese", "Ethnicity - Mixed", "Ethnicity - Other", "Ethnicity - Unknown")

#Now creating a subset which contains the ethnicity data numerically
dataset[ethnicity_cols] <- lapply(dataset[ethnicity_cols], function(x) as.numeric(as.character(x)))

#Using the which.max function to find the dominant ethnicity in each group
get_dominant_ethnicity <- function(row) {
  # replace NAs with -Inf so they are ignored by which.max
  row[is.na(row)] <- -Inf
  ethnicity_cols[which.max(row)]
}

#Now storing the dominant ethnicity in a new column
dataset$EthnicityFactor <- as.character(apply(dataset[ethnicity_cols], 1, get_dominant_ethnicity))
#After inspecting this column, dominant ethnicity was either white or unknown, unknown ethnicity often meant no data recorded
#We therefore filtered out the rows with unknown dominant ethnicity
dataset <- dataset[dataset[,25]=="Ethnicity - White",]

#Create a new column that stores the total number of people in the group with known ethnicity
dataset$TotalKnown <- rowSums(dataset[, ethnicity_cols[1:6]], na.rm = TRUE)  # sum all except Unknown

#Compute the proportion of White ethnicity in each row and store as a new column (ignoring Unknown)
dataset$PropWhite <- dataset$`Ethnicity - White` / dataset$TotalKnown

#We have now represented our three factors in a way that can be used in a predictive model for survival rate of each group

#Store and display the results of a multivariable linear regression model
lm_model <- lm(KM36 ~ Dep_Avg + Gender + PropWhite, data = dataset)
summary(model)

#Store and display the results of a GAM model
gam_model <- gam(KM36 ~ s(Dep_Avg) + Gender + PropWhite, data = dataset)
summary(gam_model)


#Random forest cannot work with NAs so we remove them
dataset_rf <- na.omit(dataset[, c("KM36", "Dep_Avg", "Gender", "PropWhite")])
#Note that the dataset loses about 50% of its entries

#Store and display the results of the random forest model
#Chose ntree=1000 since MSE stopped increasing significantly past this
#Chose mtry=2 since there are only three variables
RF_model <- randomForest(KM36 ~ Dep_Avg + Gender + PropWhite, data=dataset_rf, ntree=1000,mtry=2,importance=TRUE)
print(RF_model)

#Store residual data and plot with a straight line on y=0 to show where residuals should lie
lm_residual <- resid(lm_model)
lm_fitted <- fitted(lm_model)
plot(lm_fitted, lm_residual, xlab="Predicted survival", ylab="Residuals", main="Linear model residuals") +
  abline(h=0,col='green')

#Residuals plot for GAM model
gam_residual <- resid(gam_model)
gam_fitted <- fitted(gam_model)
plot(gam_fitted, gam_residual, xlab="Predicted survival", ylab="Residuals", main="GAM model residuals") +
  abline(h=0,col='green')

#Residuals plot for RF model
rf_residual <- RF_model$predicted
rf_fitted <- dataset_rf$KM36
plot(rf_fitted, rf_residual, xlab="Predicted survival", ylab="Residuals", main="Random forest model residuals") +
  abline(h=0,col='green')