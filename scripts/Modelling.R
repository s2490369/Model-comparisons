#Define packages - tidyverse contains usefull packages
#ggplot2 is a graphing package

library(tidyverse)
library(ggplot2)

#Import data file
dataset <- read_csv("data/processed/cleaned_data.csv")
show(dataset)

#Create a data set of numerical entries from survival change (%) at 36 Months
dataset$KM36 <- as.numeric(dataset$'Kaplan-Meier survival 36m')

#Create a data set of categorical data of Gender
dataset$Gender <- factor(dataset$Gender, levels = c("Male", "Female"))



#Creates a vector which contains entries ....
dep_cols <- c("Deprivation quintile 1 - most deprived",
              "Deprivation quintile 2",
              "Deprivation quintile 3",
              "Deprivation quintile 4",
              "Deprivation quintile 5 - least deprived")

dataset[dep_cols] <- lapply(dataset[dep_cols], function(x) as.numeric(as.character(x)))


dep_matrix <- as.matrix(dataset[dep_cols])


dep_scores <- 1:5
dataset$Dep_Avg <- apply(dep_matrix, 1, function(x) {
  sum(x * dep_scores, na.rm = TRUE) / sum(x, na.rm = TRUE)
})

ggplot(dataset,aes(x=Dep_Avg,y=KM36)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "pink" )+
  theme_minimal()




ethnicity_cols <- c("Ethnicity - White","Ethnicity - Asian excl Chinese","Ethnicity - Black", "Ethnicity - Chinese", "Ethnicity - Mixed", "Ethnicity - Other", "Ethnicity - Unknown")

dataset[ethnicity_cols] <- lapply(dataset[ethnicity_cols], function(x) as.numeric(as.character(x)))

get_dominant_ethnicity <- function(row) {
  # replace NAs with -Inf so they are ignored by which.max
  row[is.na(row)] <- -Inf
  ethnicity_cols[which.max(row)]
}


dataset$EthnicityFactor <- as.character(apply(dataset[ethnicity_cols], 1, get_dominant_ethnicity))

dataset$EthnicityFactor <- factor(dataset$EthnicityFactor)

dataset <- dataset[dataset[,25]=="Ethnicity - White",]

dataset$TotalKnown <- rowSums(dataset[, ethnicity_cols[1:6]], na.rm = TRUE)  # sum all except Unknown

# Compute proportion White (ignoring Unknown)
dataset$PropWhite <- dataset$`Ethnicity - White` / dataset$TotalKnown

model <- lm(KM36 ~ Dep_Avg + Gender + PropWhite, data = dataset)
summary(model)

sigma(model)
