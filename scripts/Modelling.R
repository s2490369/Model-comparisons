library(tidyverse)
library(ggplot2)
dataset <- read_csv("data/processed/cleaned_data.csv")
show(dataset)

dataset$KM36 <- as.numeric(dataset$'Kaplan-Meier survival 36 m')
dataset$Gender <- factor(dataset$Gender, levels = c("Male", "Female"))

ethnicity_cols <- c("names of ethnicity colls")

dataset[ethnicity_cols] <- lapply(dataset[ethnicity_cols], function(x) as.numeric(as.character(x)))

dataset$EthnicityFactor <- apply(dataset[ethnicity_cols], 1, function(row){
  ethnicity_cols[which.max(row)]
})


dataset$EthnicityFactor <- factor(dataset$EthnicityFactor)

model <- lm(KM36 ~ Dep_avg + Gender + EthnicityFactor, data = dataset)
summary(model)

sigma(model)


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
