library(tidyverse)
raw <- read_csv("data/raw/GDO_data_wide.csv")

cleaned_1 <- raw[,c(1,2,3,15,17,18,21,69,70,71,72,73,74,75,106,198)]
show(cleaned_1)

cleaned_2 <- cleaned_1[cleaned_1[,1]=="Skin tumours",]
show(cleaned_2)

cleaned_3 <-cleaned_2[cleaned_2[,3]=="Skin cancer",]
show(cleaned_3)

cleaned_4 <-cleaned_3[cleaned_3[,6]=="Male"|cleaned_3[,6]=="Female",]
show(cleaned_4)

write.csv(cleaned_4,"data/processed/cleaned_data.csv")
