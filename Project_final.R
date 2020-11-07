library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)


df <- as_tibble(read.csv("DataAnalyst.csv"))
df[df == -1] <- NA
glimpse(df)

df <- df %>%
  mutate(Location = gsub(" Arapahoe,", "", Location),
         Company.Name = gsub("[[:digit:].[:digit:]]", "", Company.Name), 
         Salary.Estimate = gsub("[\\$Ka-zA-Z\\(.*\\)]", "", Salary.Estimate),
         Revenue = gsub(" \\(.*\\)", "", Revenue),
         Job.Title = tolower(Job.Title),
         Job.Description = tolower(Job.Description)) %>%
  separate(Salary.Estimate, c("Low_bar.Salary", "High_bar.Salary"), "-", convert = TRUE) %>%
  separate(Location, c("City", "State"), ",") %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_at(c(2,5), as.character) %>%
  rename(id = X)

test <- mutate(df, Revenue = gsub(" \\(.*\\)", "", Revenue))

df %>% filter(str_detect(Location, "Arapahoe")) -> case

# Analysis

install.packages("todor")


















