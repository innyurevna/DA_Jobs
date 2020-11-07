library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)


df <- as_tibble(read.csv("DataAnalyst.csv"))
df[df == -1] <- NA
glimpse(df)

df <- df %>%
  mutate(location = gsub(" Arapahoe,", "", Location),
         cname = gsub("[[:digit:].[:digit:]]", "", Company.Name), 
         salary_est = gsub("[\\$Ka-zA-Z\\(.*\\)]", "", Salary.Estimate),
         revenue = gsub(" \\(.*\\)", "", Revenue),
         job_title = tolower(Job.Title),
         job_desc = tolower(Job.Description)) %>%
  separate(salary_est, c("salary_low", "salary_high"), "-", convert = TRUE) %>%
  separate(location, c("city", "state"), ",") %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_at(c(2,5), as.character) %>%
  rename(id = X)

test <- mutate(df, revenue = gsub(" \\(.*\\)", "", revenue))

df %>% filter(str_detect(location, "Arapahoe")) -> case

# Analysis

install.packages("todor")
# TODO: SOMETHING

















