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
         rating = as.numeric(Rating),
         job_title = tolower(Job.Title),
         job_desc = tolower(Job.Description)) %>%
  separate(salary_est, c("salary_low", "salary_high"), "-", convert = TRUE) %>%
  separate(location, c("city", "state"), ",") %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_at(c(2,5), as.character) %>%
  rename(id = X)

test <- mutate(df, revenue = gsub(" \\(.*\\)", "", revenue))
test2 <- mutate(df, rating = as.numeric(Rating))
glimpse(test2)
df %>% filter(str_detect(location, "Arapahoe")) -> case

# Analysis

install.packages("todor")
# TODO: SOMETHING

#Analysis

#Which jobs are the best judging by salary, company rating and location?

df %>% 
  group_by(rating) %>% 
  summarise(salary_low, salary_high, state) %>% 
  arrange(-rating)

df %>% 
  group_by(rating) %>% 
  summarise(salary_low, salary_high, state) %>% 
  arrange(-salary_high) 

df %>% 
  group_by(state) %>% 
  summarise(mean = mean(salary_high, na = T))














