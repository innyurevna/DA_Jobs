install.packages("tidytext")

library(tidyverse)
library(stringr)
library(modeest)
library(topicmodels)
library(tidytext)

df <- read_csv("DataAnalyst.csv") #FIXME: сделать путь идентичным Кегглу, т.е. добавить подпапку
df[df == -1] <- NA
glimpse(df)

df <- df %>%
 mutate(location = gsub(" Arapahoe,", "", Location),
         cname = gsub("[[:digit:].[:digit:]]", "", `Company Name`),
         salary_est = gsub("[\\$Ka-zA-Z\\(.*\\)]", "", `Salary Estimate`),
         revenue = gsub(" \\(.*\\)", "", Revenue),
         rating = as.numeric(Rating),
         job_title = tolower(`Job Title`),
         job_desc = tolower(`Job Description`)) %>%
  separate(salary_est, c("salary_low", "salary_high"), "-", convert = TRUE) %>%
  mutate(mean_salary = (salary_low + salary_high)/2,
         mean_low = mean(salary_low, na.rm = T),
         mean_high = mean(salary_high, na.rm = T),
         range_salary = salary_high - salary_low,
         flags_low = ifelse(salary_low < mean_low, 1, 0),
         flags_high = ifelse(salary_high < mean_high, 1, 0)) %>%
  separate(location, c("city", "state"), ",") %>% 
  mutate_if(is.character, as.factor) %>% #IDEA: возможно тоже сделать через mutate_at, чтобы не было 2 mutate
  mutate_at(c(17,18), as.character) %>%
  rename(id = X1) %>% 
  select(!c(Location, `Company Name`, `Salary Estimate`, Revenue, Rating, `Job Title`, `Job Description`))
view(df)

#IDEA: добавить новую переменную как среднее арифметическое между salary_low и salary_high 
#IDEA: добавить отдельной колонкой разницу между минимумом и максимумом (размах)
#IDEA: добавить флаги: является ли нижняя/верхняя планка выше или ниже средней
glimpse(df)

mean(df$salary_low, na.rm = T)

test <- mutate(df, revenue = gsub(" \\(.*\\)", "", revenue))
test2 <- mutate(df, rating = as.numeric(Rating))
glimpse(test2)
df %>% filter(str_detect(location, "Arapahoe")) -> case

# Analysis

# install.packages("todor")
# TODO: SOMETHING

#Analysis

#Which jobs are the best judging by salary, company rating and location?

df %>% 
  group_by(rating) %>% 
  summarise(salary_low, salary_high, state) %>% 
  arrange(-rating)

ggplot(df, aes(x = state, y = salary_low, group = rating))+
  geom_point()

df %>% 
  group_by(rating) %>% 
  summarise(salary_low, salary_high, state) %>% 
  arrange(-salary_high) 

# mode from package
mlv(df$salary_high, method = "mfv", na.rm = T)
mlv(df$salary_low, method = "mfv", na.rm = T)

desc_df <- df %>% 
  group_by(state) %>% 
  summarise(mean_salary_low = mean(salary_low, na.rm = T),
            mean_salary_high = mean(salary_high, na.rm = T),
            mean_rating = mean(rating, na.rm = T),
            median_salary_low = median(salary_low, na.rm = T),
            median_salary_high = median(salary_high, na.rm = T),
            median_rating = median(rating, na.rm = T),
            mode_salary_low = mlv(salary_low, method = "mfv", na.rm = T),
            mode_salary_high = mlv(salary_high, method = "mfv", na.rm = T))

# What salary could be expected, based on industry, location and company revenue?
model_1 <- lm(salary_high ~ state + revenue, data = df)
summary(model_1)$coef

model_2 <- lm(salary_low ~ state + revenue, data = df)
summary(model_2)$coef

levels(df$Industry)

model_1.2 <- lm(salary_high ~ Industry, data = df)
summary(model_1.2)$coef

# Is there a difference among salaries in companies with different size?
class(df$Size)
levels(df$Size)

model_3 <- lm(salary_low ~ Size, data = df)
summary(model_3)$coef

model_4 <- lm(salary_high ~ Size, data = df)
summary(model_3)$coef

# Is it harder to apply in companies with high rating score?
levels(df$Easy.Apply)
table(df$Easy.Apply, df)
sum(is.na(df$Easy.Apply))

# Do the sector, rating and type of ownership have their part in salary level?
levels(df$Sector)

model_5 <- lm(salary_high ~ Sector, data = df)
summary(model_5)$coef

model_6 <- lm(salary_low ~ Sector, data = df)
summary(model_6)$coef

levels(df$Type.of.ownership)
model_7 <- lm(salary_high ~ Type.of.ownership, data = df)
summary(model_7)$coef

model_8 <- lm(salary_low ~ Type.of.ownership, data = df)
summary(model_8)$coef

model_9 <- lm(salary_high ~ rating, data= df)
summary(model_9)$coef

model_10 <- lm(salary_low ~ rating, data = df)
summary(model_10)$coef

#text     
tidy_df <- df %>%
  select(Sector, state, job_title, job_desc)%>%
  mutate(job_desc = as.character(job_desc)) %>%
  unnest_tokens(word, job_desc) %>% 
  anti_join(stop_words)

word_count <- tidy_df %>%
  count(word) %>%
  filter(n > 1000) %>%
  mutate(word2 = fct_reorder(word, n))

custom_stop_words <- tribble(
  ~word, ~lexicon,
  "3", "CUSTOM",
  "data", "CUSTOM",
  "skills", "CUSTOM",
  "analyst", "CUSTOM",
  "analytical", "CUSTOM",
  "analytics", "CUSTOM",
  "analysis", "CUSTOM",
  "including", "CUSTOM",
  "requared", "CUSTOM",
  "requarements", "CUSTOM",
  "job", "CUSTOM",
  "reports", "CUSTOM"
  )
stop_word2 <- stop_words %>%
  bind_rows(custom_stop_words)

tidy_df2 <- df %>%
  select(Sector, state, job_title, job_desc)%>%
  mutate(job_desc = as.character(job_desc)) %>%
  unnest_tokens(word, job_desc) %>% 
  anti_join(stop_word2)

word_count2 <- tidy_df2 %>%
  count(word) %>%
  filter(n > 1000) %>%
  mutate(word2 = fct_reorder(word, n))

tidy_df3 <- tidy_df2 %>%
  count(word) %>%
  top_n(20, n)

word_count3 <- tidy_df3 %>%
  count(word) %>%
  filter(n > 500) %>%
  mutate(word2 = fct_reorder(word, n))


