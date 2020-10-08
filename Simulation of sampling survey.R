
# sampling seed 
set.seed(207)
# sample size 
n <- 10000
# setting for questions
Gender <- c("Male", "Female", "Trans or Unisex", "No reply")
Age <- c("< 18", "18-30", "31-49", "50-69", "70 or older")
Rstatus <- c("Yes", "No", "Unsure")
Rate <- c(1:5)
Vote <- c("Liberal", "Conservative", "NDP", "Green", "Other","Unsure")

library(tidyverse)
# We assume for sampling gender it is approximately for a male and female
# For the transgender and unisex, they are a mijority group
# Considering it is online survey the people are more from the range of 18 to 30
df_tc <- tibble(gender = sample(Gender, size = n, replace = TRUE, prob = c(0.475, 0.475, 0.04, 0.01)),
                age = sample(Age, size = n, replace = TRUE, prob = c(0.05, 0.35, 0.30, 0.25, 0.05)),
                party = sample(Vote, size = n, replace = TRUE, prob = c(0.45, 0.25, 0.10, 0.05, 0.10, 0.05)))

# simulation for register status
# we assume that the olders are more active for voting than young
r_young <- df_tc %>% 
  filter(age == "< 18" | age == "18-30") %>% 
  mutate(register = sample(Rstatus, size = length(which(age == "< 18" | age == "18-30")), 
                                replace = TRUE,
                                prob = c(0.35, 0.60, 0.05)))

r_older <- df_tc %>% 
  filter(age == "31-49" | age == "50-69" | age == "70 or older") %>% 
  mutate(register = sample(Rstatus, size = length(which(age == "31-49" | age == "50-69" | age == "70 or older")), 
                           replace = TRUE,
                           prob = c(0.35, 0.45, 0.20)))

df_tc <- rbind(r_young, r_older)

# simulation for rating 
df_tc <- df_tc %>% 
  mutate(rate = rnorm(n, mean = 3.30, sd = 1.35) %>% round(digits = 0))

# if the simulated number is out the range from 0 to 5, we should replace it with the 0 or 5 
df_tc <- df_tc %>% 
  mutate(rate = replace(rate, rate < 0, 0),
         rate = replace(rate, rate >5, 5))

# save data 
write.csv(df_tc,"pollingdata_in_Toronto_Centre.csv")

# Descriptive Statistics
# Age 
df_tc %>% 
  group_by(age) %>% 
  summarize(n = n()) %>% 
  mutate(freq = round(n / sum(n), 4))

df_tc %>% 
  ggplot(mapping = aes(x = age, fill = gender))+
  geom_bar() + 
  labs(x = NULL,
       y = "Count")

# Register 

df_tc %>% 
  group_by(register) %>% 
  summarize(n = n()) %>% 
  mutate(freq = round(n / sum(n), 4))
# bar plot
df_tc %>% 
  ggplot(mapping = aes(x = age, fill = register)) +
  geom_bar() + 
  labs(x = NULL,
       y = "Count")

# rate
# frequency table 
df_tc %>% 
  ggplot(mapping = aes(x = rate)) +
  geom_bar() + 
  labs(x = NULL,
       y = "Count") + 
  theme_classic() +
  scale_fill_brewer(palette = "Set1")
# bar plot 
df_tc %>% 
  group_by(rate) %>% 
  summarize(n = n()) %>% 
  mutate(freq = round(n / sum(n), 4))

# vote part 
# frequency table 
df_tc %>% 
  filter(register == "Yes") %>% 
  group_by(party) %>% 
  summarize(n = n()) %>% 
  mutate(freq = round(n / sum(n), 4))

  