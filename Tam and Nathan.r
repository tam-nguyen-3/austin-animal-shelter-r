library(mosaic)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(boot)
library(mdsr)

cat <- read_csv("C:/Users/DELL/Downloads/aac_shelter_cat_outcome_eng.csv")
animal <- read_csv("C:/Users/DELL/Downloads/aac_shelter_outcomes.csv")

str(cat)


# Cleaning up the data using lubridate
animal <- animal %>%
  mutate(date_of_outcome = date(datetime)) %>%
  mutate(age_upon_outcome_weeks = as.period(interval(date_of_birth, date_of_outcome))) %>%
  select(age_upon_outcome_weeks)

# Finding the average number of adoption per month
animal_2017 <- animal %>%
  mutate(date_of_outcome = date(datetime)) %>%
  filter(year(date_of_outcome) == "2017") %>%
  mutate(age_upon_outcome_month = time_length(as.period(interval(date_of_birth, date_of_outcome)),unit = "month")) %>%
  mutate(outcome_type = ifelse(outcome_type == "Adoption","Adoption","Other"))

# Checking the distribution
str(animal_2017)        
hist(animal_2017$age_upon_outcome_days)

favstats(age_upon_outcome_days~outcome_type, data = animal_2017)

# there are some age upon outcome that are less than 

ggplot(aes(x = age_upon_outcome_days), data = animal_2017)+
  geom_histogram(binwidth = 5)

##############################################

### Questions ----
# For cat, only cats younger than 6 years old are adopted.
# Assume that the distribution of adopted cat follows a binomial distribution.
# Possible questions:
# given n cats, what is the prop that their expected revenue
# from cat is greater than 10k$
# how many cat should at least get adopted to make the revenue > 10k$

### Revenue simulation----
n_adoption <- rbinom(100, 20000, 0.4821136735)

adopt_sim <- function(n_adoption, sims = 20) {
  n_cat <- rbinom(sims, n_adoption, 0.383)
  n_dog_young <- rbinom(sims, n_adoption, 0.147)
  n_dog_old <- rbinom(sims, n_adoption, 0.389)
  revenue <- 80*n_cat + 100*n_dog_young + 80*n_dog_old
  return(
    tibble(
      n_adoption = n_adoption,
      n_cat = n_cat,
      n_dog_young = n_dog_young,
      n_dog_old = n_dog_old,
      revenue = revenue))}

reps <- 100
sim_results <- 1:reps %>%
  map_dfr(~map_dfr(rbinom(100, 20000, 0.4821136735), adopt_sim))

hist(sim_results$revenue)
favstats(sim_results$revenue)

tr_shelter <- tibble(
  n_adoption = 20000,
  n_cat = rbinom(1000, n_adoption, 0.383),
  n_dog_young = rbinom(1000, n_adoption, 0.147),
  n_dog_old = rbinom(1000, n_adoption, 0.389),
  revenue = 80*n_cat + 100*n_dog_young + 80*n_dog_old
)

hist(tr_shelter$revenue)
favstats(tr_shelter$revenue) 
# Seems to follow a normal distribution

### Length of stay diff between cat-dog simulation ----
cat_adopted <- join_animal %>%
  filter(animal_type.x == "Cat") %>%
  filter(outcome_type == "Adoption")
cat_not_adopted <- join_animal %>%
  filter(animal_type.x == "Cat") %>%
  filter(outcome_type != "Adoption")
hist(cat_adopted$stay_month)
hist(cat_not_adopted$stay_month)

# Since people normally prefer younger kitten, plus old kats tend to stay more in the shelter, 
# we use exp dist
ggplot(cat_adopted, aes(x=stay_month)) +
  geom_histogram(aes(y=..density..)) +
  stat_function(fun = dexp,
                args = list(rate=0.6))

ggplot(cat_not_adopted, aes(x=stay_month)) +
  geom_histogram(aes(y=..density..)) +
  stat_function(fun = dexp,
                args = list(rate=1.9))
plot_a
plot_b

cat_not_adopted_stay <- cat_not_adopted$stay_month
cat_adopted_stay <- cat_adopted$stay_month
diff <- cat_adopted_stay[1:3182] - cat_not_adopted_stay[1:3182]
hist(diff)
favstats(diff)
# assume the length of stay in the shelter follows exponential distribution.
cat_not_adopted_stay <- rexp(rbinom(1,17000,), 1.9) 
dog_adopted_stay <- rexp(10000, 1)
cat_adopted_stay <- rexp(10000, 0.6)
diff <- dog_adopted_stay - cat_adopted_stay
hist(diff)
favstats(diff)

#### For dogs----
dog_adopted <- join_animal %>%
  filter(animal_type.x == "Dog") %>%
  filter(outcome_type == "Adoption")
dog_not_adopted <- join_animal %>%
  filter(animal_type.x == "Dog") %>%
  filter(outcome_type != "Adoption")
hist(dog_adopted$stay_month)
hist(dog_not_adopted$stay_month)

ggplot(dog_adopted, aes(x=stay_month)) +
  geom_histogram(aes(y=..density..)) +
  stat_function(fun = dexp,
                args = list(rate=1))
plot_c
plot_d <- ggplot(dog_not_adopted, aes(x=stay_month)) +
  geom_histogram(aes(y=..density..)) +
  stat_function(fun = dexp,
                args = list(rate=1.9))
plot_d

dog_not_adopted_stay <- dog_not_adopted$stay_month
dog_adopted_stay <- dog_adopted$stay_month
diff <- dog_adopted_stay[1:3182] - dog_not_adopted_stay[1:3182]
hist(diff)
favstats(diff)
# assume the length of stay in the shelter follows exponential distribution.
dog_not_adopted_stay <- rexp(3200, 1.9) 
dog_adopted_stay <- rexp(3200, 1)
diff <- dog_adopted_stay - dog_not_adopted_stay
hist(diff)
favstats(diff)

################################################

#### BOOTSTRAP THE DIED KITTENS IN 2017

cat %>%
  filter(cat_or_kitten == "Kitten") %>%
  filter(outcome_age_year > 0 & outcome_age_year <0.2) %>%
  ggplot(aes(x=outcome_age_year))+
  geom_density(bw = 0.02)



# there are some data that the kitten is most prone to die between 4-5 weeks of age. We want to give the shelter a heads-up on how long they should monitor the kitten since the date of adoption
# filter out the kittens from the cat output data
kitten <- cat %>%
  filter(cat_or_kitten == "Kitten")

# look at the statistics for the mortality of kitten
favstats(outcome_age_year~outcome_type, data = kitten)
298/8684 # this is the proportion of cat that died out of all of those outcome.


# look at the died kittens with a histogram. Looks like they die the most around 0.1
died_kitten <- kitten %>%
  filter(outcome_type == "Died")%>%
  ggplot(aes(x=outcome_age_year))+
  geom_histogram(binwidth = 0.02)

died_kitten

# look at the number of died cats in 2017 and their mean age of passing
favstats(~outcome_age_year, data=died_cate) # this is super skewed since the mortality rate of kitten is much higher than cat
favstats(outcome_age_year~cat_or_kitten, data=died_cate) 

died_cate <- cat %>%
  filter(outcome_type == "Died")
died_cate%>%
  ggplot(aes(x=outcome_age_year))+
  geom_histogram(binwidth = 0.05)+
  facet_wrap(~cat_or_kitten)

died_cate
# the data is indeed skewed

histogram(died_kitten$outcome_age_year)

x1 = favstats(~outcome_age_year, data=died_kitten)
range(died_kitten$outcome_age_year)



## Bootstrap on the died kitten

funct_iqr <- function(formula, data, indices){
  d <- data[indices,]
  iqr_cat <- IQR()
}

favstats(~outcome_age_year, data = died_cate)

meochet = 1:6884
boot_diedcat = sample(meochet, replace = TRUE)
nboot = 1000
bootiqr = rep(0,nboot)

for (i in 1:n){
  boot_diedcat = sample(meochet, replace = TRUE, data = died_cate)
  bootiqr[i] = range(boot_diedcat)
}

hist(bootiqr)
boot.mean
mean(bootiqr)

bootiqr


################################################
## INNER_JOINING TWO NEW DATASETS

# Import most recent data 

mr_intake <- read.csv("C:/Users/DELL/Downloads/Austin_Animal_Center_Intakes_2022.csv")
mr_outcome <- read.csv("C:/Users/DELL/Downloads/Austin_Animal_Center_Outcomes_2022.csv")

str(mr_intake)

# Merge the two datasets and convert the dates they come in and the dates they come out into periods
animal_2022 <- mr_intake %>%
  inner_join(mr_outcome, by = "Animal.ID")


animal_2022 <- animal_2022 %>%
  select(Animal.ID, Age.upon.Intake, Age.upon.Outcome,DateTime.x,DateTime.y, Animal.Type.x) %>%
  mutate(date_of_intake = date(mdy_hms(DateTime.x)),
          date_of_outcome = date(mdy_hms(DateTime.y)))




animal_2022 <- animal_2022%>%
  filter(year(date_of_outcome)==2017 & year(date_of_intake) == 2017
         & Animal.Type.x == c("Dog", "Cat"))%>%
  mutate(length_of_stay = time_length(as.period(interval(date_of_intake, date_of_outcome)),unit = "year")) %>%
  select(Animal.ID, date_of_intake,date_of_outcome,length_of_stay,Animal.Type.x)%>%
  mutate(pos_or_neg = ifelse(length_of_stay>=0,"Positive","Negative"))%>%
  filter(pos_or_neg == "Positive")

animal_2022 %>%
  filter(Animal.Type.x == "Cat") %>%
  ggplot(aes(x=length_of_stay)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white", binwidth = 0.03)+
  stat_function(fun = dexp,
                args = list(rate=0.09))



hist_age <- animal_2017 %>%
  ggplot(aes(x = age_upon_outcome_month))+
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white")+
  stat_function(fun = dexp,
                args = list(rate=0.4))

favstats(length_of_stay~Animal.Type.x, data = animal_2022)






######################################
# POISSON DISTRIBUTION
## Assume that in fiscal year 2017, there are 353 people engaging in volunteering activities amounting to 24950 hours
# which is 1,497,000 minutes.

## The rate of a volunteer individually is 6 hours per month
# https://shelterreport.blogspot.com/2012/02/calculating-staffing-requirements-based.html

## Assume that an animal will need 15 minutes of care everyday, which means 450 minutes per month, equivalent to 7.5 hours of care per month

## Average of 14,763 animals per year, total 

# They mentioned that there are 500 cats and dogs ready to adopt every day. This means that they will need a total of 7,500 minutes of care each day, or 3750 hours per month

# The volunteer is willing to spend 6 hours per month, which means 12 minutes per day.

n = 10000
one_volunteer = 6
count = 0
volunteer_no = 1

sim = tibble(
  pet_care = rpois(n,3750),
  volunteer = rpois(n,one_volunteer*volunteer_no),
  success = ifelse(volunteer-pet_care>= 0, 1, 0))
  count = count(sim$success == 1)
  
while (count/n < 0.8){
    volunteer_no = volunteer_no +1
    sim = tibble(
      pet_care = rpois(n, 3750),
      volunteer = rpois(n, one_volunteer*volunteer_no),
      success = ifelse(volunteer-pet_care>= 0, 1, 0))
    count = count(sim$success == 1)
  }
