library(dslabs)
library(tidyverse)
library(dplyr)

# installing and loading the dplyr package
install.packages("dplyr")
library(dplyr)

# adding a column with mutate
library(dslabs)
data("murders")


murders <- mutate(murders, rate = total / population * 100000)

# subsetting with filter
filter(murders, rate <= 0.71)

# selecting columns with select
new_table <- select(murders, state, region, rate)

# Use select to only show state names and abbreviations from murders. NOTE THE FIRST VARIABLE IS THE SOURCE FILE)
select(murders, state, abb)

# using the pipe
murders %>% select(state, region, rate) %>% filter(rate <= 0.71)

# We can use the data.frame() function to create data frames.
#By default, the data.frame() function turns characters into factors.
#To avoid this, we utilize the stringsAsFactors argument and set it equal to false.

# EXAMPLE FROM ABOVE. By creating a data frame with stringAsFactors = FALSE
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"),
                     exam_1 = c(95, 80, 90, 85),
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = FALSE)

# Note that if you want ranks from highest to lowest you can take the negative and then compute the ranks
x <- c(88, 100, 83, 92, 94)
rank(-x)

# Defining rate
rate <-  murders$total/ murders$population * 100000

# Redefine murders to include a column named rank
# with the ranks of rate from highest to lowest
murders <- mutate(murders, rank(-rate))

# Add the necessary columns
murders <- mutate(murders, rate = total/population * 100000, rank = rank(-rate))
# Filter to show the top 5 states with the highest murder rates
filter(murders, rank<= 5)

# Use filter to create a new data frame no_south
no_south <- filter(murders, region != "South")
# Use nrow() to calculate the number of rows
nrow(no_south)

# Create a new data frame called murders_nw with only the states from the northeast and the west
murders_nw <- data.frame(filter(murders, region %in% c("Northeast", "West")),stringsAsFactors = FALSE)

# Number of states (rows) in this category
nrow(murders_nw)

#OR
# Create a new data frame called murders_nw with only the states from the northeast and the west
murders_nw <- data.frame(filter(murders, region %in% c("Northeast", "West")))

# Number of states (rows) in this category
nrow(murders_nw)

# add the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))

# Create a table, call it my_states, that satisfies both the conditions
my_states <- data.frame(filter(murders,rate < 1 & region %in% c("Northeast", "West")) )

# Use select to show only the state name, the murder rate and the rank
select(my_states, state, rate, rank)

# show the result and only include the state, rate, and rank columns, all in one line
murders %>% filter(region %in% c("Northeast", "West")& rate < 1) %>% select(state, rate, rank)

# Create new data frame called my_states (with specifications in the instructions)
my_states <- data.frame(murders %>% mutate(rate =  total / population * 100000, rank = rank(-rate)) %>% filter(region %in% c("Northeast", "West")& rate < 1) %>% select(state, rate, rank))


# a simple scatterplot of total murders versus population
x <- murders$population /10^6
y <- murders$total
plot(x, y)

# a histogram of murder rates
hist(murders$rate)

# boxplots of murder rates by region
boxplot(rate~region, data = murders)

# Load the datasets and define some variables
library(dslabs)
data(murders)

population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total

plot(population_in_millions, total_gun_murders)

# Transform population using the log10 transformation and save to object log10_population
log10_population <- log10(murders$population)
# Transform total gun murders using log10 transformation and save to object log10_total_gun_murders
log10_total_gun_murders <- log10(murders$total)
# Create a scatterplot with the log scale transformed population and murders
plot(log10_population, log10_total_gun_murders)

# Store the population in millions and save to population_in_millions
population_in_millions <- murders$population/10^6


# Create a histogram of this variable
hist(population_in_millions)

# Create a boxplot of state populations by region for the murders dataset
boxplot(population~region, data = murders)
