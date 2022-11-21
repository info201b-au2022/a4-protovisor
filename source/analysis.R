library(tidyverse)

# The functions might be useful for A4
source("~/info201/assignments/a4-protovisor/source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Data Summary:
#----------------------------------------------------------------------------#



## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  incarceration_df %>%
  select(year, state, total_jail_pop) %>%
  drop_na() %>%
  group_by(year) %>%
  summarize("total_prison_population" = sum(total_jail_pop, na.rm = TRUE)) %>%
  return()   
}

get_year_jail_pop()

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function(){
  plot <- ggplot(get_year_jail_pop(), aes(x = year, y = total_prison_population)) + geom_col() 
  plot <- plot + labs(x = 'Year', y = 'Total Prison Population', title = 'US Prison Population Growth by Year') + scale_y_continuous(labels = scales::comma)
  return(plot)
}

plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State & Year
#----------------------------------------------------------------------------#

get_jail_pop_by_states <- function(states) {
    df <- incarceration_df %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarise("jail_pop_by_state" = sum(total_jail_pop, na.rm = TRUE))
    return(df)
}

get_jail_pop_by_states(c("WA", "UT", "OR"))

plot_jail_pop_by_states <- function(states) {
  states <- get_jail_pop_by_states(states)
  plot <- ggplot(states, aes(x = year, y = jail_pop_by_state, group = state, color = state)) + geom_line() 
  plot <- plot + labs(x = 'Year', y = 'Total Prison Population', title = 'State Prison Population Growth by Year') + scale_y_continuous(labels = scales::comma)
  return(plot)
}

plot_jail_pop_by_states(c("WA", "NY", "MT"))

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Finding the highest incarcerated population of various races in all 50 states
#----------------------------------------------------------------------------#
highest_incarcerated_race_in_states <- function(states) {
    df <- incarceration_df %>%
    
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


