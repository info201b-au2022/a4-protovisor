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

get_data <- function(num_records=-1) {
  fname <- "~/info201/data/incarceration_trends.csv"
  df <- read.csv(fname, nrows=num_records)
  return(df)
}

incarceration_df <- get_data(0)

highest_incarcerated_race_in_state <- function(input_state) {
  df <- incarceration_df %>%
    select(state, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop) %>%
    filter(state == input_state) %>%
    summarize(
      "total_aapi_jail_pop_in_state" = sum(aapi_jail_pop, na.rm = TRUE),
      "total_black_jail_pop_in_state" = sum(black_jail_pop, na.rm = TRUE),
      "total_latinx_jail_pop_in_state" = sum(latinx_jail_pop, na.rm = TRUE),
      "total_native_jail_pop_in_state" = sum(native_jail_pop, na.rm = TRUE),
      "total_white_jail_pop_in_state" = sum(white_jail_pop, na.rm = TRUE),
      "total_other_race_jail_pop_in_state" = sum(other_race_jail_pop, na.rm = TRUE)) %>%
    return(df)
}

total_race_pop_of_state <- function(input_state) {
  df <- incarceration_df %>%
    select(state, county_name, total_pop, aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64, white_pop_15to64) %>%
    drop_na() %>%
    group_by(state, county_name) %>%
    filter(state == input_state) %>%
    filter(total_pop == max(total_pop)) %>%
    filter(aapi_pop_15to64 == max(aapi_pop_15to64)) %>%
    filter(black_pop_15to64 == max(black_pop_15to64)) %>%
    filter(latinx_pop_15to64 == max(latinx_pop_15to64)) %>%
    filter(native_pop_15to64 == max(native_pop_15to64)) %>%
    filter(white_pop_15to64 == max(white_pop_15to64)) %>%
    group_by(state) %>%
    summarize(
      "aapi_pop_15to64" = sum(aapi_pop_15to64),
      "black_pop_15to64" = sum(black_pop_15to64),
      "latinx_pop_15to64" = sum(latinx_pop_15to64),
      "native_pop_15to64" = sum(native_pop_15to64),
      "white_pop_15to64" = sum(white_pop_15to64), .groups = 'drop') %>%
    return(df)  
}

highest_incarcerated_race_in_states_dataframe <- function() {
  df <- incarceration_df %>%
    select(state, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop) %>%
    group_by(state) %>%
    summarize(
      "state" = state,
      "total_aapi_jail_pop_in_state" = sum(aapi_jail_pop, na.rm = TRUE),
      "total_black_jail_pop_in_state" = sum(black_jail_pop, na.rm = TRUE),
      "total_latinx_jail_pop_in_state" = sum(latinx_jail_pop, na.rm = TRUE),
      "total_native_jail_pop_in_state" = sum(native_jail_pop, na.rm = TRUE),
      "total_white_jail_pop_in_state" = sum(white_jail_pop, na.rm = TRUE),
      "total_other_race_jail_pop_in_state" = sum(other_race_jail_pop, na.rm = TRUE)) %>%
    unique() %>%
    return(df)
}

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
  plot <- plot + labs(x = 'Year', y = 'Total Prison Population', title = 'US Prison Population Growth by Year (1970-2018)') + scale_y_continuous(labels = scales::comma)
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
  plot <- plot + labs(x = 'Year', y = 'Total Prison Population', title = 'Prison Population Growth by Year (1970-2018)') + scale_y_continuous(labels = scales::comma)
  return(plot)
}

plot_jail_pop_by_states(c("WA", "MT", "WY", "MI", "VT"))

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Comparing, by year, the black male versus white male population of prisons in the US
#----------------------------------------------------------------------------#

get_bm_vs_wm_jail_pop <- function() {
  incarceration_df %>%
    select(year, state, black_male_prison_pop, white_male_prison_pop) %>%
    drop_na() %>%
    group_by(year) %>%
    summarize("total_bm_prison_population" = sum(black_male_prison_pop, na.rm = TRUE), "total_wm_prison_population" = sum(white_male_prison_pop, na.rm = TRUE)) %>%
    return()   
}

plot_bm_vs_wm_jail_pop <- function() {
  prison_pop <- get_bm_vs_wm_jail_pop()
  plot <- ggplot(prison_pop, aes(year, y = total_wm_prison_population, color = "White Male Prison Population")) + geom_line(aes(y = total_bm_prison_population, color = "Black Male Prison Population")) + geom_line(aes(y = total_wm_prison_population))
  plot <- plot + labs(x = 'Year', y = 'Total Prison Population', title = 'Prison Population of Black Males vs White Males in the US (1970-2018)') + scale_y_continuous(labels = scales::comma)
  return(plot)
}

plot_bm_vs_wm_jail_pop()

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Showing the rates of Black American incarceration across the United States
#----------------------------------------------------------------------------#



dataframe_of_states <- 
  highest_incarcerated_race_in_states_dataframe()

#there's probably a better way to do this but this is easier

dataframe_of_states$state_full_names <- c("Alaska", 
                                          "Alabama", 
                                          "Arkansas", 
                                          "Arizona", 
                                          "California", 
                                          "Colorado", 
                                          "Connecticut", 
                                          "District of Columbia", 
                                          "Delaware",
                                          "Florida",
                                          "Georgia",
                                          "Hawaii",
                                          "Iowa",
                                          "Idaho",
                                          "Illinois",
                                          "Indiana",
                                          "Kansas",
                                          "Kentucky",
                                          "Louisiana",
                                          "Massachusetts",
                                          "Maryland",
                                          "Maine",
                                          "Michigan",
                                          "Minnesota",
                                          "Missouri",
                                          "Mississippi",
                                          "Montana",
                                          "North Carolina",
                                          "North Dakota",
                                          "Nebraska",
                                          "New Hampshire",
                                          "New Jersey",
                                          "New Mexico",
                                          "Nevada",
                                          "New York",
                                          "Ohio",
                                          "Oklahoma",
                                          "Oregon",
                                          "Pennsylvania",
                                          "Rhode Island",
                                          "South Carolina",
                                          "South Dakota",
                                          "Tennessee",
                                          "Texas",
                                          "Utah",
                                          "Virginia",
                                          "Vermont",
                                          "Washington",
                                          "Wisconsin",
                                          "West Virginia",
                                          "Wyoming") %>%
  tolower()

states <- map_data("state") %>%
  rename(state_full_names = region) %>%
  left_join(dataframe_of_states)
  


state_plot <- 
  ggplot(states) +
    geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_black_jail_pop_in_state),
    color = "white",
    size = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "#FFFFFF", high = "Red", labels = scales::comma) +
  labs(fill = "Incarcerated Black Population") +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

state_plot

1## Load data frame ---- 


