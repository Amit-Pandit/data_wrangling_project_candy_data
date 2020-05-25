# Reading the Clean Data

# Load in Libraries
library(tidyverse)
library(here)
library(readxl)
library(janitor)

# load in data
candy_2015 <- read_excel(here("raw_data/boing-boing-candy-2015.xlsx"))
candy_2016 <- read_excel(here("raw_data/boing-boing-candy-2016.xlsx"))
candy_2017 <- read_excel(here("raw_data/boing-boing-candy-2017.xlsx"))

# Primary-Cleaning candy_2015
# Cleaning names of Variables
candy_2015_clean_names <- clean_names(candy_2015)

# Adding Variable Year
candy_2015_year <- candy_2015_clean_names %>%
  mutate(year = 2015)

# Pivoting data and creating two new columns candy and emotion and storing in the data set pivoted
candy_2015_pivoted <- candy_2015_year %>%
  pivot_longer(butterfinger:york_peppermint_patties, names_to = "candy_name", values_to = "rating")

# dropping variables which are not required and selecting only 5 variables required for analysis
candy_2015_dropped <- candy_2015_pivoted %>%
   select(year, how_old_are_you, are_you_going_actually_going_trick_or_treating_yourself, candy_name, rating)

# re-naming the Variables with appropriate names
names(candy_2015_dropped) <- c("year", "age", "trick_or_treat_yourself", "candy_name", "rating")

# sorting out the age variable
candy_2015_clean <- candy_2015_dropped %>%
                      mutate(age = as.integer(age),
                             age = ifelse(age < 1 | age >= 100, NA_real_, age))

View(candy_2015_clean)



# Primary-Cleaning candy_2016
# Step 1 : checing dimentions / variable names
dim(candy_2016)
names(candy_2016)
View(candy_2016)

# Cleaning Variable names
candy_2016_clean_names <- clean_names(candy_2016)

# Adding Person_year
candy_2016_year <- candy_2016_clean_names %>%
  mutate(year = 2016)

# Pivoting data and creating two new columns candy and emotion and storing in the data set pivoted
candy_2016_pivoted <- candy_2016_year %>%
  pivot_longer( x100_grand_bar :york_peppermint_patties, names_to = "candy_name", values_to = "rating")

# dropping variables which are not required and selecting only variables required for analysis
candy_2016_dropped <- candy_2016_pivoted %>%
  select(year, how_old_are_you, are_you_going_actually_going_trick_or_treating_yourself, candy_name,
         rating,your_gender, which_country_do_you_live_in)

# re-naming the Variables with appropriate names
names(candy_2016_dropped) <- c("year", "age", "trick_or_treat_yourself", "candy_name", "rating", "gender", "country")

View(candy_2016_dropped)



# sorting out the age variable
candy_2016_clean <- candy_2016_dropped %>%
  mutate(age = as.integer(age),
         age = ifelse(age < 1 | age >= 100, NA_real_, age))

View(candy_2016_clean)

# Primary-Cleaning candy_2017
# Cleaning Variable names
candy_2017_clean_names <- clean_names(candy_2017)

# Adding year
candy_2017_year <- candy_2017_clean_names %>%
  mutate(year = 2017)

# Removing the Question tag added before all the variable names
names(candy_2017_year) <- str_remove(names(candy_2017_year), "q[0-9]+_")

#Re-Naming Vatiable names with relevant names
names(candy_2017_year) <- str_replace(names(candy_2017_year), "100_grand_bar", "x100_grand_bar")
names(candy_2017_year) <- str_replace(names(candy_2017_year), "going_out", "trick_or_treat_yourself")

# Pivoting data and creating two new columns candy and emotion and storing in the data set pivoted
candy_2017_pivoted <- candy_2017_year %>%
  pivot_longer( x100_grand_bar :york_peppermint_patties, names_to = "candy_name", values_to = "rating")

# dropping variables which are not required and selecting only variables required for analysis
candy_2017_dropped <- candy_2017_pivoted %>%
  select(year, trick_or_treat_yourself, gender, age, country, candy_name, rating)

# sorting out the age variable
candy_2017_clean <- candy_2017_dropped %>%
  mutate(age = as.integer(age),
         age = ifelse(age < 1 | age >= 100, NA_real_, age))

View(candy_2017_clean)



#BINDING THE 3 DATA-SETS
  
candy_all_years_primary_clean <- bind_rows(candy_2015_clean, candy_2016_clean, candy_2017_clean)

View(candy_all_years_primary_clean)

# Deep Cleaning
# Focus on cleaning country names United States
candy_all_years_clean <- candy_all_years_primary_clean %>%
  mutate(country = if_else(str_detect(country, "USSA"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "america"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "America"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "USAUSAUSA"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "usas"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "United States of America "), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "'merica "), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "Ahem....Amerca"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "Alaska"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "California"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "I pretend to be from Canada, but I am really from the United States"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "Murica"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "murrika"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "N. America,"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "New York"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "North Carolina"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "Sub-Canadian North America... 'Merica"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "the best one - usa"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "The United States"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "he United States of America,"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "The Yoo Ess of Aaayyyyyy"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "U S"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "u s a"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "u.s."), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "U.S."), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "u.s.a."), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "U.S.A."), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "unhinged states"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "Unied States"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "unite states"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "United  States of America"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "United Sates"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "United staes"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "United State"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "United Statea"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "United Stated"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "united states"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "united States"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "United states"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "United States"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "UNited States"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "United Stetes"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "United States of America"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "United Statss"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "united ststes"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "United ststes"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "Unites States"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "Units States"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "us"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "Us"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "US of A"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "usa"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "Usa"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "USa"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "USA (I think but it's an election year so who can really tell)"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "USA USA USA"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "USA USA USA USA"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "USA USA USA!!!!"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "USA!"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "USA! USA!"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "USA! USA! USA!"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, " USA!!!!!!"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "USA? Hard to tell anymore.."), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "USAA"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "usas"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "USAUSAUSA"), "united states", country)) %>%
  mutate(country = if_else(str_detect(country, "USSA"), "united states", country))%>%
# Focus on cleaning country names Canada
  mutate(country = if_else(str_detect(country, "Can"), "canada", country)) %>%
  mutate(country = if_else(str_detect(country, "canada"), "canada", country)) %>%
  mutate(country = if_else(str_detect(country, "Canada"), "canada", country)) %>%
  mutate(country = if_else(str_detect(country, "CANADA"), "canada", country)) %>%
  mutate(country = if_else(str_detect(country, "Canada`"), "canada", country)) %>%
  # Focus on cleaning country names united kingdom
  mutate(country = if_else(str_detect(country, "U.K."), "united kingdom", country)) %>%
  mutate(country = if_else(str_detect(country, "uk"), "united kingdom", country)) %>%
  mutate(country = if_else(str_detect(country, "Uk"), "united kingdom", country)) %>%
  mutate(country = if_else(str_detect(country, "United Kindom"), "united kingdom", country)) %>%
  mutate(country = if_else(str_detect(country, "United kingdom"), "united kingdom", country)) %>%
  mutate(country = if_else(str_detect(country, "United Kingdom"), "united kingdom", country))

View(candy_all_years_clean)

  # Country Details 
  # United States of America - 'merica, Ahem....Amerca, Alaska,america, America, California, 
  # I pretend to be from Canada, but I am really from the United States, Murica, murrika, N. America, New Jersey	
  # New York, North Carolina, Sub-Canadian North America... 'Merica, the best one - usa, The United States, 
  # The United States of America, The Yoo Ess of Aaayyyyyy, U S, u s a, u.s., U.s., U.S., u.s.a., U.S.A., unhinged states, 
  # Unied States, unite states, United  States of America, United Sates, United staes, United State, United Statea, 
  # United Stated, united states, united States, United states, United States, UNited States, united states of america, 
  # United States of America, United Statss, United Stetes, united ststes, United ststes, Unites States,Units States,
  # us, Us, US, US of A,usa, uSA, Usa,USa, USA, USA (I think but it's an election year so who can really tell), USA USA USA,
  # USA USA USA USA, USA USA USA!!!!, USA!, USA! USA!, USA! USA! USA!, USA!!!!!!, USA? Hard to tell anymore.., USAA,usas,
  #USAUSAUSA, USSA
  # Atlantis - Fictional Island so drop
  # australia ,Australia
  # Austria
  # belgium
  # Brasil
  # Can, canada, Canada, CANADA, Canada`, 
  # Canae ( Village in Italy)
  # cascadia, Cascadia (Not a conutry)
  # China
  # Costa Rica
  # croatia
  # Denial (Not a conutry
  # Denmark	
  # Earth	(Not a conutry)
  # endland,england,England, 
  # españa	
  # EUA	
  # Europe ( CNot a conutry) 
  # Fear and Loathing	(Not a conutry)
  # finland, Finland	
  # france, France	
  # germany, Germany
  # god's country	(Not a country)
  # Greece	
  # hong kong	, Hong Kong	
  # hungary	
  # I don't know anymore	( Not a conutry)
  # Iceland	
  # Indonesia	
  # insanity lately	( Not a country)
  # Ireland	
  # Japan	
  # kenya	
  # Korea	
  # Mexico	
  # murrika	
  # Narnia	(Not a Country)
  # netherlands, Netherlands, The Netherlands
  # Neverland	
  # New Zealand	
  # North Carolina	
  # Not the USA or Canada	(Not a country)
  # one of the best ones (Not a country)
  # Panama	
  # Philippines	
  # Pittsburgh	
  # Portugal	
  # Scotland	
  # See above	(Not a country
  # Singapore
  # Somewhere	(Not a country)
  # South africa	
  # South Korea	
  # soviet canuckistan
  # spain
  # subscribe to dm4uz3 on youtube	(Not a country)
  # sweden, Sweden	
  # Switzerland	
  # Taiwan	103
  # The Netherlands	
  # The republic of Cascadia	100
  # there isn't one for old men	(Not a country)
  # this one	(Not a country)
  # Trumpistan	(Not a country)
  # U.K., uk, Uk, UK, United Kindom, United kingdom, United Kingdom
  # UAE	
  # UD ( Not a country)
  
# Writing .csv file into clean data for further analysis
write_csv(candy_all_years_clean,'clean_data/candy_all_years_clean.csv')

