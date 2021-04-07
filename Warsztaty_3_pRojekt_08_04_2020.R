#######################################################
# Warsztaty z R | SKN Statystyki
#######################################################
# Spotkanie 3 - 06.04.2021: Manipulowanie danymi w dplyr
# Prowadzi Jan Jarco
#######################################################
# Oczyszczanie środowiska
rm(list = ls())

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("openxlsx")) install.packages("openxlsx")

user_path <- file.path(Sys.getenv("USERPROFILE"), fsep="\\")

# SET WD ------------------------------------------------------------------
# setwd("C:\\Users\\olape\\Desktop\\pRojekt_warsztat")
setwd(paste(user_path, "Documents\\R\\pRojekt",sep = "\\"))

# oddzielane przecinkiem i kropka jako miejsce dziesietne
data <- read.csv("beer_reviews.csv",sep = ",",  dec = ".")

# oddzielane srednikiem i przecinkiem jako miejsce dziesietne
# data <- read.csv2("beer_reviews_dirty.csv", sep = ";", dec = ",")

# EXPLORE DATA -------------------------------------------------------------

# https://data.world/socialmediadata/beeradvocate # know your data :)
# https://www.beeradvocate.com/

str(data) # structure of data frame
head(data) # see top 6 observations
tail(data, n = 3) # see bottom 3 observations
summary(data) # summarise all columns
data[data$beer_abv > 10,]
colnames(data)
# beer_abv - Alcohol by Volume :-) 

# Przygotowanie tabelki z id piwa itd.
beers <- data %>% 
  select(contains("beer"), brewery_id ) %>% 
  distinct() %>% 
  arrange(beer_beerid)

breweries <- data %>% 
  select(contains("brewery")) %>% distinct() %>%
  mutate(brewery_name = replace(brewery_name, brewery_name == "", NA)) %>%
  arrange(brewery_id)

reviews_raw <- data %>%
  select(beer_beerid, contains("review"))


write.xlsx(list("Beers" = beers, "Breweries" = breweries), "Beer_maps.xlsx")
# write.csv2(reviews, "reviews_raw.csv")


# READ - wczytywanie i laczenie danych
# rm(list = ls())

# reviews_raw <- read.csv2("reviews_raw.csv")
beers <- read.xlsx("Beer_maps.xlsx", sheet = "Beers")
breweries <- read.xlsx("Beer_maps.xlsx", sheet = "Breweries")

# Mamy reviews_raw, beers, breweries, ale jak je polaczyc? do uzupelnienia







# SELECT - wybieranie kolumn ----------------------------------------------
set.seed(2020) # zeby wszyscy mieli tak samo

reviews_sample <- reviews[sample(nrow(reviews), 50000),]

attach(reviews_sample)

reviews_sample %>% 
  select(brewery_id, review_overall)

reviews_sample %>% 
  select(brewery_id, review_overall) %>% 
  head() # select only top 6 ones

reviews_selected <- reviews_sample %>% 
  select(brewery_name, beer_name, #ID columns
         review_overall, # general note
         review_aroma, review_appearance, review_palate, review_taste, # review - specific notes
         beer_abv #specific info
  ) # save selected data

# ARRANGE - sort data -----------------------------------------------------

reviews_selected %>% 
  arrange(beer_abv) # sort ascending

reviews_selected %>% 
  arrange(beer_abv) %>% 
  select(beer_abv) # sort ascending and show one column

reviews_selected %>% 
  arrange(desc(beer_abv)) %>% 
  select(beer_abv) # sort descending

reviews_selected %>% 
  arrange(desc(beer_abv)) %>% 
  select(beer_abv) %>% 
  head() # use as many pipes as you want

reviews_selected %>% 
  arrange(beer_abv) %>% 
  select(beer_abv) %>% 
  tail() # the same as the above? ups, there are NAs

# FILTER - filter data by values ------------------------------------------

reviews_selected %>% 
  filter(!is.na(beer_abv)) 
  # %>% na.omit() %>% dim() # show data without NAs

reviews_selected %>% 
  filter(!is.na(beer_abv) & beer_abv > 10) # specific number

reviews_selected %>% 
  filter(beer_abv <= mean(!is.na(beer_abv))) # computed number 

reviews_selected <- reviews_selected %>% 
  filter(!is.na(beer_abv)) %>% 
  arrange(review_palate) # overwrite df. eliminate null values

reviews_selected %>% 
  arrange(beer_abv) %>% 
  select(beer_abv) %>% 
  tail() # compare results with the below one

reviews_selected %>% 
  arrange(desc(beer_abv)) %>% #descending or no function
  select(beer_abv) %>% 
  head() # head or tail :)

# SUMMARISE - summarise your data -----------------------------------------

reviews_selected %>% 
  summarise(Minimal = min(review_overall),
            Maximal = max(review_overall),
            Mean_value = mean(review_overall),
            StandardDeviation = sd(review_overall),
            Median_value = median(review_overall),
            Q1 = quantile(review_overall, prob = 0.25),
            Q2 = quantile(review_overall, prob = 0.5),
            Q3 = quantile(review_overall, prob = 0.75)
  ) # see the maximal value and quartiles? the difference is huge!


reviews_selected %>% count(beer_name, sort = TRUE) %>% head(10)

reviews_selected %>% group_by(beer_name) %>% 
  summarise(Reviews_number = n(),
            Minimal = min(review_overall),
            Maximal = max(review_overall),
            Mean_value = mean(review_overall),
            Median_value = median(review_overall),
            Q1 = quantile(review_overall, prob = 0.25),
            Q2 = quantile(review_overall, prob = 0.5),
            Q3 = quantile(review_overall, prob = 0.75)
  ) %>% 
  filter(Mean_value >= 2 & Reviews_number > 5) %>% 
  as.data.frame() %>% 
  arrange(Mean_value) %>% 
  relocate(beer_name,  Mean_value) %>% 
  rename(Number_reviews = Reviews_number)

# MUTATE - create a column ------------------------------------------------


reviews_selected %>% 
  mutate(Reverse_Scale_Overall = 6-review_overall) %>% 
  select(review_overall, Reverse_Scale_Overall) %>% 
  head() # Reverse scale

# Funkcja ifelse
reviews %>% 
  mutate(Good_or_Bad = ifelse(review_overall >= 3, "Good", "Bad"))


reviews_selected <- reviews_selected %>% 
  mutate(Good_or_Bad = case_when(review_overall >= 3 ~ "Good",
                                 TRUE ~ "Bad")
  ) # save new column

reviews_selected_final <- reviews_selected %>% 
  group_by(beer_name) %>% 
  mutate(Median_review_overall = median(review_overall)) %>% 
  arrange(desc(Median_review_overall)) %>% 
  ungroup() # ungroup when you save!

# MERGING DATA  ------------------------------------------------
set.seed(2020)

beers_catalogue <- merge(beers, breweries, by = "brewery_id")
dim(beers_catalogue)

# Teraz trochę namieszajmy
breweries_sample <- breweries[sample(nrow(breweries),1000),]

left_joined_catalogue <- left_join(beers, breweries_sample, by = "brewery_id")
dim(left_joined_catalogue)

inner_joined_catalogue <- inner_join(beers, breweries_sample, by = "brewery_id")
dim(inner_joined_catalogue)

merged_catalogue <- merge(beers, breweries_sample, by = "brewery_id")
dim(merged_catalogue)

# TIDY DATA
wide_to_long <- gather(data = reviews_selected_final %>% select(-Good_or_Bad, -review_overall, -Median_review_overall),  
               key = "review_parameter",
               value = rating, review_aroma:beer_abv)
Zadanie domowe w pliku 