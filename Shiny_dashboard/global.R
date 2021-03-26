## Load Packages ---- 
library(qualtRics)
library(summarytools)
library(tidyverse)
library(stringr)
library(rgdal)
library(leaflet)
library(RColorBrewer)

## Create a clean dataset for all measures ----

### Connect to Qualtrics ----
apikey="hL9ew7XqwL80gXy4Y6qdOLWNld1xXSKLMNLZJsc7"
baseurl="ucsd.co1.qualtrics.com"
qualtrics_api_credentials(api_key =apikey,base_url =baseurl, overwrite=TRUE,install = T)
(readRenviron("~/.Renviron")) # so that you can use credentials without restarting R

### Load Data ----
surveys<-all_surveys()
all_measures <- fetch_survey(surveyID = "SV_aYnKJLduguA1D7v",verbose = TRUE, force_request = T, 
                             convert = FALSE, add_var_labels = FALSE, breakout_sets = FALSE)
all_measures <- all_measures %>% 
  mutate(measure_id = str_to_lower(Q147_1))

### Get list of published  measures ----
measures_pub <- read.csv(url("https://emerge.ucsd.edu/measures-urls.php"), header = F)
colnames(measures_pub) <- "measure_id"
measures_pub$measure_id <- unique(measures_pub$measure_id)

### Filter cases to select published measures ---- 
df_measures_filtered <- all_measures %>% 
  semi_join(measures_pub, by = "measure_id")


### Select useful variables ----

df_measures <- df_measures_filtered %>% 
  select(measure_id, 
         title = Q62, 
         citation_freq = Q68,
         multi_country = Q81,
         short_measure = SHORTMEASURE,
         psych_score = Q120,
         countries = Q77,
         dim_psych_emp = Q122,
         dim_soc_emp = Q123,
         dim_edu = Q124,
         dim_legal = Q125,
         dim_political = Q126,
         dim_hh_rel = Q127,
         dim_env = Q128,
         dim_time = Q129,
         dim_gbv = Q130,
         dim_wee = Q131,
         dim_health = Q132,
         dim_mnch = Q134,
         dim_srh = Q135)

### Identify and remove duplicated values ----
df_measures <- df_measures %>% 
  mutate(duplicated = duplicated(measure_id)) %>% 
  filter(duplicated != TRUE)

### Clean dataset----

#### Function to replace "None apply" with NA----
rep_na <- function(x) (ifelse(x == "None apply", NA, x))

#### clean variables----
df_measures <- df_measures %>% 
  mutate(title = str_to_upper(title)) %>% 
  mutate(short_measure = ifelse(short_measure == "No - Not a Short Measure" | 
                                  short_measure == "Uncategorized", 
                                "No", "Yes")) %>% 
  mutate_at(vars(starts_with("dim")), rep_na) %>% 
  mutate(countries_spdf = countries) %>% 
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "Congo, Democratic Republic of the", 
                                          "Democratic Republic of the Congo")) %>% 
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "Congo, Republic of the", "Congo")) %>%
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "Micronesia, Federated States of", "Micronesia")) %>%
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "Korea, South", "South Korea")) %>%
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "Korea, North", "North Korea")) %>%
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "Samoa", "American Samoa")) %>%
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "The Bahamas", "Bahamas")) %>%
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "Brunei", "Brunei Darussalam")) %>%
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "Myanmar \\(Burma\\)", "Burma")) %>%
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "The Gambia", "Gambia")) %>%
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "Iran", "Iran (Islamic Republic of)")) %>%
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "Laos", "Lao People's Democratic Republic")) %>%
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "Libya", "Libyan Arab Jamahiriya")) %>%
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "Moldova", "Republic of Moldova")) %>%
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "Syria", "Syrian Arab Republic")) %>%
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "Macedonia", "The former Yugoslav Republic of Macedonia")) %>%
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "East Timor \\(Timor-Leste\\)", "Timor-Leste")) %>%
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "Tanzania", "United Republic of Tanzania")) %>%
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "Vietnam", "Viet Nam")) %>% 
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "United States of America", "United States")) %>%
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "Vatican City \\(Holy See\\)", "Holy See (Vatican City)")) %>% 
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "South Sudan", "Sudan")) %>% 
  mutate(countries_spdf = str_replace_all(countries_spdf, 
                                          "Kosovo", "Serbia"))


# # download data
# download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="geoDATA/world_shape_file.zip")
# # Unzip file
# system("unzip geoDATA/world_shape_file.zip")

### Read this shape file with the rgdal library. ----
world_spdf <- readOGR( 
  dsn= paste0(getwd(),"/geoDATA/world_shape_file") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose = FALSE)

# Global Functions

## Function to generate a word-frequency matrix ---- 

getTermMatrix <- function(df) {
  text <-  df$title
  docs = Corpus(VectorSource(text)) 
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  docs <- tm_map(docs, removeWords, c("and", "AND", "-")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  return(d)
}

## Function to create country and number of measures matrix

getCountrydata <- function(df) {
  # Create a list of countries in the database as strings
   country_list <- df %>% 
    select(countries_spdf) %>% 
    filter(!is.na(countries_spdf))
   
   # Create a dataframe with individual countries
   df_countries <- as.data.frame(unlist(strsplit(country_list$countries_spdf, ",")))
   colnames(df_countries) <- "countries"
   
   
   ## Match a few remaining countries 
   df_countries$countries <- str_replace_all(df_countries$countries, 
                                             "Micronesia", 
                                             "Micronesia, Federated States of")
   df_countries$countries <- str_replace_all(df_countries$countries, 
                                             "North Korea", 
                                             "Korea, Democratic People's Republic of")
   df_countries$countries <- str_replace_all(df_countries$countries, 
                                             "South Korea", 
                                             "Korea, Republic of")
   
   
   # Create a dataframe of countries with number of measures in each country
   df_countries_n <- df_countries %>% count(countries)
   colnames(df_countries_n) <- c("NAME", "num_measures")
   
  world_spdf@data <- world_spdf@data %>% 
     left_join(df_countries_n, by = "NAME") %>% 
     mutate(num_measures = ifelse(is.na(num_measures), 0, num_measures))
   return(world_spdf)
}

