#### Nishant Arora
#### CMSC 320
#### Project 1  

library(rvest)
library(tidyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(dplyr)
# scrape table
dl_tab <- url %>%
  read_html() %>%
  html_node(".table-striped") %>%
  html_table() %>%
  as.data.frame()

# rename cols
names(dl_tab) <- c("rank", "flare_classification", "date", "flare_region", "start_time", "maximum_time", "end_time", "movie")

# show first few rows
head(dl_tab)

# drop last column
dl_tab$movie <- NULL

# combine date and times 
dl_tab <- unite(dl_tab, start_datetime, date, start_time, sep = " ", remove = FALSE)
dl_tab <- unite(dl_tab, max_datetime, date, maximum_time, sep = " ", remove = FALSE)
dl_tab <- unite(dl_tab, end_datetime, date, end_time, sep = " ", remove = FALSE)

# It isn't possible to exactly replicate the table from SpaceWeatherLive from the NASA data
# simply because the NASA data and SpaceWeatherLive data differ a little bit. Specifically, 
# the NASA data is missing a few events that the SWL table has, so naturally, the tables are
# going to be a little different. They do, however, share a significant amount of events as 
# well, meaning that the SWL data can be fairly well replicated, but not completely. 

# remove extra columns
dl_tab$date <- NULL
dl_tab$start_time <- NULL
dl_tab$maximum_time <- NULL
dl_tab$end_time <- NULL

# convert to datetime (POSIX)
dl_tab$start_datetime <- as.POSIXct(dl_tab$start_datetime, tz="")
dl_tab$max_datetime <- as.POSIXct(dl_tab$max_datetime, tz="")
dl_tab$end_datetime <- as.POSIXct(dl_tab$end_datetime, tz="")

View(dl_tab)

#BILLS CODE#

flare_similarities <- function(flare1, flare2) {
  
  date_f1 <- select(flare1, start_datetime)[[1,1]]
  region_f1 <- select(flare1, flare_region)[[1,1]]
  class_f1 <- select(flare1, flare_classification)[[1,1]]
  endDate_f1 <- select(flare1, end_datetime)[[1,1]]
  
  date_f2 <- select(flare2, start_datetime)[[1,1]]
  region_f2 <- select(flare2, flare_region)[[1,1]]
  class_f2 <- select(flare2, flare_classification)[[1,1]]
  endDate_f2 <- select(flare2, end_datetime)[[1,1]]
  
  sim <- as.numeric(min(c(date_f1, date_f2))) / as.numeric(max(c(date_f1, date_f2))) * 100
  sim <- as.numeric(min(c(endDate_f1, endDate_f2))) / as.numeric(max(c(endDate_f1, endDate_f2))) * 100
  
  if (region_f1 == region_f2) {
    sim <- sim + 1
  } 
  
  if (class_f1 == class_f2) {
    sim <- sim + 1
  }
  
  sim <- sim / 4
  
  print(sim)
  
}

flare_match <- function(dt, flare) {
  Print(max(apply(df, 1, flare_similarities(flare1, flare2))))
}

url <- "https://www.spaceweatherlive.com/en/solar-activity/top-50-solar-flares"

nasa <- "https://cdaw.gsfc.nasa.gov/CME_list/radio/waves_type2.html"

#scrape nasa table
nasa_tab <- nasa %>%
  read_html() %>%
  html_node("pre") %>%
  html_text() %>%
  str_split("\\n") %>%
  as.data.frame() %>%
  slice(13:n()-3) %>%
  slice(4:n()) %>%
  separate(1, c("start_date","start_time", "end_date", "end_time", "start_frequency", "end_frequency", "flare_location", "flare_region", "flare_classification", "cme_date", "cme_time", "cme_angle", "cme_width", "cme_speed"), sep="[:space:]+")

#handling mising data
nasa_tab[nasa_tab=="????"] <- NA
nasa_tab[nasa_tab=="BACK"] <- NA
nasa_tab[nasa_tab=="Back"] <- NA
nasa_tab[nasa_tab=="Back?"] <- NA
nasa_tab[nasa_tab=="----"] <- NA
nasa_tab[nasa_tab=="---"] <- NA
nasa_tab[nasa_tab=="------"] <- NA
nasa_tab[nasa_tab=="-----"] <- NA
nasa_tab[nasa_tab=="--:--"] <- NA
nasa_tab[nasa_tab=="--/--"] <- NA
nasa_tab[nasa_tab=="LASCO DATA GAP"] <- NA

#tidying up the cme columns
nasa_tab_tidy <- nasa_tab %>%
  mutate(halo = cme_angle == "Halo") %>%
  mutate(cme_width_limit = cme_width == str_match(cme_width, ">\\d+")) %>%
  mutate(cme_width_limit = !is.na(cme_width_limit)) %>%
  separate(cme_width, c("trash", "cme_width"), sep=">", fill = "left")

#converting dates and times to single datetime columns
nasa_tab_tidy <- unite(nasa_tab_tidy, start_datetime, start_date, start_time, sep = " ", remove = FALSE)
nasa_tab_tidy <- unite(nasa_tab_tidy, end_datetime, end_date, end_time, sep = " ", remove = FALSE)
nasa_tab_tidy <- unite(nasa_tab_tidy, cme_datetime, cme_date, cme_time, sep = " ", remove = FALSE)

#clearing excess columns and getting rid of 'halo'
nasa_tab_tidy$start_date <- NULL
nasa_tab_tidy$start_time <- NULL
nasa_tab_tidy$end_date <- NULL
nasa_tab_tidy$end_time <- NULL
nasa_tab_tidy$cme_date <- NULL
nasa_tab_tidy$cme_time <- NULL
nasa_tab_tidy$trash <- NULL
nasa_tab_tidy[nasa_tab_tidy=="Halo"] <- NA

#grabbing year from start date and adding it on to the others
nasa_tab_tidy <- separate(nasa_tab_tidy, start_datetime, c("temp", "start_datetime"), sep = "/", extra = "merge")

#uniting year to all datetime columns
nasa_tab_tidy <- unite(nasa_tab_tidy, start_datetime, temp, start_datetime, sep = "/", remove = FALSE)
nasa_tab_tidy <- unite(nasa_tab_tidy, end_datetime, temp, end_datetime, sep = "/", remove = FALSE)
nasa_tab_tidy <- unite(nasa_tab_tidy, cme_datetime, temp, cme_datetime, sep = "/", remove = FALSE)

#changing appropriate columns to datetime
nasa_tab_tidy <- mutate(nasa_tab_tidy, start_datetime = ymd_hm(start_datetime))
nasa_tab_tidy <- mutate(nasa_tab_tidy, end_datetime = ymd_hm(end_datetime))
nasa_tab_tidy <- mutate(nasa_tab_tidy, cme_datetime = ymd_hm(cme_datetime))

#type converting
nasa_tab_tidy <- type_convert(nasa_tab_tidy)

#getting top 50
top_fifty <- nasa_tab_tidy %>%
  separate(flare_classification, c("class", "number"), sep=1) %>%
  filter(class=="X") %>%
  type_convert() %>%
  arrange(desc(number)) %>%
  slice(1:50) %>%
  unite(flare_classification, class, number, sep="", remove = FALSE)

View(top_fifty)
View(nasa_tab_tidy)
flare_similarities(slice(nasa_tab_tidy, 4), slice(nasa_tab_tidy, 5))
#flare_match(slice(dl_tab, ))



