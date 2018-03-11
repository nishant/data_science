### Nishant Arora
### Project 1
### CMSC 320

library(rvest)

url <- "https://www.spaceweatherlive.com/en/solar-activity/top-50-solar-flares"

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
dl_tab <- tidyr::unite(dl_tab, start_datetime, date, start_time, sep = " ", remove = FALSE)
dl_tab <- tidyr::unite(dl_tab, max_datetime, date, maximum_time, sep = " ", remove = FALSE)
dl_tab <- tidyr::unite(dl_tab, end_datetime, date, end_time, sep = " ", remove = FALSE)

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