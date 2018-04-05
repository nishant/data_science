library(tidyverse)
library(rvest)
library(stringr)

# URL base for search
base_url <- "https://www.rottentomatoes.com/celebrity/"

# let's see how this works for Diego Luna

# scrape the table from the website
dl_url <- paste0(base_url, "will_smith")
dl_html <- read_html(dl_url) 
dl_tab <-  dl_html %>%
  html_node("#filmographyTbl") %>%
  html_table() %>%
  as_tibble()

# clean it up
clean_dl_tab <- dl_tab %>% 
  # make sure the movie is rated
  filter(RATING != "No Score Yet") %>% 
  
  # make the rating look numeric
  mutate(RATING = str_replace(RATING, "%", "")) %>%
  
  # remove producer and director credits
  filter(!str_detect(CREDIT, "Prod") &
           !str_detect(CREDIT, "Dir")) %>%
  
  # convert to proper types
  readr::type_convert()

clean_dl_tab %>% head(7) %>% knitr::kable()


budget_filename <- "tmdb_5000_movies.csv.zip"

budget_tab <- read_csv(budget_filename, col_types="n-----c-n--Dn---cc--")
# clean up the result
clean_budget_tab <- budget_tab %>%
  # represent budget and gross in millions
  mutate_at(vars("budget", "revenue"), funs(. / 1e6))

clean_budget_tab %>% head(10) %>% knitr::kable()

joined_tab <- clean_dl_tab %>%
  # join the two tables together
  inner_join(clean_budget_tab, by=c(TITLE="title")) 

joined_tab %>% knitr::kable()

joined_tab %>%
  ggplot() +
  theme_bw() +
  aes(x=RATING, y=revenue, label=original_title) +
  geom_point() + geom_text(aes(label=original_title),hjust=1, vjust=1) +
  labs(title="Will Smith's movies",
       x="Rotten Tomato Rating",
       y="Domestic gross (Millions)")
