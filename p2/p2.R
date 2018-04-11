db <- DBI::dbConnect(RSQLite::SQLite(), "lahman2016.sqlite")
DBI::dbListTables(db)
DBI::dbDisconnect(db)

library(dplyr)
library(RSQLite)
library(ggplot2)
library(Lahman)

salaries_tab <- tbl_df(Salaries)

remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)


```{r, eval=TRUE}
efficiency_tab <- std_tab %>%
  mutate(exp_win_percentage = 0.5 + (0.025 * std_payroll)) %>%
  mutate(efficiency = avg_win_percent_over_time - exp_win_percentage) %>%
  
  efficiency_tab %>%
  select(yearID, teamID, efficiency) %>%
  head()

efficiency_tab %>%
  filter(teamID %in% c("OAK", "BOS", "NYA", "ATL", "TBA")) %>%
  ggplot(aes(x=yearID, y=efficiency)) + geom_point() + geom_smooth(method=lm) + facet_grid(.~teamID)
```