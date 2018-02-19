# Fortnite Weapons Project

library(tidyverse)

weapons_tab <- read_csv("/Users/nishant/Desktop/data_science/fortnite/weapons.csv")
View(weapons_tab)

# plot dmg vs DPS to see what overall best weapon is, group by type