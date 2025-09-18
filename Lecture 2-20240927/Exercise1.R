#install.packages("nycflights13")
library(nycflights13)
library(tidyverse)
View(flights)

flights %>%
  fliter(dest == "LAX" | dest == "SFO"| dest == "ORD" | dest == "LGB") %>%
  select(year, month, day, arr_delay, origin, dest, dep_time) %>% 
  arrange(year, month, day, arr_delay)

flights |> 
  group_by(dest) |> 
  tally() #tally有看答案

flights |>
  count(origin, dest, sort = TRUE) #count有看答案

flights |> 
  mutate(speed = distance / air_time * 60) |> 
  rename(YEAR = year) -> flight2 #rename有看答案

flight2 |> 
  group_by(month) |> 
  avg_speed <- mean(speed) |> 
  count(flight2) #我不會

flight2 |> 
  group_by(month) |> 
  summarize(
    avg_speed = mean(speed, na.rm=T),
    n = n()
  )

