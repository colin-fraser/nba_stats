library(tidyverse)
library(rvest)
library(lubridate)

URL <- "https://www.basketball-reference.com/leagues/NBA_2020_games-{month}.html"
source("R/paths.R")

get_schedule <- function(month) {
  url <- str_glue(URL, month = month)

  raw_table <- read_html(url) %>%
    html_node("table")

  game_info <- raw_table %>%
    html_nodes(".left:nth-child(3)") %>%
    html_attr("csk")

  visitor <- str_sub(game_info, 1, 3)
  home <- str_sub(game_info, -3)
  dates <- str_sub(game_info, 5, 12) %>%
    ymd()
  schedule <- tibble(
    date = dates,
    visitor = visitor,
    home = home
  )

  schedule
}

schedule <- month.name %>%
  str_to_lower() %>%
  .[c(10:12, 1:4)] %>%
  map_df(get_schedule)

if (!dir.exists(dirname(SCHEDULE_PATH))) {
  dir.create(dirname(SCHEDULE_PATH), recursive = TRUE)
}

write_csv(schedule, SCHEDULE_PATH)
