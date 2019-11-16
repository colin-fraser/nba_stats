# Gets a snapshot of the season stats to date from basketball reference.


library(tidyverse)
library(httr)
library(rvest)

URL <- "https://www.basketball-reference.com/leagues/NBA_2020_totals.html"
COLUMN_MAP <- list(
  'Rk' = 'rank',
  'Player' = 'player',
  'Pos' = 'position',
  'Age' = 'age',
  'Tm' = 'team',
  'G' = 'games',
  'GS' = 'games_started',
  'MP' = 'minutes_played',
  'FG' = 'fg',
  'FGA' = 'fg_att',
  'FG%' = 'fg_pct',
  '3P' = 'thr',
  '3PA' = 'thr_att',
  '3P%' = 'thr_pct',
  '2P' = 'two',
  '2PA' = 'two_att',
  '2P%' = 'two_pct',
  'eFG%' = 'eff_fg',
  'FT' = 'ft',
  'FTA' = 'ft_att',
  'FT%' = 'ft_pct',
  'ORB' = 'orb',
  'DRB' = 'drb',
  'TRB' = 'trb',
  'AST' = 'ast',
  'STL' = 'stl',
  'BLK' = 'blk',
  'TOV' = 'tov',
  'PF' = 'fouls',
  'PTS' = 'pts'
)


DATA_DIR <- path.expand('~/data/nba/snapshots')
FILE_NAME <- 'stat_snapshot_{date}.csv'


# check if snapshot exists ------------------------------------------------

if (!dir.exists(DATA_DIR)) {
  dir.create(DATA_DIR, recursive = TRUE)
}
file_name <- str_glue(FILE_NAME, date = as.character(Sys.Date()))
file_path <- file.path(DATA_DIR, file_name)
if (file.exists(file_path)) {
  stop("Snapshot exists")
}


# get table ----------------------------------------------------------------

html <- read_html(URL)
raw_table <- html %>% 
  html_node("table") %>% 
  html_table() %>% 
  as_tibble()

player_ids <- html %>% 
  html_node("table") %>% 
  html_nodes("th+ .left a") %>% 
  html_attr("href") %>% 
  str_sub(12, -6)

# process table -----------------------------------------------------------

stats <- raw_table %>% 
  filter(Player != 'Player')

## check that names haven't changed

if(!all(colnames(stats) == names(COLUMN_MAP))) {
  stop("Column names from bbref have changed.")
}

colnames(stats) = unlist(COLUMN_MAP)

stats <- stats %>% 
  mutate(bbref_id = player_ids) %>% 
  mutate_at(vars(age, games:pts), as.numeric)


# save --------------------------------------------------------------------
write_csv(stats, file_path)