library(tidyverse)

source("R/paths.R")


schedule <- read_csv(SCHEDULE_PATH)
teams <- yaml::yaml.load_file(TEAMS) %>%
  as_tibble() %>%
  gather(fantasy_team, bbref_id)
most_recent_snap <- read_csv(max(dir(SNAPSHOT_DIR, full.names = TRUE))) %>%
  left_join(teams)

today <- schedule %>%
  filter(date == Sys.Date() + 1) %>%
  gather(location, team, -1) %>%
  left_join(most_recent_snap, by = "team") %>%
  select(date, player, bbref_id, team, position, FANTASY_STATS)

today %>%
  mutate_if(is.numeric, list(~ scale(.)[, 1])) %>%
  mutate(total_pts = fg_pct + ft_pct + thr + trb + ast + stl + blk + pts) %>%
  inner_join(teams) %>%
  gather(stat, value, fg_pct:total_pts) %>%
  group_by(fantasy_team, stat) %>%
  summarise(value = sum(value)) %>%
  filter(stat != "total_pts") %>%
  ggplot(aes(x = stat, y = value, fill = fantasy_team)) +
  geom_col(position = position_dodge())

most_recent_snap %>%
  filter(!is.na(fantasy_team)) %>%
  group_by(fantasy_team) %>%
  summarise_if(is.numeric, sum) %>%
  select(fantasy_team, FANTASY_STATS) %>%
  gather(stat, value, -1) %>%
  mutate(me = fantasy_team == "uwu") %>%
  ggplot(aes(x = fantasy_team, y = value, color = me)) +
  geom_point() +
  facet_wrap(~stat, scales = "free")


drop_and_sign <- function(cteam, drop, sign, snapshot = most_recent_snap,
                          rotopoints = FALSE) {
  if (!(drop %in% (snapshot %>% filter(fantasy_team == cteam) %>% pull(bbref_id)))) {
    stop("Drop not on team")
  }

  old <- snapshot %>%
    filter(!is.na(fantasy_team)) %>%
    group_by(fantasy_team) %>%
    fantasy_points()

  if (rotopoints) {
    old <- old %>%
      rotopoints()
  }


  new <- snapshot %>%
    filter(bbref_id != drop) %>%
    mutate(fantasy_team = ifelse(bbref_id == sign, cteam, fantasy_team)) %>%
    group_by(fantasy_team) %>%
    filter(!is.na(fantasy_team)) %>%
    fantasy_points()

  if (rotopoints) {
    new <- new %>%
      rotopoints()
  }

  return(bind_rows(old = old, new = new, .id = "scenario"))
}

fantasy_points <- function(snap, rotopoints = FALSE) {
  scenario <- snap %>%
    summarise(
      fg_pct = sum(fg) / sum(fg_att),
      ft_pct = sum(ft) / sum(ft_att),
      thr = sum(thr) / sum(thr_att),
      trb = sum(trb),
      ast = sum(ast),
      stl = sum(stl),
      blk = sum(blk),
      pts = sum(pts),
      N = n()
    )
}

rotopoints <- function(fpoint_df) {
  fpoint_df %>%
    select(-N) %>%
    mutate_at(vars(FANTASY_STATS), rank) %>%
    mutate(total = fg_pct + ft_pct + thr + trb + ast + stl + blk + pts)
}


lookup_player <- function(name) {
  most_recent_snap %>%
    filter(str_starts(str_to_lower(player), name)) %>%
    select(player, bbref_id)
}
lookup_player("dan")
drop_and_sign("uwu", "aldrila01", "sabondo01") %>%
  gather(stat, value, FANTASY_STATS) %>%
  mutate(me = fantasy_team == "uwu") %>%
  ggplot(aes(x = fantasy_team, y = value, shape = fct_rev(scenario), color = me)) +
  geom_point(position = position_dodge(width = 1)) +
  facet_wrap(~stat, scales = "free")

standings <- most_recent_snap %>%
  filter(!is.na(fantasy_team)) %>%
  fantasy_points() %>%
  select(-N) %>%
  mutate_if(is.numeric, rank) %>%
  mutate(total = fg_pct + ft_pct + thr + trb + ast + stl + blk + pts) %>%
  arrange(desc(total))


drop_and_sign("uwu", "aldrila01", "sabondo01", TRUE)

best_drop <- function(cteam, sign, snap = most_recent_snap) {
  roster <- snap %>%
    filter(fantasy_team == cteam) %>%
    pull(bbref_id)

  roster %>%
    map_df(~ drop_and_sign(cteam, .x, sign, rotopoints = T) %>%
      filter(fantasy_team == cteam) %>%
      mutate(sign = sign, drop = .x)) %>%
    arrange(desc(total))
}

best_sign_and_drop <- function(cteam, snap = most_recent_snap) {
  full_roster <- snap %>%
    filter(is.na(fantasy_team)) %>%
    pull(bbref_id)

  full_roster %>%
    map_df(~ best_drop(cteam, .x, snap)) %>%
    arrange(desc(total))
}
