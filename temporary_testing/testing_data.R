
library(magrittr)
library(rlang)
library(dplyr); library(tidyr); library(stringr); library(stats)
library(tibble)

chess_players <- tibble(
  code = c("E13455", "E75354", "E95301", "E4747906", "E50345", "E93901")
  ,club = c("Hammersmith", "Westminster", "Smethwick", "Salford", "Walsall", "Exeter")
  ,rating = c(1960, 1835, 1907, 2002, 1641, 1873)
  ,score = c(3, 2.5, 3.5, 2.5, 1.5, 2)
)

expected_players <- c("E13455", "E75354", "E95301", "E47906", "E54592", "E50345", "E93901")

# use one of these
chl_i1 <- expand_grid(code = expected_players, data_id = c("club","rating","score"))
chw_i1 <- expand_grid(code = expected_players)

chess_players_long <- chess_players %>%
  pivot_longer(-code, names_to = "data_id", values_to = "value",
               values_transform = as.character)
# use one of these
chl_i1 <- chl_i1 %>%
  left_join(chess_players_long, by = c("code", "data_id"))
chw_i1 <- chw_i1 %>%
  left_join(chess_players, by = "code")


