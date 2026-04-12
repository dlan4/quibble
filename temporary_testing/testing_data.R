
#  packages <- rlang::exprs("magrittr", "tidyverse", "rlang")
#  purrr::walk(packages, \(p) eval(rlang::expr(library(!!p)) ) )

chess_players <- tibble(
  code = rep(c("E13455", "E75354", "E95301", "E4747906", "E50345", "E93901"), each=2)
  ,club = rep(c("Hammersmith", "Westminster", "Smethwick", "Salford", "Walsall", "Exeter"), each=2)
  ,body = rep(c("ecf","fide"), times=6)
  ,rating = c(1960, 1835, 1907, 2002, 1641, 1873
              ,1701, NA, 1815, 1856, 1459, 1603)
  ,score = rep(c(3, 2.5, 3.5, 2.5, 1.5, 2), each=2)
)

expected_players <- c("E13455", "E75354", "E95301", "E47906", "E54592", "E50345", "E93901")

# use one of these
chl_i1 <- expand_grid(code = expected_players, data_id = c("club","rating","score"),
                      body = c("ecf", "fide"))
chw_i1 <- expand_grid(code = expected_players)

chess_players_long <- chess_players %>%
  pivot_longer(-c(code, body), names_to = "data_id", values_to = "value",
               values_transform = as.character)
# use one of these
chl_i1 <- chl_i1 %>%
  left_join(chess_players_long, by = c("code", "data_id", "body"))
chw_i1 <- chw_i1 %>%
  left_join(chess_players, by = c("code") )


edits <- tibble::tibble(
  code = c("E75354", "E47906")
  ,data_id = c("c", "club")
  ,body = c("fide", ".")
  ,value = c("NULL", "Switzerland")
)
keys <- c("code", "data_id", "body")
.x = track(chl_i1, keys = keys)

data = .x$init_1
edits = edits
na_alias = "NULL"

merged_tree <- .x %>%
  evolve(edited1 = init_1 %>% edit_data(edits, na_alias = "NULL"),
         edited2 = .tree$init_1 %>% edit_data(edits, na_alias = "NULL"),
         edited3 = dplyr::bind_rows(init_1, tibble::tibble(code = c("E54500","E54500"), data_id=c("club","rating"),
                                                   body = c("ecf","ecf"), value=c("Battersea",1400)) ),
         edited_merge = merge_branches(edited1, edited3) )
plot(merged_tree)
class(merged_tree)
history_all(merged_tree, value)


