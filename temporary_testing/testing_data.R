
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
record = c("code" = "E54500", data_id = "club", body = "ecf")
plot(merged_tree)
class(merged_tree)
history_all(merged_tree, value, diffs = TRUE) %>%
  filter(.is_different)
plot(merged_tree, record = c("code" = "E54500", "data_id" = "rating", "body" = "ecf"))

second_data <- track(tibble::tibble(geo_id = rep(c("A123","A435","A56","B413","B350","C403","C406"),each=5),
                      data_id = rep(c("staff_exp","travel_exp","training_exp","sales_inc","services_inc"),times=7),
                      value = c(105,303,207,126,253,
                                204,100,202,303,297,
                                186,104,117,205,213,
                                192,181,0,106,52,
                                120,55,175,46,58,
                                136,108,71,94,207,
                                120,125,45,170,159)),
                     keys = c("geo_id", "data_id") )
second_data %>% plot(record = c("A123", "travel_exp") )

names(c(5, 12) %>% setNames(c("a", "b") ))


