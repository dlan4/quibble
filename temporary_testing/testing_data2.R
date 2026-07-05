
library(magrittr)
library(stringr)
data("fruit")
library(rlang)
rescale_to <- function(vec, cond, to) {
  cond <- rlang::enexpr(cond)
  cond <<- cond
  #cond_e <- eval_tidy(cond, data = list(.v = vec))
  cond_e <- eval( do.call(substitute, list(cond, list(.v = vec))))
  #cond_e <- replace_expr(cond, replace = ".v", replacement = "vec")
  rescale_vals <- vec[ cond_e ]
  ratios <- (rescale_vals - min(rescale_vals)) /
    ( max(rescale_vals) - min(rescale_vals) )
  ratios_scaled <- (max(to) - min(to)) * ratios
  rescaled_final <- min(to) + ratios_scaled
  vec[ cond_e ] <- rescaled_final
  return(vec)
}

fruit_costs <- rnorm( length(fruit),
                      mean = 1, sd = 5) %>%
  sort %>%
  rescale_to(cond = .v <= 0.05, c(0.05, 1)) %>%
  round(2) %>% tibble::tibble(fruit = fruit, cost = .)


shop_costs <- letters[1:26] %>%
  tidyr::expand_grid(x = ., y = .) %>%
  .[.$x != .$y, ] %>%
  purrr::pmap_chr(paste0) %>%
  tidyr::expand_grid(geo_id = ., fruit = fruit ) %>%
  dplyr::left_join(fruit_costs, by = "fruit") %>%
  dplyr::group_by(geo_id) %>%
  dplyr::mutate(multiplier = rnorm(1, 1, 0.2) ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(status = "Open", .after = fruit) %>%
  dplyr::mutate(multiplier = rescale_to(multiplier, TRUE, c(3/5,5/3)),
                cost_at_shop = round(cost * multiplier, 2))
into_bucket <- function(vec, width) {
  floor(vec / width) * width
}

edits <- shop_costs %>%
  dplyr::mutate(
    cost_at_shop = dplyr::case_when(
    multiplier > 1.6 ~ cost * 1.6,
    TRUE ~ NA
  ),status = dplyr::case_when(
    multiplier < 0.65 ~ "Closed",
    TRUE ~ "Open")
  ) %>%
  dplyr::filter( !is.na(cost_at_shop) | status == "Closed") %>%
  split(~status)
edits$Closed <- edits$Closed %>%
  dplyr::group_by(geo_id) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(.keep = "none",
    geo_id = geo_id, fruit = ".", status = "Closed")
edits <- purrr::list_rbind(edits)

x = shop_costs %>% dplyr::relocate(geo_id, .after = fruit)
edits = edits[c("geo_id","fruit","status","cost_at_shop")]
y = edits

std_overlay_otp <- overlay(x, y, by = c("geo_id", "fruit"))

timings <- new.env()
add_timing <- function(name, tim) {
  tim <<- rlang::enquo(tim)
  timings[[name]] <- microbenchmark::microbenchmark(
    list = rlang::list2(!!name := tim, control = std_overlay_otp) , times=5)
  eval_tidy(tim)
}
add_timing("reduce", overlay(x, y, by = c("geo_id", "fruit")) )
as.list(timings)
microbenchmark::microbenchmark(
  for_loop = overlay(x, y, by = c("geo_id", "fruit")),
  times = 50
)

track(init = shop_costs)

y = tibble(geo_id = ".", fruit = "^black", status = "Removed",
           cost_at_shop = NA)

quibble::track(init = x, keys = c("geo_id", "fruit")) %>%
  quibble::stage(edits1 = quibble::overlay(init, .env$y, by = quibble::get_keys(.))) %>%
  history_all(values = status, diffs = TRUE)

