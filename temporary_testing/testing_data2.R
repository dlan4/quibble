
library(magrittr)
library(stringr)
data("fruit")

rescale_to <- function(vec, cond, to) {

  cond_e <- substitute(cond, list(. = quote(vec)) )
  rescale_vals <- vec[ eval(cond_e) ]
  ratios <- (rescale_vals - min(rescale_vals)) /
    ( max(rescale_vals) - min(rescale_vals) )
  ratios_scaled <- (max(to) - min(to)) * ratios
  rescaled_final <- min(to) + ratios_scaled
  vec[ eval(cond_e) ] <- rescaled_final
  return(vec)
}

fruit_costs <- rnorm( length(fruit),
                      mean = 1, sd = 5) %>%
  sort %>%
  rescale_to(. <= 0.05, c(0.05, 1)) %>%
  round(2) %>% tibble::tibble(a = .) %>%
  ggplot2::ggplot(aes(x = a)) +
  geom_density() + theme_minimal() +
  scale_x_continuous(breaks = 0:4)
  tibble::tibble(fruit = fruit, cost = .)


letters[1:26] %>%
  tidyr::expand_grid(x = ., y = .) %>%
  .[.$x != .$y, ] %>%
  purrr::pmap_chr(paste0) %>%
  tidyr::expand_grid(geo_id = ., fruit = fruit ) %>%
  dplyr::left_join(fruit_costs, by = "fruit") %>%
  mutate
