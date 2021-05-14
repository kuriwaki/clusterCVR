library(mlogit)

# mlogit replication -------
ModeCanada2 <- as_tibble(ModeCanada) %>%
  select(case, alt, choice)

# complete cases matrix
NL_df <- complete(ModeCanada2, case, alt) %>%
  mutate(m = !is.na(choice)) %>%
  arrange(case, alt)

# missingness
canada_mlogit_m <- NL_df %>%
  pivot_wider(id_cols = case, names_from = alt, values_from = m) %>%
  select(-case) %>%
  mutate_if(is.logical, as.integer) %>%
  as.matrix()

# y
canada_mlogit_y <- NL_df %>%
  filter(choice == 1) %>%
  mutate(y = as.integer(alt) - 1) %>%
  pull(y)

# zeta (normalized to mean 1?)
canada_mlogit_w <- ModeCanada %>%
  distinct(case, dist) %>%
  pull(dist)


usethis::use_data(canada_mlogit_y, overwrite = TRUE)
usethis::use_data(canada_mlogit_w, overwrite = TRUE)
usethis::use_data(canada_mlogit_m, overwrite = TRUE)
