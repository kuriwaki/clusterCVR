library(tidyverse)
library(usethis)


cc18_raw <- read_rds("data-analysis/cces18_vmat.rds")

# format
cc18 <- cc18_raw %>%
  select(case_id,
         st, cd,
         pid, race, educ, newsint, ideo5,
         matches("PRS_party"),
         matches("USS_party"),
         matches("GOV_party"),
         matches("USH_party"),
         matches("ATG_party"),
         matches("SOS_party"),
         ) %>%
  select(-matches("_s$"))

cc18 %>%
  filter(st %in% c("MA", "WI", "MI")) %>%
  select(st, matches("_party"))


# subset
cces18_samp <- cc18 %>%
  filter(st %in% c("MA", "WI", "MI")) %>%
  arrange(cd)

usethis::use_data(cces18_samp, overwrite = TRUE)


