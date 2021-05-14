library(mlogit)
library(microbenchmark)
library(dplyr)
library(stringr)
library(emlogit)

data("ModeCanada", package = "mlogit")

# samp. complete cases
mc_dfidx <- dfidx(ModeCanada,
                  subset = (noalt == 4),
                  chid.var = "case",
                  alt.var = "alt")


# intercept only
# dfidx
ml_MC1a <- mlogit(choice ~ 1,
                  data = MC,
                  weights = income,
                  alt.subset = c("car", "train", "air"),
                  reflevel = "air")
ml_MC1b <- mlogit(choice ~ 1,
                  data = ModeCanada,
                  idx = c("case", "alt"),
                  weights = income,
                  alt.subset = c("car", "train", "air"),
                  reflevel = "air")
summary(ml_MC1a)
summary(ml_MC1b)


# MC2 - alternative specific
# the first part in RHS is alt-specific but generic,
# the second part is chice situation specific,
# and the third part is alternative-specific
ml_MC2a <- mlogit(choice ~ cost | 0 | ivt,
                  weights = income,
                  data = MC)

# 272 ms with choice ~ cost | 0 | ivt; w = income
# 464 ms with choice ~ cost | income
# 567 ms with choice ~ cost | income | ivt; w = income
ml_MC2b <- mlogit(choice ~ cost | income,
                  weights = income,
                  data = ModeCanada,
                  idx = c("case", "alt"))
summary(ml_MC2a)
summary(ml_MC2b)



# sample data
GOV_inc <-  tribble(
  ~alt, ~chid, ~inc,
  "R", "MA", 1,
  "D", "MA", 0,
  "R", "RI", 0,
  "D", "RI", 1,
  "R", "NY", 0,
  "D", "NY", 0,
  "R", "CT", 0,
  "D", "CT", 0,
)


