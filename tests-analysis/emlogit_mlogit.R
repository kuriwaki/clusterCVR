library(tidyverse)
library(mlogit)
library(emlogit)

data(ModeCanada)

# transform ModeCanda data from mlogit package
wide_MC <- ModeCanada %>%
  as_tibble() %>%
  filter(noalt == 4) %>%
  select(case, alt, choice, income) %>%
  pivot_wider(id_cols = c(income, case),
              names_from = alt,
              values_from = choice)

long_MC <- mlogit.data(ModeCanada,
                  subset = (noalt == 4),
                  chid.var = "case",
                  alt.var = "alt", drop.index = TRUE)


# fit models
fit_em <- emlogit(Y = as.matrix(select(wide_MC, train:car)),
                  X = as.matrix(select(wide_MC, income)))

fit_mlogit <- mlogit(choice ~ 1 | income,
                     data = long_MC,
                     reflevel = "train")

# compare coefficients
summary(fit_em)
summary(fit_mlogit)

# predictions
summary(predict(fit_em))
summary(predict(fit_mlogit, newdata = long_MC))


