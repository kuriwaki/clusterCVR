library(mlogit)
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

exp(coef(ml_MC1a))

# sample avg
MC_long <- as_tibble(MC, rownames = "var") %>%
  mutate(alt = str_extract(var, "[a-z]+"),
         pid = str_extract(var, "[0-9]+"))
bus_pid <- distinct(filter(MC_long, alt == "bus", choice == 1), pid)

g3_means <- MC_long %>%
  anti_join(bus_pid) %>%
  group_by(alt) %>%
  filter(alt %in% c("car", "train", "air")) %>%
  summarize(mean = weighted.mean(choice, income))

y1 <- g3_means$mean[2]
y2 <- g3_means$mean[3]


# this estimate
y2 / ((1 - y1)*(1 - y2) - y1*y2) # train
y1 / ((1 - y1)*(1 - y2) - y1*y2) # car

# should match
exp(coef(ml.MC1))

# emlogit
ml.MC1
head(MC)

wide_MC <- ModeCanada %>%
  as_tibble() %>%
  filter(noalt == 4) %>%
  select(case, alt, choice, income) %>%
  pivot_wider(id_cols = c(income, case),
              names_from = alt,
              values_from = choice)

fit_em <- emlogit(Y = wide_MC %>% select(train:car) %>% as.matrix(),
                  X = wide_MC %>% select(income) %>% as.matrix())
summary(fit_em)
summary(predict(fit_em))

fit_mlogit <- mlogit(choice ~ 1 | income,
                     data = MC,
                     alt.subset = c("car", "train", "air"),
                     reflevel = "air")
summary(fit_mlogit)
summary(predict(fit_mlogit, newdata = MC))


# lot of varying choice sets
ModeCanada2 <- as_tibble(ModeCanada) %>%
  count(case) %>%
  filter(n == 2) %>%
  inner_join(as_tibble(ModeCanada)) %>%
  as.data.frame()

MC2 <- mlogit.data(ModeCanada2, subset = (noalt == 2), chid.var = "case",
                   alt.var = "alt", drop.index = TRUE)

ml.MC2 <- mlogit(choice ~ 1,
                 data = MC2,
                 weights = income,
                 alt.subset = c("car", "train", "air"),
                 reflevel = "air")
exp(coef(ml.MC2))

# sample avg
MC2_long <- as_tibble(MC2, rownames = "var") %>%
  mutate(alt = str_extract(var, "[a-z]+"),
         pid = str_extract(var, "[0-9]+"))

g3_means2 <- MC2_long %>%
  group_by(alt) %>%
  filter(alt %in% c("car", "train", "air")) %>%
  summarize(mean = weighted.mean(choice, income))

y1 <- g3_means2$mean[2]
y2 <- g3_means2$mean[3]

# this estimate
y2 / ((1 - y1)*(1 - y2) - y1*y2) # train
y1 / ((1 - y1)*(1 - y2) - y1*y2) # car
exp(coef(ml.MC2))


# Soichiro emlogit


# inverse of softmax function
# I have the "sigma" (maybe), so let's back out the beta

# write the likelihood function
# try to use wants count

# derivative of e to the alpha

# take log of mu_jl = 1(M_j == 1)* + 1(M_j == 2) + .....
# and then the derivative

# do it office by office, -- independence
