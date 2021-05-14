library(Rmixmod)
data(birds)
head(birds)

out_mm <- mixmodCluster(birds, 2, seed = 2408)
plot(out_mm)

barplot(out_mm)


cc_chr <- cces18_samp %>%
  select(matches("_party")) %>%
  select(-PRS_party) %>%
  na.omit() %>%
  mutate_all(~factor(.x, levels = c("D", "R", "A"))) %>%
  as.data.frame()

str(cc_chr)


out_cc <- mixmodCluster(cc_chr, 5, seed = 02138,)
barplot(out_cc)
