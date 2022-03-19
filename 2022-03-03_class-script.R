# multiple regression

library(palmerpenguins)
library(tidyverse)
library(ggiraph)
library(ggiraphExtra)
library(car) # vif()
library(broom)
library(tidyr)


penguins_lm3 = penguins %>%
  filter(!is.na(bill_depth_mm))

lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data=penguins_lm3)
summary(lm_3)

# interaction model
lm_4 = lm(bill_depth_mm ~ bill_length_mm * species, data=penguins_lm3)
summary(lm_4)

AIC(lm_3,lm_4)
# step function
bm = step(lm_4)
summary(bm)

# visualize interaction model
ggPredict(lm_4, se=TRUE, interactive=TRUE)

# visalize
lm_4_predict = lm_4 %>%
  augment(se_fit=TRUE, interval="confidence")
head(lm_4_predict)

ggplot(data=lm_4_predict) +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_line(aes(x=bill_length_mm, y=.fitted, color=species)) +
  geom_ribbon(aes(x=bill_length_mm, ymin=.lower, ymax=.upper, fill=species),alpha=0.3) + 
  theme_bw()

# multiple continuous variables
gentoo = penguins_lm3 %>%
  filter(species == "Gentoo")
head(gentoo)

lm_1 = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
lm_2 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm, data=gentoo)
lm_3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g, data=gentoo)

summary(lm_3)
vif(lm_3)  # in car() package # look for vif below 3 (Miller et al. 2010)

bm = step(lm_3)
summary(bm)
AIC(lm_1, lm_2, lm_3)

# visualize
newdata = gentoo %>%
  select(bill_length_mm) %>%
  mutate(flipper_length_mm = median(gentoo$flipper_length_mm),
         body_mass_g = median(gentoo$body_mass_g))
head(newdata)

lm_3_predict = lm_3 %>%
  augment(newdata=newdata, interval="confidence")
head(lm_3_predict)

ggplot(data=lm_3_predict) +
  geom_line(aes(x=bill_length_mm, y=.fitted)) +
  geom_ribbon(aes(x=bill_length_mm, ymin=.lower, ymax=.upper), alpha=0.3) +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm), data=gentoo) +
  annotate("text", x=53, y=13.7, label=paste0("flipper length=", median(gentoo$flipper_length_mm)))

# Ex 5.3
# visualize
newdata = gentoo %>%
  select(flipper_length_mm) %>%
  mutate(bill_length_mm = median(gentoo$bill_length_mm),
         body_mass_g = median(gentoo$body_mass_g))
head(newdata)

lm_3_predict = lm_3 %>%
  augment(newdata=newdata, interval="confidence")
head(lm_3_predict)

ggplot(data=lm_3_predict) +
  geom_line(aes(x=flipper_length_mm, y=.fitted)) +
  geom_ribbon(aes(x=flipper_length_mm, ymin=.lower, ymax=.upper), alpha=0.3) +
  geom_point(aes(x=flipper_length_mm, y=bill_depth_mm), data=gentoo) +
  annotate("text", x=208, y=17, label=paste0("bill length=", median(gentoo$bill_length_mm)))

# ANOVA
penguin_lm = lm(body_mass_g ~ species + sex, data=penguins)
summary(penguin_lm)
anova(penguin_lm)

penguin_aov = aov(body_mass_g ~ sex + species, data=penguins)
summary(penguin_aov)

penguins %>%
  group_by(sex) %>%
  summarize(mean=mean(body_mass_g))
TukeyHSD(penguin_aov)









