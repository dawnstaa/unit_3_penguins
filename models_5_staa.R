#Amadi_linear models_03/05/2022 

library(tidyverse)
library(palmerpenguins)
library(GGally) # ggPairs()
library(ggiraph)
library(ggiraphExtra) # ggPredict()
library(broom)  # tidy() augment() #does NOT load with tidyverse
library(car) # vif()


# Exploratory data analysis:
glimpse(penguins)
summarize(penguins)


# visual correlation matrix for all continuous variables
all_pens = penguins %>%
  filter(!is.na(sex)) %>%
  select(species, body_mass_g, ends_with("_mm")) %>% # select variables with names that end in "_mm"
  GGally::ggpairs(aes(color=species))

summary(all_pens)

# visual correlation matrix for bill depth and bill length
penguins %>% 
  select(bill_depth_mm, bill_length_mm) %>%
  GGally::ggpairs()  # calling out the library can avoid ambiguity for common-named functions, or just serve as a reminder to you

#linear model
lm_1 = lm(bill_depth_mm ~ bill_length_mm, data=penguins)
summary(lm_1)

#plotting data and linear models
ggplot(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm))+
  geom_point()+
  geom_smooth(method = "lm") #adds CIs

#checking some assumptions of the linear model by saving to plot function
class(lm_1) # Note that your model output is a variable of the class "lm"
plot(lm_1)  # This actually calls plot.lm() since the first parameter is class "lm"


### Another simple linear model this time looking at one sp instead of all
gentoo = penguins %>%
  filter(species == "Gentoo")

gentoo %>% 
  select(bill_depth_mm, bill_length_mm)%>%
  GGally::ggpairs()

lm_2 = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
summary(lm_2)
#plotting
ggplot(aes(x=bill_length_mm, y=bill_depth_mm), data=gentoo)+
  geom_point()+
  geom_smooth(method = "lm")

#examining separate linear models using geom_smooth
ggplot(data=penguins)+
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species))+
  geom_smooth(aes(x=bill_length_mm, y=bill_depth_mm, color=species),
              method="lm")+
  geom_smooth(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm),
              method = "lm", color="black")

  
#Exercise 5.1 linear model predicting Gentoo bill depth as a function of flipper length
gentoo %>% 
  select(bill_depth_mm, flipper_length_mm)%>%
  GGally::ggpairs()
lm_2_1 = lm(bill_depth_mm ~ flipper_length_mm, data=gentoo)
summary(lm_2_1)
#flipper length does a better job of predicting bill depth than bill lenghth
#with a strong positive correlation of 0.707 which is significant with 
#a p-value of < 2.2x10^-16


#model will estimate a single slope associated with bill length and a different 
#intercept for each species
penguins_lm_3 = penguins %>%
  filter(!is.na(species), !is.na(bill_depth_mm),!is.na(bill_length_mm))
summary(penguins_lm_3)
lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data=penguins_lm_3)
summary(lm_3)
coef(lm_3) # vector of coefficients
coef(lm_3)[1] # subset the y-intercept
anova(lm_3) # Analysis of variance tables (typically used when all predictors are categorical)


broom::tidy(lm_3)  # ?tidy.lm
broom::tidy(lm_3, conf.int = TRUE, conf.level = 0.95) # Added confidence intervals to output
broom::tidy(lm_3, conf.int = TRUE, conf.level = 0.95) %>%
  mutate_if(is.numeric, round, 2) # round to 2 decimals


#plotting the data, the linear model predictions and the standard errors on those predictions.
ggPredict(lm_3, se=TRUE, interactive=TRUE)

#more formal and more customizable method for accessing your model predictions is using the `predict()` function in base R
lm_3_predictions = predict(lm_3, interval="confidence")# Calculates lm predictions for the original dataset
head(lm_3_predictions)
head(penguins_lm_3)
penguins_lm_3_predict=cbind(penguins_lm_3, lm_3_predictions)
head(penguins_lm_3_predict)

ggplot(penguins_lm_3_predict, aes(x=bill_length_mm, y=bill_depth_mm, color=species))+
  geom_point()+
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=species, color=NULL), alpha=.1)+
  geom_line(aes(y=fit), size=0.5)+
  theme_bw()

#make model prediction lines using the entire range of bill length 
#instead of the within-species range
# Build a new bill_length_mm dataset that spans the full range of the original data at even intervals
newdata_bill_length_mm = seq(min(penguins_lm_3$bill_length_mm),
                             max(penguins_lm_3$bill_length_mm), by=.1)
head(newdata_bill_length_mm)
# Repeat complete bill_length_mm data for each species
newdata = expand.grid(bill_length_mm = newdata_bill_length_mm,
                      species = unique(penguins_lm_3$species) ) # data.frame names must exactly match lm variables
head(newdata)



