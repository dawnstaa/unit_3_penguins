#Amadi 2/22/22 with Erin
#t-tests

library(ggplot2)
library(tidyverse)
library(dplyr)
library(palmerpenguins)
install.packages("rstatix")
library(rstatix)
library(knitr)  # prints pretty tables with RMarkdown
library(GGally)
library(broom)
library(ggiraph)
library(ggiraphExtra)
library(tidyr)
install.packages("car")
library(car)
summary(penguins)

penguins %>% dplyr::filter(sex=="male")
find(filter)

ggplot(data=penguins) +
  geom_histogram(aes(x=body_mass_g, fill=species))

gentoo = penguins %>% 
  filter(species=="Gentoo",
         !is.na(body_mass_g))
mean(gentoo$body_mass_g)
sd(gentoo$body_mass_g)

#or this way: Calculate the mean and standard deviation Gentoo body mass in our data (sometimes base R is more sensible than dplyr)
mean(gentoo$body_mass_g, na.rm=TRUE)
sd(gentoo$body_mass_g, na.rm=TRUE)

# Quickly visualize the body mass data
ggplot() +
  geom_histogram(data=gentoo, aes(x=body_mass_g ))


#QQ plot to checking normality of data (Quantile-Quantile plot)
#A Normal Q-Q plot just shows your data, sorted and plotted along the x axis, 
#versus quantiles from a standard Normal distribution on the y axis.
ggplot() +
  stat_qq(aes(sample=body_mass_g), data=gentoo)

#in base R; one sample t-test
gentoo_body_mass_g_lit = 5500 #from paper xxx et al. 2010
t.test(gentoo$body_mass_g, mu=5500)

#in tidyverse way; one sample t-test
t_test_results = gentoo %>%
  t_test(body_mass_g ~ 1, mu=5500)
kable(t_test_results) # prints results nicely with RMarkdown

#compare gentoo and adelie body mass
data_for_t_test = penguins %>%
  filter(species %in% c("Gentoo", "Adelie"),
         !is.na(body_mass_g))%>%
  select(species, body_mass_g)%>%
  droplevels()# drop what we will not be using
  summary(data_for_t_test)
  tail(data_for_t_test)
  
  
  data_for_t_test%>%
    group_by(species)%>%
    summarise(mean_body_g = mean(body_mass_g),
              sd(body_mass_g)) #not complete
  
ggplot()+
  geom_histogram(data_for_t_test, aes(x=body_mass_g))+
  facet_wrap(~species, scales="free") #use the scales if the 2 plots are on different scales

#t-test are gentoo and adelie body masses significantly different?
data_for_t_test %>%
  levene_test(body_mass_g ~ species, var.equal=TRUE)

t_test_results = data_for_t_test %>%
  t_test(body_mass_g~species)
t-test_results
 

#run correlations on bill length vs bill depth
ggplot() +
  geom_point(data=gentoo, aes(x=bill_length_mm, y=bill_depth_mm))

cor(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm)
cor.test(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm) #testing to see if correlation is significant

cor.test(x=penguins$bill_length_mm, y=penguins$bill_depth_mm) #testing to see if correlation is significant for all penguins

#correlation matrix
head(gentoo)
cor(gentoo[,c(3:6)])# all rows spo the first comma

install.packages("GGally")
library(GGally)
gentoo %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  ggpairs()
#rewriting the above code
gentoo = penguins %>%
  filter(species=="Gentoo") %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)%>%
  ggpairs() #from GGally

penguins%>%
  select(ends_with("mm"), body_mass_g, species)%>%
  ggpairs(aes(color=species))
#NB: correlation does not imply causation

#bill depth as a function of bill length (bill depth ~ bill length)
#a linear model has to be linear in the parameters not the variables

#simple linear regression
penguins %>% 
  select(bill_depth_mm, bill_length_mm) %>%
  ggpairs()

lm1 = lm(bill_depth_mm ~ bill_length_mm, data=penguins)
class(lm1)
summary(lm1)


ggplot(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

class(lm1)
plot(lm1)
#leverage is how much each point contributes to the model
penguins[308,]
head(gentoo)

lm2 = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
lm2
summary(lm2)

gentoo %>%
  select(bill_length_mm, bill_depth_mm)%>%
  ggpairs()

ggplot(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data=penguins) +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_smooth(aes(x=bill_length_mm, y=bill_depth_mm, color=species), method="lm") +
  geom_smooth(aes(x=bill_length_mm, y=bill_depth_mm, method="lm", color="black"))

###03/01/2022
#factor == categorical variable in R

#build models
#build multiple regression
#build model
summary(lm_3)
coef(lm_3)[2] #gives the 2nd element of the vector
lm_3_coefs = broom::tidy(lm_3) # then print out to a csv using write.csv 
#to remind me of where tidy comes from
lm_3_coefs = tidy(lm_3, conf.int=TRUE) %>%
  mutate_if(is.numeric, round,2) #mutate everything in the dataframe if ...(so if numeric, round to 2decimal places)
lm_3_coef

#visualize
ggPredict(lm_3, se=TRUE, interactive = TRUE)#plus standard error (se), allows you to interct with it
#visualse using base R
lm_3_predictions = predict(lm_3, interval="confidence")#plus confidence gives a dataframe
head(lm_3_predictions)
lm_3_predictions = cbind(penguins_lm_3, lm_3_predictions)

ggplot(data = lm_3_predictions) +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_line(aes(x=bill_length_mm, y=fit, color=species)) +
  geom_ribbon(aes(x=bill_length_mm, ymin=lwr, ymax=upr, fill=species), alpha=0.3) +
  theme_bw()# removes the gray background

#give predict a new set of data across which to predict fits for bill depth
newdata_bill_length = seq(from=min(penguins_lm_3$bill_length_mm), to=max(penguins_lm_3$bill_length_mm), by=0.1)
newdata_bill_length
newdata = expand.grid(bill_length_mm = newdata_bill_length, species=unique(penguins_lm_3$species))#unique give me every unique species
head(newdata)
summary(newdata)

newdata_predict_lm_3 = predict(lm_3, interval="confidence", newdata=newdata)
head(newdata_predict_lm_3)
newdata_predict_lm_3 = cbind(newdata, predict(lm_3, interval="confidence", newdata=newdata))
##missed some lines of code in this lesson.....
#visualize
ggplot() +
  geom_line(aes(x=bill_length_mm, y=bill_depth_mm, color=species), data=newdata_predict_lm_3) +
  geom_line(aes(x=bill_length_mm, y=fit, color=species), data=newdata_predict_lm_3) +
  geom_ribbon(aes(x=bill_length_mm, ymin=lwr, ymax=upr, fill=species), alpha=0.3, data=lm_3_predictions)


lm_3_predict_tidy


#03/03/2022
#interaction model
#model: bill depth ~ length * species

lm_4 = lm(bill_depth_mm ~ bill_length_mm * species, data=penguins_lm3)
summary(lm_4)
AIC(lm_3, lm_4)
best_model = step(lm_4)#takes the most complex model which I am interested in
summary(best_model)

#visualize interaction model
ggPredict(lm_4, se=TRUE, interactive=TRUE)

#Generate my own prediction
lm_4_predict = lm_4 %>%
  augment(se_fit=TRUE, interval="confidence")
head(lm_4_predict)

ggplot(data=lm_4_predict) +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_line(aes(x=bill_length_mm, y=.fitted, color=species)) +
  geom_ribbon(aes(x=bill_length_mm, ymin=.lower, ymax=.upper, fill=species), alpha=0.3) +
  theme_bw()

#multiple continuous variables
#for only gentoos
gentoo = penguins_lm3 %>%
  filter(species="Gentoo")
head (gentoo)

lm_1 = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
lm_2 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm, data=gentoo)
lm_3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g, data=gentoo)

summary(lm_3)
vif(lm_3) # in car() package #look for vif below 3 (citation)
best_model_gentoo = step(lm_3) #step function on most complex model
summary(best_model_gentoo)
AIC(lm_1, lm_2, lm_3)

#visualze
newdata1 = gentoo%>%
  select(bill_length_mm) %>%
  mutate(flipper_length_mm = median(gentoo$flipper_length_mm),
         body_mass_g = median(gentoo$body_mass_g))

lm_3_predict = lm_3 %>%
  augment(newdata1)

ggplot(data=lm_3_predict) +
  geom_line(aes(x=bill_length_mm, y=.fitted)) +
  geom_ribbon(aes(x=bill_length, ymin=.lower, ymax=.upper), alpha=0.3) +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm), data=gentoo) +
  annotate("text", x=53, y=13.7,
           paste0("flipper length = ",
                  median(gentoo$flipper_length_mm, na.rm=TRUE), "mm")
#y=17; x=208 (for bill length)
         
#Exercise 5.3    
newdata1 = gentoo%>%
  select(flipper_length_mm) %>%
  mutate(bill_length_mm = median(gentoo$bill_length_mm),
         body_mass_g = median(gentoo$body_mass_g))

lm_3_predict = lm_3 %>%
  augment(newdata= newdat1)


#ANOVA
penguin_lm = lm()
summary(penguin_lm)
anova(penguin_lm) #or use aov
penguin_aov = aov(body_mass_g ~ sex + species, data=penguin)
summary(penguin_aov) 

penguins %>%
  group_by(sex) %>%
  summarize(mean=mean(body_mass_g))
TukeyHSD(penguin)