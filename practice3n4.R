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
  geom_smooth(aes(x=bill_length_mm, y=bill_depth_mm, color=species), method = "lm",) +
  geom_smooth(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm, method = "lm", color="black"))


  
  