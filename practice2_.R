#Amadi 02/17/2022 ggplot2_with Erin
library(ggplot2)
library(tidyverse)
library(dplyr)
library(palmerpenguins)

head(penguins)
glimpse(penguins)
summary(penguins)
tail(penguins)

find ('filter')# to find out the kind of function u r using
stats::filter #specifying that you are using the stats version of filter
my_data = penguins %>%
  dplyr::select(sex, species)

species_islands = penguins %>%
  dplyr::select(species, island)#using the dplyr version of select()

glimpse(penguins)


#scatter plot (add layers with a "+")
ggplot(penguins) +
  geom_point(aes(x=flipper_length_mm, y=body_mass_g))

summary(penguins) #shows 11 NAs in data

penguins%>%
  filter(is.na(body_mass_g)) %>% #filtering to keep only NAs
  summarise(number_of_NAs = n())#count NAs

ggplot(penguins) +
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species))
#adding a smoother line to plot
ggplot(penguins) +
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species)) +
  geom_smooth(aes(x=flipper_length_mm, y=body_mass_g))
#adding to and customising plot
ggplot(data=penguins) + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species)) + 
  xlab("Flipper length (mm)")+
  ylab("Body mass (g)") +
  ggtitle("The silent cartoon penguin")

#Exercise 2.1 bill depth vs bill length, map point colors to island
adelie_plot = (penguins)%>%
  filter(species =="Adelie")%>%
  filter(!is.na(bill_length_mm))
#plotting
  ggplot(adelie_plot) + 
  geom_point(aes(x=bill_depth_mm, y=bill_length_mm, color=island)) + 
  xlab("Bill depth (mm)")+
  ylab("Bill length (g)") +
  ggtitle("The silent cartoon penguin")


#number of observations per year
penguin_ys = penguins %>%
  group_by(species, year)%>%
  summarize(num_obs_pergrp = n())

ggplot(data=penguin_ys) +
  geom_line(aes(x=year, y=num_obs_pergrp, colour=species))
    
#histogram
ggplot(data=penguins) +
  geom_histogram(aes(flipper_length_mm))

ggplot(penguins%>%filter(species =="Gentoo")) + 
  geom_histogram(aes(x=flipper_length_mm), binwidth = 5)

#bin position is 'stack' by default; "identity" so that bins for each species can overlap each other
#the alpha value makes it a bit transparent so overlapping bars can be seen
ggplot(data=penguins) +
  geom_histogram(aes(x = flipper_length_mm, fill = species), alpha = 0.5,
                 binwidth=5, position="identity") +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4"))

#box plots and jittered plots
ggplot(penguins) + 
  geom_boxplot(aes(y=flipper_length_mm, x=species)) +
  geom_jitter(aes(y=flipper_length_mm, x=species, color=species),
              width = 0.2)


#bar charts
#number of observations of each sex for each species
ggplot(data = penguins) +
  geom_bar(aes(x = sex, fill = species)) 

#a new plot for each sp
ggplot(data = penguins) +
  geom_bar(aes(x = sex, fill = species)) +
  facet_wrap(~species, nrow = 1)  # Create a new plot for each species, line them all up into 1 row


#same plot but by island and then flipping coordinates to fit island names
ggplot(penguins) +
  geom_bar(aes(x = island, fill = species)) +
  facet_wrap(~species, ncol = 1) +
  coord_flip()


ggplot() + 
  geom_bar(aes(x=sex, fill=species), data=penguins) + 
  facet_wrap(~species, ncol=1) + 
  coord_flip() + 
  theme_classic()

#saving plot
ggplot(penguins) +
  geom_bar(aes(x = island, fill = species)) +
  facet_wrap(~species, ncol = 1) +
  coord_flip()
ggsave(filename = "figures/penguin_species_per_island.png",  
       device = "png", width = 5, height = 4, units = "in", dpi = 300)


#list of colors in R
colors()

#re-creating first scatter plot with black n white themes
ggplot(penguins) + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species)) +
  theme_classic()








#################in class b4 I did the above 
#scatter plot (add layers with a "+")
ggplot(data=penguins, aes(x=flipper_length_mm, y=body_mass_g)) + 
  geom_point(data=penguins, aes(x=flipper_length_mm, y=body_mass_g,
                                color=species, shape=species)) + 
  geom_smooth() + 
  xlab("Flipper length (mm)") +
  ylab("Body mass (g)") +
  ggtitle("Penguins are cute")

penguins %>%
  filter(is.na(flipper_length_mm)) %>% #keep only values that are NA
  summarize(num_nas = n())

#Exercise 2.1


#line plots
penguins_ts = penguins %>%
  group_by(species, year) %>%
  summarize(count = n())

groups(species)

ggplot(data=penguins_ts) + 
  geom_line(aes(x=year, y=count, color = species))
# identity maps each histogram differently
#dpi to set resolution
#a unique plot for each version of a variable
#bar plot
ggplot() + 
  geom_bar(aes(x=sex, fill=species), data=penguins) + 
  facet_wrap(~species, ncol=1) + 
  coord_flip() + 
  theme_classic()
  