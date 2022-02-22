#Amadi 02/15/2022 with Erin on Tidyverse 


#* `filter()` chooses rows based on column values.
#* `arrange()` changes the order of the rows.
#* `select()` changes whether or not a column is included.
#* `rename()` changes the name of columns.
#* `mutate()` changes the values of columns and creates new columns.
#* `summarize()` collapses a group into a single row.
#* `group_by()` group data into rows with the same values
#* `ungroup()` remove grouping information from data frame.
#* `distinct()` remove duplicate rows.




install.packages("tidyverse")
library("tidyverse")
library(tidyverse)
library(dplyr)
library(palmerpenguins)

tidyverse_packages()# returns suite of package in tidyverse

my_data = as.data.frame(my_data) #to turn a tibble into a data frame when older
#functions don't work with tibble
head(penguins)
summary(penguins)
dim(penguins)
glimpse(penguins)
tail(penguins)


#Filtering
gentoo = filter(penguins, species=="Gentoo") #filter data based on gentoo sp #subset by Gentoo penguins
gentoo_ladies = filter(gentoo, sex=="female")
gentoo_ladies = filter(penguins, species=="Gentoo", sex=="female")#both in a single step
gentoo_gents = filter(gentoo, sex == "male")
summary(gentoo_ladies)
#vs base R br = base R
gentoo_ladies_br = gentoo[gentoo$species == "Gentoo" & gentoo$sex == "female", ] # vs. base R (note, this retains NAs)
gentoo_ladies_br2 = penguins[penguins$species == "Gentoo" & penguins$sex == "female", ]#my version
gentoo_ladies_brr = gentoo[gentoo$sex == "female", ] # also mine. vs. base R (note, this retains NAs)


#pipe (%>%) function
# These two lines of code are equivalent:
gentoo_ladies = filter(penguins, species=="Gentoo", sex=="female")
gentoo_ladies = penguins %>% filter(species=="Gentoo", sex=="female")

#find the mean mass (in grams) of all female penguins
mean_mass_female = penguins %>% 
  filter(sex=="female")%>%
  summarise(mean_body_mass_g = mean(body_mass_g))
mean_mass_female
#in base R
all_females = penguins[penguins$sex =="female",]
mean_mass = mean(all_females$body_mass_g)                      

#Exercise 1.1 filter chinstrap penguins and then separately filter by flip length>200 and gett sex ratio
chinstrap = filter(penguins, species =="Chinstrap")
summary(chinstrap)
chinstrap_gr8r_than200 = filter(penguins, species =="Chinstrap", flipper_length_mm>200)
summary(chinstrap_gr8r_than200)
#2nd method
chinstrap = filter(penguins, species =="Chinstrap")
summary(chinstrap)
flip_gr8r_than200_male = filter(penguins, species =="Chinstrap")%>%
  filter(flipper_length_mm>200, sex == "male")
summary(flip_gr8r_than200_male)
flip_gr8r_than200_female = filter(penguins, species =="Chinstrap")%>%
    filter(flipper_length_mm>200, sex == "female")
summary(flip_gr8r_than200_female)


# Calculate mass of each species
species_mean_mass = filter(penguins)%>%
  group_by(species)%>%
summarise(mean_mass_g = mean(body_mass_g, na.rm=T)) 

# Calculate mass of each species by sex
species_sex_mean_mass = penguins%>%
  filter(!is.na(sex))%>%# Removes rows where sex is NA. Read the ! as the word "not" here - i.e. it flips the logical value
  group_by(species, sex)%>%
  summarize(mean_mass_g = mean(body_mass_g)) %>%
  print()

# Save the table
write_csv(species_sex_mean_mass,
          path="data/processed/peguin_mean_body_mass_g_.csv")


#there are many different summary functions that can be used inside of  `summarize()`:
  
#*  Center:   mean(), median()
#*  Spread:   sd(), IQR(), mad()
#*  Range:    min(), max(), quantile()
#*  Position: first(), last(), nth(),
#*  Count:    n(), n_distinct()
#*  Logical:  any(), all()

#Exercise 1.3
#What is the mean bill length (in inches) of Adelie penguins found on 
#either Dream island or Biscoe island? 
#What is the standard deviation?
Adelie_meanbillen_Dream = penguins%>%
  filter(species == "Adelie", island == "Dream")%>%
  summarise(mean_bill_length_Dream = mean(bill_length_mm)/25.4)#bill length, inches
summary(Adelie_meanbillen_Dream)

Adelie_meanbillen_Biscoe = penguins%>%
  filter(species == "Adelie", island == "Biscoe")%>%
  summarise(mean_bill_length_Biscoe = mean(bill_length_mm)/25.4)#bill length, inches
summary(Adelie_meanbillen_Biscoe)

Adelie_meanbillen_Torgersen = penguins%>%
  filter(species == "Adelie", island == "Torgersen")%>%
  filter(!is.na(bill_length_mm))%>%
  summarise(mean_bill_length_Torgersen = mean(bill_length_mm)/25.4)#bill length, inches
summary(Adelie_meanbillen_Torgersen)
#the mean bill length for Adelie is approximately the same for all 3 islands
#?standard deviation???
  