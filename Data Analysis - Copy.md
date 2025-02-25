---
title: "Data Analysis"
author: "Jorge R. Soldevila Irizarry"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

**Load libraries**

```{r}
library(tidyverse)
library(kableExtra)
```

**Load data**

```{r}
load("data.Rdata")
```

# ***Place of birth***

```{r}
#We can group our data by unique responses in the Place variable and estimate 
#how many people were born in each place and the percentage.
plc_birth2 <- data_v11 %>% select(ID_long,race2,p01_plc_2,p02_plc_2,p03_plc_2,
                                 p04_plc_2,p05_plc_2,p06_plc_2,p07_plc_2,p08_plc_2,
                                 p09_plc_2,p010_plc_2,p011_plc_2,p012_plc_2)

plc_birth2_long <- pivot_longer(plc_birth2,
                          cols = starts_with("p0"),
                          names_to = "var",
                          values_to = "Place",
                          values_drop_na = F)

plc_birth2_long$Place[plc_birth2_long$Place==""] <- NA

plc_birth2_long <- plc_birth2_long %>% filter(!is.na(Place))

plc_birth2_freq <- plc_birth2_long %>%
  group_by(Place) %>%
  summarise(repeats = n()) %>%
  mutate(pct = repeats/sum(repeats)*100) %>%
  mutate(pct = round(pct,2)) %>%
  arrange(desc(repeats))

#We can visualize in a table or charts different statistics from the calculated data
#like frequency and proportion.

#Table
kbl(x = top_n(plc_birth2_freq,10)) %>%
  kable_classic(full_width = T) %>%
  scroll_box(width = "100%", height = "200px")

#Bar charts
plc_birth_top <- plc_birth2_freq %>% top_n(10,repeats)
barplot(plc_birth_top$pct,names.arg = plc_birth_top$Place, col = "steelblue",
        main = "Percentage Top 10 Place of Birth")
barplot(plc_birth_top$repeats,names.arg = plc_birth_top$Place, col = "steelblue",
        main = "Top 10 Place of Birth")

```

```{r}
#We can do a similar analysis utilizing the recoded variables for Place of Birth.

#We can group our data by unique responses in the Place variable and estimate 
#how many people were born in each place and the percentage.
plc_birth_rec <- data_v11 %>% select(ID_long,race2,p01_plcrec_2,p02_plcrec_2,
                                  p03_plcrec_2,p04_plcrec_2,p05_plcrec_2,
                                  p06_plcrec_2,p07_plcrec_2,p08_plcrec_2,
                                  p09_plcrec_2,p010_plcrec_2,p011_plcrec_2,
                                  p012_plcrec_2)

plc_birth_rec_long <- pivot_longer(plc_birth_rec,
                          cols = starts_with("p0"),
                          names_to = "var",
                          values_to = "Place",
                          values_drop_na = T)

plc_birth_rec_freq <- plc_birth_rec_long %>%
  group_by(Place) %>%
  summarise(repeats = n()) %>%
  mutate(pct = repeats/sum(repeats)*100) %>%
  mutate(pct = round(pct,2)) %>%
  arrange(desc(repeats))

#We can visualize in a table or charts different statistics from the calculated data
#like frequency and proportion.

#Table
kbl(x = top_n(plc_birth_rec_freq,10)) %>%
  kable_classic(full_width = T) %>%
  scroll_box(width = "100%", height = "200px")

#Bar charts
plc_birth_top <- plc_birth_rec_freq %>% top_n(10,repeats)
barplot(plc_birth_top$pct,names.arg = plc_birth_top$Place, col = "steelblue",
        main = "Percentage Top 10 Place of Birth")
barplot(plc_birth_top$repeats,names.arg = plc_birth_top$Place, col = "steelblue",
        main = "Top 10 Place of Birth")
```

# ***Race***

```{r}
race <- data_v11 %>% select(race2)
race_size <- race %>% 
  group_by(race2) %>% 
  summarise(repeats = n())

kbl(x = race_size) %>%
  kable_classic(full_width = F) %>%
  scroll_box(width = "100%", height = "200px")

```

# ***Household Income***

```{r}
#We can do an analysis including the reported income for all members 
#of the household.

income <- data_v11 %>% select(ID_long,race2,p01_incwk,p01_incmo,p01_incan,
                            p02_incwk,p02_incmo,p02_incan,
                            p03_incwk,p03_incmo,p03_incan,
                            p04_incwk,p04_incmo,p04_incan,
                            p05_incwk,p05_incmo,p05_incan,
                            p06_incwk,p06_incmo,p06_incan,
                            p07_incwk,p07_incmo,p07_incan,
                            p08_incwk,p08_incmo,p08_incan,
                            p09_incwk,p09_incmo,p09_incan,
                            p010_incwk,p010_incmo,p010_incan,
                            p011_incwk,p011_incmo,p011_incan,
                            p012_incwk,p012_incmo,p012_incan)
#Convert to long format for manipulating purposes. 
income_long <- pivot_longer(income,
                            cols = p01_incwk:p012_incan,
                            names_to = "var",
                            values_to = "values",
                            values_drop_na = F)

#Check unique values in the dataset.
unique(income_long$values)

#Some responses have characters indicating the type od income. Because, of these
#only social security is generally calculated for median house hold income, we will #do some cleaning to be able to use income from social security in our analysis. 

income_long$values <- str_replace(income_long$values, "ss", "")
#income_long$values <- str_replace(income_long$values, "(Seasonal)", "")
income_long$values <- str_replace(income_long$values, "soc sec", "")
#income_long$values <- str_replace(income_long$values, "welfare", "")
income_long$values <- str_replace(income_long$values, "s. s", "")
income_long$values <- str_replace(income_long$values, "s.s.", "")
income_long$values <- str_replace(income_long$values, "pension", "")
#income_long$values <- str_replace(income_long$values, "weekly alimony", "")
income_long$values <- str_replace(income_long$values, "-", "")
income_long$values <- str_replace(income_long$values, " ", "")

#Convert data for income to integer. 
income_long$values <- as.integer(income_long$values)

#Filter missing values
income_long <- income_long %>% filter(!is.na(income_long$values))

#Now we can calculate annual income.
income_long <- income_long %>% mutate(ann_inc = case_when(
  grepl("wk",var) ~ values * 52,
  grepl("mo",var) ~ values * 12,
  grepl("an",var) ~ values
))

#The previous analysis gives us the annual income of each individual for which an
#income was reported. Our interest is on household income. We will create a new variable that sums the incomes of all members of a household based on their ID_long
income_long_grp <- income_long %>% group_by(ID_long) %>%
  reframe(ann_inc_sum = sum(ann_inc),
            race = race2) %>%
  distinct(ID_long, .keep_all = TRUE)

#Now we can extend our analysis and group by race and determine median household income by race.
income_long_grp_race <- income_long_grp %>% 
  group_by(race) %>%
  summarise(med_hhinc = median(ann_inc_sum),
            mth_hhinc = med_hhinc/12) 

median(income_long_grp$ann_inc_sum) 

#We can aslo create histograms to see the distribution of the data.
ggplot(data=income_long_grp) +
  geom_boxplot(aes(x=ann_inc_sum)) +
  facet_wrap(~race)
```

# ***Number of rooms***

```{r}
racexroom <- data_v11 %>%
  select(race2,room_cnt_os) %>%
  filter(!is.na(race2)) %>%
  filter(!is.na(room_cnt_os)) %>%
  group_by(race2) %>%
  summarise(med_room = median(room_cnt_os))
```

# ***Family size***

```{r}
#Select variables for analysis
fam_size <- data_v11 %>% 
  select(ID_long,fam_size2,race2) %>% 
  filter(!is.na(fam_size2))

#Let's quickly look at some simple statistics.
tot_inc <- print(length(fam_size$fam_size2)) #Total records with Family Size var
mean_val <- print(mean(fam_size$fam_size2)) #Average family size
median_val <- print(median(fam_size$fam_size2)) #Median family size

#Let's look at the distribution of the family sizes
ggplot(data = fam_size, aes(x = fam_size2)) +
  geom_histogram(binwidth = 1,fill = "blue",color = "black") +
  geom_vline(aes(xintercept = mean_val), color = "red",linewidth = 1) +
  theme_bw()

#We can also do an analysis by race categories

famsize_race <- fam_size %>% 
  group_by(race2) %>%
  summarise(m_size = median(fam_size2))

totalxrace <- fam_size %>% 
  group_by(race2) %>% 
  summarise(repeats = n())

mean_famsize <- fam_size %>%
  summarise(m_size = mean(fam_size2))

ggplot(data = fam_size, aes(x = fam_size2)) +
  geom_histogram(binwidth = 1,fill = "blue",color = "black") +
  facet_wrap(~race2) +
  geom_vline(data = mean_famsize, aes(xintercept = m_size), color = "red",linewidth = 0.5) +
  stat_bin(binwidth = 1, geom = 'text', color = 'black', size = 2,
           aes(label=after_stat(count)),
           position = position_stack(vjust = 1.5)) +
  theme_bw()
```

# ***Age***

```{r}
#Since we only have the years of birth we will create a new variable that contains
#the approximate age of the members of the household.

#First we will create a data frame containing the variables for year of birth.
age <- data_v11 %>% 
  select(ID_long,race2,p01_year,p02_year,p03_year,p04_year,p05_year,p06_year,
         p07_year,p08_year,p09_year,p010_year,p011_year,p012_year)%>%
  mutate_at(c("p01_year","p02_year","p03_year","p04_year","p05_year",
              "p06_year","p07_year","p08_year","p09_year","p010_year",
              "p011_year","p012_year"), as.numeric)

age <- pivot_longer(age,
    cols = c(p01_year:p012_year),
    names_to = "var",
    values_to = "year"
  )

#Now we can create a new variable called age where we calculate the age of the 
#persons for which a year of birth was given, based on the year the relocation 
#process started.
age$age <- 1958 - age$year

#To make our analysis easier we can create a new variable with different age groups
age <- age %>%
  mutate(age_grp = case_when(age < 18 ~ "U18",
                             age >= 18 & age < 25 ~ "18to24",
                             age >= 25 & age < 34 ~ "25to34",
                             age >= 35 & age < 45 ~ "35to44",
                             age >= 45 & age < 54 ~ "45to54",
                             age >= 55 & age < 65 ~ "55to64",
                             age >= 65 ~ "elderly",
                             .default = "other")) %>%
  filter(!is.na(year))

ag_freq <- table(age$age_grp,age$race2)
ag_freq <- addmargins(ag_freq,c(1,2),sum)
ag_freq <- as.data.frame(ag_freq)
ag_freq <- ag_freq %>% 
  pivot_wider(names_from = Var2,
              values_from = Freq)
```

# ***Housing type***

```{r}
h_type <- data_v11 %>% select(race2,h_type,room_cnt_os,fam_size2)

#Confirm that there are two options for housing type.
unique(h_type$h_type)
#We see that the options re Apartment, Rooming, and NA. 

#Eliminate NA
h_type <- h_type %>% filter(!is.na(h_type))
#from the change we see that there were 268 records for which we did not have
#information on housing type

h_type_race <- table(h_type$race2,h_type$h_type)
h_type_race <- addmargins(h_type_race,c(1,2),sum)
h_type_race <- as.data.frame(h_type_race)
h_type_race <- h_type_race %>% 
  pivot_wider(names_from = Var2,
              values_from = Freq)
#write.csv(h_type_grp, "housing type by race.csv")

h_type_room <- table(h_type$h_type,h_type$room_cnt_os)
h_type_room <- addmargins(h_type_room,c(1,2),sum)
h_type_room <- as.data.frame(h_type_room)
h_type_room <- h_type_room %>% 
  pivot_wider(names_from = Var2,
              values_from = Freq)
#write.csv(h_type_room, "housing type by room count.csv")

h_type %>% 
  filter(!is.na(room_cnt_os)) %>%
  group_by(h_type) %>%
  summarise(x = median(room_cnt_os))

h_type %>% 
  filter(!is.na(fam_size2)) %>%
  filter(fam_size2 != 999) %>%
  group_by(h_type) %>%
  summarise(x = mean(fam_size2))
```

# ***Overcrowding***

```{r}
overcrwd <- data_v11 %>%
  select(ID_long,room_cnt_os,fam_size2,h_type,race2) %>% 
  filter(!is.na(room_cnt_os))

overcrwd <- overcrwd %>%
  filter(!is.na(fam_size2)) %>%
  mutate(ovrcrwd = fam_size2/room_cnt_os,
         ovr_grp = case_when(ovrcrwd <= 1.00 ~ "1.00 or less",
                             ovrcrwd > 1.00 & ovrcrwd <= 1.50 ~ "1.01 to 1.50",
                             ovrcrwd > 1.50 ~ "1.51 or more",
                             .default = "other")) #Other represents cases where                                                        #family size is NA
overcrwd_race <- table(overcrwd$ovr_grp,overcrwd$race2)
overcrwd_race <- addmargins(overcrwd_race,c(1,2),sum)
overcrwd_race <- as.data.frame(overcrwd_race)
overcrwd_race <- overcrwd_race %>% 
  pivot_wider(names_from = Var2,
              values_from = Freq)

overcrwd_htype <- table(overcrwd$ovr_grp,overcrwd$h_type)
overcrwd_htype <- addmargins(overcrwd_htype,c(1,2),sum)
overcrwd_htype <- as.data.frame(overcrwd_htype)
overcrwd_htype <- overcrwd_htype %>% 
  pivot_wider(names_from = Var2,
              values_from = Freq)
```

# ***Foreign Born Population***

```{r}
overcrwd_t <- overcrwd %>% select(ID_long,ovr_grp)
data_v11 <- data_v11 %>% left_join(overcrwd_t,by = "ID_long")

fb <- data_v11 %>% select(ID_long,p01_plcrec_2:p012_plcrec_2,race2,ovr_grp,h_type)
fb_long <- fb %>% pivot_longer(cols = starts_with("p0"),
                               names_to = "var",
                               values_to = "For_brn",
                               values_drop_na = T)
fb_long <- fb_long %>% mutate(fb = case_when(For_brn == "USA" ~ "nat",
                                             .default = "fb"))

fb_grp <- table(fb_long$fb,fb_long$race2)
fb_grp <- addmargins(fb_grp,c(1,2),sum)
fb_grp <- as.data.frame(fb_grp)
fb_grp <- fb_grp %>% 
  pivot_wider(names_from = Var2,
              values_from = Freq)

#Let's conduct an analysis of overcrowding in foreign born households vs native households. 
#To determine native or foreign household we will take the place of birth of person #1
#in the family relationship information. 

#First we filter for the first person in the family composition list. This gives us 
#unique households with the place of birth of, in most cases, the head or leading figure 
#of the household.
fb_ovrcrwd <- fb_long %>% filter(var == "p01_plcrec")

#now we want to filter both foreign born population variable and overcrowding 
#variable to remove NAs.
fb_ovrcrwd <- fb_ovrcrwd %>% 
  filter(!is.na(ovr_grp)) %>%
  filter(!is.na(fb))
#There are 1620 records for which we have information on both place of birth for
#person #1 and information on overcrowding condition in household.

#Now we can somewhat estimate how many households that were either foreign or native 
#experienced overcrowding or not.
fb_ovrcrwd <- table(fb_ovrcrwd$fb,fb_ovrcrwd$ovr_grp)
fb_ovrcrwd <- addmargins(fb_ovrcrwd,c(1,2),sum)
fb_ovrcrwd <- as.data.frame(fb_ovrcrwd)
fb_ovrcrwd <- fb_ovrcrwd %>%
  pivot_wider(names_from = Var2,
              values_from = Freq)

#What type of housing did native and foreign born live in?
fb_house <- fb_long %>% filter(var == "p01_plcrec")

fb_house <- fb_house %>%
  filter(!is.na(fb)) %>%
  filter(!is.na(h_type))

fb_house <- table(fb_house$fb,fb_house$h_type)
fb_house <- addmargins(fb_house,c(1,2),sum)
fb_house <- as.data.frame(fb_house)
fb_house <- fb_house %>% 
  pivot_wider(names_from = Var2,
              values_from = Freq)
```

# ***Number of years in site***

```{r}
site_race <- data_v11 %>%
  filter(!is.na(race2)) %>%
  filter(!is.na(no_year_site2)) %>%
  group_by(race2) %>%
  summarise(avg_site = mean(no_year_site2),
            med_site = median(no_year_site2))

yr_site <- data_v11 %>% 
  filter(!is.na(race2)) %>%
  filter(!is.na(no_year_site2))

ggplot(data=yr_site) +
  geom_boxplot(aes(x=no_year_site2)) +
  facet_wrap(~race2)
```

# ***Number of years in city***

```{r}
city_race <- data_v11 %>%
  filter(!is.na(race2)) %>%
  filter(!is.na(no_year_city2)) %>%
  group_by(race2) %>%
  summarise(avg_city = mean(no_year_city2),
            med_city = median(no_year_city2))

yr_city <- data_v11 %>% 
  filter(!is.na(race2)) %>%
  filter(!is.na(no_year_city2))

ggplot(data=yr_city) +
  geom_boxplot(aes(x=no_year_city2)) +
  facet_wrap(~race2)
```

# ***Rent on site***

```{r}
#To carry out our analysis we will first determine all cases where rent info
#was included for both on site and relocation
rent_os <- data_v11 %>% filter(!is.na(rent_os))

#Let's check the responses given for rent.
unique(rent_os$rent_os)

#From the unique responses we see that many indicate they payed rent monthly.
#Let's use that information to populate the week_rent variable, which was created
#to indicate if rent in that location was payed weekly.
rent_os$week_rent2 <- NA
rent_os$week_rent2[str_detect(rent_os$rent_os,"weekly")] = 1
rent_os$week_rent2[str_detect(rent_os$rent_os,"wk")] = 1

#We can do a similar analysis for cases where rent was payed bi-weekly.
rent_os$biweek_rent2 <- NA
rent_os$biweek_rent2[str_detect(rent_os$rent_os,"semi")] = 1
rent_os$biweek_rent2[str_detect(rent_os$rent_os,"bi")] = 1
rent_os$biweek_rent2[str_detect(rent_os$rent_os,"every")] = 1
rent_os$biweek_rent2[str_detect(rent_os$rent_os,"each")] = 1

#A number of responses contain characters or other information that is not numeric.
#Let's get rid of them.
#First we will create a new variable that contains the same values as our original 
#rent variable and where we will make changes.
rent_os$rent_os2 <- rent_os$rent_os

unique(rent_os$rent_os2)

#Now we can eliminate all other characters
rent_os$rent_os2 <- str_replace_all(rent_os$rent_os2,"R.R.","")
rent_os$rent_os2 <- str_replace(rent_os$rent_os2,"R.R","")
rent_os$rent_os2 <- str_replace(rent_os$rent_os2,"weekly","")
rent_os$rent_os2 <- str_replace(rent_os$rent_os2,"wk","") 
rent_os$rent_os2 <- str_replace(rent_os$rent_os2,"PW","")
rent_os$rent_os2 <- str_replace(rent_os$rent_os2,"sharing","") 
rent_os$rent_os2 <- str_replace(rent_os$rent_os2,"verified 02/28","")
rent_os$rent_os2 <- str_replace(rent_os$rent_os2,"semi mo","")
rent_os$rent_os2 <- str_replace(rent_os$rent_os2,"bi. mo","")
rent_os$rent_os2 <- str_replace(rent_os$rent_os2,"bi mo","")
rent_os$rent_os2 <- str_replace(rent_os$rent_os2,"semi- mo .","")
rent_os$rent_os2 <- str_replace(rent_os$rent_os2,"semi- mo.","")
rent_os$rent_os2 <- str_replace(rent_os$rent_os2,"every 2 weeks","")
rent_os$rent_os2 <- str_replace(rent_os$rent_os2,"each 2 weeks","")
rent_os$rent_os2 <- str_replace(rent_os$rent_os2,"r.r","")
rent_os$rent_os2 <- str_replace(rent_os$rent_os2,"r. r","")
rent_os$rent_os2 <- str_replace(rent_os$rent_os2,"(RR)","")
rent_os$rent_os2 <- str_replace(rent_os$rent_os2,"bi .mo","")
rent_os$rent_os2 <- str_replace(rent_os$rent_os2,"bi.mo","")

rent_os$rent_os2 <- gsub("\\$","",rent_os$rent_os2)
rent_os$rent_os2 <- gsub("\\()","",rent_os$rent_os2)
rent_os$rent_os2 <- gsub("\\/",".",rent_os$rent_os2)

#We must also remove all instances where there are blank spaces
rent_os$rent_os2<-str_replace(rent_os$rent_os2," ","")


#Here we are manually editing certain entries that contain characters or sequences
#that might be harder for the program to decipher. 
rent_os$rent_os2[rent_os$rent_os2 == "64.30, 54.65"] <- "64.30"
rent_os$rent_os2[rent_os$rent_os2 == "36.80.35.80"] <- "36.80"
rent_os$rent_os2[rent_os$rent_os2 == "30.00,47.17"] <- "47.17"

unique(rent_os$rent_os2)

#A number of entries still contain characters. These are mostly cases where 
#rent was listed as free or that the tenant was superintendent. We will remove
#these to perform analysis.
rent_os$rent_os2 <- as.numeric(rent_os$rent_os2)
colSums(is.na(rent_os)) 
#A total of 70 records were listed as having characters and have been converted to NA.
#We will remove them.
rent_os <- rent_os %>% filter(!is.na(rent_os2))
#We are left with 1,764 records.

#Now we can adjust rent payments to monthly for those that were paying weekly or bi-weekly rent.
rent_os <- rent_os %>%
  mutate(rent_os_mth = case_when(week_rent2 == 1 & is.na(biweek_rent2) ~ rent_os2*52/12,
                   is.na(week_rent2) & biweek_rent2 == 1 ~ rent_os2*26/12,
                   rent_os2 <= 20 ~ rent_os2*52/12,
                   .default = rent_os2))

rent_os_temp <- rent_os %>% select(ID_long,rent_os,rent_os2,week_rent,biweek_rent
                                   ,week_rent2,biweek_rent2,rent_os_mth,race2,fam_size2,
                                   room_cnt_os,h_type)

#Now we can conduct some analysis on rent payed.

rent_os_temp <- rent_os_temp %>% 
  mutate(rnt_x_room = rent_os_mth/room_cnt_os)

#Mean monthly household rent
mean_rent <- mean(rent_os_temp$rent_os_mth)


#Average rent for rooming units
rent_rooming <- rent_os_temp %>% 
  filter(h_type == "Rooming") %>%
  summarise(avg_rent = mean(rent_os_mth))
#Average rent for apartments
rent_apa <- rent_os_temp %>%
  filter(h_type == "Apartment") %>%
  summarise(avg_rent = mean(rent_os_mth))

#Average rent per room
room_rent <- rent_os_temp %>% 
  filter(!is.na(rnt_x_room)) %>%
  summarise(mean(rnt_x_room))

#Average rent per room apartments
room_rent_ap <- rent_os_temp %>% 
  filter(!is.na(rnt_x_room)) %>%
  filter(h_type == "Apartment") %>%
  summarise(mean(rnt_x_room))

#Average rent per room Rooming
room_rent_rm <- rent_os_temp %>% 
  filter(!is.na(rnt_x_room)) %>%
  filter(h_type == "Rooming") %>%
  summarise(mean(rnt_x_room))

#Rent per race
rent_race <- rent_os_temp %>%
  filter(!is.na(race2)) %>% 
  group_by(race2) %>%
  summarise(avg_rent = mean(rent_os_mth))
rent_race_room <- rent_os_temp %>%
  filter(!is.na(race2)) %>% 
  filter(!is.na(room_cnt_os)) %>%
  group_by(race2) %>%
  summarise(avg_rent = mean(rnt_x_room))


rent_os_temp$h_type <- as.factor(rent_os_temp$h_type)
plot(rent_os_mth~h_type, data = rent_os_temp)
```

```{r}
#We could also do an analysis for the gross rent.
#Let's calculate a new variable for gross rent.
rent_os_temp <- rent_os_temp %>%
  mutate(rent_os_grss = rent_os_mth*12)

#Median gross household rent
med_grss_rent <- median(rent_os_temp$rent_os_grss)


#Median gross rent for rooming units
rent_rooming_grss <- rent_os_temp %>% 
  filter(h_type == "Rooming") %>%
  summarise(avg_rent = median(rent_os_grss))
#Median rent for apartments
rent_apa_grss <- rent_os_temp %>%
  filter(h_type == "Apartment") %>%
  summarise(avg_rent = median(rent_os_grss))

#Rent per race
rent_race_grss <- rent_os_temp %>%
  filter(!is.na(race2)) %>% 
  group_by(race2) %>%
  summarise(avg_rent = median(rent_os_grss))

ggplot(data=rent_os_temp) +
  geom_histogram(aes(x=rent_os_grss), bins = 4, colour = "black", fill = "white") +
  geom_vline(aes(xintercept = median(rent_os_grss)),
             color="blue", linetype="dashed", linewidth=1) +
  facet_wrap(~race2)
```
