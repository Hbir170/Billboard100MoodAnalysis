#Billboard Hot 100 
#Tidy Tuesday Challenge

#Main Question: Are Number 1 Hits more cheerful than other songs? 

library(tidyverse)
library(tidytuesdayR)
tuesdata <-tidytuesdayR::tt_load('2021-09-14') 
billboard <- tuesdata$billboard
audio <- tuesdata$audio_features

View(billboard)
View(audio)

billboard %>% 
  glimpse()

#Cleaning up format for Week_Id and making it date format
billboard <- billboard %>% 
  mutate(week_id = mdy(week_id))

#Creating a Measure that determines if song was ever a Number 1 Hit
#Also removing duplicates that aren't at peak chart number per song to better analyze
#how a song did and for clarity purposes

bb_num1 <- billboard %>% 
  mutate(num1=(peak_position==1)) %>% 
  group_by(song_id) %>% 
  arrange(week_position,.by_group = TRUE) %>%  
  #Above: we arrange by the week position to validate for the highest ranking the song ever 
  #had we also use .by_group to validate the group by with arrange() function
  filter(!duplicated(song_id)) %>% 
  #Above: filters anything true, here we are grabbing all results 
  #that are not (!) duplicates (first result per group by)
  ungroup() %>% #we ungroup our variables to ensure our select is not impacted below
  select(song_id,num1)


songs <- bb_num1 %>% 
  left_join(audio,
            by='song_id')


View(songs)
#Data Assessing (discovered many nulls in some columns which don't seem to have any 
#correlation with date or with particularly the number of the record)

# Measuring how many nulls we have:
#Null percentage in key column: Valence (mood column)
mean(is.na(songs$valence))
#On average 17.53% of the valence column is null

#Comparing it to the overall datasets null percentage:
map_dbl(songs, \(x) mean(is.na(x)))

#We see here that based on our results we are very likely to be missing data such as
#energy,mode,speechiness,danceability, liveness and tempo when we are missing valence
#as they all are missing 17.53% of data individually

# This likely has to do with issues regarding Spotify metrics not downloading correctly

#Testing whether or not Valence has an impact on a song being Number 1
#null states there is no difference
t.test(valence ~ num1,
       data = songs)
#Based on the small P-Value we CAN reject the null as it is below 0.05 (0.01115)
#THOUGH we notice the difference in the means for both groups is not very high
# <2% difference in terms of likelihood to be number one (60.92% for Less Happy, 62.11%
#for Happy songs) which is small. 

#It may be important to assess how large of an effect Valence truly has (Cohen's D Test)
df <- songs %>% 
  group_by(num1) %>% 
  summarize(avg_valence = mean(valence,
                               na.rm = TRUE),
            variance_valence = var(valence,
                             na.rm=TRUE))

pooled_sd <- sqrt(sum(df$variance_valence)/2)

cohens_d <- diff(df$avg_valence)/pooled_sd
cohens_d
#Because the Cohen's D is 0.0831 and very low we can state that although there is a 
#difference between the groups the actual impact of Valence on making a song 
#become a Number 1 Hit is negligible. 

#Visualizing the difference in Number 1 Hit songs based on Valence(Mood):
ggplot(songs,aes(x=valence,
                 y=num1))+
  geom_boxplot()+
  theme_minimal()
#We see a very close relation and results for both Number 1 Hits and Non-Hits