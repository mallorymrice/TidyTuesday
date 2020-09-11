# installing the TidyTuesday package
#install.packages("tidytuesdayR", "tidyverse", "ggplot2", "ggthemes")

# once packages are installed in R, they must be 'called' by using the library() function
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(ggthemes)

# R does not have a built-in standard error function, so I include this in all of my code:
se = function(x) sd(x, na.rm = TRUE)/sqrt(length(x))

# each week a new data set comes out for TidyTuesday and can be loaded into the space with the tt_load() function
tuesdata = tt_load(2020, week=37)

# the readme file describes the activity and datasets for that week
readme(tuesdata)

# you can use the $ to call up the different data sets, for example:
tuesdata$friends

# it's easier to code if the datasets are separated out:
friends = tuesdata$friends
friends.emotions = tuesdata$friends_emotions
friends.info = tuesdata$friends_info

# there is a LOT of data here - use the glimpse() function to see the columns, data type, and sample data
glimpse(friends.info)
# can also use the head() function
head(friends.info)

# we can ask a lot of questions with this data

# I love Friends, but I'm curious which season is most popular? 
# we can look at the IMDB rating across season to address this question

# summarizing the IMDB rating across season:
friends.rating = friends.info %>% 
  mutate(season = as.factor(season)) %>% # converting season from numeric to factor for grouping
  group_by(season) %>% # grouping data by season
  summarise(mean.rating = mean(imdb_rating), # averaging IMDB rating by season
            se = se(imdb_rating)) # calculating standard error
friends.rating

# now we can create a plot of the average IMDB rating across season
ggplot(friends.rating, aes(x = season, y = mean.rating))+
  geom_bar(stat="identity", width=0.8)+ # plots data using as a bar plot
  geom_errorbar(aes(ymin = mean.rating - se, # error bar below the mean value
                    ymax = mean.rating + se), # error bar above the mean value
                width=0)+ # width of the error bar
  xlab("Season")+ # x-axis label
  ylab("IMDB rating") # y-axis label

# now that we have a basic plot, we can make it more aesthetically pleasing... 
ggplot(friends.rating, aes(x = season, y = mean.rating))+
  geom_bar(stat="identity", width=0.8)+ # plots data using as a bar plot
  geom_errorbar(aes(ymin = mean.rating - se, # error bar below the mean value
                    ymax = mean.rating + se), # error bar above the mean value
                width=0)+ # width of the error bar
  xlab("Season")+ # x-axis label
  ylab("IMDB rating")+ # y-axis label
  scale_y_continuous(limits = c(0,9), # sets the y-axis limits to better see the difference across season
                     expand = c(0,0))+ # this brings the origin to the x-axis
  theme_classic()+ # ggthemes package has a lot of plot styles to choose from that make the plot more visually appealing
  theme(axis.text=element_text(colour="black")) # makes the axis text black

# if there's a difference in season popularity it is really marginal, which makes sense for such a great show!

# but how do viewer ratings change across season?

# summarizing the viewer ratings across season:
friends.viewers = friends.info %>% 
  mutate(season = as.factor(season)) %>% # converting season from numeric to factor for grouping
  group_by(season) %>% # grouping data by season
  summarise(mean.viewers = mean(us_views_millions), # averaging IMDB rating by season
            se = se(us_views_millions)) # calculating standard error
friends.viewers

# plot of USA viewers (millions) by season
ggplot(friends.viewers, aes(x = season, y = mean.viewers))+
  geom_bar(stat="identity", width=0.8)+ # plots data using as a bar plot
  geom_errorbar(aes(ymin = mean.viewers - se, # error bar below the mean value
                    ymax = mean.viewers + se), # error bar above the mean value
                width=0)+ # width of the error bar
  xlab("Season")+ # x-axis label
  ylab("USA Viewers (millions)")+ # y-axis label
  scale_y_continuous(expand = c(0,0))+ # this brings the origin to the x-axis
  theme_classic()+ # ggthemes package has a lot of plot styles to choose from that make the plot more visually appealing
  theme(axis.text=element_text(colour="black")) # makes the axis text black

# it looks like season 2 is the most popular! I wonder why... let's explore more to find out!