
# libraries ---------------------------------------------------------------



library(dplyr)
library(tidyr)
library(ggplot2)


# data importation --------------------------------------------------------

bat_df <- read.csv(file.choose())


# looking at the batting data ---------------------------------------------

head(bat_df)
summary(bat_df)

# looking at the structure of data ----------------------------------------

str(bat_df)


# factorizing few columns right away...............................

bat_df$playerID <- factor(bat_df$playerID)
bat_df$teamID <- factor(bat_df$teamID)
bat_df$lgID <- factor(bat_df$lgID)

str(bat_df)


head(bat_df$AB)
head(bat_df$X2B)


# we will create new varaibles which were cused in the moneyball------------- 
# there are three variables which were dervied and used in moneyball-----------

#1. Batting Average(ba)
#2. On Base Percentage(obp)
#3. Slugging percentage(sp)


bat_df$ba <- bat_df$H/bat_df$AB


bat_df$obp <- (bat_df$H + bat_df$X2B + bat_df$HBP)/(bat_df$AB + bat_df$X2B
                                                    + bat_df$HBP + bat_df$SF)


bat_df$x1B <- bat_df$H - bat_df$X2B - bat_df$X3B - bat_df$HR
bat_df$slg <- ((1*bat_df$x1B) + (2*bat_df$X2B) + (3*bat_df$X3B) + (4*bat_df$HR)
               / bat_df$ba)

tail(bat_df)
str(bat_df)


# Merging salary data -----------------------------------------------------

# we now need salary data in order to seee which player is undervalued......

sal_df <- read.csv(file.choose())


# looking at the data -----------------------------------------------------

summary(sal_df)

# if we compare closely then we can see that in batting data, 1871 is the starting year...

# we will hence filter the data and only include the rows having the year from 1985......

bat_df <- subset(bat_df, yearID >= 1985)
summary(bat_df)


# Merging salary and batting data -----------------------------------------

full_df <- merge(bat_df, sal_df, by = c('playerID', 'yearID'))


# looking at the merged data ----------------------------------------------


summary(full_df)


# lets look at the three players that Oakland's A lost during the midseason.....

# Those three players were Giambi, Damon, Gustavo................

# lets look at their stats..................

player_lost <- subset(full_df, playerID %in% c('giambja01', 'damonjo01', 
                                               'saenzol01'))
head(player_lost)

# as all these key players were lost in 2001, hence we will only focus on 2001 data........

full_df <- subset(full_df, yearID == 2001)

full_df

# we can now focus on the variables which are important for us in analysis........
# we can drop the remaining varaibles or columns...............

player_lost <- player_lost[ , c('playerID', 'H', 'X2B', 'X3B', 'HR', 'obp',
                                'slg', 'ba', 'AB')]

head(player_lost)

# we have all the stats we need to replace these three players..........
# although, we have constraints.
# 1. salary can not exceed 15 million
# 2. AB needs to be either equal or more than the lost player
# 3. mean OBP has to be equal or greater than mean OBP of lost player

# lets create a quick scatter plot to see the OBP and salary distribution.....

full_df %>% ggplot(aes(x = obp, y = salary)) + geom_jitter()

# we can easily see that most of the players have salary around 8 million.....

# we will also filter out players having OBP zero.......


full_df <- filter(full_df, obp > 0, salary < 8000000)

# we will also add filter for AB and keep it around 500..........

full_df <- filter(full_df, AB >= 500)

full_df

# lets arrange the data according to OBP.................

potential_players <- arrange(full_df, desc(obp))

potential_players

head(potential_players)

# lets buy top three players except giambja01 as he is leaving the club....

potential_players[2:4,]

# if you look at the salary now, it is affordable and we are getting good players........
