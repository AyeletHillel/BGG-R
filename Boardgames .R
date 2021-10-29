library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(GGally)
library(scales)

#Downloading the data

data <- read.csv("Desktop/bgg_dataset.csv", sep = ";", dec=",", stringsAsFactors=FALSE)

#Data preparation

View(data)
class(data)
summary(data)

sum(is.na(data))
colSums(is.na(data))
data <- na.omit(data)
sapply(data, class)

games_by_domain = data %>% 
  mutate(Domains = map(Domains, ~ strsplit(.x, ", ") %>% unlist())) %>% 
  unnest(Domains) 

games_by_mech_dom = games_by_domain %>% 
  mutate(Mechanics = map(Mechanics, ~ strsplit(.x, ", ") %>% unlist())) %>% 
  unnest(Mechanics)  

head(games_by_mech_dom)

#DEA

cor_matrix <- data %>%
  select_if(is.numeric) %>%
  cor(.) #rating average and complexity average are moderately correlated (0.48)

View(cor_matrix)
#by year

class(data$Year.Published) 

data.by.years <- data %>%
  mutate(Year.Published=as.numeric(Year.Published)) %>%
  filter(!is.na(Year.Published)) %>%
  filter(Year.Published <= 2020, Year.Published >= 1960) %>%
  group_by(Year.Published) %>%
  dplyr::summarise(count = n())

View(data.by.years)

ggplot(data.by.years,aes(Year.Published,count)) +
  geom_line(col="blue", lwd=1) +
  ggtitle(paste('Board Games Released by Year, ', min(data.by.years$Year.Published),'-', max(data.by.years$Year.Published))) +
  xlab('Year') +
  ylab("Number of Games") +
  ylim(c(0,max(data.by.years$count)))

#by play time 

class(data$Play.Time)

ggplot(data %>%
         mutate(Play.Time=as.numeric(Play.Time)) %>%
         filter(Play.Time != 0, Play.Time <= 200 ),
       aes(x = Play.Time)) +
  geom_histogram(aes(y = ..density..), binwidth = .1, fill="red", alpha=.2, col="deeppink") + geom_density(col="red", lwd=1) +
  xlim(0,300) +
  xlab('Play Time (min)') +
  geom_vline(xintercept=mean(data$Play.Time, na.rm=TRUE), color="black") #This figure is no good. 

#by domains

games_by_mech_dom %>%
  group_by(Domains) %>%
  ggplot(aes(x=Domains)) +
  geom_bar(fill='lightsteelblue3') +
  coord_flip() +
  ylab('Number of Games')


#by mechanics

games_by_mech_dom_20 = games_by_mech_dom %>%
  group_by(Mechanics) %>%
  summarise(count = n()) %>%
  top_n(20)  

ggplot(data=games_by_mech_dom_20, aes(x=Mechanics, y=count)) + 
  geom_bar(stat="identity", width=0.5, fill="steelblue") + 
  coord_flip() +
  ylab('Number of Games')

#by rating 

data %>% summarise(Mean = mean(Users.Rated)) # 840.9714
data %>% summarise(Mean = mean(Rating.Average)) # 6.403227

ggplot(data, aes(x = Rating.Average)) +
  geom_histogram(aes(y = ..density..), binwidth = .1, fill="red", alpha=.2, col="deeppink") + geom_density(col="red", lwd=1) +
  xlim(0,10) +
  xlab('Average Rating') +
  ylab('Density') +
  geom_vline(xintercept=mean(data$Rating.Average, na.rm=TRUE), color="black")


#by complexity 
data %>% summarise(Mean = mean(Complexity.Average), max(Complexity.Average), min(Complexity.Average)) #1.991188 scale: 0-5

ggplot(data %>% filter(Complexity.Average !=0), aes(x = Complexity.Average)) +
  geom_histogram(aes(y = ..density..), binwidth = .1, fill="red", alpha=.2, col="deeppink") + geom_density(col="red", lwd=1) +
  xlim(0,10) +
  xlab('Complexity Average') +
  ylab('Density') +
  geom_vline(xintercept=mean(data$Complexity.Average, na.rm=TRUE), color="black") +
  scale_x_continuous(breaks= c(1:5))

#by user.owned 
data %>% filter(!is.na(Owned.Users)) %>%
  summarise(Mean = mean(Owned.Users), max(Owned.Users)) #mean = 1408.458, max = 155312 

#by min.players
data %>% summarise(Mean = mean(Min.Players), max(Min.Players), min(Min.Players)) # 0-10, mean = 2.01
data %>% filter(Min.Players == 0) %>% summarise(Name)

data %>%
  group_by(Min.Players) %>%
  filter(Min.Players != 0, Min.Players <=7 ) %>%
  ggplot(aes(x=Min.Players)) + 
  geom_bar(fill='lightsteelblue3') + 
  scale_x_continuous(breaks = c(1:7)) + 
  ylab("Number of Games") +
  xlab("Min Number of Players") + 
  scale_y_continuous(breaks = c(2000, 4000, 6000, 8000, 10000, 12000, 14000), 
                     labels = c("2k", "4k", "6k", "8k", "10k", "12k", "14k"))

data %>%
  group_by(Min.Players) %>%
  filter(Min.Players <= 3) %>%
  summarise(count = n())
  
#by max.players
data %>% summarise(Mean = mean(Max.Players), max(Max.Players), min(Max.Players)) #mean = 5.6, 0-999
data %>%
  group_by(Max.Players) %>%
  filter(Max.Players != 0, Max.Players <= 10) %>%
  ggplot(aes(x=Max.Players)) + 
  geom_bar(fill='lightsteelblue3') + 
  scale_x_continuous(breaks = c(1:10)) + 
  ylab("Number of Games") +
  xlab("Max Number of Players") +
  scale_y_continuous(breaks = c(1000, 2000, 3000, 4000, 5000, 6000, 7000), 
                     labels = c("1k", "2k", "3k", "4k", "5k", "6k", "7k"))

#by play time
data %>% summarise(Mean = mean(Play.Time), max(Play.Time), min(Play.Time))

ggplot(data %>% filter(Play.Time <= 1000), aes(x=Play.Time)) + 
  geom_histogram(aes(y = ..density..), binwidth = .1, fill="red", alpha=.2, col="deeppink") + 
  geom_density(col="red", lwd=1) +
  xlim(0,10000) +
  xlab('Number of Games') +
  ylab('Play Time (min)') +
  geom_vline(xintercept=mean(data$Play.Time, na.rm=TRUE), color="black")

#Average Rating & Number of Ratings

ggplot(data %>% filter(Users.Rated >= 10), aes(x = Users.Rated, y = Rating.Average)) +
  geom_point(alpha=.2, lwd=.2, col="deeppink") +
  geom_smooth(col="blue", lwd=.7) +
  ylab("Average Rating") +
  xlab("Number of Ratings") +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                            labels = trans_format("log10", math_format(10^.x)),
                                            limits = c(10^2,10^5)) 

quantile(data$Users.Rated, probs = seq(0, 1, 0.25), na.rm = FALSE,
         names = TRUE, type = 7)

data %>%
  mutate(category=cut(Users.Rated, breaks=c(-Inf, 55, 120, 385, Inf), labels=c("Low","Middle","High", "Very High"))) %>%
  group_by(category) %>%
  summarise(Rating = mean(Rating.Average)) %>%
  ggplot(aes(x=category, y = Rating)) +
  geom_bar(stat = "identity", fill = 'lightsteelblue3') +
  xlab('Number of Ratings') +
  ylab('Average Rating') +
  geom_text(aes(label=round(Rating, 2)))

#Average Rating & Owned by Users 

ggplot(data %>% filter(Users.Rated >= 10), aes(x = Owned.Users, y = Rating.Average)) +
  geom_point(alpha=.2, lwd=.2, col='Orange') +
  geom_smooth(col="blue", lwd=.7) +
  ylab("Average Rating") +
  xlab("Number of users who own the game") +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^2,10^5))

#Average Rating & complexity

ggplot(data %>% filter(Users.Rated >= 10, Complexity.Average !=0 ), 
       aes(x = Complexity.Average, y = Rating.Average)) + 
  geom_point(alpha=.2, lwd=.2, col="deeppink") +
  geom_smooth(col="blue", lwd=.7) +
  ylab("Average Rating") +
  xlab("Complexity Average")

quantile(data$Complexity.Average, probs = seq(0, 1, 0.25), na.rm = FALSE,
         names = TRUE, type = 7)

data %>%
  mutate(category=cut(Complexity.Average, breaks=c(-Inf, 1.33, 1.97, 2.54, Inf), labels=c("Low","Middle","High", "Very High"))) %>%
  group_by(category) %>%
  summarise(Rating = mean(Rating.Average)) %>%
  ggplot(aes(x=category, y = Rating)) +
  geom_bar(stat = "identity", fill = 'lightsteelblue3') +
  xlab('Complexity Average') +
  ylab('Average Rating') +
  geom_text(aes(label=round(Rating, 2)))

#Average rating by play time

ggplot(data %>% filter(Users.Rated >= 10, Play.Time <= 300), 
       aes(x = Play.Time, y = Rating.Average)) + 
  geom_point(alpha=.2, lwd=.2, col="deeppink") +
  geom_smooth(col="blue", lwd=.7) +
  ylab("Average Rating") +
  xlab("Play Time (min)")

quantile(data$Play.Time, probs = seq(0, 1, 0.25), na.rm = FALSE,
         names = TRUE, type = 7)

data %>%
  mutate(category=cut(Play.Time, breaks=c(-Inf, 30, 45, 90, Inf), labels=c("0-30","30-45","45-90", "over 90"))) %>%
  group_by(category) %>%
  summarise(Rating = mean(Rating.Average)) %>%
  ggplot(aes(x=category, y = Rating)) +
  geom_bar(stat = "identity", fill = 'lightsteelblue3') +
  xlab('Play Time (min)') +
  ylab('Average Rating') +
  geom_text(aes(label=round(Rating, 2)))


#Average Rating & number of players

#min
ggplot(data %>% filter(Users.Rated >= 10, Min.Players !=0, Min.Players <=7 ) %>% group_by(Min.Players), 
       aes(x = Min.Players, y = Rating.Average, fill = as.factor(Min.Players))) + 
  geom_boxplot()  +
  ylab("Average Rating") +
  xlab("Min Number of Players") +
  scale_x_continuous(breaks = c(1:7)) +
  theme(legend.position = "none")

#max
data %>%
  group_by(Max.Players) %>%
  filter(Max.Players != 0, Max.Players <= 10) %>%
  ggplot(aes(x=Max.Players, y = Rating.Average, fill = as.factor(Max.Players))) +
  geom_boxplot() +
  ylab("Average Rating") +
  xlab("Max Number of Players") +
  scale_x_continuous(breaks = c(1:10)) +
  theme(legend.position = "none")
  
#Average Rating & domains 

games_by_mech_dom %>%
  filter(Users.Rated >= 10) %>%
  group_by(Domains) %>%
  ggplot(aes(x=Domains, y = Rating.Average, fill = Domains)) +
  geom_boxplot() +
  coord_flip() +
  ylab('Average Rating') +
  theme(legend.position = "none")

#Average Rating & mechanics

mech_ = games_by_mech_dom %>%
  filter(Users.Rated >= 10) %>%
  group_by(Mechanics) %>%
  summarise(count = n(), Rating.Average= mean(Rating.Average))

sum(mech_$count)
  
ggplot(games_by_mech_dom %>%
         filter(Users.Rated >= 10) %>%
         group_by(Mechanics) %>%
         summarise(count = n(), Rating.Average = mean(Rating.Average)), 
       aes(x = log(count), y = Rating.Average)) + 
  geom_point(alpha=.2, lwd=.2, col="deeppink") +
  geom_smooth(col="blue", lwd=.7) +
  ylab("Average Rating") +
  xlab("Mechanics Count") #very hard to detect any relationship...let's try something else!

quantile(mech_$count, probs = seq(0, 1, 0.25), na.rm = FALSE,
         names = TRUE, type = 7)

mech_ %>%
  mutate(category=cut(count, breaks=c(-Inf, 15, 31, 189.5, Inf), labels=c("Low","Middle","High", "Very High"))) %>%
  group_by(category) %>%
  summarise(Rating = mean(Rating.Average)) %>%
  ggplot(aes(x=category, y = Rating)) +
  geom_bar(stat = "identity", fill = 'lightsteelblue3') +
  xlab('Mechanics Popularity') +
  ylab('Average Rating') +
  geom_text(aes(label=round(Rating, 2)))

  