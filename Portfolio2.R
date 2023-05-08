library(tidyverse)
library(ggplot2)
library(gganimate)
library(ggridges)

data <- read.csv("games_joined_data.csv")
head(data)


## Mapping user rating vs units sold
ggplot( data = data ) + geom_jitter( mapping=aes( x=user_score, y=games_sold ) )+
  geom_smooth( mapping=aes( x=user_score, y=games_sold ) )+
  xlim(0, 10) + ylim(0, 20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs( title="Video Game Units Sold vs. User Rating", subtitle="1977-2020, in Millions",
  caption="Data Source: https://bit.ly/43JPCBB",
  x = "User Rating", y = "Games Sold")

## Mapping critic rating vs units sold
ggplot( data = data ) + geom_jitter( mapping=aes( x=critic_score, y=games_sold ) )+
  geom_smooth( mapping=aes( x=critic_score, y=games_sold ) )+
  xlim(0, 10) + ylim(0, 20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs( title="Video Game Units Sold vs. Critic Rating", subtitle="1977-2020, in Millions",
        caption="Data Source: https://bit.ly/43JPCBB",
        x = "Critic Rating", y = "Games Sold")

## Mapping combined rating vs units sold
ggplot( data = data ) + geom_jitter( mapping=aes( x=combined_score, y=games_sold ) )+
  geom_smooth( mapping=aes( x=combined_score, y=games_sold ) )+
  xlim(0, 10) + ylim(0, 20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs( title="Video Game Units Sold vs. Combined Rating", subtitle="1977-2020, in Millions",
        caption="Data Source: https://bit.ly/43JPCBB",
        x = "Combined Rating", y = "Games Sold")

## Mapping platform units to year
ggplot(data, aes(x = year, y = games_sold)) +
  geom_line() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~platform, ncol = 5)

## Mapping year and units sold
ggplot(data = data, aes(x = year, y = games_sold)) + 
  geom_col(data = subset(data, year != 2008), fill = "gray") +
  geom_col(data = subset(data, year == 2008), fill = "blue") +
  labs(title = "Video Game Units Sold by Year", subtitle = "1977-2020, in Millions", 
       caption = "Data Source: https://bit.ly/43JPCBB",
       x = "Year", y = "Games Sold") +
  annotate("text", x = 2015, y = 1800, 
           label = paste(sum(data$games_sold[data$year == 2008]), "million (2008)"),
           vjust = -1.5, color = "blue", fontface = "bold") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


## Mapping year and rating
ggplot( data=data ) + geom_smooth( mapping=aes( x=year, y=combined_score ) )+
  labs( title="Video Game Combined Rating vs. Year", subtitle="1977-2020, in Millions",
        caption="Data Source: https://bit.ly/43JPCBB" )

## Mapping year and units sold and rating
ggplot( data=data ) + geom_smooth( mapping=aes( x=year, y=combined_score, color="Red" ) )+
  geom_smooth( mapping=aes( x=year, y=games_sold ) )+
  labs( title="Video Game Combined Rating and Units Sold vs. Year", subtitle="1977-2020, in Millions",
        caption="Data Source: https://bit.ly/43JPCBB" )

## GG Animate Scatterplot
ggplot( data=data , aes( critic_score, user_score, size = games_sold, color = platform)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = 'Year: {frame_time}', x = 'User Score', y = 'Critic Score') +
  transition_time(year) +
  ease_aes('linear')
anim_save("271-ggplot2-animated-gif-chart-with-gganimate1.gif")

## Histogram of Critic Ratings
ggplot(data=data, aes(x=critic_score)) + geom_histogram() +
  labs( title="Critic Ratings Histogram", subtitle="1977-2020",
        caption="Data Source: https://bit.ly/43JPCBB",
        x = "Critic Rating", y = "Count" )

## Histogram of user Ratings
ggplot(data=data, aes(x=user_score)) + geom_histogram() +
  labs( title="User Ratings Histogram", subtitle="1977-2020",
        caption="Data Source: https://bit.ly/43JPCBB",
        x = "User Rating", y = "Count" )

## Histogram of Combined Ratings
ggplot(data=data, aes(x=combined_score)) + geom_histogram() +
  labs( title="Combined Ratings Histogram", subtitle="1977-2020",
        caption="Data Source: https://bit.ly/43JPCBB",
        x = "Combined Rating", y = "Count" )

## Contour plot of user rating v critic rating
ggplot(data=data, aes(user_score, critic_score)) + geom_density_2d_filled(show.legend = FALSE, bins=155) +
  coord_cartesian(expand = FALSE) + labs(x = "User Rating", y = "Critic Rating")
  scale_colour_brewer(palette="Blues")