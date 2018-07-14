library(rworldmap)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
rank_df <- read.csv("Desktop/school/Recess/Data_set_6_Fifa_Rankings/fifa_ranking.csv", stringsAsFactors=F ,sep=",")
#Correct country names in dataset to match those in rworldmap 
rank_df$region <- tolower(rank_df$country_full)
rank_df[rank_df$region == 'antigua and barbuda','region'] <- 'antigua'
rank_df[rank_df$region == 'brunei darussalam','region'] <- 'brunei'
rank_df[rank_df$region == 'cape verde islands','region'] <- 'cape verde'
rank_df[rank_df$region == "china pr","region"] <- 'china'
rank_df[rank_df$region == "côte d'ivoire","region"] <- 'ivory coast'
rank_df[rank_df$region == "congo dr","region"] <- 'democratic republic of the congo'
rank_df[rank_df$region == "chinese taipei","region"] <-"taiwan"
rank_df[rank_df$region == "fyr macedonia","region"] <- 'macedonia'
rank_df[rank_df$region == "ir iran","region"] <- 'iran'
rank_df[rank_df$region == "korea dpr","region"] <- 'north korea'
rank_df[rank_df$region == "korea republic","region"] <- 'south korea'
rank_df[rank_df$region == "kyrgyz republic","region"] <- 'kyrgyzstan'
rank_df[rank_df$region == "northern ireland","region"] <- 'ireland'
rank_df[rank_df$region == "trinidad and tobago","region"] <- 'trinidad'
rank_df[rank_df$region == "england","region"] <- 'uk'

#Confusion in football teams...same as northern ireland thus already included in ireland
rank_df <- subset(rank_df, rank_df$region != "republic of ireland")
#Part of uk thus already mentioned
rank_df <- subset(rank_df, rank_df$region != "wales")

#Filter out most previous rankings and use
recentRank <- data.frame(rank_df %>%
                filter(rank_date=='2018-06-07') %>%
                select(region, country_abrv, confederation, rank, total_points) %>%
                arrange(region))
#Get information for countries from Maps package (Using ggplot2's function tho)
countriesMap <- map_data("world")
countriesMap$region <- tolower(countriesMap$region)
#Mutate-merge 'regions' from Maps with most recent rankings data
results <- data.frame(left_join(countriesMap, recentRank %>% select(confederation, region), by="region"))

###Magic by ggplot2
#Bar plot for the 206 countries
barRank <- recentRank %>% ggplot(aes(x=reorder(region, -rank), y=total_points, fill=confederation)) +
                          geom_bar(stat="identity") + coord_flip() + theme_light(10) +
                          scale_fill_brewer(name="", palette="Paired") +
                          theme(legend.position="None", panel.grid.major.y = element_blank()) + 
                          labs(title="The rank, according to total points, of the 206 countries of FIFA")
#Map showing domain of confederations
map<-ggplot(results) + 
  geom_polygon(aes(x=long, y=lat,group=group, fill = confederation)) + 
  theme_light(10) + 
  theme(
    panel.grid.major = element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(), 
    legend.text=element_text(size=10),legend.key.size = unit(.3, "cm")) +
  scale_fill_brewer(name='',palette='Paired', na.value='black') +
  coord_fixed(1.3) + labs(subtitle='Domain of the six confederations of FIFA')
#Bar plots of individual confederations
individualConf <- recentRank %>% 
  ggplot(aes(x=reorder(region,-rank),y=total_points,fill=confederation)) +
  geom_bar(stat='identity') + coord_flip() + theme_light(10) + 
  scale_fill_brewer(name='',palette='Paired') + 
  theme(legend.position='None', 
        panel.grid.major.y= element_blank()) +
  labs(title='Rank, with respect their total points, of the 206 countries in the FIFA',
       subtitle='as of June 2018') + facet_wrap(~confederation,scales='free')