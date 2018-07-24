library(rworldmap)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(reshape2)
rank_df <- read.csv("Desktop/school/Recess/Data_set_6_Fifa_Rankings/fifa_ranking.csv", stringsAsFactors=F ,sep=",")
#Correct country names in dataset to match those in rworldmap , remove repetitions
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
                select(region, country_abrv, confederation, rank, total_points, rank_change) %>%
                arrange(region))
#Get information for countries from Maps package (Using ggplot2's function tho)
countriesMap <- map_data("world")
countriesMap$region <- tolower(countriesMap$region)
#Mutate-merge 'regions' from Maps with most recent rankings data
results <- data.frame(left_join(countriesMap, recentRank %>% select(confederation, region), by="region"))
#Map showing domain of confederations
map <- ggplot(results) + 
  geom_polygon(aes(x=long, y=lat,group=group, fill = confederation)) + 
  theme_fivethirtyeight(10) + 
  theme(
    panel.grid.major = element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(), 
    legend.text=element_text(size=10),legend.key.size = unit(.3, "cm")) +
  scale_fill_brewer(name='',palette='Paired', na.value='black') +
  coord_fixed(1.3) + labs(subtitle='Domain of the six confederations of FIFA')

#Extract month and year from the rank_date column
rank_df$rank_date_month <- as.Date(sapply(rank_df$rank_date, function(x) paste0(strsplit(x,'-')[[1]][1],'-',strsplit(x,'-')[[1]][2],'-01')))
rank_df$rank_date_year <- as.Date(sapply(rank_df$rank_date, function(x) paste0(strsplit(x,'-')[[1]][1],'-01-01')))
  #Group by month and year
lead <- data.frame(rank_df %>% group_by(rank_date_month, region)%>%
                     summarize(medianRank = median(rank)) %>% filter(medianRank<2))
countries <- sort(unique(lead$region))

#Set colours for each team
colors <- c('#87CEEB','#DC143C','#FFD700','#000080','#E5E5E5','#4169E1','#FFA500','#8B2500')
cols <- data.frame("region"=countries, "color"=colors)
cols$region <- as.character(cols$region)

lead <- data.frame(left_join(lead, cols, by='region'))
fifaLeaders <- ggplot(lead, aes(x=rank_date_month, y=1), legend=TRUE) + 
  geom_histogram(aes(fill=color),stat='identity',color='black',size=.001) +
  scale_fill_identity() +
  theme_fivethirtyeight() + coord_flip() + scale_x_date(date_breaks = "1 year", date_labels =  "%Y") + 
  labs(title='FIFA World Ranking Leaders', subtitle='per month') + 
  theme(legend.position='right',legend.direction='vertical',axis.text.x=element_blank(),panel.grid.major.x=element_blank(),
        legend.text=element_text(size=10), legend.key.size = unit(.3, "cm") ) +
  annotate("text", x=as.Date("1997-01-01"), y = 1.2, label = "BRAZIL", size=3, colour="gray30") +
  annotate("text", x=as.Date("2002-01-01"), y = 1.22, label = "FRANCE", size=3, colour="gray30") +
  annotate("text", x=as.Date("2004-01-01"), y = 1.2, label = "BRAZIL", size=3, colour="gray30") +
  annotate("text", x=as.Date("2007-03-01"), y = 1.2, label = "ITALY", size=3, colour="gray30") +
  annotate("text", x=as.Date("2011-06-01"), y = 1.32, label = "NETHERLANDS", size=3, colour="gray30") +
  annotate("text", x=as.Date("2013-06-01"), y = 1.2, label = "SPAIN", size=3, colour="gray30") +
  annotate("text", x=as.Date("2016-01-01"), y = 1.23, label = "BELGIUM", size=3, colour="gray30") +
  annotate("text", x=as.Date("2016-08-01"), y = 1.27, label = "ARGENTINA", size=3, colour="gray30") +
  annotate("text", x=as.Date("2018-01-01"), y = 1.24, label = "GERMANY", size=3, colour="gray30") +
  ylim(0, 1.5)
#A team's rank over time
countryRank <- rank_df %>% 
  filter(region=='belgium') %>% 
  select(rank, total_points, previous_points, rank_change, cur_year_avg, rank_date_year) %>%
  group_by(rank_date_year) %>% mutate(meanRank = mean(rank,na.rm=T), meanPoints = mean(total_points)) %>%
  ggplot(aes(x=rank_date_year)) + geom_line(aes(y=meanRank),size=2,alpha=0.25, colour="#CC0000") + geom_point(aes(y=meanRank), size=2, colour="#CC0000") +
  theme_fivethirtyeight() + scale_y_reverse() + labs(title = 'Belgium rank')
#A team's total points over time
gTotalPoints <- rank_df %>% filter(region=='spain') %>% 
  select(rank,total_points, previous_points, rank_change, cur_year_avg, rank_date_year) %>%
  group_by(rank_date_year) %>% mutate(meanRank = mean(rank,na.rm=T), meanPoints = mean(total_points))%>% 
  ggplot(aes(x=rank_date_year)) + geom_line(aes(y=meanPoints),size=2,alpha=0.25, colour='#006666') + geom_point(aes(y=meanPoints),size=2, colour='#006666') +
  labs(title= 'Spain\'s total points') +
  geom_curve(aes(x = as.Date("2007-01-01"), y = 500, xend = as.Date("2009-11-01"), yend =25),curvature = -.05,arrow = arrow(length = unit(0.02, "npc")),color='#0072B2',size=.25) + 
  annotate("text", x=as.Date("2006-01-01"), y = 600, label = "Fifa ranking system update", size=4, colour="#0072B2")

#Confederation performance (average points and rank)
#Filter out records for last 
evalPeriod <- data.frame(rank_df %>%
                           filter(rank_date>='2014-06-07' & rank_date<='2018-06-07') %>%
                           select(region, country_abrv, confederation, rank, total_points) %>%
                           arrange(region))
#Bar plot for Confederation average total points
confPoints <- aggregate(total_points~confederation, evalPeriod, mean)
gPoints <- confPoints %>% ggplot(aes(x=confederation, y=total_points, fill=confederation)) +
            geom_bar(stat='identity') +
            theme_fivethirtyeight() + theme(legend.position='None') +
            scale_fill_brewer(palette='Paired') + labs(title='Confederation average points')
#Box plot for confederation average total points
gPoints <- recentRank %>% group_by(confederation) %>% ggplot(aes(x=reorder(confederation, total_points, FUN=mean),y=total_points,fill=confederation)) +
  geom_boxplot(alpha=.75,size=.25) + geom_jitter(shape=16,position=position_jitter(0.2),size=1,alpha=.25) +
  theme_fivethirtyeight() + theme(legend.position='None') +
  scale_fill_brewer(palette='Paired') + coord_flip() + labs(title='Confederation average points')

#Bar plot for confederation average rank
confRank <- aggregate(rank~confederation, evalPeriod, mean)
gRankBar <- confRank %>% ggplot(aes(x=confederation, y=rank, fill=confederation)) + geom_bar(stat = "identity")+
          theme_fivethirtyeight() + theme(legend.position='None') +
          scale_fill_brewer(palette='Paired') + labs(title='Confederation average ranking')
#Box plot for confederation average rank
gRankBox <- recentRank %>% group_by(confederation) %>% ggplot(aes(x=reorder(confederation, -rank, FUN=mean),y=rank,fill=confederation)) + geom_boxplot(alpha=.75,size=.25) + geom_jitter(shape=16,position=position_jitter(0.2),size=1,alpha=.25) +
  theme_fivethirtyeight() + theme(legend.position='None') +
  scale_fill_brewer(palette='Paired') + coord_flip() + labs(title='Confederation average ranking')

#Number of member teams
memberTeams <- as.data.frame(table(recentRank$confederation)) %>% ggplot(aes(x=Var1, y=Freq, fill=Var1)) + geom_point(size=4)+
                geom_segment(aes(x=Var1, 
                                 xend=Var1, 
                                 y=0, 
                                 yend=Freq)) + 
              theme_fivethirtyeight() + 
              theme(legend.position = 'None',
                axis.text = element_text(size = 14)
              ) +
              scale_fill_brewer(palette = 'Paired')
#Top 10 teams
topTen <- recentRank[order(recentRank$rank),]
barRankTop <- topTen[1:10,]  %>% ggplot(aes(x=reorder(region, -rank), y=total_points, fill=confederation)) +
  geom_bar(stat="identity") + coord_flip() + theme_light() +
  scale_fill_brewer(palette="Paired") +
  theme(axis.text=element_text(size=13), legend.position="None", panel.grid.major.y = element_blank()) +
  labs(title="The rank, according to total points, of the top 10 member countries of FIFA")
#Bottom 10 teams
barRankTail <- tail(topTen, 10)  %>% ggplot(aes(x=reorder(region, -rank), y=total_points, fill=confederation)) +
  geom_bar(stat="identity") + coord_flip() + theme_light() +
  scale_fill_brewer(palette="Paired") +
  theme(axis.text=element_text(size=13), legend.position="None", panel.grid.major.y = element_blank()) +
  labs(title="The rank, according to total points, of the bottom 10 member countries of FIFA")
#Worst movers
topTen <- recentRank[order(recentRank$rank_change),]
rankChangeBottom <- topTen[1:5,] %>% ggplot(aes(x=reorder(region, rank_change), y=rank_change, fill=confederation))+
  geom_bar(stat="identity") + theme_light()+
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text = element_text(size=13), legend.position = "None",
        axis.line = element_line(colour = "red", size = 1, linetype = "solid"),
        panel.grid.major.y=element_line(colour = '#A8A8A8')
        )+ 
  scale_y_reverse(name = "Rank Change", breaks = seq(0, -20, -2)) +
  scale_x_discrete(name="Country")+
  labs(title="The worst movers as of June, 2018")
#Best movers
rankChangeTop <- tail(topTen,10) %>% ggplot(aes(x=reorder(region, rank_change), y=rank_change, fill=confederation))+
  geom_bar(stat="identity") + theme_light()+
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text = element_text(size=11), legend.position = "None",
        axis.line = element_line(colour = "darkgreen", size = 1, linetype = "solid"),
        panel.grid.major.y=element_line(colour = '#A8A8A8')
  )+ 
  scale_y_continuous(name = "Rank Change", breaks = seq(0, 22, 2)) +
  scale_x_discrete(name="Country")+
  labs(title="The best movers as of June, 2018")