library(rworldmap)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(reshape2)
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
# barRank + annotation_custom(ggplotGrob(map),xmin=0, xmax=100,ymin=300,ymax=1700)
# #Bar plots of individual confederations
# individualConf <- recentRank %>%
#   ggplot(aes(x=reorder(region,-rank),y=total_points,fill=confederation)) +
#   geom_bar(stat='identity', width=0.7) + coord_flip() + theme_fivethirtyeight(10) +
#   scale_fill_brewer(name='',palette='Paired') +
#   theme(legend.position='None',
#         panel.grid.major.y= element_blank()) +
#   labs(title='Rank, with respect their total points, of the 206 countries in the FIFA',
#        subtitle='as of June 2018') + facet_wrap(~confederation,scales='free')
#Top ranked teams
  #Pick out month/year for the rank_date column
rank_df$rank_date_month <- as.Date(sapply(rank_df$rank_date, function(x) paste0(strsplit(x,'-')[[1]][1],'-',strsplit(x,'-')[[1]][2],'-01')))
rank_df$rank_date_year <- as.Date(sapply(rank_df$rank_date, function(x) paste0(strsplit(x,'-')[[1]][1],'-01-01')))
  #Group by month and year
lead <- data.frame(rank_df %>% group_by(rank_date_month, region)%>%
                     summarize(medianRank = median(rank)) %>% filter(medianRank<2))
countries <- sort(unique(lead$region))
colors <- c('#87CEEB','#DC143C','#FFD700','#000080','#E5E5E5','#4169E1','#FFA500','#8B2500')
cols <- data.frame("region"=countries, "color"=colors)
cols$region <- as.character(cols$region)

lead <- data.frame(left_join(lead, cols, by='region'))
fifaLeaders <- lead %>% ggplot(aes(x=rank_date_month,y=1)) + geom_histogram(aes(fill=color),stat='identity',color='black',size=.001) + 
  scale_fill_identity() + theme_fivethirtyeight() + coord_flip() + scale_x_date(date_breaks = "1 year", date_labels =  "%Y") + 
  labs(title='FIFA World Ranking Leaders', subtitle='per month') + 
  theme(legend.position='right',legend.direction='vertical',axis.text.x=element_blank(),panel.grid.major.x=element_blank()) + 
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
###Magic by ggplot2
# #Bar plot for the 206 countries
# barRank <- recentRank %>% ggplot(aes(x=reorder(region, -rank), y=total_points, fill=confederation)) +
#                           geom_bar(stat="identity") + coord_flip() + theme_light() +
#                           scale_fill_brewer(palette="Paired") +
#                           theme(axis.text=element_text(size=3), legend.position="None", panel.grid.major.y = element_blank()) + 
#                           labs(title="The rank, according to total points, of the 206 countries of FIFA")

#Confederation performance (average points and position)
# gPoints <- recentRank %>% group_by(confederation) %>% ggplot(aes(x=reorder(confederation, total_points, FUN=mean),y=total_points,fill=confederation)) + 
#             geom_boxplot(alpha=.75,size=.25) + geom_jitter(shape=16,position=position_jitter(0.2),size=1,alpha=.25) +
#             theme_fivethirtyeight() + theme(legend.position='None') + 
#             scale_fill_brewer(palette='Paired') + coord_flip() + labs(title='Confederation average points')
evalPeriod <- data.frame(rank_df %>%
                           filter(rank_date>='2014-06-07' & rank_date<='2018-06-07') %>%
                           select(region, country_abrv, confederation, rank, total_points) %>%
                           arrange(region))
# Confederations
caf <- data.frame(evalPeriod %>% filter(confederation=="CAF") %>% select(region, rank, total_points))
afc <- data.frame(evalPeriod %>% filter(confederation=="AFC") %>% select(region, rank, total_points))
uefa <- data.frame(evalPeriod %>% filter(confederation=="UEFA") %>% select(region, rank, total_points))
ofc <- data.frame(evalPeriod %>% filter(confederation=="OFC") %>% select(region, rank, total_points))
concacaf <- data.frame(evalPeriod %>% filter(confederation=="CONCACAF") %>% select(region, rank, total_points))
conmebol <- data.frame(evalPeriod %>% filter(confederation=="CONMEBOL") %>% select(region, rank, total_points))

totP <- mean(uefa[["total_points"]])

gPoints <- evalPeriod %>% group_by(confederation) %>% ggplot(aes(x=reorder(confederation, total_points), y=mean(confederation[,"total_points"]), fill=confederation)) +
            geom_bar(stat='identity') +
            theme_fivethirtyeight() + theme(legend.position='None') +
            scale_fill_brewer(palette='Paired') + labs(title='Confederation average points')

gRank <- recentRank %>% group_by(confederation) %>% ggplot(aes(x=reorder(confederation, -rank, FUN=mean),y=rank,fill=confederation)) + geom_boxplot(alpha=.75,size=.25) + geom_jitter(shape=16,position=position_jitter(0.2),size=1,alpha=.25) +
          theme_fivethirtyeight() + theme(legend.position='None') + 
          scale_fill_brewer(palette='Paired') + coord_flip() + labs(title='Confederation average ranking')

#