library(shinydashboard)
library(shiny)
library(ggplot2)
library(DT)
library(rworldmap)
library(RColorBrewer)
library(dplyr)
library(gridExtra)
library(reshape2)

WCR_data <- read.csv("fifa_ranking.csv")
country <- levels(WCR_data$country_full)
all_countries_sorted <- country[ order(country)]
#for the select input of confederations
conf <-levels(WCR_data$confederation)
all_conf <- c("All confederations",conf)

#Correct country names in dataset to match those in rworldmap 
WCR_data$region <- tolower(WCR_data$country_full)
WCR_data[WCR_data$region == 'antigua and barbuda','region'] <- 'antigua'
WCR_data[WCR_data$region == 'brunei darussalam','region'] <- 'brunei'
WCR_data[WCR_data$region == 'cape verde islands','region'] <- 'cape verde'
WCR_data[WCR_data$region == "china pr","region"] <- 'china'
WCR_data[WCR_data$region == "côte d'ivoire","region"] <- 'ivory coast'
WCR_data[WCR_data$region == "congo dr","region"] <- 'democratic republic of the congo'
WCR_data[WCR_data$region == "chinese taipei","region"] <-"taiwan"
WCR_data[WCR_data$region == "fyr macedonia","region"] <- 'macedonia'
WCR_data[WCR_data$region == "ir iran","region"] <- 'iran'
WCR_data[WCR_data$region == "korea dpr","region"] <- 'north korea'
WCR_data[WCR_data$region == "korea republic","region"] <- 'south korea'
WCR_data[WCR_data$region == "kyrgyz republic","region"] <- 'kyrgyzstan'
WCR_data[WCR_data$region == "northern ireland","region"] <- 'ireland'
WCR_data[WCR_data$region == "trinidad and tobago","region"] <- 'trinidad'
WCR_data[WCR_data$region == "england","region"] <- 'uk'

#Confusion in football teams...same as northern ireland thus already included in ireland
WCR_data <- subset(WCR_data, WCR_data$region != "republic of ireland")
#Part of uk thus already mentioned
WCR_data <- subset(WCR_data, WCR_data$region != "wales")

#Filter out most previous rankings and use
recentRank <- data.frame(WCR_data %>%
                           filter(rank_date=='2018-06-07') %>%
                           select(region, country_abrv, confederation, rank, total_points,rank_change) %>%
                           arrange(region))
#Get information for countries from Maps package (Using ggplot2's function tho)
countriesMap <- map_data("world")
countriesMap$region <- tolower(countriesMap$region)
#Pick out month/year for the rank_date column
rank_df$rank_date_month <- as.Date(sapply(rank_df$rank_date, function(x) paste0(strsplit(x,'-')[[1]][1],'-',strsplit(x,'-')[[1]][2],'-01')))
rank_df$rank_date_year <- as.Date(sapply(rank_df$rank_date, function(x) paste0(strsplit(x,'-')[[1]][1],'-01-01')))
lead <- data.frame(rank_df %>% group_by(rank_date_month, region)%>%
                     summarize(medianRank = median(rank)) %>% filter(medianRank<2))
countries <- sort(unique(lead$region))
colors <- c('#87CEEB','#DC143C','#FFD700','#000080','#000000','#00cc66','#FFA500','#8B2500')
cols <- data.frame("region"=countries, "color"=colors)
cols$region <- as.character(cols$region)
lead <- data.frame(left_join(lead, cols, by='region'))
#Mutate-merge 'regions' from Maps with most recent rankings data
results <- data.frame(left_join(countriesMap, recentRank %>% select(confederation, region), by="region"))
#Filter out rankings for past 4 years
evalPeriod <- data.frame(rank_df %>%
                           filter(rank_date>='2014-06-07' & rank_date<='2018-06-07') %>%
                           select(region, country_abrv, confederation, rank, total_points) %>%
                           arrange(region))
confPoints <- aggregate(total_points~confederation, evalPeriod, mean)
topTen <- recentRank[order(recentRank$rank),]
topFive <- recentRank[order(recentRank$rank_change),]
topConfedTeam <- aggregate(total_points~confederation, recentRank, max)
topConfedTeam$region <- c("Australia", "Tunisia", "Mexico", "Brazil", "New Zealand", "Germany")
# user interface

ui <- dashboardPage(
  dashboardHeader(title = "World Cup Anaylzer",
                  titleWidth = 300
  ),
  skin = "red",
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Home",
               tabName = "Home",
               icon = icon("home")
      ),
      menuItem("Team Analysis",
               tabName = "Team_Analysis",
               icon = icon("sort", class="fa-sort")
               
      ),
      menuItem("Team seeding",
               tabName = "Team_seeding",
               icon = icon("home", class="fas fa-spinner")
      ),
      menuItem("Visualisations",
               tabName = "Visualisations",
               icon = icon("bar-chart-o", class="fa-chart-pie")
      ),
      menuItem("Summary",
               tabName = "Summary",
               icon = icon("list-alt")
      )
    )
    
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      
    ),
    
    tabItems(
      tabItem(tabName = "Home",
              fluidRow(
                htmlOutput(outputId = "FIFA_desc"),
                plotOutput(outputId = "FIFA_nations"),
                htmlOutput(outputId = "FIFA_rank_desc"),
                hr()
              )
      ),
      tabItem(tabName = "Team_Analysis",
              fluidRow(
                widths = c(12, 12),
                h1("Team Performance Analysis"),
                sidebarLayout(
                  mainPanel(
                    
                    fluidRow(
                      htmlOutput(outputId = "title"),
                      valueBoxOutput("vbox1", width = 4),
                      valueBoxOutput("vbox2", width = 4),
                      valueBoxOutput("vbox3", width = 4)
                      
                    ),
                    fluidRow(
                      valueBoxOutput("vbox4", width = 4),
                      valueBoxOutput("vbox5", width = 4),
                      valueBoxOutput("vbox6", width = 4)
                    ),
                    fluidRow(
                      valueBoxOutput("vbox7", width = 6),
                      valueBoxOutput("vbox8", width = 6)
                      
                      
                    ),
                    fluidRow(
                      htmlOutput(outputId = "Conclusion")
                    )
                  ),
                  sidebarPanel(
                    selectInput(inputId = "country",
                                label ="Select a country",
                                choices = all_countries_sorted
                    ),
                    htmlOutput(outputId = "prediction_table")
                  )
                  
                ),
                fluidRow(
                  column(
                    shiny::dataTableOutput(outputId = "team_table"),width = 12
                  )
                ),
                tabsetPanel(
                  tabPanel("plot1",
                           fluidRow(
                             box(
                               title = h3(class ="plot-title", "A plot of total points against rank dates"),
                               background = "maroon", solidHeader = TRUE,
                               sliderInput(inputId = "rank_year",
                                           label = "Slide to select a year",
                                           min = 2015, max = 2018, value = 2018,step = 1,
                                           width = '400px'
                               ),
                               plotOutput(outputId = "team_plot1"),
                               width = 12
                             )
                           )
                           
                  ),
                  tabPanel("plot2",
                           fluidRow(
                             box(
                               h3("A plot of total points against rank "),
                               plotOutput(outputId = "team_plot2"),
                               background = "maroon", solidHeader = TRUE,
                               width = 12
                             )
                           )
                  ),
                  tabPanel("plot3",
                           fluidRow(
                             box(
                               h3("A plot of total points against the rank date overtime"),
                               plotOutput(outputId = "team_plot3"),
                               background = "maroon",solidHeader = TRUE,
                               width = 12
                             ),
                             box(
                               htmlOutput(outputId = "plot3_expl"),
                               width = 12
                             )
                           )
                  ),
                  tabPanel("plot4",
                           fluidRow(
                             box(
                               h3("A plot of rank against rank date overtime"),
                               plotOutput(outputId = "team_plot4"),
                               background = "maroon",solidHeader = TRUE,
                               width = 12
                             )
                           )
                  )
                )
              )
      ),
      tabItem(tabName = "Team_seeding",
              fluidRow(
                
                sidebarLayout(
                  mainPanel(
                    h1("Team Seeding"),
                    htmlOutput(outputId = "seeding_sub_title")
                  ),
                  sidebarPanel(
                    selectInput(inputId = "confederation",
                                label ="Select a confederation",
                                choices = all_conf
                    )
                  )
                ),
                fluidRow(
                  column(
                    htmlOutput(outputId = "seeding_title"), width = 12
                  ),
                  column(
                    DT::dataTableOutput(outputId = "Seeding_team_table"), width = 12
                  ),
                  column(
                    DT::dataTableOutput(outputId = "confederations_table"), width = 12
                  )
                ),
                conditionalPanel(
                  condition = "input.confederation == 'All confederations'",
                  box(width = NULL,
                      uiOutput("scale")
                  ),
                  hr(),
                  fluidRow(
                    column(DT::dataTableOutput(outputId = "selected_teams_pot1"),width = 3),
                    column(DT::dataTableOutput(outputId = "selected_teams_pot2"),width = 3),
                    column(DT::dataTableOutput(outputId = "selected_teams_pot3"),width = 3),
                    column(DT::dataTableOutput(outputId = "selected_teams_pot4"),width = 3)
                  ),
                  hr(),
                  fluidRow(
                    
                    column( actionButton("seed", "Click to seed teams"),offset = 5,width = 12,
                            style="color: #fff; border-color: #2e6da4; font-size:20px"
                    )
                    
                  ),
                  hr(),
                  
                  fluidRow(
                    column(width = 3, htmlOutput("seed_table1")),
                    column(width = 3, htmlOutput("seed_table2")),
                    column(width = 3, htmlOutput("seed_table3")),
                    column(width = 3, htmlOutput("seed_table4"))
                  ),
                  hr(),
                  fluidRow(
                    column(width = 3, htmlOutput("seed_table5")),
                    column(width = 3, htmlOutput("seed_table6")),
                    column(width = 3, htmlOutput("seed_table7")),
                    column(width = 3, htmlOutput("seed_table8"))
                  ),
                  hr()
                )
              )
      ),
      tabItem(tabName = "Visualisations",
              fluidRow(
                plotOutput(outputId = "plot_name1"),
                htmlOutput(outputId = "legend"),#
                htmlOutput(outputId = "Desc_plot_name1"),
                plotOutput(outputId = "plot_name2"),
                htmlOutput(outputId = "Desc_plot_name2"),
                plotOutput(outputId = "plot_name3"),
                htmlOutput(outputId = "Desc_plot_name3"),
                plotOutput(outputId = "plot_name4"),
                htmlOutput(outputId = "Desc_plot_name4"),
                plotOutput(outputId = "plot_name5"),
                htmlOutput(outputId = "Desc_plot_name5"),
                plotOutput(outputId = "plot_name6"),
                htmlOutput(outputId = "Desc_plot_name6"),
                plotOutput(outputId = "plot_name7"),
                htmlOutput(outputId = "Desc_plot_name7"),
                plotOutput(outputId = "plot_name8"),
                htmlOutput(outputId = "Desc_plot_name8"),
                plotOutput(outputId = "plot_name9"),
                htmlOutput(outputId = "Desc_plot_name9"),
                plotOutput(outputId = "plot_name10"),
                htmlOutput(outputId = "Desc_plot_name10"),
                plotOutput(outputId = "plot_name11"),
                htmlOutput(outputId = "Desc_plot_name11"),
                plotOutput(outputId = "plot_name12"),
                htmlOutput(outputId = "Desc_plot_name12")
              )
      ),
      tabItem(tabName = "Summary",
              fluidRow(
                htmlOutput(outputId = "Summary")
              )
      )
      
    )
  )
)

server <-function(input,output){
  # Here we write reactive expressions that establish a 
  #relationship between input and output from the UI
  
  #Home output
  
  output$FIFA_desc <- renderUI({
    HTML("<h1>FIFA</h1>",
         "<p>Fédération Internationale de Football Association(FIFA) is an international governing body of association football.  FIFA is responsible for the organization of 
         football's major international tournaments, notably the World Cup which commenced in 1930 and the Women's World Cup which commenced in 1991.</p>",
         "<p>FIFA was founded in 1904 to oversee international competition among the national associations, Headquartered in Zürich, its membership now comprises 211 national 
         associations.Member countries must each also be members of one of the six regional confederations into which the world is divided: Africa, Asia, Europe, North & Central 
         America and the Caribbean, Oceania, and South America.</p>",
         "<h3>Map of the members of FIFA according to their confederation</h3>"
         
    )
  })
  
  output$FIFA_nations <- renderPlot({
    ggplot(results) + 
      geom_polygon(aes(x=long, y=lat,group=group, fill = confederation)) + 
      theme_light(10) + 
      theme(
        panel.grid.major = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        legend.text=element_text(size=10),legend.key.size = unit(.3, "cm")) +
      scale_fill_brewer(name='',palette='Paired', na.value='black') +
      coord_fixed(1.3) + labs(subtitle='Domain of the six confederations of FIFA')
  })
  
  output$FIFA_rank_desc <- renderUI({
    HTML("<h1>FIFA WORLD RANKINGS</h1>",
         "<p>The FIFA World Ranking is a ranking system for men's national teams in association football, currently led by Germany as of june 2018. The teams of the member nations of FIFA, 
         football's world governing body, are ranked based on their game results with the most successful teams being ranked highest.A points system is used, with points being awarded based 
         on the results of all FIFA-recognised full international matches.</p>",
         "<p>Over the years,the ranking system has been revamped on several occasions, generally responding to criticism that the preceding calculation method did not effectively reflect the 
         relative strengths of the national teams. The latest calculation method will be used for the first time starting on 19 July 2018 and reflect matches held during the 2018 FIFA World Cup.</p>"
    )
  })
  
  #Team perfomance analysis
  output$title <- renderUI({
    countries_filter <-subset(WCR_data, country_full ==input$country & as.Date(rank_date) > as.Date("2014-06-07"))
    HTML("<p>The evaluation of the performance of the national teams is based on the FIFA rankings in the past 4 years due to the ranking procedure of FIFA where the total
         points gained by a national team are got by adding the teams average points from the past 4 years. This implicates that the points obtained in years before the current previous
         4 years donot count on the national team FIFA rankings, they are just history.
         </p>",
         "<h2>Details</h2>",
         "<ul>
         <li>Country Name:<b>", input$country,"</b> </li>
         <li>Country ABRV:<b>",as.character(countries_filter[1,5]),"</b> </li>
         <li>Confederation:<b>",as.character(countries_filter[1,4]),"</b> </li>
         </ul>",
         "<h4><b>",input$country,"</b> performance analysis in the past 4 years</h4>",
         "<h3>Team Statistics</h3>"
         
    )
  })
  
  
  
  #according to total points
  output$vbox1 <- renderValueBox({
    country <- subset(WCR_data, country_full == input$country & as.Date(rank_date) > as.Date("2014-06-01"))
    country_points <- country[,"total_points"]
    valueBox(max(country_points),subtitle = "Best points", width = 2, icon = icon("arrow-up"),color = "purple")
  })
  
  output$vbox2 <- renderValueBox({ 
    country <- subset(WCR_data, country_full == input$country & as.Date(rank_date) > as.Date("2014-06-01"))
    country_points <- country[,"total_points"]
    valueBox( min(country_points),"Worst points", width = 2, icon = icon("arrow-down"),color = "purple")
  })
  
  output$vbox3 <- renderValueBox({ 
    country <- subset(WCR_data, country_full == input$country & as.Date(rank_date) > as.Date("2014-06-01"))
    country_points <- country[,"total_points"]
    valueBox( round(mean(country_points)),"Average points", width = 5, icon = icon("arrow-right"),color = "purple")
  })
  
  #According to rank
  output$vbox4 <- renderValueBox({ 
    country <- subset(WCR_data, country_full == input$country & as.Date(rank_date) > as.Date("2014-06-01"))
    country_ranks <- country[,"rank"]
    valueBox( min(country_ranks),"best rank", width = 2, icon = icon("arrow-up"),color = "yellow")
  })
  
  output$vbox5 <- renderValueBox({
    country <- subset(WCR_data, country_full == input$country & as.Date(rank_date) > as.Date("2014-06-01"))
    country_ranks <- country[,"rank"]
    valueBox(max(country_ranks),subtitle = "Worst rank", width = 2, icon = icon("arrow-down"),color = "yellow")
  })
  
  output$vbox6 <- renderValueBox({ 
    country <- subset(WCR_data, country_full == input$country & as.Date(rank_date) > as.Date("2014-06-01"))
    country_ranks <- country[,"rank"]
    valueBox( round(mean(country_ranks)),"Average rank", width = 5, icon = icon("arrow-right"),color = "yellow")
  })
  
  #According to rank change
  output$vbox7 <- renderValueBox({ 
    country_gain <- subset(WCR_data, country_full == input$country & rank_change > 0)
    country_gained  <- country_gain[,"rank_change"]
    valueBox( round(sum(country_gained)),"Rank gain", icon = icon("arrow-up"),color = "green")
  })
  output$vbox8 <- renderValueBox({ 
    country_depreciation <- subset(WCR_data, country_full == input$country & rank_change < 0)
    country_depreciated <- country_depreciation[,"rank_change"]
    valueBox( round(sum(country_depreciated )),"Rank depreciation", icon = icon("arrow-down"),color = "green")
  })
  
  output$Conclusion <- renderUI({
    country_gain <- subset(WCR_data, country_full == input$country & rank_change > 0)
    country_gained  <- sum(country_gain[,"rank_change"])
    
    country_depreciation <- subset(WCR_data, country_full == input$country & rank_change < 0)
    country_depreciated <- sum(country_depreciation[,"rank_change"])
    if(abs(country_gained) < abs(country_depreciated)){
      HTML(
        "<p>Comparing the team rank change in the previous 4 years ",input$country,"'s performance has <b>deteriorated</b></p>",
        "<h2>Below is table showing all the statistics for ",input$country," since june-2014 upto june-2018</h2>"
      )
    }else if(abs(country_gained) > abs(country_depreciated)){
      HTML(
        "<p>Comparing the team rank change in the previous 4 years ",input$country,"'s performance has <b>improved</b></p>",
        "<h2>Below is table showing all the statistics for ",input$country," since june-2014 upto june-2018</h2>"
      )
    }else{
      HTML(
        "<p>Comparing the team rank change in the previous 4 years ",input$country,"'s performance has not <b>changed</b></p>",
        "<h2>Below is table showing all the statistics for ",input$country," since june-2014 upto june-2018</h2>"
      )
    }
    
  })
  
  
  
  output$prediction_table <- renderUI({
    country <- subset(WCR_data, country_full == input$country & as.Date(rank_date) > as.Date("2014-06-01"))
    country_ranks <- country[,"rank"]
    country_Conf <- country[1,"confederation"]
    avg_rank <- round(mean(country_ranks))
    
    if(country_Conf =="AFC"){
      if(avg_rank < 65){
        best_pos <-"Round 2"
      }
      else if(avg_rank < 75){
        best_pos <-"Round 1"
      }
      else{
        best_pos <-"Non Qualified"
      }
    }
    
    else if(country_Conf =="CAF"){
      if(avg_rank < 36){
        best_pos <-"Quater finalist"
      }
      else if(avg_rank < 42){
        best_pos <-"Round 2"
      }
      else if(avg_rank < 67){
        best_pos <-"Round 1"
      }
      else{
        best_pos <-"Non Qualified"
      }
    }
    
    else if(country_Conf =="CONCACAF"){
      
      if(avg_rank < 22){
        best_pos <-"Quater finalist"
      }
      else if(avg_rank < 36){
        best_pos <-"Round 2"
      }
      else if(avg_rank < 65){
        best_pos <-"Round 1"
      }
      else{
        best_pos <-"Non Qualified"
      }
    }
    
    else if(country_Conf =="CONMEBOL"){
      if(avg_rank < 4){
        best_pos <-"Winner"
      }
      else if(avg_rank < 6){
        best_pos <-"Winner"
      }
      else if(avg_rank < 10){
        best_pos <-"Fourth Place"
      }
      else if(avg_rank < 15){
        best_pos <-"Quater finalist"
      }
      else{
        best_pos <-"Non Qualified"
      }
    }
    
    else if(country_Conf =="OFC"){
      best_pos <-"Non Qualified"
    }
    
    else if(country_Conf =="UEFA"){
      if(avg_rank < 4){
        best_pos <-"Winner"
      }
      else if(avg_rank < 12){
        best_pos <-"Runner up"
      }
      else if(avg_rank < 18){
        best_pos <-"Third Place"
      }
      else if(avg_rank < 24){
        best_pos <-"Fouth Place"
      }
      else if(avg_rank < 30){
        best_pos <-"Quater finalist"
      }
      else if(avg_rank < 36){
        best_pos <-"Round 2"
      }
      else if(avg_rank < 45){
        best_pos <-"Round 1"
      }
      else{
        best_pos <-"Non Qualified"
      }
    }
    
    
    
    tags$table(style = "border: 1px solid black; padding: 1%; width: 100%;",
               class ="table",
               tags$thead(
                 tags$tr(
                   tags$th(
                     colspan ="2",
                     "Prediction")
                 ),
                 tags$tr(
                   tags$th("Tournament"),
                   tags$th("Best Perfomance")
                 )
               ),
               tags$tbody(
                 tags$tr(
                   tags$td("World Cup 2018"),
                   tags$td(best_pos)
                 )
               )
    )
  })
  
  output$team_table <- shiny::renderDataTable({
    countries_filter <-subset(WCR_data, country_full ==input$country & as.Date(rank_date) > as.Date("2014-06-01"))
    country_filter <-countries_filter[,c(1,3,6,7,8,9,10,11,12,13,14,15,16)]
  },options = list(scrollX = TRUE,pageLength = 10,order = list(1, 'desc')))
  
  output$team_plot1 <- renderPlot({
    if(input$rank_year == '2015'){
      mindate <- '2014-06-01'
      maxdate <- '2015-06-01'
    }
    else if(input$rank_year == '2016'){
      mindate <- '2015-06-01'
      maxdate <- '2016-06-01'
    }
    else if(input$rank_year == '2017'){
      mindate <- '2016-06-01'
      maxdate <- '2017-06-01'
    } else{
      mindate <- '2017-06-01'
      maxdate <- '2018-06-01'
    }
    countries_filter <-subset(WCR_data, country_full ==input$country & as.Date(rank_date) > as.Date(mindate) & as.Date(rank_date) < as.Date(maxdate))
    ggplot(countries_filter, aes(x =rank_date , y = total_points)) +
      geom_point()
  })
  
  output$team_plot2 <- renderPlot({
    countries_filter <-subset(WCR_data, country_full ==input$country & as.Date(rank_date) > as.Date("2014-06-07"))
    ggplot(countries_filter, aes(x =rank , y = total_points)) +
      geom_point()
  })
  
  output$team_plot3 <- renderPlot({
    rank_df %>% filter(country_full==input$country) %>% 
      select(rank,total_points, previous_points, rank_change, cur_year_avg, rank_date_year) %>%
      group_by(rank_date_year) %>% mutate(meanRank = mean(rank,na.rm=T), TotalPoints = mean(total_points))%>% 
      ggplot(aes(x=rank_date_year)) + geom_line(aes(y=TotalPoints),size=1,alpha=0.25, colour='#006666') + geom_point(aes(y=TotalPoints),size=2, colour='#006666') +
      geom_curve(aes(x = as.Date("2007-01-01"), y = 500, xend = as.Date("2009-11-01"), yend =25),curvature = -.05,arrow = arrow(length = unit(0.02, "npc")),color='#0072B2',size=.25) + 
      theme(axis.text.y = element_text(size=16),
            panel.grid.minor = element_line(colour = '#989898'),
            panel.grid.major = element_line(colour = '#989898'))+
      annotate("text", x=as.Date("2006-01-01"), y = 600, label = "Fifa ranking system update", size=4, colour="#0072B2")
  })
  
  output$team_plot4 <- renderPlot({
    rank_df %>% 
      filter(country_full==input$country) %>% 
      select(rank, total_points, previous_points, rank_change, cur_year_avg, rank_date_year) %>%
      group_by(rank_date_year) %>% mutate(Rank = mean(rank,na.rm=T), meanPoints = mean(total_points)) %>%
      ggplot(aes(x=rank_date_year)) + geom_line(aes(y=Rank),size=2,alpha=0.25, colour="#000080") + geom_point(aes(y=Rank), size=2, colour="#000080") +
      theme_light() + scale_y_reverse() + theme(axis.text.y = element_text(size=16),
                                                panel.grid.minor = element_line(colour = 'black'),
                                                panel.grid.major = element_line(colour = 'black')
      )
  })
  
  output$plot3_expl <- renderUI({
    HTML(
      "<p>The <i>total_points</i> is null for years earlier than 2010.",
      "<p>After the 2006 ranking system update, the evaluation period was cut from 8 years to 4 years.</p>",
      "<p>Since <i>total_points</i> is calculated based on on the results of the previous 4 years, a 4 year period (2006 - 2010) was needed to evaluate the total points.</p>"
    )
  })
  #Team seeding
  
  output$seeding_sub_title <- renderUI({
    HTML(
      "<img src='conf_logos.jpg' class='img-fluid' style='max-width: 100%;height:auto'/>"
    )
    
  })
  output$seeding_title <- renderUI({
    if(input$confederation =="All confederations"){
      HTML(
        "<h2>FIFA World Cup Tournament.</h2>",
        "<p>The FIFA World Cup, often simply called the World Cup, is an international association football competition contested by senior men's national teams with the championship 
        being awarded every after 4 years.
        </p>",
        "<img src='worldcup.jpg' class='img-fluid' style='max-width: 100%;height:auto'/>",
        "<p>Final tournament will play 32 teams, which are drawn into 8 groups with 4 teams in each group. Pot allocation is based on the FIFA rankings as of 07-june-2018.
        There are 4 pots were each pot contains 8 teams in their ranking order accoding to the teams that qualify for the world cup finals.</p>",
        "<p>FIFA decides beforehand the number of spots awarded to each of the continental zones. For the 2018 World Cup, the following numbers were used</p>",
        "<ul>
        <li>UEFA (Europe) – 13 berths</li>
        <li>CAF (Africa) – 5 berths</li>
        <li>AFC (Asia) – 4 berths</li>
        <li>CONMEBOL (South America) – 4 berths</li>
        <li>CONCACAF (North and Central America and Caribbean) – 3 berths</li>
        <li>host – 1 berths</li>
        <li>2 berths for the winners of intercontinental play-offs between the best team from the OFC (Oceania), as well as additional teams from the AFC, CONMEBOL and CONCACAF. 
        The pairings for these play-offs are determined by an open draw.</li>
        </ul>",
        "<h4>Click on a team to select it.</h4>"
      )
    }
    else if(input$confederation =="AFC"){
      HTML(
        "<h2>AFC</h2>",
        "<p>The Asian Football Confederation (AFC) is the governing body of association football in Asia and Australia and is one of the 6 FIFA confederations. For national teams 
        which are members of the AFC  4.5 slots (4 direct slots and 1 inter-confederation play-off slot) in the final tournament were available for AFC teams.</p>",
        "<img src='japan.jpg' class='img-fluid' style='max-width: 100%;height: auto'/>",
        "<h3>Road to the World Cup finals</h3>",
        "<p>The qualification structure is as follows:</p>",
        "<ul>
        <li><b>First round</b>: 12 teams (ranked 35–46 according to the FIFA rankings of AFC teams) play home-and-away over two legs. The six winners advance to the second round.</li>
        <li><b>Second round</b>: 40 teams (ranked 1–34 and six first round winners) are divided into eight groups of five teams to play home-and-away round robin matches. The
        eight group winners and the four best group runners-up advance to the third round of FIFA World Cup qualification. </li>
        <li><b>Third round</b>: The 12 teams which advance from the second round were divided into two groups of six teams to play home-and-away round-robin matches. The top two 
        teams of each group qualify for the 2018 FIFA World Cup, and the two third-placed teams advance to the fourth round.</li>
        <li><b>Fourth round</b>: The two third-placed teams of each group from the third round play home-and-away over two legs. The winners advance to the inter confederation play-offs</li>
        </ul>"
        
      )
    }
    else if(input$confederation =="CAF"){
      HTML(
        "<h2>CAF</h2>",
        "<p>The Confederation of African Football or CAF is the administrative and controlling body for African association football.CAF is the biggest of the six continental confederations of FIFA .A total 
        of 5 slots in the final tournament were available for CAF teams</p>",
        "<img src='niger.jpg' class='img-fluid' style='max-width: 100%;height: auto'/>",
        "<h3>Road to the World Cup finals</h3>",
        "<p>The qualification structure is as follows:</p>",
        "<ul>
        <li><b>First round</b>: A total of 26 teams (teams ranked 28–53) play home-and-away over two legs. The 13 winners advance to the second round.</li>
        <li><b>Second round</b>:  A total of 40 teams (teams ranked 1–27 and 13 first round winners) play home-and-away over two legs  The 20 winners advance to the third round.</li>
        <li><b>Third round</b>: The 20 teams which advance from the second round were divided into five groups of four teams to play home-and-away round-robin matches. The winners of each group qualify for the world cup final tournament.</li>
        </ul>"
        
      )
    }
    else if(input$confederation =="CONCACAF"){
      HTML(
        "<h2>CONCACAF</h2>",
        "<p>The Confederation of North, Central American and Caribbean Association Football is the continental governing body for association football in North America. A total 
        of 3.5 (3 direct slots and 1 inter-confederation play-off slot) slots in the final tournament are available for CONCACAF teams</p>",
        "<img src='mexico.jpg' class='img-fluid' style='max-width: 100%;height: auto'/>",
        "<h3>Road to the World Cup finals</h3>",
        "<p>The qualification structure is as follows:</p>",
        "<ul>
        <li><b>First round</b>: 14 teams (lowest ranked teams are assigned spots 22–35) play home-and-away over two legs. The winners advance to the second round.</li>
        <li><b>Second round</b>: 20 teams (second lowest ranked teams are assigned spots 9–21 and seven first round winners) play home-and-away over two legs. The winners advance to the third round.</li>
        <li><b>Third round</b>: 12 teams (teams ranked 7–8 by CONCACAF and ten second round winners) play home-and-away over two legs. The winners advance to the fourth round.</li>
        <li><b>Fourth round</b>: 12 teams (teams ranked 1–6 by CONCACAF and six third round winners) are divided into three groups of four teams to play home-and-away round robin matches. The top two of each group
        advance to the fifth round.</li>
        <li><b>Fifth round</b>: The six teams which advance from the fourth round play home-and-away round-robin matches in one group. The top three qualify for the FIFA World Cup tournament and the 
        fourth-placed team advance to the inter-confederations play-offs</li>
        </ul>"
        
      )
    }
    
    else if(input$confederation =="CONMEBOL"){
      HTML(
        "<h2>CONMEBOL</h2>",
        "<p>The South American Football Confederation is the continental governing body of association football in South America and it is one of FIFA's six continental confederations. It is considered one of the
        strongest confederations in the world. The World Cup qualifiers of CONMEBOL have been described as the toughest qualifiers in the world, for their simple round-robin system.</p>",
        "<img src='brazil.jpg' class='img-fluid' style='max-width: 100%;height: auto'/>",
        "<h3>Road to the World Cup finals</h3>",
        "<p>The qualification structure considers the ten teams playing in a league of home-and-away round-robin matches. The top four teams qualify for the 2018 FIFA World Cup, and 
        the fifth-placed team advances to the inter-confederation play-offs.</p>"
        
      )
    }
    
    else if(input$confederation =="OFC"){
      HTML(
        "<h2>OFC</h2>",
        "<p>The Oceania Football Confederation (OFC) is one of the six continental confederations of international association football. It promotes the game in Oceania and allows the member nations to 
        qualify for the FIFA World Cup.A total of 0.5 slots (i.e. 1 inter-confederation play-off slot) in the final tournament was available for OFC teams. OFC is predominantly made up of island nations 
        where association football is not the most popular sport. Consequently, the OFC has little influence in the wider football world</p>",
        "<img src='newzeeland.jpg' class='img-fluid' style='max-width: 100%;height: auto'/>",
        "<h3>Road to the World Cup finals</h3>",
        "<p>The qualification structure is as follows for 2018:</p>",
        "<ul>
        <li><b>First round</b>:  American Samoa, Cook Islands, Samoa, and Tonga play a round-robin tournament. The winner advances to the second round.</li>
        <li><b>Second round</b>:  A total of eight teams play the tournament  of groups. For the group stage, they are divided into two groups of four teams. The top three teams of each group advance to
        the third round of World Cup qualifying.</li>
        <li><b>Third round</b>: The six teams which had advanced from the second round are divided into two groups of three teams to play home-and-away round-robin matches. The two group winners meet in a 
        two-legged match with the winner advancing to the inter-confederation play-offs</li>
        </ul>"
        
      )
    }
    
    else if(input$confederation =="UEFA"){
      HTML(
        "<h2>UEFA</h2>",
        "<p>The Union of European Football Associations is the administrative body for association football in Europe. It is one of six continental confederations of world football's governing body FIFA. UEFA consists 
        of 55 national association members. A total of 13 slots in the final tournament were available for UEFA teams</p>",
        "<img src='belguim3.jpg' class='img-fluid' style='max-width: 100%;height: auto'/>",
        "<h3>Road to the World Cup finals</h3>",
        "<p>The qualification structure is as follows:</p>",
        "<ul>
        <li><b>First round</b>:The 54 teams are divided into nine groups of six teams each to play home-and-away round-robin matches. The winners of each group qualify for the 2018 FIFA World Cup, and the eight best 
        runners-up advance to the second round (play-offs).</li>
        <li><b>Second round</b>: The eight best runners-up from the first round play against one other team over two legs, home and away. The winners of each tie qualify for the 2018 FIFA World Cup.</li>
        </ul>"
        
      )
    }
    
  })
  
  filteredTable_data_allConf <- reactive({
    countries_filter <-subset(WCR_data, as.Date(rank_date) == as.Date("2018-06-07"))
    filtered_data <-countries_filter[,c(1,2,4,5,6,7)]
  })
  
  filteredTable_data_AFC <- reactive({
    countries_filter <-subset(WCR_data, as.Date(rank_date) == as.Date("2018-06-07") & confederation == "AFC")
    filtered_data <-countries_filter[,c(1,2,5,6,7)]
  })
  
  filteredTable_data_CAF <- reactive({
    countries_filter <-subset(WCR_data, as.Date(rank_date) == as.Date("2018-06-07") & confederation == "CAF")
    filtered_data <-countries_filter[,c(1,2,5,6,7)]
  })
  
  filteredTable_data_CONCACAF <- reactive({
    countries_filter <-subset(WCR_data, as.Date(rank_date) == as.Date("2018-06-07") & confederation == "CONCACAF")
    filtered_data <-countries_filter[,c(1,2,5,6,7)]
  })
  
  filteredTable_data_CONMEBOL <- reactive({
    countries_filter <-subset(WCR_data, as.Date(rank_date) == as.Date("2018-06-07") & confederation == "CONMEBOL")
    filtered_data <-countries_filter[,c(1,2,5,6,7)]
  })
  
  filteredTable_data_OFC <- reactive({
    countries_filter <-subset(WCR_data, as.Date(rank_date) == as.Date("2018-06-07") & confederation == "OFC")
    filtered_data <-countries_filter[,c(1,2,5,6,7)]
  })
  
  
  filteredTable_data_UEFA <- reactive({
    countries_filter <-subset(WCR_data, as.Date(rank_date) == as.Date("2018-06-07") & confederation == "UEFA")
    filtered_data <-countries_filter[,c(1,2,5,6,7)]
  })
  
  output$Seeding_team_table <- DT::renderDataTable({
    if(input$confederation =="All confederations"){
      DT::datatable(filteredTable_data_allConf(),
                    options = list(pageLength = 50),rownames=FALSE, caption = 'Table 1: You can select upto a maximum of 32 countries') %>% formatStyle(
                      'confederation',
                      target = 'row',
                      backgroundColor = styleEqual(c('AFC','CAF','CONCACAF','CONMEBOL','OFC','UEFA'), c('#c4236e','#30dd8d','#ebef0b','#d6876b','#331e12','#5c32db'))
                    )
    }
    
  })
  
  dirColors <-c("1"="#c4236e", "2"="#30dd8d", "3"="#ebef0b", "4"="#d6876b","5"="#331e12","6"="#5c32db","7"="#aeb5c1")
  output$scale <- renderUI({
    
    # Create a Bootstrap-styled table
    tags$table(class = "table",
               tags$thead(tags$tr(
                 tags$th("AFC-",span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   dirColors[1]
                 ))),
                 tags$th("CAF-",span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   dirColors[2]
                 ))),
                 tags$th("CONCACAF-",span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   dirColors[3]
                 ))),
                 tags$th("CONMEBOL-",span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   dirColors[4]
                 ))),
                 tags$th("OFC-",span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   dirColors[5]
                 ))),
                 tags$th("UEFA-",span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   dirColors[6]
                 ))),
                 tags$th("SELECTED-",span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   dirColors[7]
                 )))
               ))
    )
  })
  
  
  output$confederations_table <- DT::renderDataTable({
    if(input$confederation =="AFC"){
      filteredTable_data_AFC()
    }
    else if(input$confederation =="CAF"){
      filteredTable_data_CAF()
    }
    else if(input$confederation =="CONCACAF"){
      filteredTable_data_CONCACAF()
    }
    else if(input$confederation =="CONMEBOL"){
      filteredTable_data_CONMEBOL()
    }
    else if(input$confederation =="OFC"){
      filteredTable_data_OFC()
    }
    else if(input$confederation =="UEFA"){
      filteredTable_data_UEFA()
    }
  },options = list(pageLength = 25),rownames=FALSE, caption = 'Table show teams in a particular confederation',selection = 'none')
  
  filteredTable_selected <- reactive({
    ids <- input$Seeding_team_table_rows_selected
    table <- filteredTable_data_allConf()[ids,]
    table[order(table$rank),]
  })
  
  output$selected_teams_pot1 <- DT::renderDataTable({
    if(length(input$Seeding_team_table_rows_selected)>0){
      filteredTable_selected()[1:8,c(1,2)]
    }
  },rownames=FALSE,options=list(paging = FALSE,searching = FALSE),caption = 'POT 1',selection = 'none') 
  
  output$selected_teams_pot2 <- DT::renderDataTable({
    if(length(input$Seeding_team_table_rows_selected)>8 ){
      filteredTable_selected()[9:16,c(1,2)]
      
    }
  },rownames=FALSE,options=list(paging = FALSE,searching = FALSE),caption = 'POT 2',selection = 'none') 
  
  output$selected_teams_pot3 <- DT::renderDataTable({
    if(length(input$Seeding_team_table_rows_selected)>16 ){
      filteredTable_selected()[17:24,c(1,2)]
    }
  },rownames=FALSE,options=list(paging = FALSE,searching = FALSE),caption = 'POT 3',selection = 'none') 
  
  output$selected_teams_pot4 <- DT::renderDataTable({
    if(length(input$Seeding_team_table_rows_selected)>24){
      filteredTable_selected()[25:32,c(1,2)]
    }
  },rownames=FALSE,options=list(paging = FALSE,searching = FALSE),caption = 'POT 4',selection = 'none') 
  
  observeEvent(input$seed, {
    filteredTable_selected_pot1 <- reactive({
      table <- filteredTable_selected()[1:8,c(1,2)]
      table[sample(nrow(table), 8),]
    })
    
    filteredTable_selected_pot2 <- reactive({
      table <- filteredTable_selected()[9:16,c(1,2)]
      table[sample(nrow(table), 8),]
    })
    
    filteredTable_selected_pot3 <- reactive({
      table <- filteredTable_selected()[17:24,c(1,2)]
      table[sample(nrow(table), 8),]
    })
    
    filteredTable_selected_pot4 <- reactive({
      table <- filteredTable_selected()[25:32,c(1,2)]
      table[sample(nrow(table), 8),]
    })
    
    output$seed_table1 <- renderUI({
      build_table(filteredTable_selected_pot1(),filteredTable_selected_pot2(),filteredTable_selected_pot3(),filteredTable_selected_pot4(),1,"A")
    })
    
    output$seed_table2 <- renderUI({
      build_table(filteredTable_selected_pot1(),filteredTable_selected_pot2(),filteredTable_selected_pot3(),filteredTable_selected_pot4(),2,"B")
    })
    
    output$seed_table3 <- renderUI({
      build_table(filteredTable_selected_pot1(),filteredTable_selected_pot2(),filteredTable_selected_pot3(),filteredTable_selected_pot4(),3,"C")
    })
    
    output$seed_table4 <- renderUI({
      build_table(filteredTable_selected_pot1(),filteredTable_selected_pot2(),filteredTable_selected_pot3(),filteredTable_selected_pot4(),4,"D")
    })
    
    output$seed_table5 <- renderUI({
      build_table(filteredTable_selected_pot1(),filteredTable_selected_pot2(),filteredTable_selected_pot3(),filteredTable_selected_pot4(),5,"E")
    })
    
    output$seed_table6 <- renderUI({
      build_table(filteredTable_selected_pot1(),filteredTable_selected_pot2(),filteredTable_selected_pot3(),filteredTable_selected_pot4(),6,"F")
    })
    
    output$seed_table7 <- renderUI({
      build_table(filteredTable_selected_pot1(),filteredTable_selected_pot2(),filteredTable_selected_pot3(),filteredTable_selected_pot4(),7,"G")
    })
    
    output$seed_table8 <- renderUI({
      build_table(filteredTable_selected_pot1(),filteredTable_selected_pot2(),filteredTable_selected_pot3(),filteredTable_selected_pot4(),8,"H")
    })
  })
  
  build_table <- function(pot1,pot2,pot3,pot4,pot_row_num,letter){
    
    html.table <- tags$table(style = "border: 1px solid black; padding: 1%; width: 100%;",
                             class="table",
                             tags$thead(
                               tags$tr(
                                 tags$th("Group ",letter)
                               )
                             ),
                             tags$tbody(
                               tags$tr(
                                 tags$td(pot1[pot_row_num,2])
                               ),
                               tags$tr(
                                 tags$td(pot2[pot_row_num,2])
                               ),
                               tags$tr(
                                 tags$td(pot3[pot_row_num,2])
                               ),
                               tags$tr(
                                 tags$td(pot4[pot_row_num,2])
                               )
                             )
    )
    
    return(html.table)
  }
  
  #Visualisations
  output$plot_name1 <- renderPlot({
    ggplot(lead, aes(x=rank_date_month, y=1), legend=TRUE) + 
      geom_histogram(aes(fill=color),stat='identity',color='black',size=.001) +
      scale_fill_identity() +
      theme_light()+ coord_flip() + scale_x_date(name="Rank Date Month",date_breaks = "1 year", date_labels =  "%Y") + 
      labs(title='FIFA World Ranking Leaders', subtitle='per month') + 
      theme(legend.position='right',legend.direction='vertical',axis.text.x=element_blank(),panel.grid.major.x=element_blank(),
            legend.text=element_text(size=10), legend.key.size = unit(.3, "cm")
      ) +
      ylim(0,1.0)
  })
  CntColors <-c("1"="#87CEEB", "2"="#DC143C", "3"="#FFD700", "4"="#000080","5"="#000000","6"="#00cc66","7"="#FFA500","8"="#8B2500")
  output$legend <- renderUI({
    
    # Create a Bootstrap-styled table
    tags$table(class = "table",
               tags$thead(tags$tr(
                 tags$th("Argentina-",span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   CntColors[1]
                 ))),
                 tags$th("Belgium-",span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   CntColors[2]
                 ))),
                 tags$th("Brazil-",span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   CntColors[3]
                 ))),
                 tags$th("France-",span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   CntColors[4]
                 ))),
                 tags$th("Germany-",span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   CntColors[5]
                 ))),
                 tags$th("Italy-",span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   CntColors[6]
                 ))),
                 tags$th("Netherlands-",span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   CntColors[7]
                 ))),
                 tags$th("Spain-",span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   CntColors[8]
                 )))
               ))
    )
  })
  output$Desc_plot_name1 <- renderUI({
    HTML(
      "<p>This graph shows the time periods each national football team has been at the top of the FIFA World rankings.
      <br>Since the first rankings in 1993, Brazil has enjoyed the top spot for most of the time but Germany currently remains the top ranked team.</p>",
      "<p></p>"
    )
  })
  
  output$plot_name2 <- renderPlot({
    recentRank %>% group_by(confederation) %>% ggplot(aes(x=reorder(confederation, -rank, FUN=mean),y=rank,fill=confederation)) + geom_boxplot(alpha=.75,size=.25) + geom_jitter(shape=16,position=position_jitter(0.2),size=1,alpha=.25) +
      theme_light() + theme(legend.position='None',
                            axis.text = element_text(size=14),
                            panel.grid.minor = element_line(colour = '#505050'),
                            panel.grid.major = element_line(colour = '#505050')
      ) + 
      scale_fill_brewer(palette='Paired') + coord_flip() + labs(title='Confederation performance according to average ranking')+scale_x_discrete(name="Confederation")
  })
  
  output$Desc_plot_name2 <- renderUI({
    HTML(
      "<p> From the boxplot above, AFC, CAF, UEFA and CONCACAF have long boxes showing that their member teams have quite varying ranks.
      <br>In CONMEBOL, the ranks of the member teams are close to each other thus a short box. It means the SouthAmerican teams can be found in the same place on the rankings table.
      <br>The high median and long upper whisker observed on the CONMEBOL and UEFA boxplots mean more teams in those confederations have a high total point score. The rest of the confederations 
      have a low median because on average, their member teams have a low total points score. </p>"
    )
  })
  
  output$plot_name3 <- renderPlot({
    recentRank %>% group_by(confederation) %>% ggplot(aes(x=reorder(confederation, total_points, FUN=mean),y=total_points,fill=confederation)) +
      geom_boxplot(alpha=.75,size=.25) + geom_jitter(shape=16,position=position_jitter(0.2),size=1,alpha=.25) +
      theme_light() + theme(legend.position='None',
                            axis.text = element_text(size=14),
                            panel.grid.minor = element_line(colour = '#505050'),
                            panel.grid.major = element_line(colour = '#505050')
      ) +
      scale_fill_brewer(palette='Paired') + coord_flip() + labs(title='Confederation performance according to average points', subtitle="from most recent ranking")+scale_x_discrete(name="Confederation")
  })
  
  output$Desc_plot_name3 <- renderUI({
    HTML(
      "<p>
      From the boxplot above, CONMEBOL, UEFA and CONCACAF have long boxes showing that their member teams have quite varying total points.
      <br>In OFC, the total points of the member teams have a small range thus a short box.
      <br>The high median and long upper whisker observed on the CONMEBOL and UEFA boxplots mean more teams in those confederations have a high total point score. The rest of the confederations 
      have a low median because on average, their member teams have a low total points score. 
      </p>"
    )
  })
  
  output$plot_name4 <- renderPlot({
    confPoints %>% ggplot(aes(x=confederation, y=total_points, fill=confederation)) +
      geom_bar(stat='identity') +
      theme_light() + theme(legend.position='None', axis.text = element_text(size=14),
                            panel.grid.minor = element_line(colour = '#505050'),
                            panel.grid.major.y = element_line(colour = '#505050')
      ) +
      scale_fill_brewer(palette='Paired') + labs(title='Confederation average points (previous 4 years)') + scale_y_continuous(name="Average total_points", breaks = seq(0, 1000, 200))
  })
  
  output$Desc_plot_name4 <- renderUI({
    HTML(
      "<p>The graph above shows average points of confederations over the previous four years obtained by first getting the average of the total points of a confederation for each month.
      <br>The average of those averages for each confederation is then plotted.
      <br>Teams from South America on average have more total points and thus their confederation has more average total points than the rest.
      </p>",
      "<p></p>"
    )
  })
  
  output$plot_name5 <- renderPlot({
    aggregate(rank~confederation, evalPeriod, mean) %>% ggplot(aes(x=confederation, y=rank, fill=confederation)) + geom_bar(stat = "identity")+
      theme_light() + theme(legend.position='None',axis.text = element_text(size=14),
                            panel.grid.minor = element_line(colour = '#505050'),
                            panel.grid.major.y = element_line(colour = '#505050')
      ) +
      scale_fill_brewer(palette='Paired') + labs(title='Confederation average ranking (previous 4 years)') + scale_y_continuous(name="Average Rank")
  })
  output$Desc_plot_name5 <- renderUI({
    HTML(
      "<p>The graph above shows average rank of confederations over the previous four years obtained by first getting the average rank of a confederation for each month.
      <br>The average of those averages for each confederation is then plotted.
      <br>The South American confederation CONMEBOL,on average, has better rank than the rest owing to the small number of member teams that rank high in the FIFA rankings.
      <br><b>Note:</b> The lower the bar, the higher the rank of the confederation. A higher rank means an overall high performance.
      </p>",
      "<p></p>"
    )
  })
  output$plot_name6 <- renderPlot({
    as.data.frame(table(recentRank$confederation)) %>% ggplot(aes(x=Var1, y=Freq, fill=Var1)) + geom_point(size=4)+
      geom_segment(aes(x=Var1, 
                       xend=Var1, 
                       y=0, 
                       yend=Freq)) + 
      theme_light() + 
      theme(legend.position = 'None',
            axis.text = element_text(size = 14),
            panel.grid.major.y = element_line(colour = '#505050'),
            panel.grid.minor = element_line(colour = '#505050')
      ) +
      scale_fill_brewer(palette = 'Paired') + scale_x_discrete(name="Confederations")+scale_y_continuous(name="Number of teams", breaks = seq(0, 60, 4))
  })
  output$Desc_plot_name6 <- renderUI({
    HTML(
      "<p>The lollipop chart above shows the number of teams in each confederation as of June of 2018. CAF has the most number of member teams  
      while UEFA has just one member less than the former. CONMEBOL has the fewest member teams, a factor that has aided its overall perofrmance.
      </p>"
    )
  })
  output$plot_name7 <- renderPlot({
    topTen[1:10,]  %>% ggplot(aes(x=reorder(region, -rank), y=total_points, fill=confederation)) +
      geom_bar(stat="identity") + coord_flip() + theme_light() +
      scale_fill_brewer(palette="Paired") +
      theme(axis.text=element_text(size=13), legend.position="None", panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(colour = '#505050')
      ) +
      labs(title="The rank, according to total points, of the top 10 member countries of FIFA")+scale_x_discrete(name="Team")
  })
  output$Desc_plot_name7 <- renderUI({
    HTML(
      "<p>The top 10 spots are dominated by teams from only two confederations namely; CONMEBOL and UEFA, with UEFA having more teams.</p>"
    )
  })
  output$plot_name8 <- renderPlot({
    tail(topTen, 10)  %>% ggplot(aes(x=reorder(region, -rank), y=total_points, fill=confederation)) +
      geom_bar(stat="identity") + coord_flip() + theme_light() +
      scale_fill_brewer(palette="Paired") +
      theme(axis.text=element_text(size=13), legend.position="None", panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(colour = '#505050')
      ) +
      labs(title="The rank, according to total points, of the bottom 10 member countries of FIFA")+scale_x_discrete(name="Team")
  })
  output$Desc_plot_name8 <- renderUI({
    HTML(
      "<p>
      From the bar plot above, the bottom 6 teams have no total points due to the fact that four years haven't elapsed since they 
      became member nations of FIFA.
      <br>An evaluation period of <b>four years</b> is needed to calculate the total points of a team. 
      </p>"
    )
  })
  output$plot_name9 <- renderPlot({
    tail(topFive,5) %>% ggplot(aes(x=reorder(region, rank_change), y=rank_change, fill=confederation))+
      geom_bar(stat="identity") + theme_light()+
      scale_fill_brewer(palette = "Paired") +
      theme(axis.text = element_text(size=11), legend.position = "None",
            axis.line = element_line(colour = "darkgreen", size = 1, linetype = "solid"),
            panel.grid.major.y=element_line(colour = '#A8A8A8')
      )+
      scale_y_continuous(name = "Rank Change", breaks = seq(0, 22, 2)) +
      scale_x_discrete(name="Country")+
      labs(title="The best movers as of June, 2018")
  })
  output$Desc_plot_name9 <- renderUI({
    HTML(
      "<p>
      Azerbaijan has moved an incredible 21 positions <b>up</b> the rankings table since joining FIFA. Impressive performances at the RIO 2016 Olympics and
      FIFA World Cup qualifying competition have seen them soar this high.
      </p>"
    )
  })
  output$plot_name10 <- renderPlot({
    topFive[1:5,] %>% ggplot(aes(x=reorder(region, rank_change), y=rank_change, fill=confederation))+
      geom_bar(stat="identity") + theme_light()+
      scale_fill_brewer(palette = "Paired") +
      theme(axis.text = element_text(size=13), legend.position = "None",
            axis.line = element_line(colour = "red", size = 1, linetype = "solid"),
            panel.grid.major.y=element_line(colour = '#A8A8A8')
      )+
      scale_y_reverse(name = "Rank Change", breaks = seq(0, -20, -2)) +
      scale_x_discrete(name="Country")+
      labs(title="The worst movers as of June, 2018")
  })
  output$Desc_plot_name10 <- renderUI({
    HTML(
      "<p>
      Guyana men's team have dropped 18 positions as per the latest rankings thus the worst movers currently.
      </p>"
    )
  })
  output$plot_name11 <- renderPlot({
    ggplot(topConfedTeam, aes(x=region, y=total_points, fill=confederation)) +
      geom_bar(stat="identity") +
      theme_light() +
      scale_fill_brewer(palette="Paired") +
      theme(
        axis.text = element_text(size=11),
        legend.text=element_text(size=10),
        legend.key.size = unit(.3, "cm"),
        panel.grid.major.y=element_line(colour = '#A8A8A8')
      ) +
      scale_y_continuous(name="Total Points", breaks = seq(0, 1600, 200)) + scale_x_discrete(name="Country") +
      labs(title="The top team from each confederation")
  })
  output$Desc_plot_name11 <- renderUI({
    HTML(
      "<p>
      From the graph above, most notably is the relatively high total points score of the leading teams Germany and Brazil. They
      come from strong confederations where there's tough competition due to the high quality of the member teams. 
      </p>"
    )
  })
  output$plot_name12 <- renderPlot({
    ggplot(recentRank, aes(x=rank, y=total_points, color=confederation)) +
      geom_point() + geom_smooth(method=lm, se=FALSE) +
      scale_fill_brewer(palette="Set1") +
      theme(
        axis.text = element_text(size=11),
        legend.text=element_text(size=10),
        legend.key.size = unit(.3, "cm"),
        panel.grid.major.y=element_line(colour = '#A8A8A8')
      ) +
      scale_y_continuous(name="Total Points", breaks = seq(0, 1600, 200)) + scale_x_continuous(name="Rank", breaks=seq(0, 210, 10)) +
      labs(title="Relationship between total points and rank of a team")
  })
  output$Desc_plot_name12 <- renderUI({
    HTML(
      "<p>
      There's a negative correlation between the total points and rank of a team. The more points a team has, the smaller its rank score, the 
      higher it is on the rankings table. Low points correlated to a big rank score thus a low table position.
      </p>"
    )
  })
  output$Summary <- renderUI({
    HTML(
      "<h1>Summary from the FIFA World Rankings.</h1>",
      "<p>FIFA the world's governing football association currently contains 216 member nations which have increased from 168 since 1993 and FIFA has been 
      ranking it's member naions since then basing on the results of the games played. FIFA contains 6 confederations that help to govern football in individual 
      confederations across the whole world namely: AFC, CAF, CONCACAF, CONMEBOL, OFC and UEFA.<br/></br>
      
      Germany is the country that has attained the highest number of total points which are 1775.03 points. These are the highest points attained by Germany 
      since 24/8/2011. The countries with the least total points ever attained are Netherlands Antilles, RCS, Serbia and Montenegro, Yugoslavia and finally Zaire.<br><br>
      
      The countries that have attained the highest rank of number 1 ever since the ranking process began in 1993 are: Argentina, Belgium, Brazil, France, Germany, 
      Italy, Netherlands and Spain. <br><br>
      
      The countries with the lowest ranks since the ranking process started in 1993 are: American Samoa(205), Andorra (206), Anguilla (209), Bahamas (208), Bhutan (209), 
      British Virgin Islands (206), Cayman Islands (205), Comoros (207), Cook Islands (207), Djibouti (207), Eritrea (207), Guam (205), Mauritania (206), Mongolia (205),
      Papua New Guinea (206), San Marino (208), Somalia (207), South Sudan (205), Timor-Leste (206), Tonga (207), Turks and Caicos Islands (207).<br>
      The mean of all the countries’ total points since FIFA began the ranking process using total points on 24/8/2011 is 122.0686 points.<br></br>
      
      As of the 7th of June, 2018 ranking, the best movers are: 
      </p>",
      "<ul>
      <li>Azerbaijan moving from 126 to 105, going up by 21 positions and accumulating 76 points.</li>
      <li>Papua New Guinea moving from 180 to 166, going up by 14 positions and accumulating 25 points.</li>
      <li>El Salvador moving from 85 to 72, going up by 13 positions and accumulating 50 points.</li>
      </ul>",
      "<p>The worst movers are: </p>",
      "<ul>
      <li>Guyana moving from 164 to 182, dropping by 18 positions and losing 38 points.</li>
      <li>Kyrgyz Republic moving from 75 to 92, dropping by 17 positions and losing 61 points.</li>
      <li>Guinea-Bissau moving from 104 to 121, dropping by 17 points and losing 75 points.</li>
      </ul>",
      "<h2>History of FIFA World Rankings</h2>",
      "<h4>1993–1998 calculation method</h4>",
      "<p>The ranking formula used from August 1993 until December 1998 was very simplistic and quickly became noticed for its lack of supporting factors. When the 
      rankings were initially introduced, a team received one point for a draw or three for a victory in FIFA-recognised matches – much the same as a traditional 
      league scoring system. This was a quite simplistic approach, however, and FIFA quickly realised that there were many factors affecting international matches.</p>",
      "<h4>1999–2006 calculation method</h4>",
      "<p>In January 1999, FIFA introduced a revised system of ranking calculation. The major changes were as follows:</p>",
      "<ul>
      <li>the point ranking was scaled up by a factor of ten</li>
      <li>
      the method of calculation was changed to take into account factors including:
      <ul>
      <li>the number of goals scored or conceded</li>
      <li>whether the match was played at home or away</li>
      <li>the importance of a match or competition</li>
      <li>regional strength</li>
      </ul>
      </li>
      <li>a fixed number of points were no longer necessarily awarded for a victory or a draw</li>
      <li>match losers were able to earn points</li>
      </ul>",
      "<h4>2006–2018 calculation method</h4>",
      "<p>FIFA announced that the ranking system would be updated following the 2006 World Cup. The evaluation period was cut from eight to four years, and a simpler 
      method of calculation was used to determine rankings.Goals scored and home or away advantage were no longer taken into account, and other aspects of the calculations, 
      including the importance attributed to different types of match, were revised. The first set of revised rankings and the calculation methodology were announced on 12 
      July 2006. This is the currently used ranking system but FIFA announced on 10 June 2018 that the ranking system would be updated following the 2018 World Cup.</p>",
      
      "<h2>Uses of FIFA World Rankings</h2>",
      "<ul>
        <li>The rankings are used by FIFA to rank the progression and current ability of the national football teams of its member nations, and claims that they 
        create a reliable measure for comparing national A-teams</li>
        <li>They are used as part of the calculation, or the entire grounds to seed competitions. In the FIFA World Cup qualification tournament, the rankings 
        are used to seed the groups in the competitions involving AFC,CONCACAF,CAF and UEFA. Ranking are used to determine the seeds for the FIFA World Cup final draw</li>
        <li>The rankings are also used to determine the winners of the two annual awards national teams receive on the basis of their performance in the rankings.</li>
      </ul>"
    )
  })
  
  }
#End of server
shinyApp(ui, server)