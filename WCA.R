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
conf <-levels(data$confederation)
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
                           select(region, country_abrv, confederation, rank, total_points) %>%
                           arrange(region))
#Get information for countries from Maps package (Using ggplot2's function tho)
countriesMap <- map_data("world")
countriesMap$region <- tolower(countriesMap$region)
#Mutate-merge 'regions' from Maps with most recent rankings data
results <- data.frame(left_join(countriesMap, recentRank %>% select(confederation, region), by="region"))

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
               tabName = "Team_Analysis"
      ),
      menuItem("Team seeding",
               tabName = "Team_seeding"
      ),
      menuItem("Visualisations",
               tabName = "Visualisations"
      ),
      menuItem("Summary",
               tabName = "Summary"
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
                               title = h2(class ="plot-title", "A plot of total points against rank dates"),
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
                  )
                )
              )
      ),
      tabItem(tabName = "Team_seeding",
              fluidRow(
                h1("Team Seeding"),
                sidebarLayout(
                  mainPanel(
                    htmlOutput(outputId = "seeding_title")
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
                plotOutput(outputId = "recent_rank_totalPoints"),
                plotOutput(outputId = "confederation_plots")
              )
      ),
      tabItem(tabName = "Summary",
              fluidRow(
                
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
    avg_rank <- round(mean(country_ranks))
    if(avg_rank < 6){
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
    else if(avg_rank < 42){
      best_pos <-"Round 1"
    }
    else{
      best_pos <-"Doesnot qualify"
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
  
  #Team seeding
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
        "<h4>Click on a team to select it.</h4>"
      )
    }
    else if(input$confederation =="AFC"){
      HTML(
        "<h2>AFC</h2>",
        "<p>The Asian Football Confederation (AFC) is the governing body of association football in Asia and Australia and is one of the 6 FIFA confederations</p>",
        "<img src='japan.jpg' class='img-fluid' style='max-width: 100%;height: auto'/>"
        
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
  output$recent_rank_totalPoints <- renderPlot({
    recentRank %>% ggplot(aes(x=reorder(region, -rank), y=total_points, fill=confederation)) +
      geom_bar(stat="identity") + coord_flip() + theme_light() +
      scale_fill_brewer(palette="Paired") +
      theme(axis.text.y = element_text(size = 5), legend.position="None", panel.grid.major.y = element_blank()) + 
      labs(title="The rank, according to total points, of the 206 countries of FIFA")
    
  })
  output$confederation_plots <- renderPlot({
    recentRank %>%  ggplot(aes(x=reorder(region,-rank),y=total_points,fill=confederation)) +
      geom_bar(stat='identity') + coord_flip() + theme_light(10) + 
      scale_fill_brewer(name='',palette='Paired') + 
      theme(axis.text.y = element_text(size=5) ,legend.position='None', 
            panel.grid.major.y= element_blank()) +
      labs(title='Rank, with respect their total points, of the 206 countries in the FIFA',
           subtitle='as of June 2018') + facet_wrap(~confederation,scales='free')
  })
  
  
  }

shinyApp(ui, server)library(shinydashboard)
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
conf <-levels(data$confederation)
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
                           select(region, country_abrv, confederation, rank, total_points) %>%
                           arrange(region))
#Get information for countries from Maps package (Using ggplot2's function tho)
countriesMap <- map_data("world")
countriesMap$region <- tolower(countriesMap$region)
#Mutate-merge 'regions' from Maps with most recent rankings data
results <- data.frame(left_join(countriesMap, recentRank %>% select(confederation, region), by="region"))

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
               tabName = "Team_Analysis"
      ),
      menuItem("Team seeding",
               tabName = "Team_seeding"
      ),
      menuItem("Visualisations",
               tabName = "Visualisations"
      ),
      menuItem("Summary",
               tabName = "Summary"
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
                               title = h2(class ="plot-title", "A plot of total points against rank dates"),
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
                  )
                )
              )
      ),
      tabItem(tabName = "Team_seeding",
              fluidRow(
                h1("Team Seeding"),
                sidebarLayout(
                  mainPanel(
                    htmlOutput(outputId = "seeding_title")
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
                plotOutput(outputId = "recent_rank_totalPoints"),
                plotOutput(outputId = "confederation_plots")
              )
      ),
      tabItem(tabName = "Summary",
              fluidRow(
                
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
    avg_rank <- round(mean(country_ranks))
    if(avg_rank < 6){
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
    else if(avg_rank < 42){
      best_pos <-"Round 1"
    }
    else{
      best_pos <-"Doesnot qualify"
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
  
  #Team seeding
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
        "<h4>Click on a team to select it.</h4>"
      )
    }
    else if(input$confederation =="AFC"){
      HTML(
        "<h2>AFC</h2>",
        "<p>The Asian Football Confederation (AFC) is the governing body of association football in Asia and Australia and is one of the 6 FIFA confederations</p>",
        "<img src='japan.jpg' class='img-fluid' style='max-width: 100%;height: auto'/>"
        
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
  output$recent_rank_totalPoints <- renderPlot({
    recentRank %>% ggplot(aes(x=reorder(region, -rank), y=total_points, fill=confederation)) +
      geom_bar(stat="identity") + coord_flip() + theme_light() +
      scale_fill_brewer(palette="Paired") +
      theme(axis.text.y = element_text(size = 5), legend.position="None", panel.grid.major.y = element_blank()) + 
      labs(title="The rank, according to total points, of the 206 countries of FIFA")
    
  })
  output$confederation_plots <- renderPlot({
    recentRank %>%  ggplot(aes(x=reorder(region,-rank),y=total_points,fill=confederation)) +
      geom_bar(stat='identity') + coord_flip() + theme_light(10) + 
      scale_fill_brewer(name='',palette='Paired') + 
      theme(axis.text.y = element_text(size=5) ,legend.position='None', 
            panel.grid.major.y= element_blank()) +
      labs(title='Rank, with respect their total points, of the 206 countries in the FIFA',
           subtitle='as of June 2018') + facet_wrap(~confederation,scales='free')
  })
  
  
  }

shinyApp(ui, server)