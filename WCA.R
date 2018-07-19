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
ui <- fluidPage(
  #Write the UI using different layouts provided in shiny
  #Include inputs and outputs using *Input() and *Output functons respectively
  titlePanel("World Cup Analyser"),
  hr(),
  
  navlistPanel(
    "Dashboard",
    tabPanel("Home",
             htmlOutput(outputId = "FIFA_desc"),
             plotOutput(outputId = "FIFA_nations"),
             htmlOutput(outputId = "FIFA_rank_desc"),
             hr()
             ),
    tabPanel("Team Analysis",
             widths = c(12, 12),
             h1("Team Performance Analysis"),
             sidebarLayout(
               mainPanel(
                 htmlOutput(outputId = "title")
               ),
               sidebarPanel(
                 selectInput(inputId = "country",
                             label ="Select a country",
                             choices = all_countries_sorted
                 )
               )
               
             ),
             fluidRow(
               column(
                 shiny::dataTableOutput(outputId = "team_table"), width = 12
               )
             ),
             tabsetPanel(
               tabPanel("plot1",
                        h3("A plot of total points against rank dates"),
                        plotOutput(outputId = "team_plot1",width = "870px")
                        ),
               tabPanel("plot2",
                        h3("A plot of total points against rank "),
                        plotOutput(outputId = "team_plot2",width = "870px")
               )
             )
             
    ),
    tabPanel("Team Seeding",
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
               )
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
              column( actionButton("seed", "Click to seed teams"),offset = 4,width = 12)
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
             
             ),
    tabPanel("Visualizations", 
             plotOutput(outputId = "recent_rank_totalPoints"),
             plotOutput(outputId = "confederation_plots")
             ),
    tabPanel("Summary", verbatimTextOutput(outputId = "summary"))
    
  ),
  mainPanel(
    tabsetPanel(
      type="pills"
    )
    
  )
)

#shiny app server
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
    HTML("<h3>Details</h3>",
         "<ul>
         <li>Country Name:<b>", input$country,"</b> </li>
         <li>Country ABRV:<b>",as.character(countries_filter[1,5]),"</b> </li>
         <li>Confederation:<b>",as.character(countries_filter[1,4]),"</b> </li>
         </ul>",
         "<h5><b>",input$country,"</b> performance analysis in the past 4 years</h5>",
         "</p>Team Stats</p>"
    )
  })
  
  output$team_table <- shiny::renderDataTable({
    countries_filter <-subset(WCR_data, country_full ==input$country & as.Date(rank_date) > as.Date("2014-06-07"))
    country_filter <-countries_filter[,c(1,3,6,7,8,9,10,11,12,13,14,15,16)]
  },options = list(scrollX = TRUE,pageLength = 10,order = list(1, 'desc')))
  
  output$team_plot1 <- renderPlot({
    countries_filter <-subset(WCR_data, country_full ==input$country & as.Date(rank_date) > as.Date("2014-06-07"))
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
    req(input$confederation)
    if(input$confederation =="All confederations"){
      HTML(
        "<h2>World Cup Tournament</h2>",
        "<p>Final tournament will play 32 teams, which are drawn into 8 groups with 4 teams in each group. Pot allocation is based on the FIFA rankings as of 07-june-2018. There are 4 pots were each pot contains 8 teams in their ranking order accoding to the teams that qualify for the world cup finals.</p>",
        "<h4>Click on a team to select it.</h4>"
        )
    }
    else if(input$confederation =="AFC"){
      HTML(
        "<h2>AFC Cup Tournament</h2>"
      )
    }
    
  })
  
  filteredTable_data <- reactive({
    countries_filter <-subset(WCR_data, as.Date(rank_date) == as.Date("2018-06-07"))
    filtered_data <-countries_filter[,c(1,2,4,5,6,7)]
  })
  
  output$Seeding_team_table <- DT::renderDataTable({
    filteredTable_data()
  },options = list(pageLength = 50),rownames=FALSE, caption = 'Table 1: You can select upto a maximum of 32 countries')
  
  filteredTable_selected <- reactive({
    ids <- input$Seeding_team_table_rows_selected
    table <- filteredTable_data()[ids,]
    table[order(table$rank),]
  })
  
  output$selected_teams_pot1 <- DT::renderDataTable({
    if(length(input$Seeding_team_table_rows_selected)>0){
      filteredTable_selected()[1:8,c(1,2)]
    }
  },rownames=FALSE,options=list(paging = FALSE,searching = FALSE),caption = 'POT 1') 
  
  output$selected_teams_pot2 <- DT::renderDataTable({
    if(length(input$Seeding_team_table_rows_selected)>8 ){
      filteredTable_selected()[9:16,c(1,2)]
      
    }
  },rownames=FALSE,options=list(paging = FALSE,searching = FALSE),caption = 'POT 2') 
  
  output$selected_teams_pot3 <- DT::renderDataTable({
    if(length(input$Seeding_team_table_rows_selected)>16 ){
      filteredTable_selected()[17:24,c(1,2)]
    }
  },rownames=FALSE,options=list(paging = FALSE,searching = FALSE),caption = 'POT 3') 
  
  output$selected_teams_pot4 <- DT::renderDataTable({
    if(length(input$Seeding_team_table_rows_selected)>24){
      filteredTable_selected()[25:32,c(1,2)]
    }
  },rownames=FALSE,options=list(paging = FALSE,searching = FALSE),caption = 'POT 4') 
  
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
                             tags$tr(
                               tags$th("Group ",letter)
                             ),
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

# combines ui and server into an app.
shinyApp(ui=ui,server=server)

