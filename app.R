#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(data.table)
library(plotly)
library(tidyverse)
library(readr)
library(dplyr)
library(DT)

load("env.RData")

# Data from the merged datasets
yelp_insp_data <- Combined
yelp_insp_data_grade <- Combined_clean_grade
yelp_insp_data_price <- Combined_clean_price
yelp_insp_data_rating <- Combined_clean_rating

yelp_insp_data_rating <- yelp_insp_data_rating[yelp_insp_data_rating$boro!="Missing", ]


colnames(yelp_insp_data)[colnames(yelp_insp_data)=="cuisine_description"] <- "cuisine"
colnames(yelp_insp_data_price)[colnames(yelp_insp_data_price)=="cuisine_description"] <- "cuisine"
colnames(yelp_insp_data_grade)[colnames(yelp_insp_data_grade)=="cuisine_description"] <- "cuisine"
colnames(yelp_insp_data_rating)[colnames(yelp_insp_data_rating)=="cuisine_description"] <- "cuisine"

# Filter by boro, gradem rating to see the distriubtion of our data
count.boro <- yelp_insp_data_grade %>% group_by(boro,grade) %>% summarise(Count=n())

rev.count.boro <- yelp_insp_data_rating %>% group_by(boro,rating) %>% summarise(Count=n())

# Filter the data frame to plot cuisine and num of counts for reviews
temp <- yelp_insp_data_grade %>% group_by(cuisine,grade) %>% summarise(Count=n()) %>% mutate(sum1=sum(Count))
count.cuisine <- temp[order(-temp$sum1),][1:52,]


temp2 <- yelp_insp_data_rating %>% group_by(cuisine,rating) %>% summarise(Count=n()) %>% mutate(sum1=sum(Count))
rev.count.cuisine <- temp2[order(-temp2$sum1),][1:52,]


#List of Boro and Cuisine for the menu
borolist <- c('ALL',unique(yelp_insp_data_grade$boro))
cuisinelist1 <- c('ALL',unique(count.cuisine$cuisine)) 
cuisinelist <- c('ALL',unique(rev.count.cuisine$cuisine))

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "NYC Restaurants: Rating & Inspection Grade", titleWidth=700),
  dashboardSidebar(
    sidebarMenu(
      #Menu items for the graphs
      menuItem("Main", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Inspection Data Graphs", icon = icon("chevron-circle-down"),
               menuSubItem('By Boro',tabName='boro',icon = icon("bar-chart")),
               menuSubItem('By Cuisine',tabName='cuisine',icon = icon("bar-chart"))
               #menuSubItem('Violation Type',tabName='vio_type',icon = icon("bar-chart"))
               ),
      menuItem("Rating Data Graphs",icon = icon("chevron-circle-down"),
               menuSubItem('Rating by boro',tabName='boro_rev',icon = icon("bar-chart")),
               menuSubItem('Rating by cuisine',tabName='cuisine_rev',icon = icon("bar-chart"))),
      menuItem("Rating vs Inspection", tabName = "rev_insp_box", icon = icon("map")),
      
      menuItem("Data", tabName = "data1", icon = icon("database"))
    )
    
  ),
  dashboardBody(
    tabItems(
    tabItem(tabName = "dashboard", fluidRow(box(img(src="ny_res2.jpg", height='1200',width="200%"))
             )
            ),
    tabItem(tabName='overview', 
            h3('Overview'),
            h5("The Department of Health and Mental Hygiene has been conducting regular inspections of all restaurants and cafeterias in New York City since July 2010. As part of the regular inspection cycle, an inspector will visit a given restaurant approximately yearly, unannounced, and note any violations of City and State food safety regulations. The number and severity of these violations determine a numerical score, which corresponds to a letter grade: A, B, or C.   
               As inspections continue and grades keep appearing on restaurant windows, public awareness of the restaurant inspections, and of restauraunt sanitation in general, rises. The question on every restaurant owner's mind then, is: *how bad would it be for business to receive a B or C on our next inspection?*
                Knowing the potential consequences of posting a B or C inspection grade on a restaurant's public image, customer reviews, and resulting change in overall patronage would be extremely valuable. It may inform a restaurant owner's decision to hire (or not) that more experienced, but more expensive cook who is better trained on industry-standard sanitation practices, or to put in that new bathroom, 
               reserved for employees only. Should the restaurant eventually earn a bad inspection grade, this knowledge could also help the owner decide whether to request a hearing to dispute the grade, or to just swallow the bitter pill and post it.")),
    tabItem(tabName='boro',
            h3('Inspection Grade by Boro'),
            fluidRow(box(selectizeInput("boro1","Boro", 
                                        borolist, selected='ALL'), width=10)),
            fluidRow(box(plotlyOutput("plotboro"), width=10)),
            fluidRow(box(plotlyOutput("plotboroper"), width=10))),
    
    tabItem(tabName='cuisine',
            h3('Inspection Grade by Cuisine'),
            fluidRow(box(selectizeInput("cuisine1","Cuisine", 
                                        cuisinelist1, selected='ALL'), width=10)),
            fluidRow(box(plotlyOutput("plotcuisine"), width=10)),
            fluidRow(box(plotlyOutput("plotcuisineper"), width=10))),
    
    tabItem(tabName='vio_type',
            h3('Violation by Type'),
            fluidRow(box(plotlyOutput("vio_type_plot"), width=10))
            #fluidRow(box(plotlyOutput("vio_zip"), width=10))
            ),
    
    tabItem(tabName='boro_rev',
            h3('Resraurant Rating by Boro'),
            fluidRow(box(selectizeInput("boro2","Boro", 
                                        borolist, selected='ALL'), width=10)),
            fluidRow(box(plotlyOutput("plotboro_rev"), width=10))),
    
    tabItem(tabName='cuisine_rev',
            h3('Resraurant Rating by Cuisine'),
            fluidRow(box(selectizeInput("cuisine2","Cuisine", 
                                        cuisinelist, selected='ALL'), width=10)),
            fluidRow(box(plotlyOutput("plotcuisine_rev"), width=10))),
    
    tabItem(tabName='rev_insp_box',
            h3('Rating-Inspection Score Box Plot (Takes Some Time)'),
            fluidRow(box(plotlyOutput("rev_insp_box"), height = "150%",width='100%'))),
    
    tabItem(tabName='data1',
            h3('Data'),
            fluidRow(dataTableOutput("table1")))
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  #React for the number of inspection by boro 
  reactboro=reactive({
    count.boro %>% 
      filter_(ifelse(input$boro1=="ALL",'boro %in% unique(count.boro$boro)','boro==input$boro1')) %>%
      group_by(boro,grade) %>%
      summarise(Total=sum(Count))
  })

  #React for the % of inspection by boro 
  reactboroper=reactive({
    count.boro %>% 
      filter_(ifelse(input$boro1=="ALL",'boro %in% unique(count.boro$boro)','boro==input$boro1')) %>%
      group_by(boro) %>% mutate(countB=sum(Count)) %>% group_by(grade, add = TRUE) %>% mutate(per = Count/ countB)
  })
  
  output$plotboro=renderPlotly({
    ggplot(data = reactboro()) + geom_bar(aes(x= boro, y=Total, fill=grade), stat = "identity", position=position_dodge()) + scale_y_continuous(labels = function(l) {paste0(l/1000, "K")})+ xlab("Borough") + ggtitle("Number of Inspection Grades by Borough (11/2015 - 11/2018)")
  })
  
  output$plotboroper=renderPlotly({
    ggplot(data=reactboroper()) + geom_bar(aes(x= boro, y=per, fill=grade), stat = "identity", position=position_dodge()) + ggtitle("Percentage of Inspection Grade by Region (11/2015 - 11/2018)") + xlab("Boro") + ylab("Percentage of Inspection Grade") 
  })
  
  ##### plot of inspection grade by cuisine    
  reactcuisine=reactive({
    count.cuisine %>% 
      filter_(ifelse(input$cuisine1=="ALL",'cuisine %in% unique(count.cuisine$cuisine)','cuisine==input$cuisine1')) %>%
      group_by(cuisine,grade) %>%
      summarise(Total=sum(Count))
  })
  
  #React for the % of inspection by boro 
  reactcuisineper=reactive({
    count.cuisine %>% 
      filter_(ifelse(input$cuisine1=="ALL",'cuisine %in% unique(count.cuisine$cuisine)','cuisine==input$cuisine1')) %>%
      group_by(cuisine) %>% mutate(countB=sum(Count)) %>% group_by(grade, add = TRUE) %>% mutate(per = Count/ countB)
  })
  
  output$plotcuisine=renderPlotly({
    ggplot(data=reactcuisine()) + geom_bar(aes(x=cuisine, y=Total, fill=grade), stat="identity", position=position_dodge()) + coord_flip() +
      theme(axis.text.y = element_text(face="bold", size=9, angle=45)) + ggtitle("Number of Inspection Grade by Cuisine") + xlab("Number") + ylab("Cuisine")
  })
  
  output$plotcuisineper=renderPlotly({
    ggplot(data=reactcuisineper()) + geom_bar(aes(x=cuisine, y=per, fill=grade), stat="identity", position=position_dodge()) + coord_flip() + 
      theme(axis.text.y = element_text(face="bold", size=9, angle=45)) + ggtitle("Percentage of Inspection Grade by Cuisine") + xlab("Percentage") + ylab("Percentage of Inspection by Cuisine")
  })
  
  ##### plot of review by boro    
  reactboro_rev=reactive({
    rev.count.boro %>% 
      filter_(ifelse(input$boro2=="ALL",'boro %in% unique(rev.count.boro$boro)','boro==input$boro2')) %>%
      group_by(boro,rating) %>%
      summarise(Total=sum(Count))
  })
  
  output$plotboro_rev=renderPlotly({
    ggplot(data = reactboro_rev()) + geom_bar(aes(x= boro, y=Total, fill=rating), stat = "identity", position="dodge") + ggtitle("Number of Rating by Region") + xlab("Boro") + ylab("Number of Rating")

  })       

  ##### plot of review by cuisine    
  reactcuisine_rev=reactive({
    rev.count.cuisine %>% 
      filter_(ifelse(input$cuisine2=="ALL",'cuisine %in% unique(rev.count.cuisine$cuisine)','cuisine==input$cuisine2')) %>%
      group_by(cuisine,rating) %>%
    summarise(Total=sum(Count))
  })

  output$plotcuisine_rev=renderPlotly({
    ggplot(data = reactcuisine_rev()) + geom_bar(aes(x= cuisine, y=Total, fill=rating), stat = "identity", position="dodge") + ggtitle("Number of Rating by Cuisine") + xlab("Cuisine") + ylab("Number") + coord_flip()

  })
  
  output$rev_insp_box= renderPlotly({
    ggplot(Combined_clean_rating)+
      geom_boxplot(aes(x=as.character(Combined_clean_rating$rating), y=Combined_clean_rating$score))+
      ggtitle("Yelp Rating vs. Inspection Score")+
      ylab("Inspection Score")+
      xlab("Avg. Yelp Rating")+
      ylim(0,75)+
      geom_hline(yintercept=13, color="green", linetype='dashed')+
      geom_text(aes(0.5,13,label = "A", vjust = 1.2), color='green')+
      geom_hline(yintercept=13.5, color="yellow2" , linetype='dashed')+
      geom_text(aes(0.5,13.5,label = "B", vjust = -0.25), color='yellow3')+
      geom_hline(yintercept=27, color="yellow", linetype='dashed')+
      geom_text(aes(0.5,27,label = "B", vjust = 1), color='yellow3')+
      geom_hline(yintercept=27.5, color="red", linetype='dashed')+
      geom_text(aes(0.5,27.5,label = "C", vjust = -0.25), color='red')
  })
  
  output$vio_zip = renderPlotly({
    plt3
  })
  
  output$vio_type_plot= renderPlotly({
    ggplot(Inspection_Results, aes(fct_infreq(Inspection_Results$Violation.Category)))+
      geom_bar(aes(fill = Inspection_Results$critical.flag))+
      scale_fill_manual(values = c("Yellow2","Red", "Blue"))+
      scale_y_continuous(labels = function(l) {paste0(l/1000, "K")})+
      ggtitle("Inspection Violations by Type")+
      xlab("Violation Type")+
      ylab("Count")+
      labs(fill='Critical Flag')+
      theme(axis.text.x = element_text(angle = 270, vjust = 0.45, hjust = 0))
  })
  
  filtered_data = yelp_insp_data
  

  
  # show data using DataTable
  output$table1 <- DT::renderDataTable({
    datatable(filtered_data, rownames=FALSE, options=list(scrollX=TRUE)) %>% 
      formatStyle(input$selected1, background="skyblue", fontWeight='bold')
  })

}


# Run the application 
shinyApp(ui = ui, server = server)

