install.packages(c(
  "shiny",
  "shinydashboard",
  "ggplot2",
  "dplyr",
  "readr",
  "stringr",
  "lubridate",
  "tidyr"
))

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)

# Read Dataset
netflix <- read_csv("C:/Users/subha/OneDrive/Desktop/r project/netflix_titles.csv")

# ----------------------------
# Data Cleaning
# ----------------------------

# Convert Date
netflix$date_added <- mdy(netflix$date_added)

# Extract Year Added
netflix$year_added <- year(netflix$date_added)

# Extract Movie Duration
movies <- netflix %>%
  filter(type=="Movie") %>%
  mutate(duration_num=as.numeric(str_extract(duration,"\\d+")))

# Top Countries
country_data <- netflix %>%
  separate_rows(country, sep=",") %>%
  mutate(country=str_trim(country)) %>%
  count(country, sort=TRUE) %>%
  slice(1:10)

# Type Count
type_data <- netflix %>%
  count(type)

# Year Added
year_data <- netflix %>%
  filter(!is.na(year_added)) %>%
  count(year_added)

# Heatmap Data
heat_data <- netflix %>%
  count(type,rating)

# ----------------------------
# UI
# ----------------------------

ui <- dashboardPage(
  
  dashboardHeader(title="Netflix Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", icon=icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    
    fluidRow(
      
      valueBoxOutput("moviesBox",4),
      valueBoxOutput("tvBox",4),
      valueBoxOutput("totalBox",4)
      
    ),
    
    fluidRow(
      
      box(title="Top Countries",
          status="primary",
          solidHeader=TRUE,
          width=6,
          plotOutput("countryPlot",height=300)
      ),
      
      box(title="Movies vs TV Shows",
          status="warning",
          solidHeader=TRUE,
          width=6,
          plotOutput("piePlot",height=300)
      )
      
    ),
    
    fluidRow(
      
      box(title="Content Added Over Years",
          status="success",
          solidHeader=TRUE,
          width=6,
          plotOutput("linePlot",height=300)
      ),
      
      box(title="Movie Duration",
          status="danger",
          solidHeader=TRUE,
          width=6,
          plotOutput("boxPlot",height=300)
      )
      
    ),
    
    fluidRow(
      
      box(title="Ratings Heatmap",
          status="info",
          solidHeader=TRUE,
          width=12,
          plotOutput("heatPlot",height=400)
      )
      
    )
    
  )
)

# ----------------------------
# Server
# ----------------------------

server <- function(input, output){
  
  output$moviesBox <- renderValueBox({
    
    valueBox(
      sum(netflix$type=="Movie"),
      "Movies",
      icon=icon("film"),
      color="red"
    )
    
  })
  
  output$tvBox <- renderValueBox({
    
    valueBox(
      sum(netflix$type=="TV Show"),
      "TV Shows",
      icon=icon("tv"),
      color="green"
    )
    
  })
  
  output$totalBox <- renderValueBox({
    
    valueBox(
      nrow(netflix),
      "Total Titles",
      icon=icon("database"),
      color="blue"
    )
    
  })
  
  # ----------------------------
  # Bar Chart
  # ----------------------------
  
  output$countryPlot <- renderPlot({
    
    ggplot(country_data,
           aes(reorder(country,n),n,
               fill=n))+
      
      geom_col()+
      
      coord_flip()+
      
      labs(
        x="Country",
        y="Titles"
      )+
      
      theme_minimal(base_size=14)+
      
      theme(
        legend.position="none"
      )
    
  })
  
  # ----------------------------
  # Pie Chart
  # ----------------------------
  
  output$piePlot <- renderPlot({
    
    ggplot(type_data,
           aes(x="",
               y=n,
               fill=type))+
      
      geom_bar(
        width=1,
        stat="identity"
      )+
      
      coord_polar("y")+
      
      theme_void(base_size=15)+
      
      geom_text(
        aes(label=n),
        position=position_stack(vjust=0.5),
        color="white",
        size=5
      )
    
  })
  
  # ----------------------------
  # Line Chart
  # ----------------------------
  
  output$linePlot <- renderPlot({
    
    ggplot(year_data,
           aes(year_added,n))+
      
      geom_line(
        size=1.3,
        color="steelblue"
      )+
      
      geom_point(
        size=3,
        color="red"
      )+
      
      labs(
        x="Year",
        y="Titles Added"
      )+
      
      theme_minimal(base_size=14)
    
  })
  
  # ----------------------------
  # Box Plot
  # ----------------------------
  
  output$boxPlot <- renderPlot({
    
    ggplot(movies,
           aes(x=type,
               y=duration_num,
               fill=type))+
      
      geom_boxplot()+
      
      labs(
        x="",
        y="Minutes"
      )+
      
      theme_minimal(base_size=14)+
      
      theme(
        legend.position="none"
      )
    
  })
  
  # ----------------------------
  # Heatmap
  # ----------------------------
  
  output$heatPlot <- renderPlot({
    
    ggplot(
      heat_data,
      aes(
        rating,
        type,
        fill=n
      )
    )+
      
      geom_tile(color="white")+
      
      geom_text(aes(label=n),
                size=4)+
      
      labs(
        x="Rating",
        y="Type"
      )+
      
      theme_minimal(base_size=13)+
      
      theme(
        axis.text.x=
          element_text(
            angle=45,
            hjust=1
          )
      )
    
  })
  
}

# ----------------------------
# Run App
# ----------------------------

shinyApp(ui, server)