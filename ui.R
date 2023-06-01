library(shiny)
library(shinythemes)
library(plotly)
library(flexdashboard)

ui <- navbarPage(
  "Student Performance Dashboard",
  header = includeCSS("www/style.css"),
  theme = shinytheme('flatly'),
  fluidRow(
    column(width = 6,
      div(
        class = "card_container",
        fluidRow(
          column(width = 6,
                 div(
                   class = "card_gray",
                   flexdashboard::gaugeOutput("boys_gauge", height="200px",width = "400px"),
                   p("The average performance of boys is above average.")
                 )
                 
          ),
          column(width = 6,
                 div(
                   class = "card_gray",
                   flexdashboard::gaugeOutput("girls_gauge", height="200px",width = "400px"),
                   p("Girls have a higher average performance as compared to boys.")
                 )
          ),
          column(
            width = 12,
             div(
               class = "card_grey",
               selectInput("subjects",label = "Select subject",choices = c("math.score","reading.score","writing.score")),
               plotOutput("bar_grouped", height = "400px")
               
                 
             )
          )
          
          
        
        )
        
      
      )
      
    ),
    column( width = 6,
      div(
        class = "card_container",
        div(
          class = "card_grey",
          plotlyOutput("ethnic_bubble_plot", height = "300px"),
          p("The performance comparison amongst the ethnic groups shows quite some significant differences."),
          p("Students from Ethnic group E are leading in average performance followed by those in group D.")
        ),
        div(
          class = "card_grey",
          plotOutput("scatter_plot", height = "300px"),
          p("Interesting to note that boys generally perform well in Maths whereas girls perform well in Reading.")
        )
      )
    )
    
    
  ),
  fluidRow(
    
    column(
      width = 4,
      div(
        class = "card_grey",
        
        plotlyOutput("barplot_gender", height = "300px"),
        p("For both reading and writing score, girls perform better than boys. Boys perform better than girls in Math only."),
        p("Perhaps this is the reason for the overall mean performance of girls being better than boys.")
        
      )
    ),
    column(
      width = 4,
      div(
        class = "card_grey",
        selectInput("subject2",label = "Select Subject",choices = c("math.score","reading.score","writing.score")),
        plotOutput("combined_plot", height = "300px"),
        p("Ethnic group c had the highest number of students. Their performance was somewhat average in comparison to the other groups.")
        
      )
    ),
    column(
      width = 4,
    div(
      class = "card_grey",
      
      plotOutput("studytime_heatmap", height = "300px"),
      p("Generally, those who go out less have the best average performance."),
      p("The optimal performance is at those who study for >10 hours and going out is low.")
         
      )
    )
    
  )
  
)
