library(shiny)
library(shinythemes)
library(plotly)
library(flexdashboard)

ui <- navbarPage(
  "Student Performance Dashboard",
  header = includeCSS("www/style.css"),
  theme = shinytheme('flatly'),
  
sidebarLayout(
  sidebarPanel(width = 2,
               tags$style(".well {background-color:#ABC8D9;}"), # styling the background color of the sidebarPanel.  
               div( class = "card_grey",
                    img(src = "studperf.png", height = 70, width = 180),   # sourcing the netflix image located in the www folder. 
                    br(),
                    br(),
               p("Higher education is critical to an individual's future success. Predicting academic success becomes
                  increasingly crucial as more kids enter colleges and universities.Alyahyan and Düştegör (2020). As a
                  student, I have witnessed students struggle with low academic achievement owing to a variety of
                  causes, and by employing data exploration techniques, I am able to uncover critical aspects that
                  influence a student's success."),
               p("This analytical tool servers to address the following issues: "),
               p("1. What are the significant factors that affect the academic performance of students?"),
               p("2. Are there any significant differences in academic performance between students of different ethnic groups?"),
               p("3. How does going out and study time relate to student performance?"),
               p("4. How are the gender comparisons like?")
               )
    
  ),
  
mainPanel(width = 10,
  
  fluidRow(
    column(width = 6,
      div(
        class = "card_container",
        fluidRow(
          column(width = 6,
                 div(
                   class = "card_gray",
                   flexdashboard::gaugeOutput("boys_gauge", height="200px",width = "250px"),
                   p("The average performance of boys is above average.")
                 )
                 
          ),
          column(width = 6,
                 div(
                   class = "card_gray",
                   flexdashboard::gaugeOutput("girls_gauge", height="200px",width = "250px"),
                   p("Girls have a higher average performance as compared to boys.")
                 )
          ),
          column(
            width = 12,
             div(
               class = "card_grey",
               selectInput("subjects",label = "Select subject",choices = c("math.score","reading.score","writing.score")),
               plotOutput("bar_grouped", height = "400px"),
               p("The graph does show that there is a difference in performance among students based on the level of education
                 of their parents. Also, the same case applies to test preparedness and lunch offerring at school.")
               
                 
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
          p("Students from Ethnic group E are leading in average performance followed by those in group D. 
            Might be good to do further research to understand why such differences and perhaps seek to improve the performance of
            students from ethnic groups with low performance.")
        ),
        div(
          class = "card_grey",
          plotlyOutput("scatter_plot", height = "300px"),
          p("Interesting to note that boys generally perform well in Maths whereas girls perform well in Reading. 
            Maybe this could be of help in using their respective strengths to inform their career paths and have discussions around
            such topics.")
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
        plotlyOutput("combined_plot", height = "300px"),
        p("Ethnic group c had the highest number of students. Their performance was somewhat average in comparison to the other groups.")
        
      )
    ),
    column(
      width = 4,
    div(
      class = "card_grey",
      
      plotlyOutput("studytime_heatmap", height = "300px"),
      p("Generally, those who go out less have the best average performance."),
      p("The optimal performance is at those who study for >10 hours and going out is low.
        This relationship is key in perhaps seeking to improve the performance of those who performing poorly.")
         
      )
    )
   )
  )
 )
  
)
