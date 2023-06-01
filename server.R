
library(plotly)
library(data.table)
library(dplyr)
library(ggplot2)

stud_perf <- fread("data/student_performance.csv")
stud_perf_subjects <- read.csv("data/StudentsPerformance.csv")

stud_perf <- stud_perf %>%
  mutate(goout=case_when(
    goout == 1 ~ "Very low",
    goout == 2 ~ "Low",
    goout == 3 ~ "Medium",
    goout == 4 ~ "High",
    goout == 5 ~ "Very High"
  ),
  studytime = case_when(
    studytime == 1 ~ "<2hours",
    studytime == 2 ~ "2 - 5 hours",
    studytime == 3 ~ "5 - 10 hours",
    studytime == 4 ~ ">10 hours"
  ))

fact_cols <- c("goout","studytime")
stud_perf$goout <- factor(stud_perf$goout, levels = c("Very low","Low","Medium","High","Very High"), ordered = TRUE)
stud_perf$studytime <- factor(stud_perf$studytime, levels = c("<2hours","2 - 5 hours","5 - 10 hours",">10 hours"), ordered = TRUE) 

server <- function(input,output){
  
  #gauge.df <- stud_perf_subjects %>% group_by(gender) %>% summarise(mean.gender.score = round(mean(score),2))
  
  output$boys_gauge <- flexdashboard::renderGauge({
    
    gauge(65.84, min = 0, max = 100, symbol = '', label = paste("Boys mean score"),gaugeSectors(
      success = c(100, 6), warning = c(5,1), danger = c(0, 1), colors = c("#CC6699")))
   # fig <-  plot_ly(
   #    domain = list(x = c(0, 1), y = c(0, 1)),
   #    value = 65.84,
   #    title = list(text = "Male mean score"),
   #    type = "indicator",
   #    mode = "gauge+number") %>%
   #    layout(margin = list(l=20,r=30))
   # 
   # fig 
   
  })
  
  output$girls_gauge <- flexdashboard::renderGauge({
    
    gauge(69.57, min = 0, max = 100, symbol = '', label = paste("Girls mean score"),gaugeSectors(
      success = c(100, 6), warning = c(5,1), danger = c(0, 1), colors = c("#CC6699")))
    
  # fig <-  plot_ly(
  #     domain = list(x = c(0, 1), y = c(0, 1)),
  #     value = 69.57,#gauge.df[gauge.df$gender=="female"],
  #     title = list(text = "Female mean score"),
  #     type = "indicator",
  #     mode = "gauge+number") %>%
  #     layout(margin = list(l=20,r=30))
  # 
  # fig
    
  })
  
  
  #stud_perf_subjects$mean.score <-rowMeans(stud_perf_subjects[c('math.score', 'reading.score','writing.score')], na.rm = TRUE)
  stud_perf_subjects$score.avg <-rowMeans(stud_perf_subjects[c('math.score', 'reading.score','writing.score')], na.rm = TRUE)
  
  output$ethnic_bubble_plot <- renderPlotly({
    
    setDT(stud_perf_subjects)[,.(mean_score=mean(score.avg)),by=c("race.ethnicity")] %>%
     plot_ly() %>%
      add_trace(x = ~reorder(race.ethnicity, mean_score), 
                y = ~mean_score,
                size = ~mean_score,
                color = ~race.ethnicity,
                alpha = 1.5,
                #sizes = c(200,4000),
                text = ~paste("Ethnic group: ",race.ethnicity, "\n","Mean score: ",round(mean_score,2)),
                type = "scatter",
                mode = "markers",
                marker = list(symbol = 'circle', sizemode = 'diameter',
                              line = list(width = 2, color = '#FFFFFF'), opacity=0.4)) %>%
      layout(
        showlegend = FALSE,
        title="Mean score with respect to ethnic groups",
        xaxis = list(
          title = "Race/Ethnic group"
        ),
        yaxis = list(
          title = "Mean score"
        ),
        plot_bgcolor  = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        fig_bgcolor   = "rgba(0, 0, 0, 0)"
      ) %>%
      config(displayModeBar = FALSE, displaylogo = FALSE, 
             scrollZoom = FALSE, showAxisDragHandles = TRUE, 
             showSendToCloud = FALSE)
  })
  
  output$studytime_heatmap <- renderPlot({
    
    stud_perf[,.(mean_score=mean(G3)),by=c("goout","studytime")] %>%
      ggplot(aes(x=goout,y=studytime,fill=mean_score)) +
      geom_tile() +
      geom_text(aes(label=round(mean_score,1))) + # this is the function that helps us to label the cells
      scale_fill_gradient(low = "white",high = "blue") + # you can use your own colors using this function
      labs(title="How studytime relates to Going out",y="Study time",x="Going out") + # Customizing your axis labels and title
      theme(
        panel.background = element_rect(fill = "#ABC8D9",
                                        colour = NA_character_), # necessary to avoid drawing panel outline
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        plot.background = element_rect(fill = "#ABC8D9",
                                       colour = NA_character_), # necessary to avoid drawing plot outline
        legend.background = element_rect(fill = "#ABC8D9"),
        legend.box.background = element_rect(fill = "#ABC8D9"),
        legend.key = element_rect(fill = "#ABC8D9")
      )
  })
  
  output$scatter_plot <- renderPlot({
    
    stud_perf_subjects %>%
      ggplot(aes(x=math.score,y=reading.score,color=gender)) +
      geom_jitter() +
      labs(title="Reading and Math scores scatter plot",y="Reading score",x="Math score") +
      theme(
      panel.background = element_rect(fill = "#ABC8D9",
                                      colour = NA_character_), # necessary to avoid drawing panel outline
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      plot.background = element_rect(fill = "#ABC8D9",
                                     colour = NA_character_), # necessary to avoid drawing plot outline
      legend.background = element_rect(fill = "#ABC8D9"),
      legend.box.background = element_rect(fill = "#ABC8D9"),
      legend.key = element_rect(fill = "#ABC8D9")
    )
  })
  
  stud_perf_subjects_melt <- data.table::melt(stud_perf_subjects,variable.name = "subjects",measure.vars = c("math.score","reading.score","writing.score"),value.name = "score")
  
  output$barplot_gender <- renderPlotly({
    
    setDT(stud_perf_subjects_melt)[,.(mean_score=mean(score)),by=c("gender","subjects")] %>% 
      plot_ly(
        x = ~gender,
        y = ~mean_score,
        color = ~subjects,
        text = ~paste0("Gender: ", gender, "\n", "Subject: ", subjects,"\n","Mean score: ",round(mean_score,2))) %>%
      add_bars(showlegend=TRUE, hoverinfo='text' ) %>%
      layout(title="Mean gender performance",
             yaxis = list(title = "Mean Score"),
             xaxis = list(title = "Gender"),
             plot_bgcolor  = "#ABC8D9",
             paper_bgcolor = "#ABC8D9",
             fig_bgcolor   = "#ABC8D9")
    
  })
  
  output$bar_grouped <- renderPlot({ 
    
    stud_perf_subjects_melt.groups <- data.table::melt(stud_perf_subjects_melt, variable.name="groupings",
                                    measure.vars = c("race.ethnicity","parental.level.of.education","test.preparation.course","lunch"),
                                    value.name = "group.values")
    
    #stud_perf_subjects_melt.grp <- setDT(stud_perf_subjects_melt.groups)[,mean_score:=mean(score),by=.(groupings,group.values)]
    
    stud_perf.groupings <- stud_perf_subjects_melt.groups %>%
      group_by(groupings,group.values,subjects) %>%
      summarise(mean.score=mean(score)) #%>% ungroup()
    
   #stud_perf_subjects_melt.grp <- stud_perf_subjects_melt.grp %>% unique()
    
    stud_perf.groupings %>% 
      filter(subjects==input$subjects) %>% 
      ggplot(aes(x = group.values, y = mean.score, fill=mean.score)) +
      geom_col() +
      theme_bw() +
      theme(axis.text.x = element_text(vjust = 0.9, hjust = 0.9, angle = 45),
            panel.background = element_rect(fill = "#ABC8D9",
                                            colour = NA_character_), # necessary to avoid drawing panel outline
            panel.grid.major = element_blank(), # get rid of major grid
            panel.grid.minor = element_blank(), # get rid of minor grid
            plot.background = element_rect(fill = "#ABC8D9",
                                           colour = NA_character_), # necessary to avoid drawing plot outline
            legend.background = element_rect(fill = "#ABC8D9"),
            legend.box.background = element_rect(fill = "#ABC8D9"),
            legend.key = element_rect(fill = "#ABC8D9")) +
      facet_wrap(~groupings, scales = "free") 
     
  }) 
  
  output$combined_plot <- renderPlot({
    
    stud.number <- stud_perf_subjects %>%
      group_by(race.ethnicity)  %>%
      mutate(number.studs = n())
    
      input$subject2
      
    stud.summary <- stud.number %>% 
      group_by(race.ethnicity) %>% 
      summarise(score = mean(get(input$subject2)),number.studs=number.studs)
    
    stud.summary <- stud.summary %>% unique()
    
    ggplot(stud.summary) + 
      geom_col(aes(x = race.ethnicity, y = score, fill=score)) +
      geom_line(aes(x = race.ethnicity, y = 1*number.studs), size = 1.5, color="green", group = 1, stat="identity") + 
      scale_y_continuous(sec.axis = sec_axis(~./1, name = "Number of students")) +
      labs(title=paste("Average ",input$subject2, " score per ethnic group and number of students"),
           x="Race/Ethnicity",y=paste("Average",input$subject2,"score")) +
      theme(axis.text.x = element_text(vjust = 0.9, hjust = 0.9, angle = 45),
            panel.background = element_rect(fill = "#ABC8D9",
                                            colour = NA_character_), # necessary to avoid drawing panel outline
            panel.grid.major = element_blank(), # get rid of major grid
            panel.grid.minor = element_blank(), # get rid of minor grid
            plot.background = element_rect(fill = "#ABC8D9",
                                           colour = NA_character_), # necessary to avoid drawing plot outline
            legend.background = element_rect(fill = "#ABC8D9"),
            legend.box.background = element_rect(fill = "#ABC8D9"),
            legend.key = element_rect(fill = "#ABC8D9"))
    
  })
  
}
