library(shiny)
library(shinythemes)
library(data.table)
library(Rfacebook)
library(ggplot2)
library(ggthemes)
library(plotly)
library(scales)

# Change this with your Facebook API token
token <-'431098017231964|RcbxQoCWfsjuycYXVwd3WXM6Sas'

ui <- fluidPage(theme = shinytheme("slate"),
                
                headerPanel('How many likes, shares, comments and reactions does your page have?'),
                
                fluidRow(tags$head(tags$style(type="text/css", "
                    #loadmessage {
                      position: fixed;
                      top: 0px;
                      left: 0px;
                      width: 100%;
                      padding: 0.5px 0px 0px 0px;
                      text-align: center;
                      font-weight: bold;
                      font-size: 100%;
                      color: #000000;
                      background-color: #2196F3;
                      z-index: 105;
                      }
                    ")),
                  
                  column(3, h4('1st STEP - Define the Page'),
                         
                         textInput('page','Insert the name of the page you want to find', 'Simpsons'),
                         numericInput('npages', 'Number of pages to retrieve', 5),
                         actionButton(inputId = "gopages", label = "Get Pages"),
                         downloadButton('downloadData1', 'Download')),
                  
                  column(3, h4('2nd STEP - Extract Posts'),
                         
                         selectInput('page_id', 'Select the page', 'Simpsons'),
                         numericInput('nposts', 'Number of posts to retrieve', 100),
                         dateInput('since', 'Since when? (Have in mind Facebook released reaction buttons on 2016/02/24)', value = '2016/02/24', format = 'yyyy/mm/dd'),
                         actionButton(inputId = "retrieve", label = "Retrieve Facebook Data"),
                         downloadButton('downloadData2', 'Download')),
                  
                  column(4, h4('3rd STEP - Generate the Chart'), offeset=1,
                         
                         selectInput('xvar','Select the variable to display', c('likes_count', 'shares_count','love_count', 'haha_count',
                                                                                'love_count', 'wow_count', 'sad_count','angry_count'),'likes_count'),
                         selectInput('color','Select the variable to categorize your chart: ', c('type', 'day_of_the_week', 'period'), 'type'),
                         actionButton(inputId = "plot", label = "Plot Chart"))),
                
                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                 tags$div("Loading...",id="loadmessage")),
                
                titlePanel("Tabsets"),
                
                tabsetPanel(id='inTabset',
                  tabPanel('Pages', dataTableOutput('df_pages_out')),
                  tabPanel('Posts',dataTableOutput('df_data_out')),
                  tabPanel('Plot', plotlyOutput('plot1')))
                
                
)

server<-function(input, output, session) {
  
<<<<<<< HEAD
<<<<<<< HEAD
=======
  token <-'XXXX' # enter your own token here
=======
  token <-'431098017231964|RcbxQoCWfsjuycYXVwd3WXM6Sas' # enter your own token here
>>>>>>> 0644c0ec9aef5007ab2f6c872e0589faf317f377
  
>>>>>>> adaba859b843e387737380261e5258f26c062adf
  values <- reactiveValues(df_data = NULL, df_chart = NULL)
  
  observeEvent(input$gopages, {
    values$df_pages <- searchPages(input$page, token = token, n = input$npages)
    output$df_pages_out <- renderDataTable({values$df_pages}, options = list(pageLength = 5, scrollX = TRUE,
                                                                            lengthChange = FALSE, autowidth = TRUE,
                                                                            columnDefs = list(list(width = '70%', targets = 1))))
    updateTabsetPanel(session, "inTabset", selected = 'Pages')
  })
  
  output$downloadData1 <- downloadHandler(
    filename = function() { paste(input$page, '.csv', sep='') },
    content = function(file) {
      write.csv(values$df_pages, file)
    })
  
  observe({
  updateSelectInput(session, "page_id",
                    label = "Select the page",
                      choices = values$df_pages[,c('name')],
                    selected = head(values$df_pages[,c('name')], 1)
                    )
    })
  
  observeEvent(input$retrieve, {
    values$df_data <- getPage(values$df_pages[which(values$df_pages$name==input$page_id),c('id')], token = token, n=input$nposts, reactions = T, since = input$since)
    
    values$df_data$message_clean<-sapply(values$df_data$message,function(row) iconv(row, "latin1", "ASCII", sub=""))
    values$df_data$date<-as.POSIXct(values$df_data$created_time, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
    values$df_data$day_of_the_week<-format(values$df_data$date, "%A")
    values$df_data$hour<-format(values$df_data$date, "%H")
    values$df_data$period<-ifelse(values$df_data$hour %in% c("00","01","02","03","04","05"),"Dawn",
                                  ifelse(values$df_data$hour %in% c("06","07","08","09","10","11"),"Morning",
                                         ifelse(values$df_data$hour %in% c("12","13","14","15","16","17"),"Afternoon","Evening")))
    values$df_data<-values$df_data[,c('id', 'from_id', 'from_name', 'type', 'link', 'likes_count', 'story', 'comments_count',
                                      'shares_count', 'love_count', 'haha_count', 'wow_count', 'sad_count', 'angry_count',
                                      'message_clean', 'date', 'day_of_the_week', 'hour', 'period')]
    output$df_data_out <- renderDataTable({Sys.sleep(2); values$df_data}, options = list(lengthMenu = c(5, 25, 50), pageLength = 5, scrollX = TRUE,
                                                                           lengthChange = FALSE, autowidth = TRUE,
                                                                           columnDefs = list(list(width = '70%', targets = 1))))
    updateTabsetPanel(session, "inTabset", selected = 'Posts')
  })
  
  output$downloadData2 <- downloadHandler(
    filename = function() { paste(input$page, ' posts.csv', sep='') },
    content = function(file) {
      write.csv(values$df_data, file)
    })
  
  observeEvent(input$plot, {
    temp <- values$df_data[, c('date', input$xvar, input$color, 'message_clean')]
   
    output$plot1 <- renderPlotly({
      p<-ggplot(temp,aes(x = as.Date(date), y = temp[,2], color = temp[,3], text = message_clean)) +
        geom_point(aes(size=1)) +
        theme_hc(bgcolor = "darkunica") +
        scale_colour_hc("darkunica")+
        theme(
          panel.background = element_rect(fill = "grey16",
                                          colour = "grey16",
                                          size = 1, linetype = "blank"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
        labs (y= names(temp[2])) +
        labs (x= 'Period') +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%m-%Y") +
        labs(color= names(temp[3]))
      print(ggplotly(p, tooltip=c('text')))
      
  })
    updateTabsetPanel(session, "inTabset",
                      selected = 'Plot')
  })
}

shinyApp(ui = ui, server = server)
