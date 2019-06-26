library(shiny)
library(dplyr)
library(jsonlite)
library(ggplot2)
library(anytime)
library(httr)
library(stringr)
library(gtools)

ui <- fluidPage(
  titlePanel("Operant DataViz"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput('subj'),
      radioButtons('time',label="Time range:", choices = list("Today" = 1, "Yesterday" = 2, "This week" = 3, "Custom" = 4)),
      conditionalPanel(
        condition = "input.time == 4",
        dateRangeInput("dates", label = "Date range")
      ),
      uiOutput('exp'),
      strong(textOutput('records')),
      downloadButton("downloadData", "Download"),
      br(),
      hr(),
      actionButton("update", "Update")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Timeline", plotOutput("cummulative")),
                  tabPanel("Outcomes", plotOutput("outcome")),
                  tabPanel("Cummulative", plotOutput("cumindex")),
                  tabPanel("Response time", plotOutput("rt")),
                  tabPanel("Mistakes", plotOutput("mistake"))
      )
    )
  )
)

server <- function(input,output) {
  
  #Get list of subjects from API
  output$subj = renderUI({
    s = fromJSON(url("https://aplonis.psyc.virginia.edu/decide/api/subjects/"))
    subjlist = s$name[s$name!="dummy"]
    selectInput('subject','Subject:',subjlist)
  })
  
  #Set start date of data request
  startdate = reactive({
    if (input$time == 1) {
      t = paste(Sys.Date(),"05:00:00 EDT")
      as.POSIXct(t)
    } else if (input$time == 2) {
      t = paste(Sys.Date()-1,"05:00:00 EDT")
      as.POSIXct(t)
    } else if (input$time == 3) {
      t = paste(Sys.Date()-7,"05:00:00 EDT")
      as.POSIXct(t)
    } else {
      t1 = paste(input$dates[1],"05:00:00 EDT")
      as.POSIXct(t1)
    }
  })
  
  #Set end date of data request
  stopdate = reactive({
    if (input$time == 2) {
      t2 = paste(Sys.Date()-1,"22:00:00 EDT")
      as.POSIXct(t2)
    } else if (input$time < 4) {
      Sys.time()
    } else {
      t2 = paste(input$dates[2],"22:00:00 EDT")
      as.POSIXct(t2)
    }
  })
  
  #Set plot label based on time selection
  labeltype = reactive({
    if (input$time == 1 | input$time == 2) {
      "%H:%M"
    } else {
      "%D"
    }
  })
  
  #Enable data update with button press
  update = eventReactive(input$update, {}, ignoreNULL = FALSE)

  #Request data from API and perform some computations for additional columns
  timedata = reactive({
    req(input$subject)
    u = update()
    subj = paste("https://aplonis.psyc.virginia.edu/decide/api/trials/?nocomment=True&no_page&subject=", input$subject, sep="")
    timerange = paste(strftime(startdate(), "%Y-%m-%dT%H:%M:%S%z"),strftime(stopdate(), "%Y-%m-%dT%H:%M:%S%z"), sep=',')
    query = paste(subj, paste('time__range', timerange, sep = "="), sep = "&")
    resp = GET(url = query)
    trials = jsonlite::fromJSON(url(query))
    trials = filter(trials,!is.na(trial))
    stimA = c()
    stimB = c()
    for (i in seq(1,length(trials$stimulus))) {
      stimA = c(stimA,trials$stimulus[[i]][1])
      stimB = c(stimB,trials$stimulus[[i]][2])
    }
    trials = subset(trials, select=-c(stimulus))
    trials$stimA = stimA
    trials$stimB = stimB
    trials$date = anytime(trials$time)
    trials = arrange(trials, date)
    trials$cumul = as.numeric(trials$correct)
    trials$cumul = cumsum(trials$cumul)
    trials$rtime = trials$rtime/1e6
    trials$ind = seq(1,dim(trials)[1])
    trials$response = as.factor(trials$response)
    trials$outcome = with(trials, interaction(response,correct))
    trials
  })
  
  #Get list of experiments within time range
  output$exp = renderUI({
    selectInput('experiment','Experiment:',unique(timedata()$experiment[!is.na(timedata()$experiment)]))
  })
  
  #Filter data for chosen experiment
  expdata = reactive({
    req(input$experiment)
    filter(timedata(),experiment == input$experiment)
  })
  
  #Show number of records returned by API
  output$records = renderText({
    req(input$experiment)
    if (dim(expdata())[1] > 0) {
      paste("Returned",as.character(dim(expdata())[1]),"records:",sep=" ")
    } else {
      "Fetching records..."
    }
  })
  
  #Plotting code
  output$cummulative = renderPlot({
    req(input$experiment)
    if (dim(expdata())[1] == 0) {
      return()
    } else {
      ggplot(expdata(),aes(x=date,y=cumul)) +
        geom_line() + 
        geom_point() + 
        scale_x_datetime(date_labels=labeltype(),limits=c(min(expdata()$date),max(expdata()$date))) + 
        xlab("Time of trials") + 
        ylab("Cumulative correct trials") + 
        ggtitle("Correct trials over time")
    }
  })
  
  bw = reactive({ 2 * IQR(expdata()$date) / length(expdata()$date)^(1/3) })
  
  output$outcome = renderPlot({
    ggplot(expdata(), aes(date,fill=outcome)) + 
      geom_histogram(binwidth = bw()) + 
      xlab("Days") + 
      ylab("Number of trials") + 
      ggtitle("Summary of performance over time")
  })
  
  output$rt = renderPlot({
    cortr = filter(expdata(), outcome=="peck_left.TRUE")
    ggplot(cortr, aes(x=ind,y=rtime)) + 
      geom_point() + 
      geom_smooth(method='lm', se = TRUE) + 
      xlab("Trials") + 
      ylab("Response time") + 
      ggtitle("Response time of pecks after target stimulus")
  })
  
  output$cumindex = renderPlot({
    ggplot(expdata(),aes(x=ind,y=cumul)) + 
      geom_line() + 
      geom_point() +
      xlab("Trials") + 
      ylab("Correct responses") +
      ggtitle("Cummulative correct responses across trials")
  })
  
  output$mistake = renderPlot({
    early = filter(expdata(), outcome=="stimA.FALSE")
    ggplot(early, aes(x=ind,y=rtime)) + 
      geom_point() + 
      geom_smooth(method='lm', se = TRUE) +
      xlab("Trials") +
      ylab("Response time") +
      ggtitle("Response time for incorrect pecks")
  })
  
  #Enable download of plotting data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$subject, Sys.Date(), 'trials.csv', sep='_')
    },
    content = function(con) {
      write.table(expdata(), con, sep=',')
    }
  )
}

shinyApp(ui = ui, server = server)
