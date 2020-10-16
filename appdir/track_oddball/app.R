library(shiny)
library(dplyr)
library(jsonlite)
library(ggplot2)
library(anytime)
library(httr)
library(stringr)
library(gtools)
library(lubridate)

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
                  tabPanel("Odds Ratio", plotOutput("lor")),
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
    timerange = paste(strftime(startdate(), "%Y-%m-%dT%H:%M:%S"),strftime(stopdate(), "%Y-%m-%dT%H:%M:%S"), sep=',')
    query = paste(subj, paste('time__range', timerange, sep = "="), sep = "&")
    #print(query)
    resp = GET(url = query)
    trials = jsonlite::fromJSON(url(query))
    if (length(!is.na(trials$trial)) > 0) {
      trials = filter(trials,!is.na(trial))
    } else { return () }
    stimA = c()
    stimB = c()
    if (!is.null(trials$stimulus)) {
      for (i in seq(1,length(trials$stimulus))) {
        if (is.null(trials$stimulus[[i]])) {
          stimA = c(stimA,NA)
          stimB = c(stimB,NA)
        }
        stimA = c(stimA,trials$stimulus[[i]][1])
        stimB = c(stimB,trials$stimulus[[i]][2])
      }
      trials = subset(trials, select=-c(stimulus))
      trials$stimA = stimA
      trials$stimB = stimB
    } else {
      trials$stimA = NA
      trials$stimB = NA
    }
    trials$date = anytime(trials$time)
    trials = arrange(trials, date)
    if (!is.null(trials$correct)) {
      trials$rtime = trials$rtime/1e6
    } else {
      trials$correct = TRUE
      trials$rtime = NA
    }
    trials$cumul = as.numeric(trials$correct)
    trials$cumul[is.na(trials$cumul)] = 0
    trials$cumul = cumsum(trials$cumul)
    trials$ind = seq(1,dim(trials)[1])
    trials$response = as.factor(trials$response)
    trials$outcome = with(trials, interaction(response,correct))
    trials$H = NA
    trials$M = NA
    trials$CR = NA
    trials$FA = NA
    for (i in seq(1,dim(trials)[1])) {
      trials$H[i] = ifelse(trials$response[i] == "peck_left", 1, 0)
      trials$FA[i] = ifelse(trials$response[i] == "stimA", 1, 0)
      trials$M[i] = ifelse(trials$response[i] == "stimA" && trials$oddball[i] == TRUE, 1, 0)
      if (!is.null(trials$peckposition) && !is.na(trials$peckposition[i]) && trials$peckposition[i] > 0) {
        trials$CR[i] = ifelse(trials$peckposition[i] >= trials$bposition[i], (trials$bposition[i]-2), (trials$peckposition[i]-2))
        trials$CR[i] = ifelse(trials$peckposition[i] > (trials$bposition[i]+1), (trials$CR[i] + ((trials$peckposition[i]-1) - (trials$bposition[i]+1))), trials$CR[i])
      } else {
        trials$CR[i] = ifelse(trials$correct[i] == TRUE, (trials$block[i]-1), ifelse(trials$bposition[i] == trials$block[i], (trials$block[i]-2), (trials$block[i]-3)))
        trials$M[i] = ifelse(trials$correct[i] == TRUE, 0, 1)
      }
    }
    trials
  })
  
  #Get list of experiments within time range
  output$exp = renderUI({
    explist = unique(timedata()$experiment[!is.na(timedata()$experiment)])
    shapelist = unique(timedata()$program[!is.na(timedata()$program)])
    explist = rbind(explist,shapelist)
    explist = sort(explist,decreasing=TRUE)
    selectInput('experiment','Experiment:',explist)
  })
  
  #Filter data for chosen experiment
  expdata = reactive({
    req(input$experiment)
    if (!is.null(timedata()$experiment)) {
      filtereddata = filter(timedata(),experiment == input$experiment)
    } else {
      filtereddata = filter(timedata(),program == input$experiment)
    }
    if(dim(filtereddata)[1] == 0) {
      return()
    } else { filtereddata }
  })
  
  #Show number of records returned by API
  output$records = renderText({
    req(input$experiment)
    if (!is.null(dim(expdata())[1]) && dim(expdata())[1] > 0) {
      paste("Returned",as.character(dim(expdata())[1]),"records:",sep=" ")
    } else {
      "Fetching records..."
    }
  })
  
  #Plotting code
  output$cummulative = renderPlot({
    if (is.null(expdata())) {
      return()
    }
    req(input$experiment)
    if (is.null(dim(expdata())[1])) {
      return()
    } else if (dim(expdata())[1] == 0) {
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
  
  lorf = function(H,M,CR,FA) {
    if (!is.na(M) && M == 0) {
      M = 0.5
    }
    if (!is.na(FA) && FA == 0) {
      FA = 0.5
    }
    hm = (H/M)
    crfa = (CR/FA)
    return (log((hm*crfa)^0.5))
  }
  
  runningavg = reactive({
    if (input$time == 1 | input$time == 2) {
      20
    } else {
      50
    }
  })
  
  output$lor = renderPlot({
    if (is.null(expdata())) {
      return()
    } else if (!is.null(expdata()$program)) {
      return()
    }
    data = filter(expdata(),response != "early")
    #data = mutate(data,peckposition = case_when(!is.na(position) ~ position, !is.na(peckposition) ~ peckposition),position=NULL)
    data = filter(data, is.na(peckposition) | peckposition != 1)
    #data = filter(data,as.integer(format(anytime(date),"%H")) >= 10, as.integer(format(anytime(date),"%H")) <= 17)
    #data = filter(data,wday(anytime(date)) > 1, wday(anytime(date)) < 7)
    y = c()
    addon = ifelse(runningavg() < 50, 5, 10)
    for (i in seq(1,as.integer(dim(data)[1]),addon)) {
      t = lorf(sum(data$H[i:(i+runningavg())]),sum(data$M[i:(i+runningavg())]),sum(data$CR[i:(i+runningavg())]),sum(data$FA[i:(i+runningavg())]))
      y = c(y,t)
    }
    y = y[!is.na(y)]
    if (length(y) == 0){
      return()
    }
    oddsratio = as.data.frame(cbind(seq(1,length(y)),y))
    colnames(oddsratio) = c("x","y")
    oddsratio$y[which(oddsratio$y < 0)] = 0
    ggplot(oddsratio, aes(x=x, y=y)) + 
      geom_line(size=2) + 
      geom_hline(yintercept = 0, color="tomato", size=1) + 
      geom_hline(yintercept = 1, color="seagreen1",size=1) + 
      geom_hline(yintercept = 0.65, color="gold", linetype="dotted", size=1) + 
      scale_y_continuous(limits=c(0,max(y))) +
      xlab(paste("Running blocks of",as.character(runningavg()))) +
      ylab("Log Odds Ratio") +
      ggtitle("Running average of odds ratio performance")
  })
  
  bw = reactive({ 2 * IQR(expdata()$date) / length(expdata()$date)^(1/3) })
  
  output$outcome = renderPlot({
    if (is.null(expdata())) {
      return()
    }
    ggplot(expdata(), aes(date,fill=outcome)) + 
      geom_histogram(binwidth = bw()) + 
      xlab("Days") + 
      ylab("Number of trials") + 
      ggtitle("Summary of performance over time")
  })
  
  output$rt = renderPlot({
    if (is.null(expdata())) {
      return()
    }
    cortr = filter(expdata(), outcome=="peck_left.TRUE")
    ggplot(cortr, aes(x=ind,y=rtime)) + 
      geom_point() + 
      geom_smooth(method='lm', se = TRUE) + 
      xlab("Trials") + 
      ylab("Response time") + 
      ggtitle("Response time of pecks after target stimulus")
  })
  
  output$cumindex = renderPlot({
    if (is.null(expdata())) {
      return()
    }
    ggplot(expdata(),aes(x=ind,y=cumul)) + 
      geom_line() + 
      geom_point() +
      xlab("Trials") + 
      ylab("Correct responses") +
      ggtitle("Cummulative correct responses across trials")
  })
  
  output$mistake = renderPlot({
    if (is.null(expdata())) {
      return()
    }
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
