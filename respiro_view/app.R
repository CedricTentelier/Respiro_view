library(shiny)
library(DT)
# The filter function (J. Labonne)
filter<-function(data,debut,nclose,nopen) {
  len<-length(data$Logtime)
  iter<-c(1:len)
  databis<- subset(data, iter>(debut-1) & (((iter-debut)/(nopen+nclose)-floor((iter-debut)/(nopen+nclose)))+0.00001)<(nclose/(nclose+nopen)) )
  nsession<-(round((length(databis$Logtime))/(nclose)))
  session<-rep(1:nsession,each=(nclose))
  databis<-subset(databis,c(1:length(databis$Logtime))<=length(session))
  intratime<-rep(1:nclose,nsession)
  datafinal<-data.frame(cbind(databis,session,intratime))
  return(datafinal)
}

ui <- fluidPage(
   
   # Application title
   titlePanel("Plot and analyse respiration data"),
   
   # Sidebar
   sidebarLayout(
      sidebarPanel(
        fileInput('data', 'Choose data file'),
        column(width=6,radioButtons("filetype", label = "File type",
                     choices = list("Raw" = 1, "Preformatted" = 2), 
                     selected = 1)),
        column(width=6,selectInput("sep_dec", label="Decimal separator",choices=c(",","."),selected =".")),
        numericInput("debut","First point to consider",value=20,step=1),
        fluidRow(
          column(width=6,numericInput(inputId = "nclose",label="Number of points close",value=17,step=1)),
          column(width=6,numericInput(inputId = "nopen",label="Number of points open",value=23,step=1))
        ),
        hr(),
        numericInput(inputId = "Nphases", "Number of phases",value=6,step=1),
        column(width=4,uiOutput("phase_type")),
        column(width=8,uiOutput("phase_slopes"))
      ),
      
      # Show a plot of the data
      mainPanel(
        plotOutput("plot", height = 500, width=800,
                   dblclick = "plot_dblclick",
                   click = "plot_click",
                   brush = brushOpts(id = "plot_brush",resetOnNew = TRUE)
        ),
        h3("Info on points near click"),
        verbatimTextOutput("click_info"),
        #verbatimTextOutput("lm_summary"),
        h3("Slopes"),
        tableOutput("table_lm"),
        h3("Data used in the analysis"),
        DT::dataTableOutput("table")
    )
  )
)

# The server part uses input to compute things and output them
server <- function(input, output) {
  output$phase_type <- renderUI({
    Nphases <- as.integer(input$Nphases)
    lapply(1:Nphases, function(i) {
      selectInput(inputId = paste0("phase",i), label = paste("Phase",i), 
                                 choices = list("Adapt" = 1, "Control" = 2, "Test" = 3), 
                                 selected = 1)
    })
  })
  output$phase_slopes <- renderUI({
    Nphases <- as.integer(input$Nphases)
    lapply(1:Nphases, function(i) {
      numericInput(inputId = paste0("slopes_phase",i),label=paste("Number of slopes in phase",i)
                                  ,value=0,step=1)
    })
  })
  
  # First, load the data file
  data <- reactive({ 
    req(input$data) ## ?req #  require that the input is available
    inFile <- input$data 
    if(input$filetype==1){
      df <- read.table(inFile$datapath, skip=39, sep=";", dec=input$sep_dec)
      colnames(df)<-c("Date","Time","Logtime","Oxy","Phase","Amp","Temp")
    }
    else
      df <- read.table(inFile$datapath,header = T)
 
    return(df)
  })
   
  fdata<-reactive(filter(data(),input$debut, input$nclose, input$nopen))
  contrast<-reactive(rep(c(input$phase1,input$phase2,input$phase3,input$phase4,input$phase5,input$phase6),c(input$slopes_phase1,input$slopes_phase2,input$slopes_phase3,input$slopes_phase4,input$slopes_phase5,input$slopes_phase6)*input$nclose))
  testB<-reactive(subset(fdata(),contrast()!=1))
  contrast2<-reactive(subset(contrast(),contrast()!=1))
  lm1<-reactive(summary(lm(testB()$Oxy~testB()$intratime*contrast2())))
  output$table_lm<-renderTable(matrix(nrow=2,ncol=3,
                 data=c("Control slope",lm1()$coefficients[2], 
                        "Test slope", lm1()$coefficients[2]+lm1()$coefficients[4],
                        "Test-control slope",lm1()$coefficients[4])),colnames=F)
  #output$lm_summary<-renderPrint({lm1()})
  output$table <- DT::renderDataTable(DT::datatable({
    data <- testB()
    data
  }))
  output$click_info <- renderPrint({
    nearPoints(data(), input$plot_click, xvar="Logtime", yvar="Oxy")
  })
  
  # Plot the data
  ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
   output$plot <- renderPlot({
    plot(data()$Logtime,data()$Oxy,xlim=ranges$x,ylim=ranges$y, xlab="Logtime", ylab=expression('O'[2]*' concentration'))
    points(fdata()$Logtime,fdata()$Oxy,col="red")
    points(testB()$Logtime,testB()$Oxy,col="red",pch=19)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

