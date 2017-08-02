#Shiny app for dashboard

library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(plotly)

sampledata <- read.csv("C:/Users/hnandani/Documents/R files/Dummy Data.csv", header= TRUE, sep = ",", row.names = NULL)
sampledata[, "LPLV"] <- as.Date(sampledata[, "LPLV"], "%m/%d/%Y")


#UI side of the app

ui <- dashboardPage(
      
      dashboardHeader(title = "CDM Dashboard",
                      titleWidth = 230),
      
      dashboardSidebar(
        
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          
          checkboxGroupInput(inputId = "TA",
                             label = "Therapeutic Area",
                             choices = levels(unique(sampledata$Therapeutic.Area)),
                             selected = levels(unique(sampledata$Therapeutic.Area))),
          
          checkboxGroupInput(inputId = "phase",
                             label = "Trial Phase (Phase 1/2/3)",
                             choices = levels(unique(sampledata$Phase)),
                             selected = levels(unique(sampledata$Phase))),
          
          sliderInput(inputId = "LPLV",
                      label = "Choose LPLV Range",
                      min = min(sampledata$LPLV),
                      max = max(sampledata$LPLV),
                      value = c(min(sampledata$LPLV),max(sampledata$LPLV)))
          )),
      
      dashboardBody(
  
        fluidRow(
        column(width = 4,
                
        box(plotOutput("phaseTA", height = 340), title = "Number of Studies by TA and Phase", status= "primary",
        solidHeader = TRUE, width = NULL)),
    
        column(width = 3,
              
              box(title = "Approaching Milestones", width = NULL, footer=NULL, status = "primary", solidHeader = TRUE,
               
              infoBoxOutput(outputId = "LPLVbox", width = NULL),
    
              infoBoxOutput(outputId = "LPLVbox1", width = NULL),
              
              infoBoxOutput(outputId = "LPLVbox2", width = NULL)
                )),
        
        column(width = 5,
               
              box(plotlyOutput("scatter", height = 340), title = "Scatter plot of Pending SDV/Entry/Open Queries",
                  
                  status = "primary", solidHeader = TRUE, width = NULL)
                  )),
    
        fluidRow(  
  
column(12, DT::dataTableOutput("table"))
)))


# as.Date(LPLV, "%m/%d/%Y") <= last(as.Date(LPLV, "%m/%d/%Y")), as.Date(LPLV, "%m/%d/%Y") >= first(as.Date(LPLV, "%m/%d/%Y")))
#server side of the app
server <- function(input, output) {
  
  #creating datatable of all data
  
        output$table <- DT::renderDataTable(
              
            filter(sampledata, Phase %in% input$phase, Therapeutic.Area %in% input$TA, LPLV >= input$LPLV[1], LPLV <= input$LPLV[2]), 
                    options = list(scrollX = TRUE, dom = "tpi", columnDefs = list((list(width=50, targets= c(4,5,6,7))))), 
                    rownames = FALSE, extensions = "Responsive")

  #creating plot of phase and therapeutic area breakdowns
        
        output$phaseTA <- renderPlot({
            graphdata <- summarize(group_by(sampledata, Therapeutic.Area, Phase), count = n())
           ggplot(data=graphdata, aes(x = graphdata$Therapeutic.Area, y=graphdata$count, fill=graphdata$Phase, label=graphdata$count)) + geom_bar(stat="identity")+
              xlab("Therapeutic Area")+
              ylab("Study Count")+
              guides(fill=guide_legend("Study Phase"))+
              geom_text(size=4, position = position_stack(vjust=0.5))+
              theme(axis.text.y = element_text(size = 12, hjust=1, face = "bold"), axis.title.y = element_text(size = 12, face="bold"))+
              theme(plot.title = element_text(size=14, hjust=0.5, face = "bold"))+
              theme(axis.text.x = element_text(size = 12, angle = 30, vjust = 0.5, face = "bold"), axis.title.x = element_text(size = 12, face="bold"))+
              scale_x_discrete(limit = c("Nephrology", "Heart Disease","Parkinson's Disease (PD)", "Rare Diseases (RD)", "Type II Diabetes (T2D)"),
                               labels = c("Neph","Heart", "PD", "RD", "T2D"))+
              theme(panel.border = element_rect(colour = "black", fill=NA, size=2))})
           
    #creating data for the infobox LPLV
        
        LPLV_approach <- sampledata[,c("Therapeutic.Area", "Study.Number", "LPLV")]
        LPLV_approach <- filter(LPLV_approach, LPLV > Sys.Date())
        LPLV_approach <- filter(LPLV_approach, LPLV == min(LPLV))
      
    #creating LPLV infobox
        
        output$LPLVbox <- renderInfoBox({
            infoBox("Upcoming LPLV in", HTML(paste(paste(LPLV_approach[,3]-Sys.Date(), "days", sep=" "),LPLV_approach[,2],LPLV_approach[,1],
                                                sep="<br/>")),
            icon= shiny::icon("thermometer-full"),
            color = "red", fill=TRUE)})
          
    #creating data for infobox LPLV1
          
        LPLV_approach1 <- sampledata[,c("Therapeutic.Area", "Study.Number", "LPLV")]  
        LPLV_approach1 <- filter(LPLV_approach1, LPLV > Sys.Date())
        LPLV_approach1 <- arrange(LPLV_approach1, LPLV)
        LPLV_approach1 <- slice(LPLV_approach1, 2:2)
        
        
    #creating LPv infobox for LPLV1
        
        output$LPLVbox1 <- renderInfoBox({
            infoBox("Upcoming LPLV in", HTML(paste(paste(LPLV_approach1[,3]-Sys.Date(), "days", sep=" "),LPLV_approach1[,2],LPLV_approach1[,1],
                                                 sep="<br/>")),
                  icon= shiny::icon("thermometer-half"),
                  color = "yellow", fill=TRUE)})
        
        
        LPLV_approach2 <- sampledata[,c("Therapeutic.Area", "Study.Number", "LPLV")]  
        LPLV_approach2 <- filter(LPLV_approach2, LPLV > Sys.Date())
        LPLV_approach2 <- arrange(LPLV_approach2, LPLV)
        LPLV_approach2 <- slice(LPLV_approach2, 3:3)  
        
        
        output$LPLVbox2 <- renderInfoBox({
          infoBox("Upcoming LPLV in", HTML(paste(paste(LPLV_approach2[,3]-Sys.Date(), "days", sep=" "),LPLV_approach2[,2],LPLV_approach2[,1],
                                                 sep="<br/>")),
                  icon= shiny::icon("thermometer-0"),
                  color = "green", fill=TRUE)})
        
        
    #creating scatter plot of CRFs DE, SDV, Queries  
        
        output$scatter <- renderPlotly({
          
          scatterdata <- sampledata[,c("Therapeutic.Area", "Study.Number", "CRFs_PendingEntry", "CRFs_pendingSDV", "OpenQueries")]
          feat <- list(
            showline = TRUE,
            linecolor = toRGB("black"),
            linewidth = 3, rangemode = "tozero"
          )
          pal <- c("red", "green", "blue", "yellow", "orange")
          plot_ly(scatterdata, x = ~CRFs_PendingEntry, y = ~CRFs_pendingSDV, size = ~OpenQueries,
                  color = ~Therapeutic.Area, colors= pal, marker = list(sizeref = 0.15),
                  text = ~paste("Study: ", Study.Number, "PendingEntry: ", CRFs_PendingEntry, "Pending SDV: ", CRFs_pendingSDV, "OpenQueries: ", OpenQueries)) %>% layout(xaxis = feat, yaxis= feat)
        })
        
        
}
    
shinyApp(ui = ui, server = server)




