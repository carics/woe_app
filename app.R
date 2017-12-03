#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(scorecard)
library(ggplot2)
#library(caret)

hmeq<-read.csv("hmeq.csv")
hmeq$BAD <- as.factor(hmeq$BAD)
levels(hmeq$BAD)<-c("good","bad")
levels(hmeq$REASON)<-c("missing", "Debt_Cons", "Home_Improv")
levels(hmeq$JOB)[1] <- c("missing")
set.seed(101)
train.index <- caret::createDataPartition(hmeq$BAD, p = 0.75, list = FALSE)
train <- hmeq[train.index,]
#test  <- hmeq[-train.index,]
train_n <- train
train_n$REASON <- NULL; train_n$JOB<-NULL
n <- length(names(train_n))
train_c <- train[,c("BAD", "REASON","JOB")]
n2 <- length(names(train_c))
#bins <- woebin(train[,1:13], "BAD") # automatic binning
# woes have an opposite sign; they are calculated as distr.BAD/distr.GOOD
#woebin_plot(bins)$DEBTINC # plot woe bins for DEBTINc

# Define UI for application
ui <- function() {tagList(navbarPage("WOE Binning - Example",
    tabPanel("Continuous Variables",
              sidebarPanel(
                #img(src="datasci.png", height = 50, width = 150),
                #hr(),
                selectInput("variable","Choose the variable:", 
                    choices=names(train_n[,2:n]),
                    selected = "LOAN"),
                textInput("breaks", "Choose the breakpoints:", value=""),
                submitButton(text="Apply changes"),
                hr(),
                p("If you want, for example, to create four intervals with 
                    breakpoints 5,000, 10,000 and 30,000 you should type breakpoints 
                    in the following format:"),
                p(strong("5000 10000 30000")),
                hr(),
                tags$table(class="table",
                  tags$thead(tags$tr(tags$th("IV"), tags$th("Predictive power"))),
                  tags$tr(tags$th("0.02 to 0.10"), tags$th("Weak predictor")),
                  tags$tr(tags$th("0.10 to 0.30"), tags$th("Medium predictor")),
                  tags$tr(tags$th("0.30 to 0.50"), tags$th("Strong predictor")),
                  tags$tr(tags$th("> 0.50"), tags$th("Suspicious or too good to be true"))
                  )
              ),
              # Show a plot 
              mainPanel(
                plotOutput("plot")
              )
   ),
   tabPanel("Categorical Variables",
            sidebarPanel(
              #img(src="datasci.png", height = 50, width = 150),
              selectInput("variable2","Choose the variable:", 
                          choices=names(train_c[,2:n2]),
                          selected = "JOB"),
              textInput("breaks2", "Choose the categories:", value=""),
              submitButton(text="Apply changes"),
              hr(),
              p("If you want, for example, to merge 'Mgr' with 'Other' category, 
                'Self' with 'Sales', while leaving 'missing', 'Office' and 'ProfExe'
                 as individual categories, you should use the following format:"),
              p(strong("missing Office ProfExe Mgr+Other Self+Sales")),
              hr(),
              tags$table(class="table",
                         tags$thead(tags$tr(tags$th("IV"), tags$th("Predictive power"))),
                         tags$tr(tags$th("0.02 to 0.10"), tags$th("Weak predictor")),
                         tags$tr(tags$th("0.10 to 0.30"), tags$th("Medium predictor")),
                         tags$tr(tags$th("0.30 to 0.50"), tags$th("Strong predictor")),
                         tags$tr(tags$th("> 0.50"), tags$th("Suspicious or too good to be true"))
              )
            ),
            # Show a plot 
            mainPanel(
              plotOutput("plot2")
            )
          )
))}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$plot <- renderPlot({
     #bins<-woebin(train[,c("BAD", input$variable)],"BAD")
     if (nchar(input$breaks)==0) {
       bins <- woebin(train, y = "BAD", x = c(input$variable))
     } else {
       breaks <- strsplit(input$breaks, " ")
       breaks<-as.numeric(breaks[[1]])
       breaks<-list(breaks)
       names(breaks)<-input$variable
       bins <- woebin(train, y="BAD", x=c(input$variable), breaks_list=breaks)
     }
    df_loan<-data.frame(bins[[input$variable]])
    df_loan$bin <- factor(df_loan$bin, levels = df_loan$bin)
    total_iv<-max(df_loan$total_iv)
    df_loan$woe <- df_loan$woe*100
    bin_iv_max <- max(-df_loan$woe)
    bin_iv_min <- min(-df_loan$woe)
    ggplot(df_loan, aes(x=bin,y=(-woe)))+
      geom_bar(stat="identity", alpha=0.95, colour="black", fill="royalblue1")+ 
      geom_text(aes(label=paste(count,"loans"),y=bin_iv_min-10), size=4)+
      geom_text(aes(label=paste0("bad rate=",round(badprob*100,1),"%"),
                                y=bin_iv_max+15),size=4)+
      geom_text(aes(label=paste("IV =",round(bin_iv,4)),y=bin_iv_max+35),size=4)+
      labs(title=paste0(input$variable,": Total IV = ",as.character(round(total_iv,4))),
           caption="Data.SCi")+ 
      theme_light()+
      theme(axis.text.x = element_text(color="gray33",size=12))+
      xlab("Bins")+ ylab("WOE")+
      theme(plot.title = element_text(hjust = 0.5, face="bold", size=16),
            plot.caption = element_text(color = "blue"))
    #woebin_plot(bins)[input$variable]
   })


  output$plot2 <- renderPlot({
    if (nchar(input$breaks2)==0) {
      bins <- woebin(train, y = "BAD", x = c(input$variable2))
      bins[[input$variable2]]$bin<-gsub("%,%", "+", bins[[input$variable2]]$bin,
                                        fixed = T)
    } else {
      breaks <- strsplit(input$breaks2, " ")
      breaks <- breaks[[1]]
      breaks_l<-gsub("+","%,%",breaks, fixed=TRUE)
      breaks_l<-list(breaks_l)
      names(breaks_l)<-input$variable2
      i <- grep("+", breaks, fixed=TRUE)
      breaks[i] # "Mgr+Other"  "Self+Sales"
      trainx <- train
      trainx$Z <- as.character(trainx[,input$variable2])
      for (j in breaks[i]) {
        x <- strsplit(j, "%,%", fixed=TRUE) #list
        x <- x[[1]] #e.g. c("Mgr","Other")
        for (k in x) {
          trainx$Z[trainx[,input$variable2]==k] <- j # changing the names of categories (aka merging)
        }
      }
      trainx$Z<-factor(trainx$Z)
      trainx[,input$variable2]<-trainx$Z
      bins <- woebin(train, y="BAD", x=c(input$variable2), breaks_list = breaks_l)
      bins[[input$variable2]]$bin<-gsub("%,%","+",bins[[input$variable2]]$bin)
    }
    #woebin_plot(bins)[input$variable2]
    df_loan<-data.frame(bins[[input$variable2]])
    df_loan$bin <- factor(df_loan$bin, levels = df_loan$bin)
    total_iv<-max(df_loan$total_iv)
    df_loan$woe <- df_loan$woe*100
    bin_iv_max <- max(-df_loan$woe)
    bin_iv_min <- min(-df_loan$woe)
    ggplot(df_loan, aes(x=bin,y=(-woe)))+
      geom_bar(stat="identity", alpha=0.95, colour="black", fill="royalblue1")+
      geom_text(aes(label=paste(count,"loans"),y=bin_iv_min-10), size=4)+
      geom_text(aes(label=paste0("bad rate=",round(badprob*100,1),"%"),
                    y=bin_iv_max+15),size=4)+
      geom_text(aes(label=paste("IV =",round(bin_iv,4)),y=bin_iv_max+35),size=4)+
      labs(title=paste0(input$variable2,": Total IV = ",as.character(round(total_iv,4))),
           caption="Data.SCi")+
      theme_light()+
      theme(axis.text.x = element_text(color="gray33",size=12))+
      xlab("Bins")+ ylab("WOE")+
      theme(plot.title = element_text(hjust = 0.5, face="bold", size=16),
            plot.caption = element_text(color = "blue"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

