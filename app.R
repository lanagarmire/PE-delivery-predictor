library(markdown)
library(shiny)
library(ggplot2)
library(glmnet)
library(survival)
library(survminer)
source("helper.R")

options(shiny.host = '0.0.0.0')
options(shiny.port = 8091)

models = c("Baseline Model", "Full Model", "EOPE Baseline Model", "EOPE Full Model")

  
ui <- shinyUI(
  navbarPage("Preeclampsia Prognosis Prediction Tool",
   tabPanel("Individual Prediction",
            sidebarLayout(
              sidebarPanel(
                radioButtons("select_model2", "Please select model:", models),
                fileInput('target_upload2', 'Upload new data (see instruction)',
                          accept = c(
                            'text/csv',
                            'text/comma-separated-values',
                            '.csv'
                          )),
                radioButtons("separator2","Separator: ",choices = c(";",",",":"), selected=",",inline=TRUE),
                radioButtons("data_process2","Does the data need to be processed?", choices = c("Yes", "No"), selected = "No")
              ),
              mainPanel(
                fluidRow(
                  h1("Individual Patient Prediction Panel"),
                  p("Use the prediction panel to predict the prognosis index (PI) of one new patients."),
                  p("Select the preferred model and upload a csv file containing required patient information to make new predictions."),
                  strong("Details see instruction")
                ),
                fluidRow(
                  plotOutput("individual_dist")
                ),
                fluidRow(
                  dataTableOutput("pi_table_ind")
                )
              )
            )
            ),
   # predict a group of patient           
   tabPanel("Group Prediction",
      sidebarLayout(
        sidebarPanel(
          radioButtons("select_model1", "Please select model:", models),
          fileInput('target_upload1', 'Upload new data (see instruction)',
                    accept = c(
                      'text/csv',
                      'text/comma-separated-values',
                      '.csv'
                    )),
          radioButtons("separator1","Separator: ",choices = c(";",",",":"), selected=",",inline=TRUE),
          radioButtons("data_process1","Does the data need to be processed?", choices = c("Yes", "No"), selected = "No")
        ),
        mainPanel(
          fluidRow(
            h1("Group Prediction Panel"),
            p("Use the prediction panel to predict the prognosis index (PI) of a group of new patients."),
            p("Select the preferred model and upload a csv file containing required patient information to make new predictions."),
            strong("Details see instruction")
          ),
          fluidRow(
            plotOutput("group_dist")
          ),
          fluidRow(
            dataTableOutput("pi_table")
          )
        )
      )
    ),
   tabPanel("Instruction",
            includeHTML("instruction.html")
            )
  )
)



server <- shinyServer(function(input, output){
  
################################### group prediction#############################

    # load data based on selection on individual prediction
  data_ind1 = reactive({
    m = input$select_model1
    if(m == "Baseline Model"){
      folder = "base"
    }else if(m == "Full Model"){
      folder = "full"
    }else if(m == "EOPE Baseline Model"){
      folder = "eope_base"
    }else if(m == "EOPE Full Model"){
      folder = "eope_full"
    }
    
    load(paste0("data/", folder, "/model.RData"))
    W = read.csv(paste0("data/",folder, "/Wr.csv"), header = F)
    b = read.csv(paste0("data/",folder, "/br.csv"), header = F)
    test = read.csv(paste0("data/",folder, "/hold_out_x.csv"), header = T)
    #train = read.csv(paste0("data/",folder, "/cv_x.csv"), header = T)
    time_test = read.csv(paste0("data/",folder, "/hold_out_y.csv"))
    sta_test = read.csv(paste0("data/",folder, "/hold_out_y_status.csv"),header=T)
    #time_train = read.csv(paste0("data/",folder, "/cv_y.csv"),header=T)
    #sta_train = read.csv(paste0("data/",folder, "/cv_y_status.csv"), header=T)
    
    
    return(list(W, b, test, time_test, sta_test, cv.tr, predTrain))
  })
  
  
  #calculate new theta
  get_theta <- reactive({
    #load variable list
    dat = data_ind1()
    #read in test data
    inFile <- input$target_upload1
    if (is.null(inFile)){
      test = dat[[3]]
      
    }else{
      test <- read.csv(inFile$datapath, header = TRUE,sep = input$separator1, row.names = NULL)
    }
    #process raw data
    if(input$data_process1 == "Yes"){
      test = scale_test(test)
    }
    
    #calculate theta using function from source file
    theta = calculate_theta(dat[[1]], dat[[2]], test, dat[[6]], dat[[7]])
    return(list(theta[[1]], theta[[2]]))
  })
  
  
  #Generate group distribution plot
  output$group_dist = renderPlot({
    result <- get_theta()
    predTrain = result[[1]]
    predTest = result[[2]]
    df = data.frame(c(predTrain, predTest))
    colnames(df) = "train"
    #plot train theta histogram and mark test theta location
    ggplot(df, aes(x = train))+
      geom_histogram(aes(y = ..density..), 
                     binwidth = .5, colour = "black", fill = "white")+
      geom_density(alpha = .2, fill="#FF6655")+
      #geom_vline(aes(xintercept = predTest),
      #           colour = "red", linetype ="longdash", size = .8)+
      #annotate("text", x = predTest + 0.5, y =0.25, colour = "red",size = 6, 
      #         label = paste0("PI: ", round(predTest,2), "\n Higher risk than ", percent*100, "% patients."))+
      xlab("Prognosis Index")
  })
  
  output$pi_table = renderDataTable({
    result = get_theta()
    percent_tbl = percentile(result)
    pi_table = cbind(c(1:nrow(percent_tbl)),percent_tbl)
    colnames(pi_table) = c("Patient id", "Predicted PI", "Higher risk than % patients")
    pi_table
  })
  
  
  
  
  ################################# Individual Prediction ###################################
  
  # data loader
  data_ind2 = reactive({
    m = input$select_model2
    if(m == "Baseline Model"){
      folder = "base"
    }else if(m == "Full Model"){
      folder = "full"
    }else if(m == "EOPE Baseline Model"){
      folder = "eope_base"
    }else if(m == "EOPE Full Model"){
      folder = "eope_full"
    }
    load(paste0("data/", folder, "/model.RData"))
    W = read.csv(paste0("data/",folder, "/Wr.csv"), header = F)
    b = read.csv(paste0("data/",folder, "/br.csv"), header = F)
    test = read.csv(paste0("data/",folder, "/hold_out_x.csv"), header = T)
    test = test[1, ]
    
    return(list(W, b, test, cv.tr, predTrain))
  })
  
  # predict test PI(theta)
  get_theta_ind = reactive({
    #load variable list
    dat = data_ind2()
    #read in test data
    inFile <- input$target_upload2
    if (is.null(inFile)){
      test = dat[[3]]
      
    }else{
      test <- read.csv(inFile$datapath, header = TRUE,sep = input$separator2, row.names = NULL)
    }
    #process raw data
    if(input$data_process2 == "Yes"){
      test = scale_test(test)
    }
    

    theta = calculate_theta(dat[[1]], dat[[2]], test, dat[[4]], dat[[5]])
    return(list(theta[[1]], theta[[2]]))
  })
  
  # Generate individual prediction plot
  output$individual_dist = renderPlot({
    result <- get_theta_ind()
    predTrain = result[[1]]
    predTest = result[[2]]

    percent = percentile(result)[,2]
    df = data.frame(c(predTrain, predTest))
    colnames(df) = "train"
    #plot train theta histogram and mark test theta location
    ggplot(df, aes(x = train))+
      geom_histogram(aes(y = ..density..), 
                     binwidth = .5, colour = "black", fill = "white")+
      geom_density(alpha = .2, fill="#FF6655")+
      geom_vline(aes(xintercept = predTest),
                 colour = "red", linetype ="longdash", size = .8)+
      annotate("text", x = predTest + 0.5, y =0.25, colour = "red", size = 6,
               label = paste0("PI: ", round(predTest,2), "\n Higher risk than ", percent, "% patients."))+
      xlab("Prognosis Index")
  })
  
  output$pi_table_ind = renderDataTable({
    result = get_theta_ind()
    predTest = result[[2]]
    percent_tbl = percentile(result)
    pi_table = cbind(c(1:nrow(percent_tbl)),percent_tbl)
    colnames(pi_table) = c("Patient id", "Predicted PI", "Higher risk than % patients")
    pi_table
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)
