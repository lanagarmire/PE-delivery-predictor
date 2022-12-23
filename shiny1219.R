library(markdown)
library(shiny)
library(ggplot2)
library(glmnet)
library(survival)
library(survminer)
source("helper.R")

models = c("Baseline Model", "Full Model", "EOPE Baseline Model", "EOPE Full Model")

  
ui <- shinyUI(
  navbarPage("Preeclampsia Prognosis Prediction Tool",
   # predict one patient           
   tabPanel("Prediction",
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
            h1("Prediction Panel"),
            p("Use the prediction panel to predict the prognosis index (PI) of new patients."),
            p("Select the preferred model and upload a csv file containing required patient information to make new predictions."),
            strong("Details see instruction")
          ),
          fluidRow(
            plotOutput("individual_dist")
          ),
          fluidRow(
            dataTableOutput("pi_table")
          )
        )
      )
    ),
   # input is a group of patients
   tabPanel("Validation",
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
                  h1("Validation Panel"),
                  p("Use the validation panel to validate pretrained model using new patient data and survival status."),
                  p("Select the preferred model and upload a csv file containing required patient information to validate model. Details see instruction")
                ),
                fluidRow(
                  column(6, plotOutput("time_theta_plt")),
                  column(6, plotOutput("survival_curve")),
                ),
                fluidRow(textOutput("cindex"))
              )
            )),
   tabPanel("Instruction",
            includeHTML("instruction.html")
            )
  )
)



server <- shinyServer(function(input, output){
  
################################### individual prediction#############################

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
  
  
  
  #Generate distribution plot
  output$individual_dist = renderPlot({
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
  
  
  
  
  ################################# validation ###################################
  
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
    #train = read.csv(paste0("data/",folder, "/cv_x.csv"), header = T)
    time_test = read.csv(paste0("data/",folder, "/hold_out_y.csv"))
    sta_test = read.csv(paste0("data/",folder, "/hold_out_y_status.csv"),header=T)
    # time_train = read.csv(paste0("data/",folder, "/cv_y.csv"),header=T)
    # sta_train = read.csv(paste0("data/",folder, "/cv_y_status.csv"), header=T)
    
    #process raw data
    if(input$data_process2 == "Yes"){
      test = scale_test(test)
    }
    
    return(list(W, b, test, time_test, sta_test, cv.tr, predTrain))
  })
  
  # predict test PI(theta)
  get_theta_group = reactive({
    #load variable list
    dat = data_ind2()
    #read in test data
    inFile <- input$target_upload2
    if (is.null(inFile)){
      test = dat[[3]]
      time_test = dat[[4]]
      
    }else{
      df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator2, row.names = NULL)
      test = df[,-ncol(df)]
      time_test = data.frame(df[, "time_to_deliver"])
    }

    theta = calculate_theta(dat[[1]], dat[[2]], test, dat[[6]], dat[[7]])
    return(list(theta[[1]], theta[[2]], time_test))
  })
  
  #plot theta vs time scatterplot
  output$time_theta_plt = renderPlot({
    result <- get_theta_group()
    predTrain = result[[1]]
    predTest = result[[2]]
    time_test = result[[3]]
   
    #extract predicted test theta and real test time
    coxnnet_result = data.frame(predTest, time_test)
    colnames(coxnnet_result) = c("theta", "time")
    ggplot(coxnnet_result, aes(x = theta, y = time))+
      geom_point(col = "steelblue")+
      labs(y = "Time to deliver(days)", x = "Prognosis Index")
  })
  
  #plot survival curve of high risk vs low risk group
  runSur <- reactive({
    result <- get_theta_group()
    predTrain = result[[1]]
    predTest = result[[2]]
    time_test = result[[3]]

    risk = c()
    # plot KM curves
    for(i in 1:length(predTest)){
      if(predTest[i]>quantile(predTest, probs = 0.75)){
        risk[i] = "High"
      }else if(predTest[i]<quantile(predTest, probs=0.25)){
        risk[i] = "Low"
      }else{
        risk[i] = "Middle"
      }
    }
    
    #high_risk = theta>median(theta)
    surv_curve = data.frame(time_test, risk)
    colnames(surv_curve)[1] = "time_test"
    form <- formula(paste("Surv(time_test, rep(1, length(time_test))) ~ risk"))
    eval(bquote(survfit(.(form),data=surv_curve)))
  })
  
  output$survival_curve = renderPlot({
    result <- get_theta_group()
    predTrain = result[[1]]
    predTest = result[[2]]
    time_test = result[[3]]
    
    risk = c()
    # plot KM curves
    for(i in 1:length(predTest)){
      if(predTest[i]>quantile(predTest, probs = 0.75)){
        risk[i] = "High"
      }else if(predTest[i]<quantile(predTest, probs=0.25)){
        risk[i] = "Low"
      }else{
        risk[i] = "Middle"
      }
    }
    
    #high_risk = theta>median(theta)
    surv_curve = data.frame(time_test, risk)
    #surv_test = survfit(, data = surv_curve)
    survminer::ggsurvplot(fit = runSur(), data = surv_curve, conf.int = T)+labs(x = "Time to deliver(days)")
  })
  
#text output testing cindex
  output$cindex = renderText({
    result = get_theta_group()
    predTest = result[[2]]
    time_test = result[[3]]

    sta_test = rep(1, nrow(time_test))
    ytest = data.frame(time = time_test, status = sta_test)
    colnames(ytest) = c("time", "status")
    cindex = round(Cindex(as.matrix(predTest),ytest),3)
    paste("C-index of the Cox-nnet model is :" , cindex)
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)
