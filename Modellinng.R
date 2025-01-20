source("H:/Restricted Share/DA P&U/Tech Modelling/01 Home/Phase 2/15 R&D/Modelling_ui/Reactive_calc.R")


# Reason for modelling  GBM in R
# Better modelling experience in general
# 1. Fitting raw data i.e. more predictive than banded data
# 2. More efficient at feature selection i.e. boruta or HP search i.e. Bayes optz or Flaml
# 3. Better explanation tools such as SHAP which theoretically based on a more realistic assumptions and generally better visualsation compared to PDP
# 4. Other functionality such as interaction constraints i.e. insurer code or AY

# we will have 4 tabs
# 1. feature selection
# 2. tune
# 3. train including feature importance, SHAP ,interaction_gain
# 3. Model explain
# 3. Model Comparison

# 6. Model performance
# 5. Extreme value
# 7. AvE
# 8. stability

# Reasons for modelling in Radar
# 1. Deployability subject to recon
# 2. WTW ecosystem
# 3. Predictions unlikely to go wrong
# 
# df_eng_sample <- fread("df_eng_sample.csv")
# train <- df_eng_sample %>% sample_n(100000)



# Shuffle the dataset
# df_eng_sample <- df_eng_sample[sample(nrow(df_eng_sample)), ]


# train_model


# target_cols <- df_eng_sample %>% select(starts_with(c("freq" , "sev" , "exposure"))) %>%  names()
# df_eng_sample %>% select(-target_cols) %>% select_if(~is.numeric(.) ) %>% names -> fts
# 
# start = Sys.time()
# tune_model(fts = fts[1:20], model = "eow_f_b",train =train ) -> test
# train_model(fts = fts,model = "eow_s_b",train = train,parallel = T,max_depth = 10, nrounds = 30 ) -> test2
# Sys.time() -start



server <- function(input, output, session) {
  initial_data <- data.frame(
    Features = fts,
    Use_Feature = rep(FALSE, length(fts)),
    Monotonicity = rep(0, length(fts)),
    Interaction_Constraints = rep(FALSE, length(fts)),
    stringsAsFactors = FALSE
  )
  
  observeEvent(input$kfold, {
    if (input$kfold) {
      updateCheckboxInput(session, "Trainvalidate", value = FALSE)
    }
  })
  
  observeEvent(input$Trainvalidate, {
    if (input$Trainvalidate) {
      updateCheckboxInput(session, "kfold", value = FALSE)
    }
  })
  
  
  EDA_result<- eventReactive(input$EDA, {
    
    weight = model_spec[[input$model]]$exposure
    response = model_spec[[input$model]]$response
    objective =  model_spec[[input$model]]$objective
    eval_metric = model_spec[[input$model]]$eval_metric
    train <- train[train[[weight]] >0 ]
    train_y <- train[[response]]
    train_weight <- train[[weight]]
    
    
    req(input$ft_table)
    ft_spec_table <- hot_to_r(input$ft_table)
    fts_to_tune <- ft_spec_table$Features[which(ft_spec_table$Use_Feature == TRUE)]
    
    EDA<- summarise_dataframe(train %>% select(fts_to_tune) )
    total_weighted_response <- sum(train_y*train_weight)
    total_exposure <- sum(train_weight)
    weighted_avg_response <- total_weighted_response/total_exposure
    Max_response = max(train_y)
    Min_response = min(train_y)
    max_weight = max(train_weight)
    min_weight = min(train_weight)
    
    list(EDA = EDA, Claim_data = data.table(total_weighted_response,
                                            weighted_avg_response,
                                            Max_response,
                                            Min_response,
                                            max_weight,
                                            min_weight,
                                            total_weight=total_exposure, 
                                            Total_Risk = length(train_y)) %>% melt)
                                          
  })
  
  # Reactive value to store the table data
  table_data <- reactiveVal(initial_data)
  
  output$ft_table <- renderRHandsontable({
    rhandsontable(table_data(), useTypes = TRUE) %>%
      hot_col("Use_Feature", type = "checkbox") %>%
      hot_col("Monotonicity", type = "dropdown", source = c(-1, 0, 1)) %>%
      hot_col("Interaction_Constraints", type = "checkbox")
  })
  
  # Observer to reset the table data
  observeEvent(input$reset_table, {
    table_data(initial_data)
    output$action_message_feature <- renderText("Table has been reset to initial data.")
  })
  
  

  observeEvent(input$select_all, {
    table_data(initial_data %>% mutate(Use_Feature = rep(T, length(fts))))
    
  })
  
  
  
  
  tune_result <- eventReactive(input$tune, {
    gc()
    if (input$kfold == T    ){
      kfold = input$kfold_val
    }else{
      kfold = 0
    }
    req(input$ft_table)
    ft_spec_table <- hot_to_r(input$ft_table)%>% filter(Use_Feature==T) 
    fts_to_tune <- ft_spec_table$Features
    ft_spec_table %>% filter(Use_Feature==T ) %>% select(Monotonicity) %>% pull -> monotonicity_constraints
    interaction_constraints <-  lapply(which(ft_spec_table$Interaction_Constraints==T),function(x) c(x)) 
    tune_model(fts = fts_to_tune,
               model = input$model,
               train = train,
               kfold = kfold,
               train_validate_ratio = input$Ratio,
               eta = input$eta,
               max_depth = input$max_depth,
               min_child_weight = input$min_child_weight,
               subsample = input$subsample,
               colsample_bytree = input$colsample_bytree,
               lambda = input$lambda,
               alpha = input$alpha,
               nrounds = input$nrounds,
               parallel = input$Distribute_Computation,
               interaction_constraints = interaction_constraints,
               monotonicity_constraints =monotonicity_constraints )
  })
  
  
  
  train_result <- eventReactive(input$train, {
    gc()
    if (input$kfold == T    ){
      kfold = input$kfold_val
    }else{
      kfold = 0
    }
    req(input$ft_table)
    ft_spec_table <- hot_to_r(input$ft_table)
    fts_to_train <- ft_spec_table$Features[which(ft_spec_table$Use_Feature == TRUE)]
    ft_spec_table %>% filter(Use_Feature==T ) %>% select(Monotonicity) %>% pull -> monotonicity_constraints
    ft_spec_table %>% filter(Use_Feature==T & Interaction_Constraints ==T) %>% select(Features) %>% pull -> interaction_constraints
    interaction_constraints <- lapply(interaction_constraints,function(x) c(x)) 
    train_model(fts = fts_to_train,
               model = input$model,
               train = train,
               kfold = kfold,
               train_validate_ratio = input$Ratio,
               use_tunred_HP = NULL,
               eta = input$eta,
               max_depth = input$max_depth,
               min_child_weight = input$min_child_weight,
               subsample = input$subsample,
               colsample_bytree = input$colsample_bytree,
               lambda = input$lambda,
               alpha = input$alpha,
               nrounds = input$nrounds,
               parallel = input$Distribute_Computation,
               interaction_constraints = interaction_constraints,
               monotonicity_constraints =monotonicity_constraints )
  })
  
  output$tune_iteration_plot <- renderPlotly({
    tune_result()$hyperparameters_trends$tune_iteration
  })
  output$eta_plot <- renderPlotly({
    tune_result()$hyperparameters_trends$eta
  })
  output$max_depth_plot <- renderPlotly({
    tune_result()$hyperparameters_trends$max_depth
  })
  output$min_child_weight_plot <- renderPlotly({
    tune_result()$hyperparameters_trends$min_child_weight
  })
  output$subsample_plot <- renderPlotly({
    tune_result()$hyperparameters_trends$subsample
  })
  output$colsample_bytree_plot <- renderPlotly({
    tune_result()$hyperparameters_trends$colsample_bytree
  })
  output$lambda_plot <- renderPlotly({
    tune_result()$hyperparameters_trends$lambda
  })
  output$alpha_plot <- renderPlotly({
    tune_result()$hyperparameters_trends$alpha
  })
  output$opt_result_plot <- DT::renderDataTable({
    tune_result()$opt_results %>% mutate_all(~ round(., 3))
  })
  
  output$EDA_data <- DT::renderDataTable({
    datatable( EDA_result()$EDA ,options =  list(pageLength = 25) ) 
  })
  
  output$Claim <- DT::renderDataTable({
    EDA_result()$Claim_data 
  })
  
  
  # Function to save Feature Spec tab state
  save_feature_spec <- function() {
    final_data <- hot_to_r(input$ft_table)
    file_name <- paste0(input$file_name_feature, ".rds")
    saveRDS(list(data = final_data, inputs = reactiveValuesToList(input)), file_name)
    output$action_message_feature <- renderText("Feature Spec state has been saved.")
  }
  
  # Function to load Feature Spec tab state
  load_feature_spec <- function() {
    file_name <- paste0(input$file_name_feature, ".rds")
    if (file.exists(file_name)) {
      loaded_state <- readRDS(file_name)
      table_data(loaded_state$data)
      for (name in names(loaded_state$inputs)) {
        if (name %in% names(input)) {
          updateSliderInput(session, name, value = loaded_state$inputs[[name]])
        }
      }
      output$action_message_feature <- renderText("Feature Spec state has been loaded.")
    } else {
      output$action_message_feature <- renderText("File not found.")
    }
  }
  
  # Function to save Tuning tab state
  save_tuning <- function() {
    file_name <- paste0(input$file_name_tuning, ".rds")
    saveRDS(list(inputs = reactiveValuesToList(input), optz_result = tune_result()), file_name)
    output$action_message_tuning <- renderText("Tuning state has been saved.")
  }
  
  # Function to load Tuning tab state
  load_tuning <- function() {
    file_name <- paste0(input$file_name_tuning, ".rds")
    if (file.exists(file_name)) {
      loaded_state <- readRDS(file_name)
      for (name in names(loaded_state$inputs)) {
        if (name %in% names(input)) {
          updateSliderInput(session, name, value = loaded_state$inputs[[name]])
        }
      }
      output$action_message_tuning <- renderText("Tuning state has been loaded.")
    } else {
      output$action_message_tuning <- renderText("File not found.")
    }
  }
  
  observeEvent(input$save_feature, {
    save_feature_spec()
  })
  
  observeEvent(input$load_feature, {
    load_feature_spec()
  })
  
  observeEvent(input$save_tuning, {
    save_tuning()
  })
  
  observeEvent(input$load_tuning, {
    load_tuning()
  })
}

shinyApp(ui = ui, server = server)

