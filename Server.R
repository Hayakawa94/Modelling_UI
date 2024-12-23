source("H:/Restricted Share/DA P&U/Tech Modelling/01 Home/Phase 2/15 R&D/Modelling_ui/Reactive_calc.R")
source("H:/Restricted Share/DA P&U/Tech Modelling/01 Home/Phase 2/15 R&D/Modelling_ui/UI.R")

# Reason for modelling  GBM in R
# Better modelling experience in general
# 1. Fitting raw data i.e. more predictive than banded data
# 2. More efficient at feature selection i.e. boruta or HP search i.e. Bayes optz or Flaml
# 3. Better explanation tools such as SHAP which theoretically based on a more realistic assumptions and generally better visualsation compared to PDP
# 4. Other functionality such as interaction constraints i.e. insurer code or AY

# we will have 4 tabs
# 1. feature selection
# 2. tune
# 3. train including feature importance, SHAP ,interaction_gain , tree plot , EIX plots
# 3. Model explain , waterfall plot for individual risk (use EIX package)
# 3. Model Comparison

# 6. Model performance
# 5. Extreme value
# 7. AvE
# 8. stability
# 9. github versioning contron

# Reasons for modelling in Radar
# 1. Deployability subject to recon
# 2. WTW ecosystem
# 3. Predictions unlikely to go wrong
# 
# train <- fread("df_eng_sample.csv")

# train <- df_eng_sample %>% sample_n(100000)



# Shuffle the dataset
# df_eng_sample <- df_eng_sample[sample(nrow(df_eng_sample)), ]


# train_model


# target_cols <- df_eng_sample %>% select(starts_with(c("freq" , "sev" , "exposure"))) %>%  names()
# df_eng_sample %>% select(-target_cols) %>% select_if(~is.numeric(.) ) %>% names -> fts
# 
# start = Sys.time()

# tune_model(fts = fts[1:20], model = "eow_s_b",train =train ,monotone_constraints = rep(0,20),interaction_constraints = list(1:20) ,min_child_weight = c(0.001,0.001)) -> test

##################### todo###############################
# model <- readRDS("Training.rds")
# readRDS("tuning.rds")$optz_result$best_params -> tuned_param
# tuned_param$monotone_constraints <- NULL
# tuned_param$interaction_constraints <-NULL
# tuned_param$gamma = 0 

# readRDS("feature_spec.rds")-> ft_spec
# readRDS("tuning_adsc.rds") -> tune_result
# tune_result$optz_result$best_params$min_child_weight ->min_child_weight
# tune_result$optz_result$best_params$min_child_weight<-NULL
# # # 
# train_model(fts = ft_spec %>% filter( Use_Feature==T) %>% dplyr::select(Features ) %>% pull,
#             model = "ad_s_c",
#             train = train,
#             parallel = T  ,
#             train_validate_ratio = 0.8,
#             use_tunred_HP = tune_result$optz_result$best_params ,
#             min_child_weight = min_child_weight ) -> model_result
# # # 
# debug(train_model)

# > test2$imp_plot$EIXinteraction_gain -> testzzz
# > testzzz[,c("meanGain" ,"mean5Gain" , "meanCover")] <-0
# testzzz[,c("meanGain" ,"mean5Gain" , "meanCover")] <-0
#plot( test2$imp_plot$EIXinteraction_gain ,radar = F , xmeasure = "frequency", ymeasure = "sumGain" , top = 4 )
#https://cran.r-project.org/web/packages/EIX/vignettes/EIX.html#:~:text=Package%20EIX%20is%20the%20set%20of%20tools%20to,measures.%20EIX%20consists%20several%20functions%20to%20visualize%20results.
# Sys.time() -start

# todo 
# train<-df
# fts<- num_fts
# train <- fread("C:\\Users\\Khoa.Truong\\Work\\P2_ui_DATA\\train.csv")
# fts <- train %>% select(starts_with("num") )%>% names
server <- function(input, output, session) {
  initial_data <- data.frame(
    Features = fts,
    Use_Feature = rep(FALSE, length(fts)),
    Monotonicity = rep(0, length(fts)),
    Interaction_Constraints = rep(FALSE, length(fts)),
    stringsAsFactors = FALSE
  ) %>% arrange(Features)
  
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
  
  
  observeEvent(input$train_kfold, {
    if (input$train_kfold) {
      updateCheckboxInput(session, "train_Trainvalidate", value = FALSE)
    }
  })
  
  observeEvent(input$train_Trainvalidate, {
    if (input$train_Trainvalidate) {
      updateCheckboxInput(session, "train_kfold", value = FALSE)
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
    ft_spec_table  %>% select(Monotonicity) %>% pull -> monotone_constraints
    init_X = seq(1,length(fts_to_tune))
    interaction_constraints <-  lapply(which(ft_spec_table$Interaction_Constraints==T),function(x) c(x)) 
    append( list(setdiff(init_X, unlist(interaction_constraints))) ,interaction_constraints ) ->interaction_constraints
    print(interaction_constraints)
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
               monotone_constraints =monotone_constraints,
               gamma=input$gamma)
  })
  
  
  
  train_result <- eventReactive(input$train, {
    # req(input$use_early_stopping_rounds)
    gc()
    if (input$train_kfold == T    ){
      kfold = input$train_kfold_val
    }else{
      kfold = 0
    }
    if(input$use_early_stopping_rounds== T){
      early_stopping_rounds = 5
    }else{
      early_stopping_rounds = NULL
    }
    req(input$ft_table)
    ft_spec_table <- hot_to_r(input$ft_table)%>% filter(Use_Feature==T) 
    fts_to_train <- ft_spec_table$Features
    ft_spec_table  %>% select(Monotonicity) %>% pull -> monotone_constraints
    init_X = seq(1,length(fts_to_train))
    interaction_constraints <-  lapply(which(ft_spec_table$Interaction_Constraints==T),function(x) c(x)) 
    append( list(setdiff(init_X, unlist(interaction_constraints))) ,interaction_constraints ) ->interaction_constraints
    print(interaction_constraints)
    train_model(fts = fts_to_train,
               model = input$model,
               train = train,
               kfold = kfold,
               train_validate_ratio = input$train_Ratio,
               # use_tunred_HP = NULL,
               eta = input$train_eta,
               max_depth = input$train_max_depth,
               min_child_weight = input$train_min_child_weight,
               subsample = input$train_subsample,
               colsample_bytree = input$train_colsample_bytree,
               lambda = input$train_lambda,
               alpha = input$train_alpha,
               nrounds = input$train_nrounds,
               parallel = input$Distribute_Computation,
               gamma=input$gamma,
               interaction_constraints = interaction_constraints,
               monotone_constraints = monotone_constraints,
               early_stopping_rounds=early_stopping_rounds
               )
  })
  trained_model <- reactiveVal(NULL)
  
  shap_values <- reactiveVal(NULL)
  
  observeEvent(input$load_trained_model, {
    
    if (file.exists(glue("{input$file_name_SHAP}.rds"))){
      readRDS(glue("{input$file_name_SHAP}.rds"))$model_output -> model_output
      shap_values(model_output$shap_values)
      
      ft_imp <- model_output$imp_plot$imp_shap$data$variable
      updateSelectInput(session, "SHAP_ft", choices = ft_imp)
      updateSelectInput(session, "SHAP_X_ft1", choices = ft_imp)
      updateSelectInput(session, "SHAP_X_ft2", choices = ft_imp)
      output$Shap_value_loaded <- renderText("Shap_Value loaded.")
    }else{
      output$Shap_value_loaded <- renderText("File not found.")
    }

  
    
  })
  
  
  
  SHAP_plots <- reactive({
    req(shap_values())
    
    KT_plot_shap(sv =shap_values()$main_effect$shap_main_effect[[input$SHAP_ft]],
                              ft = shap_values()$main_effect$pred_data_main_effect[,input$SHAP_ft],
                              ft_name = "",
                              loess_strength = input$SHAP_smooth_strength ,
                              point_size = input$SHAP_pt_size,
                              alpha = input$SHAP_alpha,
                              sample_size = input$SHAP_sample_size)
    
    
  })
  
  
  SHAP_plots_X <- reactive({
    req(shap_values())
    if(input$SHAP_X_Fit_loess == T){
      loess_strength = input$SHAP_smooth_strength
    }else{
      loess_strength = 0
    }
    sv_X <- glue("{input$SHAP_X_ft1}.{input$SHAP_X_ft2}")
    KT_plot_shap_w_interaction(sv = shap_values()$interaction_effect$shap_interaction[[sv_X]],
                                              ft =shap_values()$interaction_effect$pred_data_interaction[,input$SHAP_X_ft1],
                                              ft_name = sv_X,
                                               interaction =shap_values()$interaction_effect$pred_data_interaction[,input$SHAP_X_ft2] ,
                                              loess_strength = loess_strength ,
                                              point_size = input$SHAP_pt_size,
                                              alpha = input$SHAP_alpha,
                                              sample_size = input$SHAP_sample_size)
    
  })
  

# Update the slider based on the number of trees in the model
  observeEvent(train_result(), {
    model <- train_result()$model
    trained_model(model)
    updateSliderInput(session, "tree_index", max = model$niter - 1)
  })

  # Render the tree plot
  output$tree_plot <- DiagrammeR::renderGrViz({
    req(trained_model())
    model <- trained_model()
    tree_index <- input$tree_index
    xgb.plot.tree(model = model, trees = tree_index)
  })

  
  output$SHAP_plot <-renderPlot({
    SHAP_plots()
  })
  
  output$SHAP_X_plot <-renderPlot({
    SHAP_plots_X()
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
  
  output$Gain_imp_plot <-renderPlotly({
    ggplotly( train_result()$imp_plot$imp_gain + theme_light())  %>% layout( height = 1000 )
  })
  
  output$SHAP_imp_plot <-renderPlotly({
     ggplotly( train_result()$imp_plot$imp_shap)%>% layout( height = 1000 )
  })
  # 
  # output$Interaction_matrix <-renderPlot({
  #   plot(train_result()$imp_plot$EIXinteraction_gain_matrix) +  theme(axis.text.x = element_text(size = 19),
  #                                                       axis.text.y = element_text(size = 19),
  #                                                     legend.title = element_text(size = 19),
  #                                                     legend.text = element_text(size = 19))
  # })
  
  output$Interaction_matrix <-renderPlotly({
    
    ggplotly(dcast(train_result()$imp_plot$EIXinteraction_gain_matrix , Parent ~ Child, value.var = "sumGain") %>%
     melt(id.vars = "Parent") %>%
     filter(!is.na(value)) %>%
     rename(Child = variable, sumGain = value) %>%
     ggplot(aes(x = Child, y = Parent, fill = sumGain))+
     geom_tile() +
     scale_fill_gradientn(colors =c( "aliceblue", "lightblue", "blue"))+
     theme_light()+
     theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=0.9)) ) %>% layout( height = 1000,width = 1000 )

     
   
  })
  
  observeEvent(train_result(), {
    model <- train_result()$model
    trained_model(model)
    updateSliderInput(session, "topfeatures", max = model$nfeatures-1)
    updateSelectInput(session,"y_axis" ,choices = c("sumGain" , "meanGain" , "sumCover" , "meanCover","frequency"))
    updateSelectInput(session,"x_axis" ,choices = c("sumCover" , "sumGain" , "meanGain"  , "meanCover","frequency"))
  })
  
  output$Interaction_gain <-renderPlot({
    plot(train_result()$imp_plot$EIXinteraction_gain ,top = input$topfeatures , text_size = 6) +  theme(
      legend.text = element_text(size = 17),  
      legend.title = element_text(size = 17)  
    )})
  
  output$Interaction_gain2 <-renderPlot({
    plot(train_result()$imp_plot$EIXinteraction_gain ,
         top = input$topfeatures , 
         xmeasure = input$x_axis, 
         ymeasure = input$y_axis , radar = F) })
  
  
  output$gain <-renderPlot({
    plot(train_result()$imp_plot$EIX_gain ,top = input$topfeatures , text_size = 6) +   theme(
      legend.text = element_text(size = 17),  
      legend.title = element_text(size = 17)  
    ) })
  
  output$gain2 <-renderPlot({
    plot(train_result()$imp_plot$EIX_gain ,
         top = input$topfeatures , 
         xmeasure = input$x_axis, 
         ymeasure = input$y_axis , radar = F ) })

  
  
  # Function to save Feature Spec tab state
  save_feature_spec <- function() {
    final_data <- hot_to_r(input$ft_table)
    file_name <- paste0(input$file_name_feature, ".rds")
    saveRDS(final_data, file_name)  # Save only the feature specification data
    output$action_message_feature <- renderText("Feature Spec state has been saved.")
  }
  
  load_feature_spec <- function() {
    file_name <- paste0(input$file_name_feature, ".rds")
    if (file.exists(file_name)) {
      loaded_data <- readRDS(file_name)
      table_data(loaded_data)  # Load the feature specification data
      output$action_message_feature <- renderText("Feature Spec state has been loaded.")
    } else {
      output$action_message_feature <- renderText("File not found.")
    }
  }
  
  # Function to save Tuning tab state
  save_tuning <- function() {
    file_name <- paste0(input$file_name_tuning, ".rds") 
    
    if (is.null( tune_result())){
      tune_attr <-list(inputs = reactiveValuesToList(input))
    }else {
      tune_attr <-list(inputs = reactiveValuesToList(input), optz_result = tune_result())
    }
    
    saveRDS(tune_attr, file_name)
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
  
  
  load_tuning_result <- function() {
    file_name <- paste0(input$file_name_tuned, ".rds")
    if (file.exists(file_name)) {
      loaded_state <- readRDS(file_name)$optz_result$best_params
      # for (name in names(loaded_state)) {
      #   if (name %in% names(input)) {
      #     updateSliderInput(session, name, value = loaded_state[[name]])
      #   }
      # }
      updateSliderInput(session, "train_eta", value = loaded_state$eta)
      updateSliderInput(session, "train_min_child_weight", value = loaded_state$min_child_weight)
      updateSliderInput(session, "train_max_depth", value = loaded_state$max_depth)
      updateSliderInput(session, "train_alpha", value = loaded_state$alpha)
      updateSliderInput(session, "train_lambda", value = loaded_state$lambda)
      updateSliderInput(session, "train_colsample_bytree", value = loaded_state$colsample_bytree)
      updateSliderInput(session, "train_subsample", value = loaded_state$subsample)
      updateSliderInput(session, "train_nrounds", value = loaded_state$nrounds)
      updateSliderInput(session, "train_gamma", value = loaded_state$gamma)
      output$action_message_tuning <- renderText("Tuning state has been loaded.")
    } else {
      output$action_message_tuning <- renderText("File not found.")
    }
  }
  # Function to save Training tab state
  save_Training <- function() {
    file_name <- paste0(input$file_name_Training, ".rds")
    saveRDS(list(
      train_eta = input$train_eta,
      train_min_child_weight = input$train_min_child_weight,
      train_max_depth = input$train_max_depth,
      train_alpha = input$train_alpha,
      train_lambda = input$train_lambda,
      train_colsample_bytree = input$train_colsample_bytree,
      train_subsample = input$train_subsample,
      train_nrounds = input$train_nrounds,
      train_Ratio = input$train_Ratio,
      train_kfold_val = input$train_kfold_val,
      model_output = train_result()
    ), file_name)
    
    output$action_message_training <- renderText("Training state has been saved.")
  }
  
  # Function to load Training tab state
  load_Training <- function() {
    file_name <- paste0(input$file_name_Training, ".rds")
    if (file.exists(file_name)) {
      loaded_state <- readRDS(file_name)
      updateSliderInput(session, "train_eta", value = loaded_state$train_eta)
      updateSliderInput(session, "train_min_child_weight", value = loaded_state$train_min_child_weight)
      updateSliderInput(session, "train_max_depth", value = loaded_state$train_max_depth)
      updateSliderInput(session, "train_alpha", value = loaded_state$train_alpha)
      updateSliderInput(session, "train_lambda", value = loaded_state$train_lambda)
      updateSliderInput(session, "train_colsample_bytree", value = loaded_state$train_colsample_bytree)
      updateSliderInput(session, "train_subsample", value = loaded_state$train_subsample)
      updateSliderInput(session, "train_nrounds", value = loaded_state$train_nrounds)
      updateSliderInput(session, "train_Ratio", value = loaded_state$train_Ratio)
      updateSliderInput(session, "train_kfold_val", value = loaded_state$train_kfold_val)
      updateSliderInput(session, "train_gamma", value = loaded_state$nrounds)
      output$action_message_training <- renderText("Training state has been loaded.")
    } else {
      output$action_message_training <- renderText("File not found.")
    }
  }
  
  observeEvent(input$save_Training, {
    save_Training()
  })
  
  observeEvent(input$load_Training, {
    load_Training()
  })
  
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
  observeEvent(input$load_tuning_result, {
    load_tuning_result()
  })
}

shinyApp(ui = ui, server = server)

