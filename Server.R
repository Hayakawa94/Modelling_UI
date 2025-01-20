
source("H:/Restricted Share/DA P&U/Tech Modelling/Users/Khoa/RPMtools/RPMtools.R")
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
# train$none = "NA"
# fts <- train %>% select(starts_with("num") )%>% names %>% sort
# 
# nrow(train)
# set.seed(1)
# train$idx = 1:nrow(train)
# train %>% sample_frac(0.2) -> test
# train[setdiff(train$idx , test$idx)] %>% select(-idx) -> train
# test<- test %>% select(-idx)
# nrow(train) + nrow(test)

source("H:/Restricted Share/DA P&U/Tech Modelling/01 Home/Phase 2/15 R&D/Modelling_ui/Reactive_calc.R")
source("H:/Restricted Share/DA P&U/Tech Modelling/01 Home/Phase 2/15 R&D/Modelling_ui/UI.R")



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
    
    table_data(hot_to_r(input$ft_table) %>% mutate(Use_Feature = rep(T, nrow(hot_to_r(input$ft_table)))))
    session
    
  })
  
  
  correlation <-reactive({
    req(EDA_result())
    weight = model_spec[[input$model]]$exposure
    fts <-  hot_to_r(input$ft_table) %>% filter(Use_Feature==T)  %>% select(Features) %>% pull
    KT_plot_top_n_correlation(train[train[[weight]] >0 ] %>% select(fts) ,n = input$top_corr)
  })
  
  output$corr_topn_slider <- renderUI({
    n_fts <-  hot_to_r(input$ft_table) %>% filter(Use_Feature==T)  %>% select(Features) %>% pull %>% length()
    sliderInput("top_corr", "Number of Top Correlations:", 
                min = 1, max = n_fts, value = 5 , step = 1)
  })
  output$corr_plot <-  renderPlotly({
    correlation()
  })
  
  ######### Boruta
  load_boruta_result<- reactiveVal()
  Boruta_result <- eventReactive(input$Boruta_run  , {
    model = input$model
    weight = model_spec[[model]]$exposure
    response = model_spec[[model]]$response
    fts <-  hot_to_r(input$ft_table) %>% filter(Use_Feature==T)  %>% select(Features) %>% pull
    
    Bresult <- KT_Boruta(train = train[train[[weight]] >0] %>% select(fts),
              train_y = train[train[[weight]] >0][[response]],
              weight = train[train[[weight]] >0][[weight]],
              max_Runs = input$Boruta_max_run,
              eval_metric = model_spec[[model]]$eval_metric,
              objective = model_spec[[model]]$objective,
              nrounds = input$Boruta_nrounds,
              max.depth = input$Boruta_max_depth,
              eta =  input$Boruta_eta,
              early_stopping_rounds = 5
              )
    show("Update_ft_spec") 
    return(Bresult)
  })
  
  
  output$Boruta_imp <- renderPlot({
    Boruta_result()$Boruta_p
  })
  output$Boruta_shap_imp <- renderPlot({
    Boruta_result()$SHAP_imp_plot
  })
  
  observeEvent(input$Update_ft_spec, {
    if(is.null(load_boruta_result())){
      boruta_selected_fts <-Boruta_result()$selected_fts
    }else{
      boruta_selected_fts <-load_boruta_result()$selected_fts
    }
    hot_to_r(input$ft_table) %>% 
      left_join(data.table(Features=boruta_selected_fts , use = T) , by = "Features") %>%
      mutate(Use_Feature =use) %>% select(-use) -> boruta_ft_spec
    
    table_data(boruta_ft_spec)
    session
  })
  
  
  observe({
    
    req(Boruta_result())
    print("Print saving Boruta_result")
    saveRDS(list(result =  Boruta_result(),
                 Boruta_max_run=input$Boruta_max_run,
                 Boruta_eta = input$Boruta_eta,
                 Boruta_max_depth = input$Boruta_max_depth,
                 Boruta_nrounds=input$Boruta_nrounds), 
                 glue("{input$file_name_boruta}.rds"))
    print("finished saving Boruta_result")
  })
  
    
    observeEvent(input$load_boruta,{
    # req(Boruta_result())
    Boruta_result <- readRDS(glue("{input$file_name_boruta}.rds"))
    updateSliderInput(session, "Boruta_max_run",value = Boruta_result$Boruta_max_run)
    updateSliderInput(session, "Boruta_eta",value =Boruta_result$Boruta_eta)
    updateSliderInput(session, "Boruta_max_depth",value =Boruta_result$Boruta_max_depth)
    updateSliderInput(session, "Boruta_nrounds",value =Boruta_result$Boruta_nrounds)
    load_boruta_result(Boruta_result$result)
    output$Boruta_imp <- renderPlot({
      Boruta_result$result$Boruta_p
    })
    output$Boruta_shap_imp <- renderPlot({
      Boruta_result$result$SHAP_imp_plot
    })
    show("Update_ft_spec") 
   
  })
  
  
  tune_result <- eventReactive(input$tune, {
    gc()
    if (input$kfold == T    ){
      kfold = input$kfold_val
    }else{
      kfold = 0
    }
    req(input$ft_table)
    ft_spec_table <- hot_to_r(input$ft_table) %>% filter(Use_Feature==T) 
    if(nrow(ft_spec_table) ==0){
      print("No features were selected")
    }
    req(nrow(ft_spec_table)>0)
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
    if(nrow(ft_spec_table) ==0){
      print("No features were selected")
    }
    req(nrow(ft_spec_table)>0)
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
               parallel = T,
               gamma=input$gamma,
               interaction_constraints = interaction_constraints,
               monotone_constraints = monotone_constraints,
               early_stopping_rounds=early_stopping_rounds
               )
  })
  trained_model <- reactiveVal(NULL)
  
  shap_values <- reactiveVal(NULL)
  
  observeEvent(input$load_trained_model, {
    
    if (file.exists(glue("{input$file_name_Training}.rds"))){
      readRDS(glue("{input$file_name_Training}.rds"))$model_output -> model_output
      shap_values(model_output$shap_values)
      
      ft_imp <- model_output$imp_plot$imp_shap$data$variable
      updateSelectInput(session, "SHAP_ft", choices = sort( ft_imp))
      updateSelectInput(session, "SHAP_X_ft1", choices = sort( ft_imp))
      updateSelectInput(session, "SHAP_X_ft2", choices = sort( ft_imp))
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
  

  observeEvent(train_result(), {
    model <- train_result()$model
    trained_model(model)
    updateSliderInput(session, "tree_index", max = model$niter - 1)
  })

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
  
  output$gamma_plot <- renderPlotly({
    tune_result()$hyperparameters_trends$gamma
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
  
  
  output$imp_comparison <- renderPlot({
    KT_plot_compare_ft_imp(train_result()$model_output$imp_plot$imp_gain$data$Feature,
                           train_result()$model_output$imp_plot$imp_shap$data$variable) +
      theme(legend.position = "none") +
      theme_light(base_size = 18) + ggtitle("gain vs SHAP importance") 
  })

  
  output$Interaction_matrix <-renderPlotly({
    
    ggplotly(dcast(train_result()$imp_plot$EIXinteraction_gain_matrix , Parent ~ Child, value.var = "sumGain") %>%
     melt(id.vars = "Parent") %>%
     filter(!is.na(value)) %>%
     rename(Child = variable, sumGain = value) %>%
     arrange(-sumGain) %>%
     head(input$top_gain_X) %>%
     ggplot(aes(x = Child, y = Parent, fill = sumGain))+
     geom_tile() +
     scale_fill_gradientn(colors =c( "aliceblue", "lightblue", "blue"))+
     theme_light()+
     theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=0.9)) ) %>% layout( height = 1000,width = 1000 )

     
   
  })
  
  
  output$SHAP_Interaction_matrix <-renderPlotly({
    ggplotly(train_result()$imp_plot$imp_shap_X %>% 
               head(input$top_shap_X) %>%
               ggplot(.,aes(x = ft , y = interaction , fill = value)) + 
               geom_tile() + 
               scale_fill_gradientn(colors =c( "aliceblue", "lightblue", "blue"))+
               theme_light()+
               theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=0.9)) ) %>% 
      layout( height = 1000,width = 1000 )
    
    
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
      tune_result <- readRDS(file_name)
      loaded_state <- tune_result$input
      updateSliderInput(session, "eta", value = loaded_state$eta)
      updateSliderInput(session, "min_child_weight", value = loaded_state$min_child_weight)
      updateSliderInput(session, "max_depth", value = loaded_state$max_depth)
      updateSliderInput(session, "alpha", value = loaded_state$alpha)
      updateSliderInput(session, "lambda", value = loaded_state$lambda)
      updateSliderInput(session, "colsample_bytree", value = loaded_state$colsample_bytree)
      updateSliderInput(session, "subsample", value = loaded_state$subsample)
      updateSliderInput(session, "nrounds", value = loaded_state$nrounds)
      updateSliderInput(session, "gamma", value = loaded_state$gamma)
      
      hp_plots <- tune_result$optz_result$hyperparameters_trends
      
      
      output$tune_iteration_plot <- renderPlotly({
        hp_plots$tune_iteration
      })
      
      output$eta_plot <- renderPlotly({
        hp_plots$eta
      })
      
      output$max_depth_plot <- renderPlotly({
        hp_plots$max_depth
      })
      
      output$min_child_weight_plot <- renderPlotly({
        hp_plots$min_child_weight
      })
      
      output$colsample_bytree_plot <- renderPlotly({
        hp_plots$colsample_bytree
      })
      
      output$subsample_plot <- renderPlotly({
        hp_plots$subsample
      })
      
      output$lambda_plot <- renderPlotly({
        hp_plots$lambda
      })      
      output$alpha_plot <- renderPlotly({
        hp_plots$alpha
      })
      
      output$gamma_plot <- renderPlotly({
        hp_plots$gamma
      })
      
      output$opt_result_plot <- DT::renderDataTable({
        tune_result$optz_result$opt_results %>% mutate_all(~ round(., 3))
      })
      output$action_message_tuning <- renderText("Tuning state has been loaded.")
      
      
    } else {
      output$action_message_tuning <- renderText("File not found.")
    }
  }
  
  
  load_tuning_result <- function() {
    file_name <- paste0(input$file_name_tuned, ".rds")
    if (file.exists(file_name)) {
      tune_result<- readRDS(file_name)
      
      loaded_state <- tune_result$optz_result$best_params

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
      train_use_early_stopping_rounds = input$use_early_stopping_rounds,
      model_output = train_result(),
      ft_spec = hot_to_r(input$ft_table)
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
      updateSliderInput(session, "train_gamma", value = loaded_state$train_gamma)
      updateSliderInput(session, "use_early_stopping_rounds", value = loaded_state$train_use_early_stopping_rounds)
      updateSliderInput(session, "tree_index", max = loaded_state$model_output$model$niter -1)
      table_data(loaded_state$ft_spec)
      # Trigger the rendering of plots
      output$Gain_imp_plot <- renderPlotly({
        ggplotly(loaded_state$model_output$imp_plot$imp_gain + theme_light()) %>% layout(height = 1000)
      })
      
      output$SHAP_imp_plot <- renderPlotly({
        ggplotly(loaded_state$model_output$imp_plot$imp_shap) %>% layout(height = 1000)
      })
      
      output$gain <- renderPlot({
        plot(loaded_state$model_output$imp_plot$EIX_gain, top = input$topfeatures, text_size = 6) + theme(
          legend.text = element_text(size = 17),
          legend.title = element_text(size = 17)
        )
      })
      
      output$gain2 <- renderPlot({
        plot(loaded_state$model_output$imp_plot$EIX_gain,
             top = input$topfeatures,
             xmeasure = input$x_axis,
             ymeasure = input$y_axis,
             radar = FALSE)
      })
      
      output$Interaction_matrix <- renderPlotly({
        ggplotly(
          dcast(loaded_state$model_output$imp_plot$EIXinteraction_gain_matrix, Parent ~ Child, value.var = "sumGain") %>%
            melt(id.vars = "Parent") %>%
            filter(!is.na(value)) %>%
            rename(Child = variable, sumGain = value) %>%
            arrange(-sumGain) %>%
            head(input$top_gain_X) %>%
            ggplot(aes(x = Child, y = Parent, fill = sumGain)) +
            geom_tile() +
            scale_fill_gradientn(colors = c("aliceblue", "lightblue", "blue")) +
            theme_light() +
            theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.9))
        ) %>% layout(height = 1000, width = 1000)
      })
      
      
      output$SHAP_Interaction_matrix <-renderPlotly({
        ggplotly(loaded_state$model_output$imp_plot$imp_shap_X %>% 
                   head(input$top_shap_X) %>%
                   ggplot(.,aes(x = ft , y = interaction , fill = value)) + 
                   geom_tile() + 
                   scale_fill_gradientn(colors =c( "aliceblue", "lightblue", "blue"))+
                   theme_light()+
                   theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=0.9)) ) %>% 
          layout( height = 1000,width = 1000 )
        
        
      })
      
      
      output$Interaction_gain <- renderPlot({
        plot(loaded_state$model_output$imp_plot$EIXinteraction_gain, top = input$topfeatures, text_size = 6) + theme(
          legend.text = element_text(size = 17),
          legend.title = element_text(size = 17)
        )
      })
      
      output$Interaction_gain2 <- renderPlot({
        plot(loaded_state$model_output$imp_plot$EIXinteraction_gain,
             top = input$topfeatures,
             xmeasure = input$x_axis,
             ymeasure = input$y_axis,
             radar = FALSE)
      })
      
      output$tree_plot <- DiagrammeR::renderGrViz({
        req(loaded_state$model_output)
        model <- loaded_state$model_output$model
        # tree_index <- input$tree_index
        xgb.plot.tree(model = model, trees = input$tree_index)
      })
      
      
      output$imp_comparison <- renderPlot({
        KT_plot_compare_ft_imp(loaded_state$model_output$imp_plot$imp_gain$data$Feature,
                               loaded_state$model_output$imp_plot$imp_shap$data$variable) +
          theme(legend.position = "none")+
          theme_light(base_size = 18) + ggtitle("gain vs SHAP importance") 
      })
      
      output$action_message_training <- renderText("Training state has been loaded.")
    } else {
      output$action_message_training <- renderText("File not found.")
    }
  }
  
  
  
  observeEvent(train_result() ,{
    
    req(train_result())
    print("Saving trained result")
    save_Training()
    print("finished saving trained result")
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
  
  observeEvent(tune_result(), {
  
    req(tune_result())
    print("Saving tuned result")
    save_tuning()
    print("finished saving tuned result")
  })
  
  observeEvent(input$load_tuning, {
    load_tuning()
  })
  observeEvent(input$load_tuning_result, {
    load_tuning_result()
  })
  
  ######### GLM  Overlays ########
  drawn_shapes <- reactiveVal(list())
  x_range <- reactiveVal(NULL)
  y_range <- reactiveVal(NULL)
  y2_range <- reactiveVal(NULL)
  
  base_pred <- reactive( {
    req(file.exists(glue("{input$file_name_Training}.rds")))
    readRDS(glue("{input$file_name_Training}.rds"))$model_output$pred
  })
  
  overlays <- eventReactive({
    input$Fit
    input$Load_base_model
    base_pred()
    }, {
    req(base_pred())
    weight = model_spec[[input$model]]$exposure
    response = model_spec[[input$model]]$response
    fam = model_spec[[input$model]]$fam
    splines_dt <- do.call(rbind, drawn_shapes())
    glm_train <- train[train[[weight]] > 0] %>% select(unique(splines_dt$feature))
    if (nrow(splines_dt) > 0 && !is.null(splines_dt)) {
      glm_fit(glm_train, splines_dt, train[train[[weight]] > 0][[response]], base_pred(), train[train[[weight]] > 0][[weight]], fam)
    } else {
      return(NULL)
    }
  })
  
  fit_plot <- reactive({
    req(base_pred())
    weight = model_spec[[input$model]]$exposure
    response = model_spec[[input$model]]$response
    
    challenger <- if (is.null(overlays())) {
      base_pred()
    } else {
      base_pred() * overlays()$adj
    }
    plot_fit(
      ft = train[train[[weight]] > 0][[input$ft]],
      actual = train[train[[weight]] > 0][[response]] * train[train[[weight]] > 0][[weight]],
      pred = base_pred(),
      challenger = challenger,
      weight = train[train[[weight]] > 0][[weight]],
      rebase = TRUE,
      point_size = input$size_pt,
      lwd = input$size_line,
      fit_lines = input$fit_lines,
      ft_name= input$ft
    )
  })
  
  output$overlay_plot <- renderPlotly({
    req(fit_plot())
    p <- ggplotly(fit_plot()) %>% layout(
      legend = list(
        orientation = 'h',  
        x = 0.5,           
        xanchor = 'center', 
        y = -0.2            
      )
    )
    shapes <- drawn_shapes()[[input$ft]]
    
    if (!is.null(shapes)) {
      p <- p %>%
        add_segments(
          data = shapes,
          x = ~x0, xend = ~x1, y = ~y0, yend = ~y1,
          line = list(color = "red"),
          showlegend = FALSE
        )
    }
    
    p <- p %>%
      layout(
        dragmode = if (input$draw_mode) "drawline" else "pan",
        newshape = list(line = list(color = "red")),
        yaxis2 = list(overlaying = "y", side = "right", range = y2_range() %||% range(p$data$value))
      ) %>%
      event_register("plotly_relayout")
    
    isolate({
      if (is.null(x_range())) x_range(range(p$data$ft))
      if (is.null(y_range())) y_range(range(p$data$value))
      if (is.null(y2_range())) y2_range(range(p$data$value))
    })
    
    p
  })
  
  observeEvent(event_data("plotly_relayout"), {
    relayout_data <- event_data("plotly_relayout")
    if (!is.null(relayout_data)) {
      if (!is.null(relayout_data[["xaxis.range[0]"]])) x_range(c(relayout_data[["xaxis.range[0]"]], relayout_data[["xaxis.range[1]"]]))
      if (!is.null(relayout_data[["yaxis.range[0]"]])) y_range(c(relayout_data[["yaxis.range[0]"]], relayout_data[["yaxis.range[1]"]]))
      if (!is.null(relayout_data[["yaxis2.range[0]"]])) y2_range(c(relayout_data[["yaxis2.range[0]"]], relayout_data[["yaxis2.range[1]"]]))
      
      if (!is.null(relayout_data$shapes)) {
        shapes <- relayout_data$shapes
        if (is.data.frame(shapes) && nrow(shapes) > 0) {
          new_shapes <- data.frame(x0 = numeric(0), y0 = numeric(0), x1 = numeric(0), y1 = numeric(0), id = character(), feature = character(), range = character())
          for (i in seq_along(shapes$x0)) {
            if (!(shapes$x0[i] == 0 && shapes$y0[i] == 0 && shapes$x1[i] == 1 && shapes$y1[i] == 1)) {
              existing_shapes <- drawn_shapes()[[input$ft]]
              overlap <- any(existing_shapes$x0 <= shapes$x1[i] & existing_shapes$x1 >= shapes$x0[i])
              if (overlap) {
                existing_shape <- existing_shapes[existing_shapes$x1 >= shapes$x0[i], ]
                y_intercept <- existing_shape$y1
                shapes$y0[i] = y_intercept
                shapes$x0[i] = existing_shape$x1
                new_shapes <- rbind(new_shapes, data.frame(x0 = existing_shape$x0, y0 = existing_shape$y0, x1 = existing_shape$x1, y1 = existing_shape$y1, id = glue("{input$ft}_{round(existing_shape$x0, 0)}to{round(existing_shape$x1, 0)}"), feature = input$ft, range = glue("{round(existing_shape$x0, 0)}to{round(existing_shape$x1, 0)}")))
                existing_shapes <- existing_shapes[existing_shapes$x1 < shapes$x0[i] | existing_shapes$x0 > shapes$x1[i], ]
              }
              new_shapes <- rbind(new_shapes, data.frame(x0 = shapes$x0[i], y0 = shapes$y0[i], x1 = shapes$x1[i], y1 = shapes$y1[i], id = glue("{input$ft}_{round(shapes$x0[i], 0)}to{round(shapes$x1[i], 0)}"), feature = input$ft, range = glue("{round(shapes$x0[i], 0)}to{round(shapes$x1[i], 0)}")))
            }
          }
          all_shapes <- drawn_shapes()
          all_shapes[[input$ft]] <- rbind(existing_shapes, new_shapes)
          drawn_shapes(all_shapes)
          updateCheckboxGroupInput(session, "undo_shapes", choices = do.call(rbind, all_shapes)$id)
        }
      }
    }
  })
  
  observeEvent(input$undo, {
    req(input$undo_shapes)
    all_shapes <- drawn_shapes()
    for (ft in names(all_shapes)) {
      shapes <- all_shapes[[ft]]
      shapes <- shapes[!shapes$id %in% input$undo_shapes, ]
      all_shapes[[ft]] <- shapes
    }
    drawn_shapes(all_shapes)
    updateCheckboxGroupInput(session, "undo_shapes", choices = do.call(rbind, all_shapes)$id)
  })
  
  output$glm_fit <- renderTable({
    all_shapes <- drawn_shapes()
    if (is.null(overlays())) {
      do.call(rbind, all_shapes)%>% select(-x0,-x1 , -y0, -y1)
    } else {
      do.call(rbind, all_shapes) %>% left_join(overlays()$fit, by = "id") %>% select(-x0,-x1 , -y0, -y1)
    }
  })
  
  output$glm_summary <- renderPrint({
    req(overlays())
    summary(overlays()$model)
  })
  
  observeEvent(input$reset, {
    drawn_shapes(list())
    updateCheckboxGroupInput(session, "undo_shapes", choices = NULL)
  })
  
  output$export <- downloadHandler(
    filename = function() {
      paste("drawn_shapes", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      all_shapes <- drawn_shapes()
      combined_shapes <- do.call(rbind, all_shapes)
      write.csv(combined_shapes, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$save_glm, {
    req(drawn_shapes(),overlays(), base_pred())
    saveRDS(list(drawn_shapes = drawn_shapes(), undo_shapes = input$undo_shapes, pred =  base_pred() * overlays()$adj), file = paste0(input$glm_overlay_out, ".rds"))
  })
  
  observeEvent(input$load_glm, {
    loaded_data <- readRDS(paste0(input$glm_overlay_out, ".rds"))
    drawn_shapes(loaded_data$drawn_shapes)
    updateCheckboxGroupInput(session, "undo_shapes", choices = do.call(rbind, loaded_data$drawn_shapes)$id, selected = loaded_data$undo_shapes)
  })
  
  # AvE
  observeEvent(input$Load_base_model , {
    show("factor_consistency") 
  })
  observe({
    feature_choices <- c("none", "rnd_factor", fts)
    # updateSelectInput(session, "model", choices = sort(names(model_spec)))
    updateSelectInput(session, "ft", choices =  update_fts_list())
    updateSelectInput(session, "factor_consistency", choices = feature_choices)
    updateSelectInput(session, "filter_feature", choices = fts)
    updateSelectInput(session, "secondary_filter_feature", choices = fts)
    updateSelectInput(session, "tertiary_filter_feature", choices = fts)
  })
  
  update_fts_list <- eventReactive(base_pred(), {
    gbm_fts <- readRDS(glue("{input$file_name_Training}.rds"))$model_output$model$feature_name
  
    lapply(fts, function(x) 
      if(x %in% gbm_fts){paste(x , "(xgb fitted)" , " ")
    } else{x} )  %>% unlist() -> lab
    gbm_fts <-lapply(fts, function(x) x) %>% setNames(.,lab) 
    return(gbm_fts)
  
  })
  
  
  sampling <- reactive( {
    set.seed(1)
    weight <- tolower(model_spec[[input$model]]$exposure)
    response <- tolower(model_spec[[input$model]]$response)
    
    
    challenger <- if (is.null(overlays())) {
      base_pred()
    } else {
      base_pred() * overlays()$adj
    }
    df_sample = train[train[[weight]] > 0 ] %>% 
      select(fts,c(weight, response )) %>%
      mutate(pred =  challenger,
             none  = "NA") %>% sample_frac(input$samplesize) 
    
    return(
      list(df_sample=df_sample))
  })
  
  
  observeEvent( {
    input$filter_feature 
    input$Load_base_model },{
    # req( sampling()$df_sample)
    req(input$filter_feature)
    feature_data <- sampling()$df_sample[[input$filter_feature]]
    if (is.numeric(feature_data)) {
      output$filter_ui <- renderUI({
        sliderInput("feature_range", "Feature Range:", min = min(feature_data, na.rm = TRUE), max = max(feature_data, na.rm = TRUE), value = range(feature_data, na.rm = TRUE))
      })
    } else {
      output$filter_ui <- renderUI({
        checkboxGroupInput("feature_categories", "Select Categories:", choices = KT_dym_sort(unique(feature_data)), selected = KT_dym_sort(unique(feature_data)))
      })
    }
  })
  
  observeEvent({
    input$secondary_filter_feature
    input$Load_base_model }, {
    req(input$secondary_filter_feature)
    feature_data <- sampling()$df_sample[[input$secondary_filter_feature]]
    
    if (is.numeric(feature_data)) {
      output$secondary_filter_ui <- renderUI({
        sliderInput("secondary_feature_range", "Secondary Feature Range:", min = min(feature_data, na.rm = TRUE), max = max(feature_data, na.rm = TRUE), value = range(feature_data, na.rm = TRUE))
      })
    } else {
      output$secondary_filter_ui <- renderUI({
        checkboxGroupInput("secondary_feature_categories", "Select Secondary Categories:", choices = KT_dym_sort(unique(feature_data)), selected = KT_dym_sort(unique(feature_data)))
      })
    }
  })
  
  observeEvent({
    input$tertiary_filter_feature 
    input$Load_base_model }, {
    req(input$tertiary_filter_feature)
    feature_data <- sampling()$df_sample[[input$tertiary_filter_feature]]
    
    if (is.numeric(feature_data)) {
      output$tertiary_filter_ui <- renderUI({
        sliderInput("tertiary_feature_range", "Tertiary Feature Range:", min = min(feature_data, na.rm = TRUE), max = max(feature_data, na.rm = TRUE), value = range(feature_data, na.rm = TRUE))
      })
    } else {
      output$tertiary_filter_ui <- renderUI({
        checkboxGroupInput("tertiary_feature_categories", "Select Tertiary Categories:", choices = KT_dym_sort(unique(feature_data)), selected = KT_dym_sort(unique(feature_data)))
      })
    }
  })
  
  

  
  
  
  results <- reactive( {
    # req(input$ft, input$filter_feature, input$secondary_filter_feature, input$tertiary_filter_feature, input$model)
    df_sample <- sampling()$df_sample
    rnd_factor <- rep(1:4, as.integer(nrow(df_sample) / 4) + 1)
    df_sample$rnd_factor <- head(rnd_factor, nrow(df_sample))
    
    ft_data <- df_sample[[input$ft]]
    factor_consistency_data <- df_sample[[input$factor_consistency]]
    if (is.numeric(df_sample[[input$filter_feature]])) {
      filtered_data <- df_sample[df_sample[[input$filter_feature]] >= input$feature_range[1] & df_sample[[input$filter_feature]] <= input$feature_range[2], ]
    } else {
      filtered_data <- df_sample[df_sample[[input$filter_feature]] %in% input$feature_categories, ]
    }
    
    if (is.numeric(df_sample[[input$secondary_filter_feature]])) {
      filtered_data <- filtered_data[filtered_data[[input$secondary_filter_feature]] >= input$secondary_feature_range[1] & filtered_data[[input$secondary_filter_feature]] <= input$secondary_feature_range[2], ]
    } else {
      filtered_data <- filtered_data[filtered_data[[input$secondary_filter_feature]] %in% input$secondary_feature_categories, ]
    }
    
    if (is.numeric(df_sample[[input$tertiary_filter_feature]])) {
      filtered_data <- filtered_data[filtered_data[[input$tertiary_filter_feature]] >= input$tertiary_feature_range[1] & filtered_data[[input$tertiary_filter_feature]] <= input$tertiary_feature_range[2], ]
    } else {
      filtered_data <- filtered_data[filtered_data[[input$tertiary_filter_feature]] %in% input$tertiary_feature_categories, ]
    }
    print(glue("test {input$feature_categories}"))
    print(c(input$filter_feature , input$secondary_filter_feature ,input$tertiary_filter_feature ))
  
    weight <- tolower(model_spec[[input$model]]$exposure)
    response <- tolower(model_spec[[input$model]]$response)
    filtered_data$actual <- filtered_data[[response]] * filtered_data[[weight]]
    filtered_data$weight <- filtered_data[[weight]]

    suppressWarnings(calc_ave(ft = filtered_data[[input$ft]], 
                              actual = filtered_data$actual, 
                              pred = filtered_data$pred, 
                              weight = filtered_data$weight, 
                              factor_consistency = filtered_data[[input$factor_consistency]], 
                              rebase = input$rebase,
                              ft_name= input$ft))
  })
  
  cosmetic <- reactive({
    cosmetic_changes(p = results()$ave_plot,
                     alpha_pt = input$alpha_pt,
                     alpha_line = input$alpha_line,
                     size_pt = input$size_pt,
                     size_line = input$size_line,
                     fit_loess = input$fitloess,
                     smooth_strength = input$smooth_strength,
                     control_yaxis = input$y_lim, 
                     upper_lim =  input$y_interval[2],
                     lower_lim = input$y_interval[1])
  })
  
  
  output$avePlot <- renderPlotly({
    plot <- cosmetic()$ave_plot
    if (is.null(plot)) {
      return(NULL)}
    plot
  })
  
  # output$Sample_submitted <- renderText({  sample_flag <- sampling()$flag 
  # if (is.null(sample_flag)){
  #   return(NULL)
  # }
  # sample_flag })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ave_data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(list(ave_data = results()$ave_df , smoothed_data  = cosmetic()$smooth_data) , file)
    }
  )
  
  # ## Performance


  Performance <- eventReactive( input$performance, {
   
  
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
      if(nrow(ft_spec_table) ==0){
        print("No features were selected")
      }
      req(nrow(ft_spec_table)>0)
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
                  parallel = T,
                  gamma=input$gamma,
                  interaction_constraints = interaction_constraints,
                  monotone_constraints = monotone_constraints,
                  early_stopping_rounds=early_stopping_rounds,
                  return_pred_only = T,
                  seed = 123
      ) -> gbm_train_pred_w_diff_seed 
    
    
    
    train_result <- readRDS(glue("{input$file_name_Training}.rds"))
    weight = model_spec[[input$model]]$exposure
    response = model_spec[[input$model]]$response
    
    gbm_model <-  train_result$model_output$model
    
    
    
    gbm_train_pred <- train_result$model_output$pred 
    
    pred_diff <- gbm_train_pred_w_diff_seed/gbm_train_pred
    ggplot(data.table(diff=pred_diff),aes(x = diff ))+geom_histogram(bins=100)+ theme_light(base_size = 18) -> stability_hist
    
    lapply(seq(0.01,0.5,0.01), function(x) ifelse(abs(pred_diff -1 ) < x ,1,0  )) %>% 
      setNames(., as.character(seq(0.01,0.5,0.01))) %>% as.data.table() %>% summarise_all( list(mean)) %>% 
      melt -> stability_test  
    stability_test %>% rename(threshold = variable) %>% ggplot(.,aes(x = threshold, y= value , group = 1)) + geom_line() + geom_point() +
       theme_light(base_size = 18)+
      theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust=0.9)) + ylab("Proportion of trained predictions matched") -> stability_threshold
    
    
    
    gbm_test_pred <- predict(gbm_model, 
                             newdata = as.matrix(test[test[[weight]]>0] %>% select(gbm_model$feature_name)) ,
                             type = "response")
    
    
    
    
    
    if (is.null(overlays())) {
      train_overlays_adj <- 1
      test_overlays_adj <- 1
    }else{
      train_overlays_adj <- overlays()$adj
      
      splines_dt <- do.call(rbind, drawn_shapes())
      print(splines_dt)
      overlay_fts_dt <- create_splines(df = test[test[[weight]]>0] %>% select(splines_dt$feature), splines_dt = splines_dt)
      
      test_overlays_adj <- predict(overlays()$model,newdata = model.matrix(~ . ,  data =overlay_fts_dt )  , type = "response" )
    }
    
    train_pred <- gbm_train_pred*train_overlays_adj
    test_pred <- gbm_test_pred*test_overlays_adj
   
    


   KT_resample_gini(n = input$n_resample,
                      actual = train[train[[weight]] > 0][[response]] * train[train[[weight]] > 0][[weight]] ,
                      weight =  train[train[[weight]] > 0][[weight]] ,
                  predicted = train_pred,
                    normalize = T
                       ) -> gini_train

   # gini <- gini + scale_fill_manual('variable', values = c('xgb', 'xgb+adj')) + theme_light(base_size = 18)
   
   
   KT_plot_lift(n =input$n_resample ,pred = train_pred ,                       
                actual = train[train[[weight]] > 0][[response]] * train[train[[weight]] > 0][[weight]] ,
                weight =  train[train[[weight]] > 0][[weight]] , 
                nbin = input$lift_plot_bin,
                title = "lift plot train"   ) $plot$lift_plot +theme_light(base_size = 18)  -> lift_train
   
   
   KT_resample_gini(n = input$n_resample,
                    actual = test[test[[weight]] > 0][[response]] * test[test[[weight]] > 0][[weight]] ,
                    weight =  test[test[[weight]] > 0][[weight]] ,
                    predicted = test_pred,
                    normalize = T) -> gini_test
   # gini <- gini + scale_fill_manual('variable', values = c('xgb', 'xgb+adj')) + theme_light(base_size = 18)
   
   
   KT_plot_lift(n =input$n_resample ,pred = test_pred ,                       
                actual = test[test[[weight]] > 0][[response]] * test[test[[weight]] > 0][[weight]] ,
                weight =  test[test[[weight]] > 0][[weight]] , 
                nbin = input$lift_plot_bin,
                title = "lift plot test"   )$plot$lift_plot +theme_light(base_size = 18)   -> lift_test
   
   
   # print(data.table(gini_train=gini_train , gini_test = gini_test) )
   
   data.table(gini_train=gini_train , gini_test = gini_test) %>%
     melt() %>% ggplot(.,aes( x = value, group = variable , fill = variable))+ geom_density(alpha = 0.5) +  theme_light(base_size = 18)->gini
   
   validation_input<- list(test_pred =test_pred,
                           stability_test=stability_test,
                           test_weight = test[test[[weight]] > 0][[weight]] ,
                           test_response = test[test[[weight]] > 0][[response]]
                           )
   
   saveRDS(validation_input , glue("{input$file_name_Training}_validation_input.rds"))

   return(list(gini = gini,
               lift_train =ggplotly(lift_train),
               lift_test =ggplotly(lift_test),
               stability_hist=stability_hist,
               stability_threshold=stability_threshold))
  })
  
  output$gini <- renderPlot({
    Performance()$gini
  })

  
  output$lift_train <- renderPlotly({
    Performance()$lift_train
  })
  output$lift_test <- renderPlotly({
    Performance()$lift_test
  })
  
  output$stability1 <- renderPlot({
    Performance()$stability_hist
                      
  })
  output$stability2 <- renderPlotly({
    ggplotly(Performance()$stability_threshold)
    
  })
  
  # Model comparison

  
  load_model_file <- eventReactive(input$Run_comparison, {
    tryCatch({
      list(
        base = list(
          validation = readRDS(glue("{input$base_file}_validation_input.rds")),
          train_result = readRDS(glue("{input$base_file}.rds"))
        ),
        challenger = list(
          validation = readRDS(glue("{input$challenger_file}_validation_input.rds")),
          train_result = readRDS(glue("{input$challenger_file}.rds"))
        )
      )
    }, error = function(e) {
      showNotification("Error loading files. Please check the file names and try again.", type = "error")
      NULL
    })
    
    
  })
  
  observe({
    req(load_model_file())

    same_fts <- intersect(load_model_file()$base$train_result$model_output$model$feature_names, 
                          load_model_file()$challenger$train_result$model_output$model$feature_names)
    updateSelectInput(session, "SHAP_common_ft", choices = same_fts)
    

  })
  
  observe({
    req(input$SHAP_common_ft)
    
    # Generate SHAP comparison plot
    SHAP_comp_plot <- KT_plot_compare_shap(sv_base = load_model_file()$base$train_result$model_output$shap_values$main_effect$shap_main_effect[[input$SHAP_common_ft]],
                                           sv_challenger = load_model_file()$challenger$train_result$model_output$shap_values$main_effect$shap_main_effect[[input$SHAP_common_ft]],
                                           base_ft = load_model_file()$base$train_result$model_output$shap_values$main_effect$pred_data_main_effect[, input$SHAP_common_ft],
                                           challenger_ft = load_model_file()$challenger$train_result$model_output$shap_values$main_effect$pred_data_main_effect[, input$SHAP_common_ft],
                                           ft_name = input$SHAP_common_ft,
                                           loess_strength = input$SHAP_comp_smooth_strength)
    
    output$shap_model_comparison <- renderPlot({
      if (is.null(SHAP_comp_plot)) {
        showNotification("Plot generation failed. Please check the inputs and try again.", type = "error")
      } else {
        SHAP_comp_plot
      }
    })
    

    
    
  })
  
  
  observe({
    req(load_model_file())
    base_hp <- load_model_file()$base$train_result[c("train_eta" , "train_min_child_weight" , "train_max_depth" , "train_alpha" , 
                                                     "train_lambda", "train_colsample_bytree" ,"train_subsample", "train_nrounds")] %>% 
      as.data.table() %>% melt %>% mutate(scenario = "base")
    challenger_hp <- load_model_file()$challenger$train_result[c("train_eta" , "train_min_child_weight" , "train_max_depth" , "train_alpha" , 
                                                                 "train_lambda", "train_colsample_bytree" ,"train_subsample", "train_nrounds")] %>% 
      as.data.table()%>% melt %>% mutate(scenario = "challenger")
    rbind(base_hp , challenger_hp) %>% pivot_wider(names_from = "scenario" , values_from = "value") %>% 
      mutate(diff = challenger/base) -> compare_hp
    
    
    output$compare_hp <- renderDataTable({
      compare_hp
    })
    
    KT_plot_compare_ft_imp(load_model_file()$base$train_result$model_output$imp_plot$imp_gain$data$Feature,
                           load_model_file()$challenger$train_result$model_output$imp_plot$imp_gain$data$Feature)-> compare_gain_imp
    KT_plot_compare_ft_imp(load_model_file()$base$train_result$model_output$imp_plot$imp_shap$data$variable,
                           load_model_file()$challenger$train_result$model_output$imp_plot$imp_shap$data$variable)-> compare_shap_imp
    
    
    output$SHAP_imp_comparison <- renderPlot({
      compare_shap_imp
    })
    output$gain_imp_comparison <- renderPlot({
      compare_gain_imp
    })
    
    rbind(load_model_file()$base$validation$stability_test %>% mutate(scenario= "base"),
          load_model_file()$challenger$validation$stability_test %>% mutate(scenario= "challenger")) %>%
      ggplot(.,aes(x = variable , y = value , group = scenario , color = scenario)) + geom_line() + geom_point() -> compare_stability
    
    output$stability_comparison  <- renderPlot({
      compare_stability
    })
    

    

  })
  
  observe({
    req(load_model_file())
    KT_plot_dl(n =input$dl_resample_size,
               actual = load_model_file()$base$validation$test_weight*base$validation$test_response,
               weight =load_model_file()$base$validation$test_weight,
               base = load_model_file()$base$validation$test_pred,
               challenger = load_model_file()$challenger$validation$test_pred,
               nbin = input$nbin )$dl_rb_plot ->double_lift
    
    output$dl  <- renderPlot({
      double_lift
    })
  })
  
  observe({
    req(load_model_file())
    KT_plot_compare_gini(n =input$gini_resample_size,
                         actual = load_model_file()$base$validation$test_weight*load_model_file()$base$validation$test_response,
                         weight =load_model_file()$base$validation$test_weight,
                         base = load_model_file()$base$validation$test_pred,
                         challenger = load_model_file()$challenger$validation$test_pred,normalize = T )->compare_gini
    
    output$gini_comparison  <- renderPlot({
      compare_gini
    })
  })
  

}


shinyApp(ui = ui, server = server)

#Note for tomorrow
# Finish compare stuff
# Draw diagram,
# Prepare github some exhibits
# prepare speech in each part
# prepare radar, pmml e.t.c