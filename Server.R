
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
# set.seed(1)
# train %>% sample_frac(0.2) -> train
# train$none = "NA"
# fts <- train %>% select(starts_with("num") )%>% names %>% sort
source("H:/Restricted Share/DA P&U/Tech Modelling/01 Home/Phase 2/15 R&D/Modelling_ui/UI.R")

source("H:/Restricted Share/DA P&U/Tech Modelling/01 Home/Phase 2/15 R&D/Modelling_ui/Reactive_calc.R")

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
      train_use_early_stopping_rounds = input$use_early_stopping_rounds,
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
      updateSliderInput(session, "train_gamma", value = loaded_state$train_gamma)
      updateSliderInput(session, "use_early_stopping_rounds", value = loaded_state$train_use_early_stopping_rounds)
      updateSliderInput(session, "tree_index", max = loaded_state$model_output$model$niter -1)
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
  
  ######### GLM  Overlays ########
  drawn_shapes <- reactiveVal(list())
  x_range <- reactiveVal(NULL)
  y_range <- reactiveVal(NULL)
  y2_range <- reactiveVal(NULL)
  
  base_pred <- eventReactive(input$Load_base_model, {
    readRDS(glue("{input$Base_pred_path}.rds"))$model_output$pred
  })
  
  overlays <- eventReactive({
    input$Fit
    input$Load_base_model
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
      do.call(rbind, all_shapes)
    } else {
      do.call(rbind, all_shapes) %>% left_join(overlays()$fit, by = "id")
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
    saveRDS(list(drawn_shapes = drawn_shapes(), undo_shapes = input$undo_shapes), file = paste0(input$glm_overlay_out, ".rds"))
  })
  
  observeEvent(input$load_glm, {
    loaded_data <- readRDS(paste0(input$glm_overlay_out, ".rds"))
    drawn_shapes(loaded_data$drawn_shapes)
    updateCheckboxGroupInput(session, "undo_shapes", choices = do.call(rbind, loaded_data$drawn_shapes)$id, selected = loaded_data$undo_shapes)
  })
  
  # AvE
  
  observe({
    feature_choices <- c("none", "rnd_factor", fts)
    # updateSelectInput(session, "model", choices = sort(names(model_spec)))
    updateSelectInput(session, "ft", choices =  update_fts_list())
    updateSelectInput(session, "factor_consistency", choices = feature_choices)
    updateSelectInput(session, "filter_feature", choices = fts)
    updateSelectInput(session, "secondary_filter_feature", choices = fts)
    updateSelectInput(session, "tertiary_filter_feature", choices = fts)
  })
  
  update_fts_list <- eventReactive(input$Load_base_model, {
    gbm_fts <- readRDS(glue("{input$Base_pred_path}.rds"))$model_output$model$feature_name
  
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
  
  
}

shinyApp(ui = ui, server = server)

