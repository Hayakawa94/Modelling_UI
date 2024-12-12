source("H:/Restricted Share/DA P&U/Tech Modelling/Users/Khoa/RPMtools/RPMtools.R")



df_eng_sample <- fread("xgb_modelling_data_300k.csv")
df_eng_sample <- df_eng_sample %>% sample_n(100000)
fts<- readRDS("fts.rds")
model_spec <-list(ad_f_b = list(exposure = 'freqexposure_adbclaim',
                                response = 'freqmodels_adbclaim',
                                objective = 'count:poisson',
                                eval_metric='poisson-nloglik'),
                  ad_s_b = list(exposure = 'sevexposure_adbclaim',
                                response = 'sevmodels_adbclaim',
                                objective = 'reg:gamma',
                                eval_metric='gamma-nloglik'),
                  ad_f_c = list(exposure = 'freqexposure_adcclaim',
                                response = 'freqmodels_adcclaim',
                                objective = 'count:poisson',
                                eval_metric='poisson-nloglik'),
                  ad_s_c = list(exposure = 'sevexposure_adcclaim',
                                response = 'sevmodels_adcclaim',
                                objective = 'reg:gamma',
                                eval_metric='gamma-nloglik'),
                  eow_f_b = list(exposure = 'freqexposure_eowbclaim',
                                 response = 'freqmodels_eowbclaim',
                                 objective = 'count:poisson',
                                 eval_metric='poisson-nloglik'),
                  eow_s_b = list(exposure = 'sevexposure_eowbclaim',
                                 response = 'sevmodels_eowbclaim',
                                 objective = 'reg:gamma',
                                 eval_metric='gamma-nloglik'),
                  eow_f_c = list(exposure = 'freqexposure_eowcclaim',
                                 response = 'freqmodels_eowcclaim',
                                 objective = 'count:poisson',
                                 eval_metric='poisson-nloglik'),
                  eow_s_c = list(exposure = 'sevexposure_eowcclaim',
                                 response = 'sevmodels_eowcclaim',
                                 objective = 'reg:gamma',
                                 eval_metric='gamma-nloglik'),
                  flood_f_b = list(exposure = 'freqexposure_floodbclaim',
                                   response = 'freqmodels_floodbclaim',
                                   objective = 'count:poisson',
                                   eval_metric='poisson-nloglik'),
                  flood_s_b = list(exposure = 'sevexposure_floodbclaim',
                                   response = 'sevmodels_floodbclaim',
                                   objective = 'reg:gamma',
                                   eval_metric='gamma-nloglik'),
                  flood_f_c = list(exposure = 'freqexposure_floodcclaim',
                                   response = 'freqmodels_floodcclaim',
                                   objective = 'count:poisson',
                                   eval_metric='poisson-nloglik'),
                  flood_s_c = list(exposure = 'sevexposure_floodcclaim',
                                   response = 'sevmodels_floodcclaim',
                                   objective = 'reg:gamma',
                                   eval_metric='gamma-nloglik'),
                  storm_f_b = list(exposure = 'freqexposure_stormbclaim',
                                   response = 'freqmodels_stormbclaim',
                                   objective = 'count:poisson',
                                   eval_metric='poisson-nloglik'),
                  storm_s_b = list(exposure = 'sevexposure_stormbclaim',
                                   response = 'sevmodels_stormbclaim',
                                   objective = 'reg:gamma',
                                   eval_metric='gamma-nloglik'),
                  storm_f_c = list(exposure = 'freqexposure_stormcclaim',
                                   response = 'freqmodels_stormcclaim',
                                   objective = 'count:poisson',
                                   eval_metric='poisson-nloglik'),
                  storm_s_c = list(exposure = 'sevexposure_stormcclaim',
                                   response = 'sevmodels_stormcclaim',
                                   objective = 'reg:gamma',
                                   eval_metric='gamma-nloglik'),
                  theft_f_b = list(exposure = 'freqexposure_theftbclaim',
                                   response = 'freqmodels_theftbclaim',
                                   objective = 'count:poisson',
                                   eval_metric='poisson-nloglik'),
                  theft_s_b = list(exposure = 'sevexposure_theftbclaim',
                                   response = 'sevmodels_theftbclaim',
                                   objective = 'reg:gamma',
                                   eval_metric='gamma-nloglik'),
                  theft_f_c = list(exposure = 'freqexposure_theftcclaim',
                                   response = 'freqmodels_theftcclaim',
                                   objective = 'count:poisson',
                                   eval_metric='poisson-nloglik'),
                  theft_s_c = list(exposure = 'sevexposure_theftcclaim',
                                   response = 'sevmodels_theftcclaim',
                                   objective = 'reg:gamma',
                                   eval_metric='gamma-nloglik'),
                  fire_f_b = list(exposure = 'freqexposure_firebclaim',
                                  response = 'freqmodels_firebclaim',
                                  objective = 'count:poisson',
                                  eval_metric='poisson-nloglik'),
                  fire_s_b = list(exposure = 'sevexposure_firebclaim',
                                  response = 'sevmodels_firebclaim',
                                  objective = 'reg:gamma',
                                  eval_metric='gamma-nloglik'),
                  fire_f_c = list(exposure = 'freqexposure_firecclaim',
                                  response = 'freqmodels_firecclaim',
                                  objective = 'count:poisson',
                                  eval_metric='poisson-nloglik'),
                  fire_s_c = list(exposure = 'sevexposure_firecclaim',
                                  response = 'sevmodels_firecclaim',
                                  objective = 'reg:gamma',
                                  eval_metric='gamma-nloglik'),
                  other_f_b = list(exposure = 'freqexposure_otherbclaim',
                                   response = 'freqmodels_otherbclaim',
                                   objective = 'count:poisson',
                                   eval_metric='poisson-nloglik'),
                  other_s_b = list(exposure = 'sevexposure_otherbclaim',
                                   response = 'sevmodels_otherbclaim',
                                   objective = 'reg:gamma',
                                   eval_metric='gamma-nloglik'),
                  other_f_c = list(exposure = 'freqexposure_othercclaim',
                                   response = 'freqmodels_othercclaim',
                                   objective = 'count:poisson',
                                   eval_metric='poisson-nloglik'),
                  other_s_c = list(exposure = 'sevexposure_othercclaim',
                                   response = 'sevmodels_othercclaim',
                                   objective = 'reg:gamma',
                                   eval_metric='gamma-nloglik'),
                  subs_f_b = list(exposure = 'freqexposure_subsbclaim',
                                  response = 'freqmodels_subsbclaim',
                                  objective = 'count:poisson',
                                  eval_metric='poisson-nloglik'),
                  subs_s_b = list(exposure = 'sevexposure_subsbclaim',
                                  response = 'sevmodels_subsbclaim',
                                  objective = 'reg:gamma',
                                  eval_metric='gamma-nloglik'),
                  unsp_f_pp = list(exposure = 'freqexposure_unspecifiedppcclaim',
                                   response = 'freqmodels_unspecifiedppcclaim',
                                   objective = 'count:poisson',
                                   eval_metric='poisson-nloglik'),
                  unsp_s_pp = list(exposure = 'sevexposure_unspecifiedppcclaim',
                                   response = 'sevmodels_unspecifiedppcclaim',
                                   objective = 'reg:gamma',
                                   eval_metric='gamma-nloglik'))


# Shuffle the dataset
df_eng_sample <- df_eng_sample[sample(nrow(df_eng_sample)), ]

# Define the split ratios
train_ratio <- 0.6
validate_ratio <- 0.2
holdout_ratio <- 0.2

# Calculate the indices for each set
train_index <- 1:floor(train_ratio * nrow(df_eng_sample))
validate_index <- (max(train_index) + 1):(max(train_index) + floor(validate_ratio * nrow(df_eng_sample)))
holdout_index <- (max(validate_index) + 1):nrow(df_eng_sample)

# Split the data
train <- df_eng_sample[train_index, ]
validate <- df_eng_sample[validate_index, ]
holdout <- df_eng_sample[holdout_index, ]






# xgb doesn't like cat variables so need to OHE
tune_model <- function(fts,
                        model,
                       train ,
                       validate ,
                       eta = c(0.01, 0.1),
                       max_depth = c(2L, 5L),
                       min_child_weight = c(1, 100),
                       subsample = c(0.7, 1),
                       colsample_bytree = c(0.7, 1),
                       lambda = c(3,3) ,
                       alpha = c(3,3),
                       monotonicity_constraints,
                       interaction_constraints,
                       nrounds= 100,
                       parallel = T,
                       iters.k = 1,
                       iters.n = 4,
                       ncluster  =  max(floor(parallel::detectCores()*2/3),1),
                       initPoints=10 ){
  
  weight = model_spec[[model]]$exposure
  response = model_spec[[model]]$response
  objective =  model_spec[[model]]$objective
  eval_metric = model_spec[[model]]$eval_metric
  train <- train[train[[weight]] >0 ]
  train_y <- train[[response]]
  train_weight <- train[[weight]]
  validate <- validate[validate[[weight]] >0 ]
  validate_y <- validate[[response]]
  validate_weight <- validate[[weight]]
  # bounds <- append(bounds)
  
  if(missing(monotonicity_constraints)){
    monotonicity_constraints= rep(0,nrow(train)) 
  }
  if(missing(interaction_constraints)){
    interaction_constraints= list() 
  }
  min_child_weight * sum(train_weight)
  
  bounds =  list(eta=eta ,
                 max_depth=max_depth ,
                 min_child_weight=min_child_weight,
                 subsample=subsample ,
                 colsample_bytree=colsample_bytree,
                 lambda=lambda ,
                 alpha =alpha)
  
  lapply(bounds, function(x) if(x[1] == x[2]){x[1]} else{NULL}  ) %>% setNames(names(bounds))  %>% compact() -> fixed_param
  lapply(bounds, function(x) if(x[1] == x[2]){NULL} else{x}  ) %>% setNames(names(bounds))  %>% compact() -> bounds
  
  
  KT_xgb_baysian_tune(train = train %>% select(fts),
                      train_y = train_y,
                      train_weight = train_weight,
                      validate = validate %>%  select(fts),
                      validate_y = validate_y,
                      validate_weight = validate_weight,
                      bounds = bounds,HP_fixed = fixed_param,
                      monotonicity_constraints=monotonicity_constraints , 
                      interaction_constraints=interaction_constraints,
                      nrounds = nrounds,
                      objective =objective,
                      eval_metric = eval_metric,
                      parallel =parallel,
                      iters.k = iters.k,
                      iters.n = 4,
                      ncluster = ncluster , 
                      initPoints =  initPoints )
  
}

target_cols <- df_eng_sample %>% select(starts_with(c("freq" , "sev" , "exposure"))) %>%  names()
df_eng_sample %>% select(-target_cols) %>% select_if(~is.numeric(.) ) %>% names -> fts
 
tune_model(fts = fts,  model = "eow_f_b",train = train ,validate = validate) -> test
library(shiny)
library(rhandsontable)
library(plotly)
library(DT)
ui <- fluidPage(
  titlePanel("Tick Box Table with Duplicate Tabs"),
  tabsetPanel(
    tabPanel("Feature Spec",
             sidebarLayout(
               sidebarPanel(actionButton("reset_table", "Reset Table"),
                            actionButton("load_feature", "Load Feature Spec"),
                            actionButton("save_feature", "Save Feature Spec"),
                            textInput("file_name_feature", "File Name", value = "feature_spec")),
               mainPanel(
                 textOutput("action_message_feature"),
                 rHandsontableOutput("ft_table")
               )
             )
    ),
    tabPanel("Tuning",
             sidebarLayout(
               sidebarPanel(
                 actionButton("load_tuning", "Load Tuning"),
                 actionButton("save_tuning", "Save Tuning"),
                 textInput("file_name_tuning", "File Name", value = "tuning"),
                 br(),  # Line break for spacing
                 selectInput("model", "Select Model", choices = names(model_spec)),
                 actionButton("Select_HP_bounds", "HP Bounds"),
                 conditionalPanel(
                   condition = "input.Select_HP_bounds % 2 == 1",
                   sliderInput("eta", "Learning Rate (eta):", min = 0.001, max = 0.3, value = c(0.001,0.1), step = 0.001),
                   sliderInput("min_child_weight", "Minimum Child Weight:", min = 0.0001, max = 0.1, value = c(0.0001,0.01), step = 0.0001),
                   sliderInput("max_depth", "Max Depth:", min = 1, max = 15, value = c(1,5), step = 1),
                   sliderInput("alpha", "alpha (L1 regularization):", min = 0, max = 1, value = c(0.001,0.2), step = 0.001),
                   sliderInput("lambda", "lambda (L2 regularization):", min = 0, max = 10, value = c(0.001,2), step = 0.01),
                   sliderInput("colsample_bytree", "colsample bytree:", min = 0.1, max = 1, value = c(0.2,0.4), step = 0.01),
                   sliderInput("subsample", "Subsample:", min = 0.1, max = 1, value = c(0.2,0.4), step = 0.01),
                   sliderInput("nrounds", "Number of Rounds:", min = 10, max = 500, value = 100)
                 ),
                 actionButton("tune", "Tune_model")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("tune_iteration", plotlyOutput("tune_iteration_plot")),
                   tabPanel("eta", plotlyOutput("eta_plot")),
                   tabPanel("max_depth", plotlyOutput("max_depth_plot")),
                   tabPanel("min_child_weight", plotlyOutput("min_child_weight_plot")),
                   tabPanel("colsample_bytree", plotlyOutput("colsample_bytree_plot")),
                   tabPanel("lambda", plotlyOutput("lambda_plot")),
                   tabPanel("alpha", plotlyOutput("alpha_plot")),
                   tabPanel("Tune_result", DT::dataTableOutput("opt_result_plot"))
                 ),
                 textOutput("action_message_tuning")
               )
             )
    )
  )
)
server <- function(input, output, session) {
  initial_data <- data.frame(
    Features = fts,
    Use_Feature = rep(FALSE, length(fts)),
    Monotonicity = rep(0, length(fts)),
    Interaction_Constraints = rep(FALSE, length(fts)),
    stringsAsFactors = FALSE
  )
  
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
  
  tune_result <- eventReactive(input$tune, {
    req(input$ft_table)
    ft_spec_table <- hot_to_r(input$ft_table)
    fts_to_tune <- ft_spec_table$Features[which(ft_spec_table$Use_Feature == TRUE)]
    tune_model(fts = fts_to_tune,
               model = input$model,
               train = train,
               validate = validate,
               eta = input$eta,
               max_depth = input$max_depth,
               min_child_weight = input$min_child_weight,
               subsample = input$subsample,
               colsample_bytree = input$colsample_bytree,
               lambda = input$lambda,
               alpha = input$alpha,
               nrounds = input$nrounds)
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
    saveRDS(list(inputs = reactiveValuesToList(input), plots = list(
      tune_iteration = tune_result()$hyperparameters_trends$tune_iteration,
      eta = tune_result()$hyperparameters_trends$eta,
      max_depth = tune_result()$hyperparameters_trends$max_depth,
      min_child_weight = tune_result()$hyperparameters_trends$min_child_weight,
      subsample = tune_result()$hyperparameters_trends$subsample,
      colsample_bytree = tune_result()$hyperparameters_trends$colsample_bytree,
      lambda = tune_result()$hyperparameters_trends$lambda,
      alpha = tune_result()$hyperparameters_trends$alpha,
      opt_result = tune_result()$opt_results
    )), file_name)
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