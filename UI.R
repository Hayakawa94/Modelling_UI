library(shiny)
library(rhandsontable)
library(plotly)
library(DT)
library(DiagrammeR)
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .full-width-btn {
        width: 100%;
      }
      .global-inputs {
        display: flex;
        align-items: center;
        background-color: #f0f0f0;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 20px;
      }
      .global-inputs > div {
        margin-right: 20px;
      }
      .red-btn {
        background-color: green;
        color: white;
      }
      .tune-btn-container {
        display: flex;
        justify-content: flex-end;
        margin-bottom: 20px;
      }
      .sidebar {
        width: 300px !important;
        flex-shrink: 0 !important;
      }

    "))
  ),
  
  titlePanel("GBM Modelling"),
  
  # Global inputs with background effect and horizontal alignment
  div(class = "global-inputs",
      div(selectInput("model", "Select Model", choices = names(model_spec))),
      div(checkboxInput("Distribute_Computation", "Distribute Computation", value = TRUE))
  ),
  
  tabsetPanel(
    tabPanel("Feature Spec",
             sidebarLayout(
               sidebarPanel(
                 textInput("file_name_feature", "File Name", value = "feature_spec"),
                 actionButton("load_feature", "Load Feature Spec", class = "full-width-btn"),
                 actionButton("save_feature", "Save Feature Spec", class = "full-width-btn"),
                 tags$hr(),
                 actionButton("reset_table", "Reset Table", class = "full-width-btn"),
                 actionButton("select_all", "Select all features", class = "full-width-btn"),
                 tags$hr(),
                 actionButton("EDA", "Update Data", class = "full-width-btn"),
                 tags$hr()
               ),
               mainPanel(
                 textOutput("action_message_feature"),
                 tabsetPanel(
                   tabPanel("Feature_Selection", rHandsontableOutput("ft_table")),
                   tabPanel("EDA", DT::dataTableOutput("EDA_data")),
                   tabPanel("Claim", DT::dataTableOutput("Claim"))
                 )
               )
             )
    ),
    tabPanel("Tuning",
             sidebarLayout(
               sidebarPanel(
                 actionButton("load_tuning", "Load Tuning"),
                 actionButton("save_tuning", "Save Tuning"),
                 textInput("file_name_tuning", "File Name", value = "tuning"),
                 tags$hr(),
                 actionButton("Sampling", "Sampling", class = "full-width-btn"),
                 conditionalPanel(
                   condition = "input.Sampling % 2 == 1",
                   checkboxInput("Trainvalidate", "Train Validate Split", value = TRUE),
                   sliderInput("Ratio", "Ratio:", min = 0.5, max = 1, value = 0.8, step = 0.001),
                   checkboxInput("kfold", "kfold Cross Validate", value = FALSE),
                   sliderInput("kfold_val", "Select number of folds:", min = 2, max = 10, value = 5, step = 1)
                 ),
                 tags$hr(),
                 actionButton("Select_HP_bounds", "HP Bounds", class = "full-width-btn"),
                 conditionalPanel(
                   condition = "input.Select_HP_bounds % 2 == 1",
                   sliderInput("eta", "Learning Rate (eta):", min = 0.001, max = 0.3, value = c(0.001, 0.1), step = 0.001),
                   sliderInput("min_child_weight", "Minimum Child Weight:", min = 0.0001, max = 0.1, value = c(0.0001, 0.01), step = 0.0001),
                   sliderInput("max_depth", "Max Depth:", min = 1, max = 15, value = c(1, 5), step = 1),
                   sliderInput("alpha", "alpha (L1 regularization):", min = 0, max = 1, value = c(0.001, 0.2), step = 0.001),
                   sliderInput("lambda", "lambda (L2 regularization):", min = 0, max = 10, value = c(0.001, 2), step = 0.01),
                   sliderInput("colsample_bytree", "colsample bytree:", min = 0.1, max = 1, value = c(0.2, 0.4), step = 0.01),
                   sliderInput("subsample", "Subsample:", min = 0.1, max = 1, value = c(0.2, 0.4), step = 0.01),
                   sliderInput("gamma", "Min Split Loss:", min = 0, max = 100, value = c(0, 0), step = 0.001),
                   sliderInput("nrounds", "Number of Rounds:", min = 10, max = 2000, value = 100, step = 1)
                 )
               ),
               mainPanel(
                 div(class = "tune-btn-container",
                     actionButton("tune", "Tune Model", class = "full-width-btn red-btn")
                 ),
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
    ),
    tabPanel("Training",
             sidebarLayout(
               sidebarPanel(
                 actionButton("load_Training", "Load Training"),
                 actionButton("save_Training", "Save Training"),
                 textInput("file_name_Training", "File Name", value = "Training"),
                 tags$hr(),
                 actionButton("load_tuning_result", "Load Tuned HPs"),
                 textInput("file_name_tuned", "File Name", value = "tuning"),
                 tags$hr(),
                 actionButton("train_Sampling", "Sampling", class = "full-width-btn"),
                 conditionalPanel(
                   condition = "input.train_Sampling % 2 == 1",
                   checkboxInput("train_Trainvalidate", "Train Validate Split", value = TRUE),
                   sliderInput("train_Ratio", "Ratio:", min = 0.5, max = 1, value = 0.8, step = 0.001),
                   checkboxInput("train_kfold", "kfold Cross Validate", value = FALSE),
                   sliderInput("train_kfold_val", "Select number of folds:", min = 2, max = 10, value = 5, step = 1)
                 ),
                 tags$hr(),
                 actionButton("train_Select_HP", "HPs", class = "full-width-btn"),
                 checkboxInput("use_early_stopping_rounds", "use early stopping rounds", value = F),
                 conditionalPanel(
                   condition = "input.train_Select_HP % 2 == 1",
                   
                   sliderInput("train_eta", "Learning Rate (eta):", min = 0.001, max = 0.3, value = 0.1, step = 0.001),
                   sliderInput("train_min_child_weight", "Minimum Child Weight:", min = 0.0001, max = 0.1, value =  0.01, step = 0.0001),
                   sliderInput("train_max_depth", "Max Depth:", min = 1, max = 15, value =  5, step = 1),
                   sliderInput("train_alpha", "alpha (L1 regularization):", min = 0, max = 1, value =  0.2, step = 0.001),
                   sliderInput("train_lambda", "lambda (L2 regularization):", min = 0, max = 10, value =  2, step = 0.01),
                   sliderInput("train_colsample_bytree", "colsample bytree:", min = 0.1, max = 1, value =  0.75, step = 0.01),
                   sliderInput("train_subsample", "Subsample:", min = 0.3, max = 1, value =  1, step = 0.01),
                   sliderInput("train_gamma", "Min Split Loss:", min = 0, max = 100, value = 0, step = 0.001),
                   sliderInput("train_nrounds", "Number of Rounds:", min = 10, max = 2000, value = 100, step = 1)
                 )
               ),
               mainPanel(
                 div(class = "tune-btn-container",
                     actionButton("train", "Train Model", class = "full-width-btn red-btn")
                 ),
                 tabsetPanel(
                   tabPanel("Feature Importance (SHAPley value contribution)", plotlyOutput("SHAP_imp_plot")),
                   tabPanel("SHAP_Interaction_matrix", 
                            sliderInput("top_shap_X", "Top X", min = 1, max = 40, value = 8, step = 1),
                            plotlyOutput("SHAP_Interaction_matrix", width = "1000", height = "1000")),
                   tabPanel("Feature Importance (Gain)", plotlyOutput("Gain_imp_plot")),
                   
                   tabPanel("gain",
                            sliderInput("topfeatures", "Top Features", min = 1, max = 30, value = 10, step = 1),
                            plotOutput("gain", width = "1000", height = "1000")
                   ),
                   tabPanel("gain2",
                            sliderInput("topfeatures", "Top Features", min = 1, max = 30, value = 10, step = 1),
                            selectInput("y_axis", "y axis", choices = c("sumGain" , "meanGain" , "sumCover" , "meanCover","frequency") ,selected =  "sumCover"),
                            selectInput("x_axis", "x axis", choices = c("sumGain" , "meanGain" , "sumCover" , "meanCover","frequency") , selected = "meanGain" ),
                            plotOutput("gain2", width = "1000", height = "1000")
                   ),
                   
                   tabPanel("Interaction_matrix", 
                            sliderInput("top_gain_X", "Top X", min = 1, max = 40, value = 8, step = 1),
                            plotlyOutput("Interaction_matrix", width = "1000", height = "1000")),

                   
                   tabPanel("Interaction_gain",
                            sliderInput("topfeatures", "Top Features", min = 1, max = 30, value = 10, step = 1),
                            plotOutput("Interaction_gain", width = "1000", height = "1000")
                   ),
                   tabPanel("Interaction_gain2",
                            sliderInput("topfeatures", "Top Features", min = 1, max = 30, value = 10, step = 1),
                            selectInput("y_axis", "y axis", choices = c("sumGain" , "meanGain" , "sumCover" , "meanCover","frequency") ,selected =  "sumCover"),
                            selectInput("x_axis", "x axis", choices = c("sumGain" , "meanGain" , "sumCover" , "meanCover","frequency") , selected = "meanGain" ),
                            plotOutput("Interaction_gain2", width = "1000", height = "1000")
                   ),
                  
                   tabPanel("Tree Plot", sliderInput("tree_index", "Tree Index", min = 0, max = 0, value = 0, step = 1) , 
                            grVizOutput("tree_plot", width = "100%", height = "900"))
                 ),
                 textOutput("action_message_training")
               )
             )
    ),
    tabPanel("Explain",
             sidebarLayout(
               sidebarPanel(
                 actionButton("load_trained_model", "Load model"),
                 textInput("file_name_SHAP", "File Name", value = "Training"),
                 tags$hr(),
                 sliderInput("SHAP_sample_size", "Sample Size", min = 0.001, max = 1, value = 1, step = .0001),
                 sliderInput("SHAP_smooth_strength", "Smooth Strength", min = 0, max = 1, value = 0.9, step = .001),
                 sliderInput("SHAP_pt_size", "Point Size", min = 0, max = 4, value = 1, step = .0001),
                 sliderInput("SHAP_alpha", "Alpha", min = 0, max = 1, value = 1, step = .0001),
                 
                 
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("SHAP", 
                            # div(class = "tune-btn-container",
                            #     actionButton("Update_SHAP_chart", "Update Chart", class = "full-width-btn red-btn")
                            # ),
                            # checkboxInput("Fit_loess", "Fit Loess", value = T),
                            selectInput("SHAP_ft", "Choose Feature", choices = NULL),
                            plotOutput("SHAP_plot")),
                   
                   tabPanel("SHAP X", 
                            # div(class = "tune-btn-container",
                            #     actionButton("Update_SHAP_X_chart", "Update Chart", class = "full-width-btn red-btn")
                            # ),
                            checkboxInput("SHAP_X_Fit_loess", "Fit Loess", value = F),
                            selectInput("SHAP_X_ft1", "Choose Feature", choices = NULL),
                            selectInput("SHAP_X_ft2", "Choose Interaction", choices = NULL),
                            plotOutput("SHAP_X_plot"))
                 ),
                 textOutput("Shap_value_loaded")
               )
             )
    ),
    tabPanel("Overlays",
             sidebarLayout(
               sidebarPanel(
                 actionButton("Load_base_model", "Load base model"),
                 textInput("Base_pred_path", "Base Model", value = "Training"),
                 tags$hr(),
                 actionButton("save_glm", "Save Splines"),
                 actionButton("load_glm", "Load Splines"),
                 textInput("glm_overlay_out", "glm_overlay_out_path", value = "glm_overlays"),
                 tags$hr(),
                 checkboxInput("draw_mode", "Enable Drawing Mode", value = FALSE),
                 actionButton("reset", "Reset Drawing"),
                 tags$hr(),
                 selectInput("ft", "Select Feature", choices = sort(fts)),
                 selectInput("factor_consistency", "Select Interaction:", choices = fts),
                 checkboxGroupInput("undo_shapes", "Select Spline Undo", choices = NULL),
                 actionButton("undo", "Undo")

               ),
               mainPanel(
                 fluidRow(
                   column(8,  # Adjust the width as needed
                          div(class = "tune-btn-container",
                              actionButton("Fit", "Fit", class = "full-width-btn red-btn")
                          ),
                          tabsetPanel(
                            tabPanel("GLM fit",
                                     checkboxGroupInput("fit_lines", "",
                                                        choices = c("CA_base", "CA_challenger", "obs", "CU_unadj_base", "CU_unadj_challenger"),
                                                        selected = c("CA_challenger", "obs", "CU_unadj_challenger", "CU_unadj_base"),
                                                        inline = TRUE
                                     ),
                                     plotlyOutput("overlay_plot"),
                                     plotlyOutput("avePlot"),  # Placing avePlot below overlay_plot
                                     tableOutput("glm_fit")
                            ),
                            tabPanel("Model Summary",
                                     verbatimTextOutput("glm_summary")
                            )
                          )
                   ),
                   column(4,  # Adjust the width as needed
                          sliderInput("samplesize", "Sample Size:", value = 1, min = 0.01, max = 1),
                          checkboxInput("rebase", "Rebase:", value = TRUE),
                          selectInput("filter_feature", "Select Feature to Filter:", choices = NULL),
                          uiOutput("filter_ui"),
                          selectInput("secondary_filter_feature", "Select Secondary Feature to Filter:", choices = NULL),
                          uiOutput("secondary_filter_ui"),
                          selectInput("tertiary_filter_feature", "Select tertiary Feature to Filter:", choices = NULL),
                          uiOutput("tertiary_filter_ui"),
                          
                          checkboxInput("fitloess", "Fit LOESS", value = TRUE),
                          sliderInput("smooth_strength", "Smooth Strength:", value = 0.75, min = 0, max = 1),
                          actionButton("Chart_Cosmetic","AvE analysis",  class = "full-width-btn"),
                          conditionalPanel(condition = "input.Chart_Cosmetic % 2 == 1",
                                           
                                           sliderInput("alpha_line", "AVE line alpha:", value = 0.3, min = 0, max = 1, step = 0.001),
                                           sliderInput("alpha_pt", "AVE point alpha:", value = 1, min = 0, max = 1, step = 0.001),
                                           sliderInput("size_line", "AVE line size:", value = 0.6, min = 0.001, max = 4, step = 0.01),
                                           sliderInput("size_pt", "AVE point size:", value = 1.5, min = 0.001, max = 3.5, step = 0.001),
                                           checkboxInput("y_lim", "Control y lim", value = TRUE),
                                           sliderInput("y_interval", "y axis lim:", value = c(0, 2), min = 0, max = 5, step = 0.0001),
                                           downloadButton("downloadData", "Download Data")
                          )
                   )
                 ),
                 tableOutput("aveTable")
               )
             )
    )
  )
)

options(shiny.reactlog = TRUE)
library(reactlog)