source("H:/Restricted Share/DA P&U/Tech Modelling/Users/Khoa/RPMtools/RPMtools.R")

library("flexdashboard")

summarise_dataframe <- function(df) {
  summary <- data.frame(
    Feature = character(),
    Min = numeric(),
    Max = numeric(),
    Mode = numeric(),
    Proportion_Missing = numeric(),
    Data_Type = character(),
    stringsAsFactors = FALSE
  )
  
  for (col in colnames(df)) {
    min_val <- min(df[[col]], na.rm = TRUE)
    max_val <- max(df[[col]], na.rm = TRUE)
    mode_val <- as.numeric(names(sort(table(df[[col]]), decreasing = TRUE)[1]))
    proportion_missing <- mean(is.na(df[[col]]))
    data_type <- class(df[[col]])
    
    summary <- rbind(summary, data.frame(
      Feature = col,
      Min = min_val,
      Max = max_val,
      Mode = mode_val,
      Proportion_Missing = proportion_missing,
      Data_Type = data_type
    ))
  }
  
  return(summary)
}

# summarize_dataframe(test$train %>% select(fts)) -> test2
# test<- KT_create_sample(df = df_eng_sample,df_eng_sample$exposure,y = df_eng_sample$freqexposure_eowbclaim,kfold = 4 )

KT_create_sample <- function(df , weight, y, kfold , train_validate_split= 0.8){
  set.seed(1)
  if (!missing(kfold)){
    KT_create_fold_idx(df,k = kfold) -> kfold_idx
    train <- df
    train_y <- y
    train_weight <- weight
    return(list(train = train , 
                train_y= train_y , 
                train_weight=train_weight,
                kfold_idx = kfold_idx))
  }else{
    kfold_idx =NULL
    idx <- sample(seq_len(nrow(df)), size = 0.8 * nrow(df))
    train <- df[idx, ]
    train_y <- y[idx]
    train_weight <- weight[idx]
    validate <- df[-idx, ]
    validate_y <- y[-idx]
    validate_weight <- weight[-idx]
    return(list(train = train , 
                train_y= train_y , 
                train_weight=train_weight,
                validate_y=validate_y ,
                validate_weight=validate_weight, 
                validate = validate))
  }
  
}

# xgb doesn't like cat variables so need to OHE
tune_model <- function(fts,
                       model,
                       train ,
                       kfold=0,
                       train_validate_ratio=0.8,
                       eta = c(0.01, 0.1),
                       max_depth = c(2L, 5L),
                       min_child_weight = c(1, 100),
                       subsample = c(0.7, 1),
                       colsample_bytree = c(0.7, 1),
                       lambda = c(3,3) ,
                       alpha = c(3,3),
                       monotone_constraints,
                       interaction_constraints,
                       gamma = c(1,1),
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
  
  if (kfold>0){
    KT_create_sample(df = train, weight = train_weight, y = train_y, kfold = kfold   ) -> sample_result
  }else{
    KT_create_sample(df = train, weight = train_weight, y = train_y, train_validate_split =  train_validate_ratio ) -> sample_result
  }
  
  min_child_weight= min_child_weight* length(sample_result$train_weight)
  
  bounds =  list(eta=eta ,
                 max_depth=max_depth ,
                 min_child_weight=min_child_weight,
                 subsample=subsample ,
                 colsample_bytree=colsample_bytree,
                 lambda=lambda ,
                 alpha =alpha,
                 gamma=gamma)
  
  lapply(bounds, function(x) if(x[1] == x[2]){x[1]} else{NULL}  ) %>% setNames(names(bounds))  %>% compact() -> fixed_param
  lapply(bounds, function(x) if(x[1] == x[2]){NULL} else{x}  ) %>% setNames(names(bounds))  %>% compact() -> bounds
  
  if (kfold > 0){
    
    
    
    KT_xgb_baysian_tune(train =  sample_result$train %>% select(fts),
                        train_y =  sample_result$train_y,
                        train_weight =  sample_result$train_weight,
                        folds = sample_result$kfold,
                        
                        bounds = bounds,
                        HP_fixed = fixed_param,
                        monotone_constraints=monotone_constraints , 
                        interaction_constraints=interaction_constraints,
                        nrounds = nrounds,
                        objective =objective,
                        eval_metric = eval_metric,
                        parallel =parallel,
                        iters.k = iters.k,
                        iters.n = 4,
                        ncluster = ncluster , 
                        initPoints =  initPoints )
  }else{
    
    KT_xgb_baysian_tune(train =  sample_result$train %>% select(fts),
                        train_y =  sample_result$train_y,
                        train_weight =  sample_result$train_weight,
                        validate =  sample_result$validate %>%  select(fts),
                        validate_y =  sample_result$validate_y,
                        validate_weight =  sample_result$validate_weight,
                        bounds = bounds,
                        HP_fixed = fixed_param,
                        monotone_constraints=monotone_constraints , 
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
  
  
}



train_model <- function(fts,
                        model,
                        train ,
                        kfold=0,
                        train_validate_ratio=0.8,
                        parallel = T,
                        use_tunred_HP= NULL,
                        min_child_weight,
                        early_stopping_rounds,
                        ...
){
  
  weight = model_spec[[model]]$exposure
  response = model_spec[[model]]$response
  objective =  model_spec[[model]]$objective
  eval_metric = model_spec[[model]]$eval_metric
  train <- train[train[[weight]] >0 ]
  train_y <- train[[response]]
  train_weight <- train[[weight]]
  
  if (kfold>0){
    KT_create_sample(df = train, weight = train_weight, y = train_y, kfold = kfold   ) -> sample_result
  }else{
    KT_create_sample(df = train, weight = train_weight, y = train_y, train_validate_split =  train_validate_ratio ) -> sample_result
  }
  
  
  min_child_weight= min_child_weight* length(sample_result$train_weight)
  
  if(!is.null(use_tunred_HP)){
    params = use_tunred_HP
  }else{
    params =  list(objective =objective ,eval_metric=eval_metric,min_child_weight=min_child_weight, ...)
  }
  if(parallel){
    nthread =  max(floor(parallel::detectCores()*2/3),1)
  }else{
    nthread = -1
  }
  
 
  
  if (kfold > 0){
    
    KT_xgb_cv(train =  sample_result$train %>% select(fts),
              train_y =  sample_result$train_y,
              train_weight =  sample_result$train_weight,
              folds = sample_result$kfold,
              params = params,
              nthread = nthread) -> train_result
    
  }else{
    
    KT_xgb_train(train =  sample_result$train %>% select(fts),
                 train_y =  sample_result$train_y,
                 train_weight =  sample_result$train_weight,
                 validate =  sample_result$validate %>%  select(fts),
                 validate_y =  sample_result$validate_y,
                 validate_weight =  sample_result$validate_weight,
                 params = params,
                 nthread = nthread,
                 early_stopping_rounds = early_stopping_rounds) -> train_result
  }
  
  explain_result <- KT_xgb_explain(model = train_result$model, pred_data = sample_result$train %>% select(fts) )
  
  pred <- predict(train_result$model ,newdata =as.matrix(train %>% select(fts))   , type  = "response")
  
  
  return(list(imp_plot = list(imp_gain   = train_result$imp_plot,
                              imp_shap = explain_result$ft_importance_plot,
                              imp_shap_X = explain_result$ft_importance_X,
                              EIXinteraction_gain_matrix  =explain_result$EIXimportance_matrix,
                              EIX_gain=explain_result$EIXimportance,
                              EIXinteraction_gain=explain_result$EIXimportanceX
                              ),
              shap_values = list(main_effect =explain_result$main_effect,
                                 interaction_effect = explain_result$interaction ),
              model =train_result$model,
              pred = pred,
              pmml_fmap =    r2pmml::as.fmap(as.matrix(sample_result$train %>% select(train_result$model$feature_names)))))
}



glm_fit <- function(glm_train,splines_dt, response , base , weight ,fam ){
  overlay_fts_dt <- data.table(idx  = 1:length(weight))
  for(i in  1:nrow(splines_dt)){
    feature <- splines_dt[["feature"]][i]
    spline <- splines_dt[["id"]][i]
    min_val  <- splines_dt[["x0"]][i]
    max_val <- splines_dt[["x1"]][i]
    overlay_fts_dt[[spline]] <- normalize_feature(feature = glm_train[[feature]],min_val =min_val,max_val =  max_val  )
    
  }
  x <- model.matrix(~ . ,  data =overlay_fts_dt %>% select(-idx) )
  
  
  
  base_ave <- response/ base 
  suppressWarnings({
  adj_fit <- fastglm(x =x , y =base_ave , weights = base*weight ,family = fam )})
  adj<- predict(adj_fit , newdata = x, type = "response")
  coefficients <- coef(adj_fit)
  
  # Create data table
  result <- data.table(
    id = names(coefficients),
    estimate = exp(  coefficients)
  )
  
  return(list(adj=adj, model = adj_fit, fit = result, model =adj_fit ))
  
  
}  


normalize_feature <- function(feature, min_val, max_val) {
  ifelse(feature < min_val, 0, ifelse(feature > max_val, 1, (feature - min_val) / (max_val - min_val)))
}


plot_fit = function(ft,actual,pred,challenger, weight,ft_name, rebase = T, point_size = 2, lwd = 0.8, 
                    fit_lines =  c("CA_base", "CA_challenger", "obs", "CU_unadj_base", "CU_unadj_challenger")){
  
  
  if(rebase){
    pred =   pred*(sum(actual)/sum(pred*weight )) # rebase
    challenger =   challenger*(sum(actual)/sum(challenger*weight )) # rebase
  }
  
  df = data.frame(ft,actual,pred,challenger,weight )
  df %>% 
    mutate_at(vars(c("pred", "challenger")) , ~.x*weight) -> df
  df  %>% select(-ft) %>%
    summarise_all(list(sum))  %>%
    mutate(actual_overall_avg = actual/weight,
           pred_overall_avg = pred/weight , 
           challenger_overall_avg = challenger/weight) ->overall
  df %>%  group_by(ft) %>%
    summarise_all(list(sum)) %>%
    mutate(actual=actual/weight,
           pred=pred/weight,
           challenger=challenger/weight,
           ave = actual/pred,
           challenger_ave = actual/challenger,
           actual_overall_avg = overall$actual_overall_avg,
           pred_overall_avg = overall$pred_overall_avg,
           actual_overall_avg = overall$actual_overall_avg
           
    )%>%
    mutate_at(vars(c("pred" , "challenger" , "actual")), ~ .x/actual_overall_avg ) %>%
    mutate(weight = weight/sum(weight)) %>%
    select(c("ft", "pred" , "challenger" , "actual" , "weight" , "ave" , "challenger_ave" )) %>%
    rename(CA_base = pred,
           CA_challenger =challenger ,
           obs = actual , 
           CU_unadj_base = ave,
           CU_unadj_challenger = challenger_ave) %>% 
    melt(id.var = c("ft" ) ) %>% as.data.table -> ave_df  
  ave_df %>% filter(variable!= "weight") %>%
    filter(variable %in% fit_lines) %>% 
    ggplot(.,aes(x = ft , y = value , group = variable , color = variable , shape = variable))+
    geom_bar(data =  ave_df[variable == "weight"],
             aes( y=   ave_df[variable == "weight"]$value/max(ave_df[variable == "weight"]$value)),
             stat="identity", size=.1, alpha=1 , position = "dodge", color = "yellow", fill = "yellow")+
    theme_light()+
    geom_line(lwd = lwd, alpha = 0.7) +
    geom_point(size = point_size)+
    scale_color_manual(values = c("CA_base" = "darkgreen",
                                  "CA_challenger"="darkgreen",
                                  "obs" = '#da14ff',
                                  "CU_unadj_base" ="orange",
                                  "CU_unadj_challenger" = "orange")) +
    scale_shape_manual(values = c("CA_base" = 16,
                                  "CA_challenger"=3,
                                  "obs" = 16,
                                  "CU_unadj_base" =16,
                                  "CU_unadj_challenger" = 3))+
    xlab(ft_name)+
    
    
    scale_y_continuous(name = "",sec.axis = sec_axis(~.*max(ave_df[variable == "weight"]$value), name="weight")) 
  
}



# Define your function here
calc_ave <- function(ft, actual, pred, weight, challenger, factor_consistency,ft_name, rebase = FALSE) {
  # Convert to data.table
  
  
  
  df <- data.table(ft =ft,
                   actual = actual,
                   pred = pred,
                   weight = weight,
                   factor_consistency =  factor(factor_consistency, levels = unique(factor_consistency)))
  
  if (rebase) {
    rb_factor <- df[, .(weighted_pred = sum(pred * weight), actual = sum(actual)), by = factor_consistency]
    rb_factor[, rb_factor := actual / weighted_pred]
    df <- merge(df, rb_factor[, .(factor_consistency, rb_factor)], by = "factor_consistency")
    df[, pred := pred * rb_factor]
  }
  
  df[, `:=`(pred = pred * weight)]
  ave_df <- df[, .(pred = sum(pred), actual = sum(actual), weight = sum(weight)), by = .(ft, factor_consistency)]
  ave_df[, `:=`(pred = pred / weight, actual = actual / weight, ave = actual / pred)]
  ave_df <- ave_df[, .(ft, pred, actual, weight, weight_norm = weight / sum(weight), ave, sample = factor_consistency)]
  ave_df$ft = factor(ave_df$ft , levels = KT_dym_sort(unique(ave_df$ft)))
  scale <- max(ave_df$weight_norm)
  p <- ggplot(ave_df, aes(x = ft, group = sample, fill = sample, color = sample, weight = weight)) +
    theme_light() +
    geom_hline(yintercept = 1, color = '#39ff14', linetype = "dashed") +
    geom_point(aes(y = ave, color = sample), shape = 4) +
    geom_line(aes(y = ave, color = sample)) +
    geom_bar(aes(y = weight_norm / scale), stat = "identity", size = .1, alpha = .4, position = "dodge") +
    scale_y_continuous(name = "Actual/Expected", sec.axis = sec_axis(~ . * scale * exp(scale), name = "weight")) +
    theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 0.9)) +
    ylab("Actual/Expected") +
    xlab(ft_name)
    
  
  return(list(ave_df = ave_df, ave_plot = p, rebase_data = if (rebase) rb_factor else NULL))
}

cosmetic_changes <- function(p, alpha_pt=1,alpha_line=0.5, size_pt =2, size_line=1, fit_loess = T  ,  smooth_strength = 0.4, control_yaxis = T ,  upper_lim  = 2, lower_lim = 0){
  # p = test_plot$ave_plot
  p$layers[[2]]$aes_params$size = size_pt
  p$layers[[2]]$aes_params$alpha = alpha_pt
  p$layers[[3]]$aes_params$size = size_line
  p$layers[[3]]$aes_params$alpha = alpha_line
  
  if (control_yaxis){
    p <- p + coord_cartesian(ylim = c(lower_lim,upper_lim)) 
  }
  
  
  if(fit_loess){
    pout <- p + geom_smooth( aes(y = ave), method = "loess", span = smooth_strength, se = FALSE)
    smooth_data =data.table(x  =  ggplot_build(pout)$data[[5]][[3]] , y =  ggplot_build(pout)$data[[5]][[4]])
  }else{
    pout<-p
    smooth_data =data.table(x  = NULL , y =  NULL)
  }
  
  
  pout<-ggplotly(pout)   %>% layout(
    legend = list(
      orientation = 'h',  
      x = 0.5,           
      xanchor = 'center', 
      y = -0.2            
    )
  )
  return(list(ave_plot = pout , smooth_data = smooth_data))
}


model_spec <-list(ad_f_b = list(exposure = 'freqexposure_adbclaim',
                                response = 'freqmodels_adbclaim',
                                objective = 'count:poisson',
                                eval_metric='poisson-nloglik',
                                fam = poisson()),
                  ad_s_b = list(exposure = 'sevexposure_adbclaim',
                                response = 'sevmodels_adbclaim',
                                objective = 'reg:gamma',
                                eval_metric='gamma-nloglik',
                                fam = Gamma(link = "log")),
                  ad_f_c = list(exposure = 'freqexposure_adcclaim',
                                response = 'freqmodels_adcclaim',
                                objective = 'count:poisson',
                                eval_metric='poisson-nloglik',
                                fam = poisson()),
                  ad_s_c = list(exposure = 'sevexposure_adcclaim',
                                response = 'sevmodels_adcclaim',
                                objective = 'reg:gamma',
                                eval_metric='gamma-nloglik',
                                fam = Gamma(link = "log")),
                  eow_f_b = list(exposure = 'freqexposure_eowbclaim',
                                 response = 'freqmodels_eowbclaim',
                                 objective = 'count:poisson',
                                 eval_metric='poisson-nloglik',
                                 fam = poisson()),
                  eow_s_b = list(exposure = 'sevexposure_eowbclaim',
                                 response = 'sevmodels_eowbclaim',
                                 objective = 'reg:gamma',
                                 eval_metric='gamma-nloglik',
                                 fam = Gamma(link = "log")),
                  eow_f_c = list(exposure = 'freqexposure_eowcclaim',
                                 response = 'freqmodels_eowcclaim',
                                 objective = 'count:poisson',
                                 eval_metric='poisson-nloglik',
                                 fam = poisson()),
                  eow_s_c = list(exposure = 'sevexposure_eowcclaim',
                                 response = 'sevmodels_eowcclaim',
                                 objective = 'reg:gamma',
                                 eval_metric='gamma-nloglik',
                                 fam = Gamma(link = "log")),
                  flood_f_b = list(exposure = 'freqexposure_floodbclaim',
                                   response = 'freqmodels_floodbclaim',
                                   objective = 'count:poisson',
                                   eval_metric='poisson-nloglik',
                                   fam = poisson()),
                  flood_s_b = list(exposure = 'sevexposure_floodbclaim',
                                   response = 'sevmodels_floodbclaim',
                                   objective = 'reg:gamma',
                                   eval_metric='gamma-nloglik',
                                   fam = Gamma(link = "log")),
                  flood_f_c = list(exposure = 'freqexposure_floodcclaim',
                                   response = 'freqmodels_floodcclaim',
                                   objective = 'count:poisson',
                                   eval_metric='poisson-nloglik',
                                   fam = poisson()),
                  flood_s_c = list(exposure = 'sevexposure_floodcclaim',
                                   response = 'sevmodels_floodcclaim',
                                   objective = 'reg:gamma',
                                   eval_metric='gamma-nloglik',
                                   fam = Gamma(link = "log")),
                  storm_f_b = list(exposure = 'freqexposure_stormbclaim',
                                   response = 'freqmodels_stormbclaim',
                                   objective = 'count:poisson',
                                   eval_metric='poisson-nloglik',
                                   fam = poisson()),
                  storm_s_b = list(exposure = 'sevexposure_stormbclaim',
                                   response = 'sevmodels_stormbclaim',
                                   objective = 'reg:gamma',
                                   eval_metric='gamma-nloglik',
                                   fam = Gamma(link = "log")),
                  storm_f_c = list(exposure = 'freqexposure_stormcclaim',
                                   response = 'freqmodels_stormcclaim',
                                   objective = 'count:poisson',
                                   eval_metric='poisson-nloglik',
                                   fam = poisson()),
                  storm_s_c = list(exposure = 'sevexposure_stormcclaim',
                                   response = 'sevmodels_stormcclaim',
                                   objective = 'reg:gamma',
                                   eval_metric='gamma-nloglik',
                                   fam = Gamma(link = "log")),
                  theft_f_b = list(exposure = 'freqexposure_theftbclaim',
                                   response = 'freqmodels_theftbclaim',
                                   objective = 'count:poisson',
                                   eval_metric='poisson-nloglik',
                                   fam = poisson()),
                  theft_s_b = list(exposure = 'sevexposure_theftbclaim',
                                   response = 'sevmodels_theftbclaim',
                                   objective = 'reg:gamma',
                                   eval_metric='gamma-nloglik',
                                   fam = Gamma(link = "log")),
                  theft_f_c = list(exposure = 'freqexposure_theftcclaim',
                                   response = 'freqmodels_theftcclaim',
                                   objective = 'count:poisson',
                                   eval_metric='poisson-nloglik',
                                   fam = poisson()),
                  theft_s_c = list(exposure = 'sevexposure_theftcclaim',
                                   response = 'sevmodels_theftcclaim',
                                   objective = 'reg:gamma',
                                   eval_metric='gamma-nloglik',
                                   fam = Gamma(link = "log")),
                  fire_f_b = list(exposure = 'freqexposure_firebclaim',
                                  response = 'freqmodels_firebclaim',
                                  objective = 'count:poisson',
                                  eval_metric='poisson-nloglik',
                                  fam = poisson()),
                  fire_s_b = list(exposure = 'sevexposure_firebclaim',
                                  response = 'sevmodels_firebclaim',
                                  objective = 'reg:gamma',
                                  eval_metric='gamma-nloglik',
                                  fam = Gamma(link = "log")),
                  fire_f_c = list(exposure = 'freqexposure_firecclaim',
                                  response = 'freqmodels_firecclaim',
                                  objective = 'count:poisson',
                                  eval_metric='poisson-nloglik',
                                  fam = poisson()),
                  fire_s_c = list(exposure = 'sevexposure_firecclaim',
                                  response = 'sevmodels_firecclaim',
                                  objective = 'reg:gamma',
                                  eval_metric='gamma-nloglik',
                                  fam = Gamma(link = "log")),
                  other_f_b = list(exposure = 'freqexposure_otherbclaim',
                                   response = 'freqmodels_otherbclaim',
                                   objective = 'count:poisson',
                                   eval_metric='poisson-nloglik',
                                   fam = poisson()),
                  other_s_b = list(exposure = 'sevexposure_otherbclaim',
                                   response = 'sevmodels_otherbclaim',
                                   objective = 'reg:gamma',
                                   eval_metric='gamma-nloglik',
                                   fam = Gamma(link = "log")),
                  other_f_c = list(exposure = 'freqexposure_othercclaim',
                                   response = 'freqmodels_othercclaim',
                                   objective = 'count:poisson',
                                   eval_metric='poisson-nloglik',
                                   fam = poisson()),
                  other_s_c = list(exposure = 'sevexposure_othercclaim',
                                   response = 'sevmodels_othercclaim',
                                   objective = 'reg:gamma',
                                   eval_metric='gamma-nloglik',
                                   fam = Gamma(link = "log")),
                  subs_f_b = list(exposure = 'freqexposure_subsbclaim',
                                  response = 'freqmodels_subsbclaim',
                                  objective = 'count:poisson',
                                  eval_metric='poisson-nloglik',
                                  fam = poisson()),
                  subs_s_b = list(exposure = 'sevexposure_subsbclaim',
                                  response = 'sevmodels_subsbclaim',
                                  objective = 'reg:gamma',
                                  eval_metric='gamma-nloglik',
                                  fam = Gamma(link = "log")),
                  unsp_f_pp = list(exposure = 'freqexposure_unspecifiedppcclaim',
                                   response = 'freqmodels_unspecifiedppcclaim',
                                   objective = 'count:poisson',
                                   eval_metric='poisson-nloglik',
                                   fam = poisson()),
                  unsp_s_pp = list(exposure = 'sevexposure_unspecifiedppcclaim',
                                   response = 'sevmodels_unspecifiedppcclaim',
                                   objective = 'reg:gamma',
                                   eval_metric='gamma-nloglik',
                                   fam = Gamma(link = "log")))

# fts<- readRDS("fts.rds")
