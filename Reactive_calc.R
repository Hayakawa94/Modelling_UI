source("H:/Restricted Share/DA P&U/Tech Modelling/Users/Khoa/RPMtools/RPMtools.R")

library("flexdashboard")

get_mode <- function(v, bins = 10) {
  v <- na.omit(v)
  
  if (is.character(v) || is.factor(v)) {
    uniqv <- unique(v)
    return(uniqv[which.max(tabulate(match(v, uniqv)))])
  } else if (is.numeric(v)) {
    if (length(unique(v)) > bins) {
      breaks <- seq(min(v), max(v), length.out = bins + 1)
      bin_counts <- cut(v, breaks, include.lowest = TRUE)
      return(levels(bin_counts)[which.max(tabulate(bin_counts))])
    } else {
      uniqv <- unique(v)
      return(uniqv[which.max(tabulate(match(v, uniqv)))])
    }
  } else if (is.logical(v)) {
    uniqv <- unique(v)
    return(uniqv[which.max(tabulate(match(v, uniqv)))])
  } else if (inherits(v, c("IDate", "Date", "POSIXct"))) {
    uniqv <- unique(v)
    return(uniqv[which.max(tabulate(match(v, uniqv)))])
  } else {
    stop("Unsupported data type")
  }
}

summarise_dataframe <- function(df) {
  gc()
  df <- df %>% 
    mutate_if(is.character, ~ na_if(., ""))
  summary <- data.frame(
    Feature = character(),
    Min = character(),
    Max = character(),
    Mode = character(),
    Proportion_Missing = numeric(),
    Data_Type = character(),
    Levels = character(),
    stringsAsFactors = FALSE
  )
  
  for (col in colnames(df)) {
    data_type <- class(df[[col]])
    col_data <- df[[col]]
    
    if (any(data_type %in% c("numeric", "integer"))) {
      min_val <- as.character(round(min(col_data, na.rm = TRUE), 3))
      max_val <- as.character(round(max(col_data, na.rm = TRUE), 3))
      mode_val <- as.character(round(as.numeric(get_mode(col_data)), 3))
      levels_val <- NA
    } else if (any(data_type %in% c("factor", "character"))) {
      min_val <- NA
      max_val <- NA
      mode_val <- as.character(get_mode(col_data))
      levels_val <- paste(levels(as.factor(col_data)), collapse = ", ")
    } else if (any(data_type %in% c("POSIXct", "IDate", "Date"))) {
      min_val <- as.character(as.Date(min(col_data, na.rm = TRUE)))
      max_val <- as.character(as.Date(max(col_data, na.rm = TRUE)))
      mode_val <- as.character(as.Date(get_mode(col_data)))
      levels_val <- NA
    } else {
      min_val <- NA
      max_val <- NA
      mode_val <- NA
      levels_val <- NA
    }
    
    proportion_missing <- mean(is.na(col_data))
    
    summary <- rbind(summary, data.frame(
      Feature = col,
      Min = min_val,
      Max = max_val,
      Mode = mode_val,
      Proportion_Missing = proportion_missing,
      Data_Type = data_type,
      Levels = levels_val
    ))
  }
  
  return(summary)
}

# summarize_dataframe(test$train %>% select(fts)) -> test2
# test<- KT_create_sample(df = df_eng_sample,df_eng_sample$exposure,y = df_eng_sample$freqexposure_eowbclaim,kfold = 4 )

KT_create_sample <- function(df , weight, y, kfold , train_validate_split= 0.8){
  gc()
  set.seed(1)
  if (!missing(kfold)){
    KT_create_fold_idx(df,k = kfold) -> kfold_idx
    train <- df
    train_y <- y
    train_weight <- weight
    
    print(glue("train dim:{dim(train)} with {kfold} fold(s)"))
    return(list(train = train , 
                train_y= train_y , 
                train_weight=train_weight,
                kfold_idx = kfold_idx))
  }else{
    kfold_idx =NULL
    idx <- sample(seq_len(nrow(df)), size = train_validate_split * nrow(df))
    train <- df[idx, ]
    train_y <- y[idx]
    train_weight <- weight[idx]
    validate <- df[-idx, ]
    validate_y <- y[-idx]
    validate_weight <- weight[-idx]
    
    print(glue("train dim:{dim(train) } | validate dim:{dim(validate)}"))

    return(list(train = train , 
                train_y= train_y , 
                train_weight=train_weight,
                validate_y=validate_y ,
                validate_weight=validate_weight, 
                validate = validate))
  
  }
  gc()
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
                       ncluster  = parallel::detectCores()-1,
                       initPoints=10 ){
  gc()
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
    
    
    return(
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
                        initPoints =  initPoints ))
  }else{
    
   return( KT_xgb_baysian_tune(train =  sample_result$train %>% select(fts),
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
                        initPoints =  initPoints ))
  }
  gc()
  
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
                        seed = 1,
                        return_pred_only = F,
                      
                        ...
){
  gc()
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
    nthread =  detectCores()
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
                 early_stopping_rounds = early_stopping_rounds,
                 seed = seed) -> train_result
  }
  if(return_pred_only){
    return(predict(train_result$model ,newdata =as.matrix(train %>% select(fts))   , type  = "response"))
  }
  else{
    explain_result <- KT_xgb_explain(model = train_result$model, pred_data = sample_result$train %>% select(fts) )
    
    pred <- predict(train_result$model ,newdata =as.matrix( mltools::one_hot(train %>% select(fts) , train %>% select(fts) %>%  select_if(is.factor) %>% names)    )   , type  = "response")
    
    
    return(list(imp_plot = list(imp_gain   = train_result$imp_plot,
                                imp_shap = explain_result$ft_importance_plot,
                                imp_comparison =    KT_plot_compare_ft_imp(train_result$imp_plot$data%>% arrange(Gain) %>% select(Feature) %>% pull,
                                                                           explain_result$ft_importance_plot$data %>% arrange(pc_contri) %>% select(variable) %>% pull) +
                                  theme(legend.position = "none") +
                                  theme_light(base_size = 18) + ggtitle("gain vs SHAP importance") ,
                                imp_shap_X = explain_result$ft_importance_X,
                                EIXinteraction_gain_matrix  =explain_result$EIXimportance_matrix,
                                EIX_gain=explain_result$EIXimportance,
                                EIXinteraction_gain=explain_result$EIXimportanceX
    ),
    shap_values = list(main_effect =explain_result$main_effect,
                       interaction_effect = explain_result$interaction ),
    model =train_result$model,
    pred = pred,
    pmml_fmap =    r2pmml::as.fmap(as.data.table(sample_result$train %>% select(fts)))))
  }
  gc()
}



create_splines <- function(df,splines_dt){
  gc()
  splines_dt<- splines_dt %>% distinct()
  overlay_fts_dt <- data.table(idx  = 1:nrow(df))
  for(i in  1:nrow(splines_dt)){
    feature <- splines_dt[["feature"]][i]
    spline <- splines_dt[["id"]][i]
    min_val  <- as.numeric( splines_dt[["x0_lvl_name"]][i] )
    max_val <- as.numeric( splines_dt[["x1_lvl_name"]][i])
    overlay_fts_dt[[spline]] <- normalize_feature(feature = df[[feature]],min_val =min_val,max_val =  max_val  )
    
  }
  return(overlay_fts_dt %>% select(-idx))
}
Sys.time() -> t0

glm_fit <- function(glm_train, splines_dt, response, base, weight, fam, pmml_max_band = 2000) {
  # browser()
  splines_dt <- splines_dt %>% distinct()
  overlay_fts_dt <- create_splines(df = glm_train %>% mutate_all(~ifelse(is.na(.), KT_calculate_mode(.), .)), splines_dt = splines_dt)
  
  x <- model.matrix(~., data = overlay_fts_dt)

  base_ave <- response / base

  suppressWarnings({
    adj_fit <- fastglm(x = x, y = base_ave, weights = base * weight, family = fam)
  })
  adj <- predict(adj_fit, newdata = x, type = "response")
  indiv_eff <- list()
  for (ft in splines_dt$feature %>% unique()) {
    excl_terms <- names(adj_fit$coefficients)[!grepl(glue("^{ft}"), names(adj_fit$coefficients))]
    temp_model <- adj_fit
    temp_model$coefficients[excl_terms[excl_terms != "(Intercept)"]] <- 0
    indiv_eff[[ft]] <- predict(temp_model, newdata = x, type = "response")
  }
  coefficients <- coef(adj_fit)
  
  # Create data table
  result <- data.table(
    id = gsub("`", "", names(coefficients)),
    estimate = coefficients
  )
  # create LP_model 
  fitted_fts <- splines_dt$feature %>% unique
  LP_models <- lapply(fitted_fts, function(ft) {
    temp <- adj_fit
    temp$coefficients[['(Intercept)']] <- 0
    effect_to_remove <- setdiff(fitted_fts, ft)
    coef_name <- names(temp$coefficients)
    temp$coefficients[coef_name[grepl(paste0("^", effect_to_remove, collapse = "|"), coef_name)]] <- 0
    temp
  }) %>% setNames(.,fitted_fts)
  # Create rdr_lookup tables
  imp_values <- lapply(glm_train, function(x) KT_calculate_mode(x)) %>% setNames(., names(glm_train))
  feature_range <- lapply(glm_train, function(x) c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))) %>% setNames(., names(glm_train))
  lookup_tables_list <- list()
  band_logic_for_rdr_list <- list()
  result %>% left_join(splines_dt, by = "id") -> model_summary
  
  for (x in unique(model_summary$feature[!is.na(model_summary$feature)])) {
    model_summary %>% filter(feature == x) -> temp
    lapply(temp$id, function(y) 
      normalize_feature(imp_values[[x]],
                        min_val = temp[temp$id == y][["x0_lvl_name"]],
                        max_val = temp[temp$id == y][["x1_lvl_name"]]) * temp[temp$id == y][["estimate"]]) %>% 
      Reduce("+", .) -> imp_rel
    
    band_req <- if (unique(temp$dtype) == "integer" & sort(feature_range[[x]])[1] == 0 & sort(feature_range[[x]])[2] == 1) {
      FALSE
    } else {
      TRUE
    }
    
    if (band_req) {
      model_summary %>%
        filter(feature == x) %>%
        select(x0_lvl_name, x1_lvl_name, estimate) %>%
        rename(x0 = x0_lvl_name, x1 = x1_lvl_name) %>%
        arrange(x0) %>%
        mutate(range = x1 - x0,
               prop = range / sum(range),
               band_dist = round(pmml_max_band * prop)) %>%
        mutate(gap = x1 - lead(x0, 1),
               gap = ifelse(is.na(gap), 0, gap),
               x0lag1 = lead(x0, 1)) %>%
        rowwise() %>%
        mutate(
          spline = ifelse(gap == 0, 
                          list(seq(x0, x1, length.out = band_dist) %>% round(3) %>% unique), 
                          list(c(seq(x0, x1, length.out = band_dist), seq(x1, x0lag1, length.out = 2))  %>% round(3)%>% unique)),
          spline_norm = list(normalize_feature(spline, min_val = x0, max_val = x1)),
          rel = list(as.vector(spline_norm)[as.vector(spline_norm) > 0] * estimate),
          last_rel = (rel[[length(rel)]]),
          band = list(create_pairs(spline %>% as.vector() ))
        ) %>% ungroup() %>%
        mutate(band = lapply(seq_along(band), function(x) {
          if (length(band) == 1) {
            c(glue("<={spline[[x]][1]}"), band[[x]], glue(">{tail(spline[[x]], 1)}"), "default")
          } else if (x == 1) {
            c(glue("<={spline[[x]][1]}"), band[[x]])
          } else if (x == length(band)) {
            c(band[[x]], glue(">{tail(spline[[x]], 1)}"), "default")
          } else {
            band[[x]]
          }
        }),
        last_rel = cumsum(last_rel),
        rel = lapply(1:length(rel), function(x) if (x == 1) { rel[[x]] } else { rel[[x]] + last_rel[x - 1] }),
        rel = lapply(seq_along(rel), function(x) {
          if (length(rel) == 1) {
            c(0, rel[[x]], tail(rel[[x]], 1), imp_rel)
          } else if (x == 1) {
            c(0, rel[[x]])
          } else if (x == length(rel)) {
            c(rel[[x]], tail(rel[[x]], 1), imp_rel)
          } else {
            rel[[x]]
          }
        })
        ) -> rel_data
      # browser()
      lapply(1:nrow(rel_data), function(x) data.table(band = rel_data[x,]$band %>% unlist(), relativity = rel_data[x,]$rel %>% unlist())) %>% 
        rbindlist(.) %>% group_by(band) %>% 
        summarise(relativity = mean(relativity)) %>% ungroup() %>%
        mutate(band = factor(band, levels = KT_dym_sort(band)), relativity = (relativity)) %>%
        arrange(band) %>%
        rename({{x}} := band)   -> lookup_table
      
      interval <- lookup_table[[x]] %>% as.character
      interval[2:(length(interval) - 2)] -> interval
      
      sub("\\(([^,]+),.*", "\\1", interval) -> lb
      sub(".*,([^]]+)\\]", "\\1", interval) -> ub
      
      band_logic_for_rdr <- data.frame(
        LO = c("<=", rep(">", length(lb)), ">", "Default"),
        LB = c(lb[1], lb, ub[length(ub)], ""),
        UO = c("", rep("<=", length(ub)), "", ""),
        UB = c("", ub, "", ""),
        ln = c(lookup_table[[x]] %>% as.character)
      )
      
    } else {
      lapply(temp$id, function(y) 
        normalize_feature(c(0, 1),
                          min_val = temp[temp$id == y][["x0_lvl_name"]],
                          max_val = temp[temp$id == y][["x1_lvl_name"]]) * temp[temp$id == y][["estimate"]]) %>% 
        Reduce("+", .) -> relativity # quick fix
      lookup_table <- data.table()
      feature_range[[x]] %>% sort %>% as.character -> ft
      lookup_table[, (x) := factor(c(ft, "default"))]
      lookup_table$relativity <- c(relativity, imp_rel)
      band_logic_for_rdr <- data.frame(
        LO = c("=", "=", "Default"),
        LB = c("0", "1", ""),
        UO = c("", "", ""),
        UB = c("", "", ""),
        ln = c("0", "1", "default")
      )
    }
    
    lookup_tables_list[[x]] <- lookup_table
    band_logic_for_rdr_list[[x]] <- band_logic_for_rdr
  }
  
  intercept <- model_summary[model_summary$id == "(Intercept)"]$estimate
  lookup_tables_list$intercept <- data.table(intercept = 1, relativity = intercept)
  
  print(glue("glm fit total run time {Sys.time() - t0}"))
  return(list(
    adj = as.numeric(adj),
    model = adj_fit,
    fit = result,
    indiv_eff = indiv_eff,
    imp_values = imp_values,
    feature_range = feature_range,
    lookup_tables = lookup_tables_list,
    band_logic_for_rdr = band_logic_for_rdr_list,
    LP_models=LP_models
  ))
  gc()
} 
create_pairs <- function(vector) {
  pairs <- sapply(1:(length(vector) - 1), function(i) {
    paste0("(", vector[i], ",", vector[i + 1], "]")
  })
  return(pairs)
}

normalize_feature <- function(feature, min_val, max_val) {
  ifelse(feature < min_val, 0, ifelse(feature > max_val, 1, (feature - min_val) / (max_val - min_val)))
}


plot_fit = function(ft,actual,pred,challenger, weight,ft_name, rebase = T, point_size = 2, lwd = 0.8, 
                    fit_lines =  c( "CA_challenger", "obs", "CU_challenger" , "CM"),
                    band_ft =T,
                    band_method = "equal",
                    nbreaks=20,
                    indiv_eff ){
  
  # browser()
  if(rebase){
    pred =   pred*(sum(actual)/sum(pred*weight )) # rebase
    indiv_eff = indiv_eff* (sum(actual)/sum(indiv_eff*weight ))
    challenger =   challenger*(sum(actual)/sum(challenger*weight )) # rebase
  }
  ft <- if (band_ft){
       band_data(KT_quantile_clip(ft, 0.001 , 0.999),nbreaks = nbreaks,method = band_method, weight = weight  )
      }else{
        ft
      }
   
  # browser()
  df = data.frame(ft,actual,pred,challenger,weight ,indiv_pred= indiv_eff )
  df %>% 
    mutate_at(vars(c("pred", "challenger" , "indiv_pred")) , ~.x*weight) -> df
  df  %>% select(-ft) %>%
    summarise_all(list(sum))  %>%
    mutate(actual_overall_avg = actual/weight,
           pred_overall_avg = pred/weight , 
           indiv_pred_overall_avg = indiv_pred/weight,
           challenger_overall_avg = challenger/weight) ->overall
  df %>%  group_by(ft) %>%
    summarise_all(list(sum)) %>%
    mutate(actual=actual/weight,
           pred=pred/weight,
           challenger=challenger/weight,
           indiv_pred = indiv_pred/weight,
           ave = actual/pred,
           challenger_ave = actual/challenger,
           actual_overall_avg = overall$actual_overall_avg,
           pred_overall_avg = overall$pred_overall_avg,
           actual_overall_avg = overall$actual_overall_avg
           
    )%>%
    mutate_at(vars(c("pred" , "challenger" , "actual" , "indiv_pred")), ~ .x/actual_overall_avg ) %>%
    mutate(weight = weight/sum(weight)) %>%
    select(c("ft", "pred" , "challenger" , "actual" , "weight" , "ave" , "challenger_ave" , "indiv_pred" )) %>%
    rename(CA_base = pred,
           CA_challenger =challenger ,
           CM = indiv_pred,
           obs = actual , 
           CU_base = ave,
           CU_challenger = challenger_ave) %>% 
    mutate(CU_base = CU_base*CM,
           CU_challenger=CU_challenger*CM)%>% 
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
                                  "CU_base" ="orange",
                                  "CU_challenger" = "orange",
                                  "CM" = "green")) +
    scale_shape_manual(values = c("CA_base" = 16,
                                  "CA_challenger"=3,
                                  "obs" = 16,
                                  "CU_base" =16,
                                  "CU_challenger" = 3,
                                  "CM" = 1))+
    xlab(ft_name)+
    theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust=0.9))+
    
    
    scale_y_continuous(name = "",sec.axis = sec_axis(~.*max(ave_df[variable == "weight"]$value), name="weight")) 

}



# Define your function here
calc_ave <- function(ft, actual, pred, weight, challenger, factor_consistency,ft_name, rebase = FALSE , band_ft = T,nbreaks=20,band_method = "equal" ) {
  # Convert to data.table
  gc()
  
  ft <- if (band_ft){
    
    band_data(KT_quantile_clip(ft, 0.001 , 0.999),nbreaks = nbreaks,method = band_method, weight = weight  )
    # band_data(ft,nbreaks = nbreaks)
  }else{
    ft
  }
  
  # browser()
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
  # ave_df$ft = factor(ave_df$ft , levels = KT_dym_sort(unique(ave_df$ft)))
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
  gc()
}

cosmetic_changes <- function(p, alpha_pt=1,alpha_line=0.5, size_pt =2, size_line=1, fit_loess = T  ,  smooth_strength = 0.4, control_yaxis = T ,  upper_lim  = 2, lower_lim = 0){
  # p = test_plot$ave_plot
  gc()
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
  gc()
}
band_data <- function(x, weight = 1, nbreaks = 100, method = c("equal", "quantile")) {
  method <- match.arg(method)
  # browser()
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  
  x <- pmin(pmax(x, min_val), max_val)
  
  if (method == "equal") {
    breaks <- seq(min_val, max_val, length.out = nbreaks + 1)
  } else if (method == "quantile") {
    # Use exposure to calculate quantiles
    breaks <- unique(quantile(x, probs = seq(0, 1, length.out = nbreaks + 1), na.rm = TRUE, weights = weight))
  }
  
  if (is.integer(x)) {
    breaks <- round(breaks)
    breaks <- unique(breaks) 
  }
  
  # Ensure the first and last breaks are min_val and max_val respectively
  breaks[1] <- min_val
  breaks[length(breaks)] <- max_val
  
  labels <- paste0("(", formatC(head(breaks, -1), format = "f", digits = 3), ",", formatC(tail(breaks, -1), format = "f", digits = 3), "]")
  
  return(cut(x, breaks = breaks, labels = labels, include.lowest = TRUE))
  gc()
}

create_EDA_agg <- function(ft= 1,
                           y = 1,
                           weight =1 , 
                           interaction= 1, 
                           ft_nbreaks = 30, 
                           interaction_nbreaks = 30,
                           ft_band_type = "equal",
                           interaction_band_type = "quantile"){
  
  
  gc()
  ft <- if (is.numeric(ft) & length(unique(ft)) > 5) {
    band_data(x = ft, nbreaks = ft_nbreaks, method = ft_band_type )
  } else {
    ft
  }
 
  
  interaction <- if (is.numeric(interaction) & length(unique(interaction)) > 4) {
    band_data(x = interaction, nbreaks = interaction_nbreaks, method = interaction_band_type)
  } else {
    interaction
  }
  
  tot_weight <- sum(weight)
  return(data.table(ft = ft, interaction = interaction, y = y, weight = weight) %>%
    group_by(ft, interaction) %>%
    summarise(y = sum(y) / sum(weight), weight = sum(weight) / tot_weight) %>%
    ungroup())
  gc()
  
}

EDA_plot <- function(agg_df,
                     bar_alpha = 0.5,
                     lwd = 1,
                     point_size = 2.5,
                     line_alpha = 1,
                     point_alpha = 1,
                     ft_name = "x",
                     interaction_name= "y",
                     smooth_strength =0.7) {
  
  gc()
  # browser()
  suppressMessages(
  p <- ggplot(agg_df, aes(x = ft, group = interaction, fill = interaction , weight = weight)) +
    theme_light(base_size = 15) +
    geom_bar(aes(y = weight / mean(agg_df$weight)*(sum(agg_df$y*agg_df$weight)/10)), stat = "identity", alpha = bar_alpha) +
    geom_line(aes(y = y, color = interaction), lwd = lwd, alpha =  line_alpha ) +
    geom_point(aes(y = y,color = interaction), size = point_size, alpha = point_alpha, shape = 4) +
    scale_y_continuous(name = "", sec.axis = sec_axis(~ . * mean(agg_df$weight), name = "weight")) +
    theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 0.9))+
    labs(fill = interaction_name,
         color = interaction_name,
         x = ft_name) )
  
  if(smooth_strength >0){
    p<-p+geom_smooth( aes(y = y, color = interaction), method = "loess", span = smooth_strength, se = FALSE)
  }
  
  gc()
  p
}

custom_round <- function(x, digits = 2) {
  sapply(x, function(val) {
    if (abs(val) < 1) {
      return(signif(val, digits))
    } else {
      return(round(val, digits))
    }
  })
}

glm_spline_predict <- function(model_out , pred_df, type = "response",predict_LP = F){
  pred_df <-  sapply(model_out$glm_model_out$imp_values %>% names, function(x) replace_na( pred_df[[x]] ,model_out$glm_model_out$imp_values[[x]] ) ) %>% as.data.table()
  if(predict_LP){
    lapply(model_out$glm_model_out$LP_models, function(x) predict(x, 
                                                                  newdata = model.matrix(~., data = create_splines(df =pred_df,splines_dt=model_out$drawn_shapes %>% rbindlist(.)  )) , 
                                                                  type = type) )%>%
      setNames(.,names(model_out$glm_model_out$LP_models))
  }else{
    predict(model_out$glm_model_out$model, newdata = model.matrix(~., data = create_splines(df =pred_df,splines_dt=model_out$drawn_shapes %>% rbindlist(.)  )) , type = type) 
  }
}

