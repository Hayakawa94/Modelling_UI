
model = readRDS("Training.rds")
model$model_output$model -> xgb_model


gbm.data <- train %>%  dplyr::select(  xgb_model$feature_names ) 

# model.fmap = data.frame(
#   "id" = seq(from = 0, (to = length(gbm.data)-1)),
#   "name" = as.factor(names(gbm.data) ),
#   "type" = as.factor(  rep("q", length(names(gbm.data))))
# )

r2pmml::as.fmap(as.matrix(gbm.data))->model.fmap

r2pmml::r2pmml(xgb_model, fmap=model.fmap, "training.pmml" , response_name = "target")


gbm.data$pred = predict(xgb_model,
                        newdata = as.matrix(gbm.data%>% select( xgb_model$feature_names ) ) ,
                        # predcontrib = T,
                        # strict_shape = F,
                        # ntreelimit = xgb_model$niter
                        # ntreelimit = 136
                        iterationrange = c(1,1)
                        # outputmargin=TRUE
                        ) 
 

fwrite(gbm.data,"pmml_test_Train_data.csv")


# xgb.plot.tree(model = xgb_model, trees = 17 )

rdr_out <- fread("pmml_test_out.csv")
rdr_out$idx <- 1:nrow(rdr_out)
gbm.data$idx <- 1:nrow(gbm.data)
gbm.data %>% inner_join(rdr_out,by= "idx", suffix =  c("_R" , "_rdr")) %>% 
  mutate(diff=  pred/target -1 )  %>% select(diff) %>% summary


xgb.model.dt.tree(model = xgb_model) %>% count(Tree) %>% filter( n== 1)



(predict(xgb_model,
        newdata = as.matrix(gbm.data%>% select( xgb_model$feature_names ) ) ,
        # predcontrib = T,
        # strict_shape = F,
        # ntreelimit = xgb_model$niter
        # ntreelimit = 136
        iterationrange = c(1,1)
        # outputmargin=TRUE
) /predict(xgb_model,
           newdata = as.matrix(gbm.data%>% select( xgb_model$feature_names ) ) ,
           # predcontrib = T,
           # strict_shape = F,
           # ntreelimit = xgb_model$niter
           # ntreelimit = 136
           iterationrange = c(1,xgb_model$best_iteration)
           # outputmargin=TRUE
) ) %>% summary
