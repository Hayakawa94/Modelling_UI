
# todo
# 1. stress test
# 2. Structure code
      # Script to specify the inputs going into the app.
      # Script for designing the UI, i.e., what you see on the app.
      # Script to store tools/functions.
      # Server script to bring these tools into a reactive mode.
          # Break server into multiple code
      # maybe add imputation column?
# 3. add comments
# 4. try add cm line 
# 5. add more EDA
# 7. add PDP
# 9. PMML on frequency data including OHE feature



source("H:/Restricted Share/DA P&U/Tech Modelling/Users/Khoa/RPMtools/RPMtools.R")

library(parallel)

exp <- "panel_prem_model"

base_path <- "H:/Restricted Share/DA P&U/Tech Modelling/01 Home/Phase 2/15 R&D/Modelling_ui"

exp_path <- glue("{base_path}/{exp}") 
if ( !file.exists(exp_path)){
  
  dir.create(exp_path)
}
# # specify modelling data 
train <- fread("H:\\Restricted Share\\DA P&U\\Tech Modelling\\01 Home\\Phase 2\\15 R&D\\Modelling_ui\\panel_prem_modelling_data.csv")
train[train$ay_unbanded < 2023] -> train




# specify fts
fts <- train %>% select(!ends_with(c("Claim"))) %>% names %>% sort
# #
# Sampling
# nrow(train)
set.seed(1)
train$idx = 1:nrow(train)
train %>% sample_frac(0.2) -> test
train[setdiff(train$idx , test$idx)] %>% select(-idx) -> train
test<- test %>% select(-idx)
nrow(train) + nrow(test)
# specify model spec
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


selected_model = "theft_s_c"


file.copy(from =   glue("{base_path}/Server.R") , to = glue("{exp_path}/Server.R"),overwrite = T)
shiny::runApp(glue("{exp_path}/Server.R"))

