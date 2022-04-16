# 
# max_na_fraction <- 0.01
# training_fraction <- 0.75
# 
# prep <- esbl_tests %>%
#   mutate(outcome = factor(esbl, levels = c(TRUE, FALSE))) %>%
#   select(outcome, where(is.double)) %>%
#   # remove columns that do not comply to max_na_fraction
#   select(where(~sum(is.na(.)) / length(.) <= max_na_fraction)) %>%
#   # remove rows that have NA in outcome or predictors
#   filter_all(~!is.na(.))
# df_split <- rsample::initial_split(prep, prop = training_fraction)
# 
# tune_spec <- parsnip::rand_forest(mtry = tune(),
#                                   trees = tune()) %>% 
#   parsnip::set_engine("ranger") %>% 
#   parsnip::set_mode("classification")
# 
# tree_grid <- dials::grid_regular(dials::mtry(range = c(1, 17)),
#                                  dials::trees(),
#                                  levels = 5)

# training_data <- df_split %>%
#   rsample::training()
# 
# vfold <- rsample::vfold_cv(training_data)

tree_wf <- workflows::workflow() %>%
  workflows::add_model(tune_spec) %>%
  workflows::add_formula(outcome ~ .)

tree_res <- tree_wf %>% 
  tune::tune_grid(
    resamples = vfold,
    grid = tree_grid
  )
tree_res
