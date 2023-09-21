# Load libs
pacman::p_load(
  tidyverse, # data wrangling
  tidymodels # modeling
)

# Read data
train <- read.csv("Data/train.csv")
test <- read.csv("Data/test.csv")
submission_id <- read.csv("Data/test.csv") %>% select(id)

# Train data wrangling
train <- train %>% mutate_if(is.character, ~na_if(., ""))
train <- train %>% mutate_if(is.numeric, ~na_if(., 0))

train <- train %>% 
  select(-c(
    circle_id, # same in all rows,
    loc_og_t2o_mou, # all blank or 0
    std_og_t2o_mou, # all blank or 0
    loc_ic_t2o_mou, # all blank or 0
    std_og_t2c_mou_6, # all blank or 0
    std_og_t2c_mou_7, # all blank or 0
    std_og_t2c_mou_8, # all blank or 0
    std_ic_t2o_mou_6, # all blank or 0
    std_ic_t2o_mou_7, # all blank or 0
    std_ic_t2o_mou_8, # all blank or 0
    date_of_last_rech_6, # nothing usefull
    date_of_last_rech_7, # nothing usefull
    date_of_last_rech_8, # nothing usefull
    last_day_rch_amt_6, # nothing usefull
    last_day_rch_amt_7, # nothing usefull
    last_day_rch_amt_8, # nothing usefull
    date_of_last_rech_data_6, # nothing usefull
    date_of_last_rech_data_7, # nothing usefull
    date_of_last_rech_data_8, # nothing usefull
    #last_date_of_month_6, # nothing usefull
    #last_date_of_month_7, # nothing usefull
    #last_date_of_month_8, # nothing usefull
    total_og_mou_6, # totals will be calculated later on
    total_og_mou_7, # totals will be calculated later on
    total_og_mou_8, # totals will be calculated later on
    total_ic_mou_6, # totals will be calculated later on
    total_ic_mou_7, # totals will be calculated later on
    total_ic_mou_8, # totals will be calculated later on
    total_rech_num_6, # totals will be calculated later on
    total_rech_num_7, # totals will be calculated later on
    total_rech_num_8, # totals will be calculated later on
    total_rech_amt_6, # totals will be calculated later on
    total_rech_amt_7, # totals will be calculated later on
    total_rech_amt_8, # totals will be calculated later on
    total_rech_data_6, # totals will be calculated later on
    total_rech_data_7, # totals will be calculated later on
    total_rech_data_8 # totals will be calculated later on
  ))

train <- train %>% 
  rename(
    vbc_3g_8 = aug_vbc_3g,
    vbc_3g_7 = jul_vbc_3g,
    vbc_3g_6 = jun_vbc_3g,
    #circle = circle_id,
    churn = churn_probability
  ) %>% 
  pivot_longer(
    cols = -c(id,
              churn,
              aon)) %>% 
  mutate(month_id = str_extract(name, ".$"),
         month_id = case_when(
           month_id == "6" ~ "Jun",
           month_id == "7" ~ "Jul",
           month_id == "8" ~ "August"
         ),
         name = str_replace(name, ".{2}$", "")) %>% 
  pivot_wider(
    names_from = month_id,
    values_from = value
  ) %>% 
  mutate(
    mean = round(rowMeans(select(., starts_with("Jun"), starts_with("Jul"), starts_with("August")), na.rm = TRUE),0),  # Create the 'mean' column
    total = round(rowSums(select(., starts_with("Jun"), starts_with("Jul"), starts_with("August")), na.rm = TRUE),0)  # Create the 'total' column
  ) %>% 
  select(-c(
    August,
    Jul,
    Jun
  )) %>% 
  pivot_wider(
    names_from = name,
    values_from = c(mean, 
                    total)
  ) %>%
  select(-total_fb_user) %>% 
  rename(fb_user = mean_fb_user) # is user if was at least in one month

# Replace NAs
train[is.na(train)] <- 0

# Final dataset
train_cleaned <- train %>% 
  mutate(
    #circle = as.factor(circle),
    churn = as.factor(churn),
    fb_user = as.factor(fb_user)
    ) %>% 
  select(-id)


# Create a tidymodels recipe for data preprocessing
model_recipe <- recipe(churn ~ ., data = train_cleaned) %>%
  step_date(c(    date_of_last_rech_6, 
                  date_of_last_rech_7, 
                  date_of_last_rech_8, 
                  last_day_rch_amt_6, 
                  last_day_rch_amt_7, 
                  last_day_rch_amt_8, 
                  date_of_last_rech_data_6, 
                  date_of_last_rech_data_7, 
                  date_of_last_rech_data_8 ), features = c("month")) %>% 
  step_scale(all_numeric_predictors()) %>%
  step_center(all_numeric_predictors())# %>% 
  #step_pca(all_numeric_predictors())


# Split the data into training and testing sets
set.seed(123)  # For reproducibility
data_split <- initial_split(train_cleaned, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)


# Lets see how do PCAs look like, for reference check https://juliasilge.com/blog/cocktail-recipes-umap/

pca_prep <- prep(model_recipe)

tidied_pca <- tidy(pca_prep, 2)

tidied_pca %>%
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

# Fit the recipe to the training data
data_preprocessor <- prep(model_recipe, training = train_data)

# Transform both training and testing data
train_data_preprocessed <- bake(data_preprocessor, new_data = train_data)
test_data_preprocessed <- bake(data_preprocessor, new_data = test_data)

# Build your Random Forest model using tidymodels
model_spec <- rand_forest() %>%
  set_engine("randomForest") %>%
  set_mode("classification")

# Fit the model to the data
final_model <- model_spec %>%
  fit(churn ~ ., data = train_data_preprocessed)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf <- workflow() %>%
  add_recipe(model_recipe) %>%
  add_model(tune_spec)

trees_folds <- vfold_cv(train_data)

tune_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = 20
)

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# Make predictions on the test set
predictions <- final_model %>%
  predict(new_data = test_data_preprocessed) %>%
  pull(.pred_class)

# Evaluate your model's performance (e.g., accuracy)
accuracy <- mean(predictions == test_data_preprocessed$churn)
print(paste("Accuracy:", accuracy))


#####################################################################
#############     TEST DATASET
#####################################################################

# same steps as with train dataset just without churn

test <- test %>% mutate_if(is.character, ~na_if(., ""))

test <- test %>% 
  select(-c(
    select(-c(
      circle_id, # same in all rows,
      loc_og_t2o_mou, # all blank or 0
      std_og_t2o_mou, # all blank or 0
      loc_ic_t2o_mou, # all blank or 0
      std_og_t2c_mou_6, # all blank or 0
      std_og_t2c_mou_7, # all blank or 0
      std_og_t2c_mou_8, # all blank or 0
      std_ic_t2o_mou_6, # all blank or 0
      std_ic_t2o_mou_7, # all blank or 0
      std_ic_t2o_mou_8, # all blank or 0
      date_of_last_rech_6, # nothing usefull
      date_of_last_rech_7, # nothing usefull
      date_of_last_rech_8, # nothing usefull
      last_day_rch_amt_6, # nothing usefull
      last_day_rch_amt_7, # nothing usefull
      last_day_rch_amt_8, # nothing usefull
      date_of_last_rech_data_6, # nothing usefull
      date_of_last_rech_data_7, # nothing usefull
      date_of_last_rech_data_8, # nothing usefull
      #last_date_of_month_6, # nothing usefull
      #last_date_of_month_7, # nothing usefull
      #last_date_of_month_8, # nothing usefull
      total_og_mou_6, # totals will be calculated later on
      total_og_mou_7, # totals will be calculated later on
      total_og_mou_8, # totals will be calculated later on
      total_ic_mou_6, # totals will be calculated later on
      total_ic_mou_7, # totals will be calculated later on
      total_ic_mou_8, # totals will be calculated later on
      total_rech_num_6, # totals will be calculated later on
      total_rech_num_7, # totals will be calculated later on
      total_rech_num_8, # totals will be calculated later on
      total_rech_amt_6, # totals will be calculated later on
      total_rech_amt_7, # totals will be calculated later on
      total_rech_amt_8, # totals will be calculated later on
      total_rech_data_6, # totals will be calculated later on
      total_rech_data_7, # totals will be calculated later on
      total_rech_data_8 # totals will be calculated later on
    ))
  ))

test <- test %>% 
  rename(
    vbc_3g_8 = aug_vbc_3g,
    vbc_3g_7 = jul_vbc_3g,
    vbc_3g_6 = jun_vbc_3g,
    #circle = circle_id,
  ) %>% 
  pivot_longer(
    cols = -c(id,
              aon)) %>% 
  mutate(month_id = str_extract(name, ".$"),
         month_id = case_when(
           month_id == "6" ~ "Jun",
           month_id == "7" ~ "Jul",
           month_id == "8" ~ "August"
         ),
         name = str_replace(name, ".{2}$", "")) %>% 
  pivot_wider(
    names_from = month_id,
    values_from = value
  ) %>% 
  mutate(
    mean = round(rowMeans(select(., starts_with("Jun"), starts_with("Jul"), starts_with("August")), na.rm = TRUE),0),  # Create the 'mean' column
    total = round(rowSums(select(., starts_with("Jun"), starts_with("Jul"), starts_with("August")), na.rm = TRUE),0)  # Create the 'total' column
  ) %>% 
  select(-c(
    August,
    Jul,
    Jun
  )) %>% 
  pivot_wider(
    names_from = name,
    values_from = c(mean, 
                    total)
  ) %>%
  select(-total_fb_user) %>% 
  rename(fb_user = mean_fb_user) # is user if was at least in one month

test[is.na(test)] <- 0

test <- test %>% 
  mutate(
    fb_user = as.factor(fb_user)
  ) %>% 
  select(-id)

# Predicting

test_data_preprocessed_final <- bake(data_preprocessor, new_data = test)

test_predictions <- final_model %>%
  predict(new_data = test_data_preprocessed_final)

submission <- cbind(submission_id, test_predictions)
colnames(submission) <- c("id", "churn_probability")

write.csv(submission, "submission2.csv", row.names = FALSE)
