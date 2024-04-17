## Ok let's apply the methodology, but to an underwriting example 
library(CASdatasets)
library(tidyverse)

################### Data split ########################
data("norauto")

set.seed(999)
small_data <- rbind(norauto %>% 
          filter(Male == 0),
        norauto %>% 
          sample_n(1e4 - sum(norauto$Male == 0))) %>% 
  mutate(id = 1:10000)

dat <- small_data %>% 
  dplyr::select(-NbClaim) 

make_splitted_dfs <- function (formula, data, banned_variables, exposure = NULL, response = NULL) 
{
  dummy_mod <- glm(formula = formula, data = data, family = "poisson")
  xnd <- model.matrix(dummy_mod)
  d_id <- sapply(banned_variables, function(s) {
    which(grepl(s, colnames(xnd)))
  })
  X <- xnd[, -1 * c(1, d_id)]
  D <- xnd[, d_id]
  to_output_x <- as.data.frame(X)
  names(to_output_x) <- colnames(xnd)[-1 * c(1, d_id)]
  to_output_d <- as.data.frame(D)
  names(to_output_d) <- colnames(xnd)[d_id]
  if (!is.null(exposure)) {
    to_output_x[[exposure]] <- data[[exposure]]
  }
  if (!is.null(response)) {
    to_output_x[[response]] <- data[[response]]
  }
  list(X = to_output_x, D = to_output_d, response = response, 
       exposure = exposure)
}

temp_dat <- make_splitted_dfs(formula = ClaimAmount ~ Young + DistLimit + Male +
                                          GeoRegion + Expo,
                                        data = dat,
                                        banned_variables = 'Male',
                                        response = 'ClaimAmount',
                              exposure = 'Expo')

set.seed(1234)
train_id <- sample(1:nrow(dat),
                   floor(0.75 * nrow(dat)),
                   replace = FALSE)

split_the_splitted_df <- function (split_df, split_id) 
{
  list(X = split_df$X[split_id,], D = split_df$D[split_id,], 
       response = split_df$response, exposure = split_df$exposure)
}

train_dat <- temp_dat # split_the_splitted_df(temp_dat, train_id)
test_dat <- temp_dat #split_the_splitted_df(temp_dat, -train_id)
proj <- actufair::create_projector(train_dat)

train <- dat# [train_id,]
test <- dat # [-1 * train_id, ]


use_projector <- function(new_split_df, projector) 
{
  to_remove_from_proj <- c(new_split_df$response, new_split_df$exposure)
  newX <- new_split_df$X %>% select(-all_of(to_remove_from_proj))
  newD <- new_split_df$D
  if(is_null(dim(newD))) {
    newD <- as.data.frame(new_split_df$D)
    names(newD) <- (projector[[1]]$coefficients %>% names)[2]
  }
  X_star2 <- as.data.frame(sapply(1:length(projector), function(b) {
    mod <- projector[[b]]
    var <- newX[[b]]
    var - predict(mod, newdata = newD)
  }))
  names(X_star2) <- names(newX)
  if(!is.null(new_split_df$response)) {
    X_star2[[new_split_df$response]] <- new_split_df$X %>% 
      pull(new_split_df$response)
  }
  if (!is.null(new_split_df$exposure)) {
    X_star2[[new_split_df$exposure]] <- new_split_df$X %>% 
      pull(new_split_df$exposure)
  }
  return(X_star2)
}

train_num <- use_projector(train_dat, proj) %>% 
  mutate(id = train$id)
test_num <- use_projector(test_dat, proj) %>% 
  mutate(id = test$id)

train <- train_dat$X %>% 
  mutate(id = train$id,
         Male = train$Male)

test <- test_dat$X %>% 
  mutate(id = test$id,
         Male = test$Male)

names(train) <- str_replace_all(names(train), ' ', '_')
names(train) <- str_replace_all(names(train), "[+]", '_p')
names(train) <- str_replace_all(names(train), '-', '_m')

names(test) <- str_replace_all(names(test), ' ', '_')
names(test) <- str_replace_all(names(test), '[+]', '_p')
names(test) <- str_replace_all(names(test), '-', '_m')

names(train_num) <- str_replace_all(names(train_num), ' ', '_')
names(train_num) <- str_replace_all(names(train_num), '[+]', '_p')
names(train_num) <- str_replace_all(names(train_num), '-', '_m')

names(test_num) <- str_replace_all(names(test_num), ' ', '_')
names(test_num) <- str_replace_all(names(test_num), '[+]', '_p')
names(test_num) <- str_replace_all(names(test_num), '-', '_m')

write_csv(train, 'train.csv')
write_csv(test, 'test.csv')
write_csv(train_num, 'train_num.csv')
write_csv(test_num, 'test_num.csv')


