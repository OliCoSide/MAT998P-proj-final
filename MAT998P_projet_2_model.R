################### READ THE DATA ######################
library(tidyverse)
train <- read_csv('train.csv')
test <- read_csv('test.csv')
train_num <- read_csv('train_num.csv')
test_num <- read_csv('test_num.csv')

################### RISK ESTIMATE ######################

library(lightgbm)

# tg <- glm(Young ~ Male, family = gaussian,
#     data = train)
# 
# ((train$Young - tg$fitted.values) %>% unname)[1:5]
# train$Male[1:5]
# train_num$Young[1:5]

### Side of estimate ----

# p <- tweedie::tweedie.profile(ClaimAmount~. + offset(log(train$Expo)), 
#                               p.vec = seq(1.40, 1.60, by = 0.025),
#                               data = train %>% 
#                                 dplyr::select(-id),
#                               method = 'series',
#                               do.plot = TRUE)
# 
# full_mod <- fishMod::tglm(ClaimAmount~. + offset(log(train$Expo)),
#               data = train %>% 
#                 dplyr::select(-id),
#               p = p$p.max)
# 
# (mono_constraint <- full_mod$coef %>% tail(-1) %>% sign())

## Best estimate --------

t_b <- lightgbm::lgb.convert_with_rules(train  %>% 
                                      dplyr::select(-highcost, -id))
train_lgb_b <- lgb.Dataset(data = t_b$data %>% as.matrix,
                           categorical_feature = 1:4,
                         label = train$highcost)

# 
# vec_mono_constr <- sapply(train_lgb_b$get_colnames(), function(n){
#   cleaned_names <- str_replace_all(names(mono_constraint), "`", "")
#   to_ret <- mono_constraint[which(cleaned_names == n)] %>% unname
#   return(to_ret)
# }) %>% unlist()

parameters <- list(max_depth = 6, # interaction depth + 1
                   min_data_in_leaf = floor(0.75*0.01*nrow(train)),
                   verbose = -1,
                   seed = 42,
                   learning_rate = 0.005,
                   bagging_fraction = 0.75,
                   objective = "binary",
                   # monotone_constraints = vec_mono_constr %>% unname,
                   num_thread = parallel::detectCores(logical=FALSE))

lgb_cv_b <- lgb.cv(train_lgb_b,
                 nrounds = 300,
                 params = parameters,
                 nfold = 5)

lgb_b <- lightgbm(train_lgb_b,
                  nrounds = lgb_cv_b$best_iter,
                  params = parameters)

pred_b <- lgb_b$predict(t_b$data %>% as.matrix)
# 
# factor_b <- weighted.mean(train$ClaimAmount, w = train$Expo)/
#   weighted.mean(lgb_b$predict(data = train %>%
#                                                          dplyr::select(-Expo, -ClaimAmount, -id) %>%
#                                                          as.matrix), w = test$Expo)

preds <- data.frame('id' = test$id,
                    'best' = pred_b)


## Unaware ------

t_u <- lightgbm::lgb.convert_with_rules(train  %>% 
                                          dplyr::select(-highcost, -Male, -id))
train_lgb_u <- lgb.Dataset(data = t_u$data %>% as.matrix,
                           categorical_feature = 1:3,
                           label = train$highcost)


lgb_cv_u<- lgb.cv(train_lgb_u,
                  nrounds = 300,
                  params = parameters,
                  nfold = 5)

lgb_u <- lightgbm(train_lgb_u,
                  nrounds = lgb_cv_u$best_iter,
                  params = parameters)

pred_u <- lgb_u$predict(t_u$data %>% as.matrix)
preds$un <- pred_u 

## Aware ------

library(fairadapt)

data_for_fairadapt <- train %>% 
  dplyr::select(-id, -highcost) %>% 
  mutate(resp = preds$best)

adj_mat <- matrix(data = diag(ncol(data_for_fairadapt)) - diag(ncol(data_for_fairadapt)),
                  byrow = FALSE,
                  nrow = ncol(data_for_fairadapt),
                  dimnames = list(colnames(data_for_fairadapt),
                                  colnames(data_for_fairadapt)))

adj_mat[1, ] <- 1
adj_mat[1, 1] <- 0
adj_mat[, ncol(data_for_fairadapt)] <- 1
adj_mat[ncol(data_for_fairadapt), ncol(data_for_fairadapt)] <- 0
plot(fairadapt::graphModel(adj.mat = adj_mat,
                           res.vars = c("Young", "DistLimit",
                                        "GeoRegion")))

fair_a <- fairadapt::fairadapt(formula = resp ~.,
                     adj.mat = adj_mat,
                     prot.attr = 'Male',
                     res.vars = colnames(adj_mat)[2:4],
                     train.data = data_for_fairadapt)


comp_df <- data.frame('eps_y_a' = fair_a$adapt.train$resp,
           'y' = fair_a$train$resp,
           'Male' = train$Male) 

ggplot(comp_df, aes(x = eps_y_a, y = y)) + geom_point()

## lgb 

train_lgb_a <- lgb.Dataset(data = t_u$data %>% as.matrix,
                           categorical_feature = 1:3,
                           label = comp_df$eps_y_a)

parameters_a <- list(max_depth = 6, # interaction depth + 1
                   min_data_in_leaf = floor(0.75*0.01*nrow(train)),
                   verbose = -1,
                   seed = 42,
                   learning_rate = 0.005,
                   bagging_fraction = 0.75,
                   objective = "cross_entropy",
                   # monotone_constraints = vec_mono_constr %>% unname,
                   num_thread = parallel::detectCores(logical=FALSE))

lgb_a <- lightgbm(train_lgb_a,
                  nrounds = lgb_cv_u$best_iter,
                  params = parameters_a)

pred_a <- lgb_a$predict(data = t_u$data %>% as.matrix)

preds$aw <- pred_a  

# ## Hyperaware ------
# 
# plot(fairadapt::graphModel(adj.mat = adj_mat))
# 
# fair_h <- fairadapt::fairadapt(formula = resp ~.,
#                                adj.mat = adj_mat,
#                                prot.attr = 'Male',
#                                res.vars = NULL,
#                                train.data = data_for_fairadapt)
# 
# factor_h <- sum(fair_h$train$resp)/sum(fair_h$adapt.train$resp)
# 
# comp_df$eps_y_h <- fair_h$adapt.train$resp * factor_h
# 
# comp_df %>% 
#   reshape2::melt(id.vars = 'Male') %>% 
#   ggplot(aes(x = value, fill = factor(Male), color = factor(Male))) + 
#   geom_density(alpha = 0.2, bw = 150, size = 1) + 
#   facet_grid(~variable) + 
#   scale_color_brewer(palette = 'Dark2') + 
#   scale_fill_brewer(palette = 'Dark2') +
#   theme_bw() # good stuff
# 
# comp_df %>% 
#   ggplot(aes(x = eps_y_h, y = y, col = factor(Male))) + 
#   geom_point(alpha = 0.7, size= 2) + 
#   scale_color_brewer(palette = 'Dark2')
# 
# summary(comp_df) 
# 
# ## lgb 
# train_lgb_h <- lgb.Dataset(data = train %>% 
#                              dplyr::select(-Expo, -ClaimAmount, -Male, -id) %>% 
#                              as.matrix,
#                            init_score = log(train$Expo),
#                            label = comp_df$eps_y_h)
# 
# lgb_h <- lightgbm(train_lgb_a,
#                   nrounds = lgb_cv_u$best_iter,
#                   params = parameters_a)
# 
# pred_h <- lgb_h$predict(data = test %>% 
#                           dplyr::select(-Expo, -ClaimAmount, -Male, -id) %>% 
#                           as.matrix)
# 
# factor_h <- mean(train$ClaimAmount)/mean(lgb_h$predict(data = train %>%
#                                                          dplyr::select(-Expo, -ClaimAmount, -Male, -id) %>%
#                                                          as.matrix))
# 
# preds$hyp <- pred_h
# preds$hypb <- pred_h * factor_h
# 
# ## Corrective -------
# 
# # p <- tweedie::tweedie.profile(ClaimAmount~., p.vec = seq(1.40, 1.60, by = 0.05),
# #                               data = train,
# #                               method = 'series',
# #                               do.plot = TRUE)
# # 
# # model_corr <- glm(ClaimAmount~. + offset(log(Expo)), data = train_num,
# #     family = statmod::tweedie(var.power = p$p.max))
# # 
# # pred_c <- predict(model_corr, newdata = test_num, type = 'response')
# # 
# # factor_c <- factor_h <- mean(train$ClaimAmount)/mean(predict(model_corr,
# #                                                              newdata = test_num,
# #                                                              type = 'response'))
# # preds$corr <- pred_c * factor_c
# 
# 
# ## lgb 
# train_lgb_c <- lgb.Dataset(data = train_num %>% 
#                              dplyr::select(-Expo, -ClaimAmount, -id) %>% 
#                              as.matrix,
#                            init_score = log(train$Expo),
#                            label = train_num$ClaimAmount)
# 
# lgb_cv_c<- lgb.cv(train_lgb_c,
#                   nrounds = 1000,
#                   params = parameters2,
#                   nfold = 5)
# 
# lgb_c <- lightgbm(train_lgb_c,
#                   nrounds = lgb_cv_c$best_iter,
#                   params = parameters2)
# 
# pred_c <- lgb_c$predict(data = test_num %>% 
#                           dplyr::select(-Expo, -ClaimAmount, -id) %>% 
#                           as.matrix)
# 
# factor_c <- weighted.mean(train$ClaimAmount, w = train$Expo)/
#   weighted.mean(lgb_c$predict(data = train_num %>%
#                               dplyr::select(-Expo, -ClaimAmount, -id) %>%
#                               as.matrix))
# 
# preds$corr <- pred_c
# preds$corrb <- pred_c * factor_c
# 
# ### THE PREDS
# preds %>% 
#   dplyr::select('id','best', 'bestb', 'un', 'unb', 'aw', 'awb') %>% 
#   mutate(Male = test$Male) %>% 
#   reshape2::melt(id.vars = c('id', 'Male')) %>% 
#   ggplot(aes(x = value, fill = factor(Male), color = factor(Male))) + 
#   geom_density(size =1, alpha = 0.5, bw = 500) + 
#   facet_grid(~variable) + 
#   scale_color_brewer(palette = 'Dark2') + 
#   scale_fill_brewer(palette = 'Dark2')

write_csv(preds, 'preds_lgb.csv')
