################### READ THE DATA ######################

library(tidyverse)

test <- read_csv('test.csv')
preds <- read_csv('preds_lgb.csv') 
train <- read_csv('train.csv')

fdat <- test %>% 
  left_join(preds, by = 'id') %>% 
  mutate('mean' = mean(highcost))

################### analysis ######################


library(ROCR)


nms <- c('best', 'un', 'aw', 'mean')

sapply(nms, function(name){
  
  performance(prediction(fdat[[name]], test$highcost),"auc")@y.values %>% 
    as.numeric
})


# train$ClaimAmount %>% quantile(0.95)

# Sx_twee <- function(preds, tresh = 5000){
#   sapply(preds, function(mu){ 1 - tweedie::ptweedie(tresh,
#                                                     mu = mu,
#                                                     phi = p$phi.max,
#                                                     power = p$p.max)})
# }
# 
# fdat$best_p <- Sx_twee(preds$best/fdat$Expo)
# fdat$bestb_p <- Sx_twee(preds$bestb/fdat$Expo)
# fdat$un_p <- Sx_twee(preds$un/fdat$Expo)
# fdat$unb_p <- Sx_twee(preds$unb/fdat$Expo)
# fdat$aw_p <- Sx_twee(preds$aw/fdat$Expo)
# fdat$awb_p <- Sx_twee(preds$awb/fdat$Expo)

# 
# fdat$bestl <- glm(I(ClaimAmount>5000) ~. -id,
#                   data = train,
#                   family = binomial)$fitted
# 
# fdat$unl <- glm(I(ClaimAmount>5000) ~. -Male-id,
#                   data = train,
#                   family = binomial)$fitted

# preds$hyp_p <- Sx_twee(preds$hyp)
# preds$corr_p <- Sx_twee(preds$corr)

lr_table_aw <- actuarialmetrics::get_lr_table(fdat,
                               ref_name = 'aw',
                               comp_name = 'un',
                               loss_name = 'highcost',
                               n_cuts = 5)

dbl_aw <- actuarialmetrics::dbl_lift_chart(lr_table_aw)

lr_table_best <- actuarialmetrics::get_lr_table(fdat,
                                           ref_name = 'un',
                                           comp_name = 'best',
                                           loss_name = 'highcost',
                                           n_cuts = 5)

dbl_best <- actuarialmetrics::dbl_lift_chart(lr_table_best)

ggpubr::ggarrange(dbl_best, dbl_aw)
# actuarialmetrics::lrlift_graph(lr_table)

ggsave(
  "figures/calibration_dbl.pdf",
  plot = ggpubr::ggarrange(dbl_best + theme(legend.position = 'bottom'),
                           dbl_aw + theme(legend.position = 'bottom'), 
                           nrow = 1,
                           common.legend = FALSE),
  height = 5.5,
  width = 11.5
)

################# Fairness ##################

# fdat$highcost <- ifelse(fdat$ClaimAmount > 5000, 1, 0)

################################### ANALYZE ####################################

# Check calibration
(p_calibration_best <- fdat %>%
  mutate(Genre = ifelse(Male == 1, "Homme", "Femme")) %>%
  ggplot(aes(x =best, y = highcost + 0.0, color = Genre)) +
  geom_smooth(formula = y ~ s(x, bs = "cs"), method = "gam",
              fullrange = TRUE, se=FALSE) +
  scale_color_brewer(palette = "Set2") +
   scale_x_continuous(latex2exp::TeX('Probabilité estimée de coûts élevés $\\hat{p}$'),
                      label = scales::percent_format()) +
   scale_y_continuous("Proportion de coûts élevés", label = scales::percent_format()) +
  coord_fixed(xlim = c(0.02,0.07), ylim = c(0.02,0.07), expand = FALSE) + #, ylim = c(0,1), expand = FALSE) +
  theme_bw() +
  labs(color = NULL) +
  theme(
    legend.position = ''
  )+ 
   geom_abline(intercept = 0,
               slope = 1,
               lty = 'dashed',
               alpha = 0.3))

# Check calibration
(p_calibration_un <- fdat %>%
    mutate(Genre = ifelse(Male == 1, "Homme", "Femme")) %>%
    ggplot(aes(x =un, y = highcost + 0.0, color = Genre)) +
    geom_smooth(formula = y ~ s(x, bs = "cs"), method = "gam",
                fullrange = TRUE, se=FALSE) +
    scale_color_brewer(palette = "Set2") +
    scale_x_continuous(latex2exp::TeX('Probabilité estimée de coûts élevés $\\hat{p}$'),
                       label = scales::percent_format()) +
    scale_y_continuous("Proportion de coûts élevés", label = scales::percent_format()) +
    coord_fixed(xlim = c(0.02,0.07), ylim = c(0.02,0.07), expand = FALSE) + #, ylim = c(0,1), expand = FALSE) +
    theme_bw() +
    labs(color = NULL) +
    theme(
      legend.position = c(0.01, 1.04),
      legend.justification = c(0,1),
      legend.background = element_rect(fill = alpha("white", 0))
    )+ 
    geom_abline(intercept = 0,
                slope = 1,
                lty = 'dashed',
                alpha = 0.3))

# Check calibration
(p_calibration_aw <- fdat %>%
    mutate(Genre = ifelse(Male == 1, "Homme", "Femme")) %>%
    ggplot(aes(x =aw, y = highcost + 0.0, color = Genre)) +
    geom_smooth(formula = y ~ s(x, bs = "cs"), method = "gam",
                fullrange = TRUE, se=FALSE) +
    scale_color_brewer(palette = "Set2") +
    scale_x_continuous(latex2exp::TeX('Probabilité estimée de coûts élevés $\\hat{p}$'), label = scales::percent_format()) +
    scale_y_continuous("Proportion de coûts élevés", label = scales::percent_format()) +
    coord_fixed(xlim = c(0.02,0.07), ylim = c(0.02,0.07), expand = FALSE) + #, ylim = c(0,1), expand = FALSE) +
    theme_bw() +
    labs(color = NULL) +
    theme(
      legend.position = '')+ 
    geom_abline(intercept = 0,
                slope = 1,
                lty = 'dashed',
                alpha = 0.3))


library(ggpubr)
ggsave(
  "figures/calibration.pdf",
  plot = ggpubr::ggarrange(p_calibration_best + ggtitle('Best'),
                           p_calibration_un + ggtitle('Un'), 
                           p_calibration_aw + ggtitle('Aw'), 
                           nrow = 1,
                           common.legend = TRUE),
  height = 3.5,
  width = 9.5
)


Pareto_full <- function(data, pred = 'un', perc = 0.05, real = TRUE,
                        manual_vline = NULL,  manual_ylim = NULL,
                        n_y = c(-5.5, 3.5)){
  ## renaming
  data[['the_pred']] <- data[[pred]]
  
  # Generate Pareto frontier for all patients
  # 2% of people enrolled in program
  n_prog <- as.integer(nrow(data) * perc)
  # Threshold is highcost of marginal person
  # id_M <- sort(data$the_pred[data$Male == 1],
  #           index.return = TRUE,
  #           decreasing = TRUE)$ix[1:n_prog]
  # id_F <- sort(data$the_pred[data$Male == 0],
  #              index.return = TRUE,
  #              decreasing = TRUE)$ix[1:n_prog]
  
  
  
  if(real){
    id_M <- sort(data$the_pred[data$Male == 1],
              index.return = TRUE,
              decreasing = TRUE)$ix[1:n_prog]
    id_F <- rev(sort(data$the_pred[data$Male == 0],
                 index.return = TRUE,
                 decreasing = TRUE)$ix[1:n_prog])
    t_Male <- data$highcost[id_M]
    t_Female <- data$highcost[id_F]
    
    data[['the_pred']] <- data$highcost
    
  } else {
    t_Male <- sort(data$the_pred[data$Male == 1],
                   decreasing = TRUE)[1:n_prog]
    t_Female <- rev(sort(data$the_pred[data$Male == 0],
                         decreasing = TRUE)[1:n_prog])
  }
  # Number of expected high-cost individuals from each group
  n_Male <- c(0, cumsum(t_Male)) 
  n_Female <- rev(c(0, cumsum(rev(t_Female))))
  
  # Proportion of positive decisions
  p_Male <- with(data, seq(0, n_prog) / sum(Male == 1))
  p_Female <- with(data, seq(n_prog, 0) / sum(Male == 0))
  
  
  # False negative rate. NOTE: For smoothing, we use the best model predictions.
  fnr_Male <- 1 - n_Male / with(data, sum(the_pred[Male == 1]))
  fnr_Female <- 1 - n_Female / with(data, sum(the_pred[Male == 0]))
  # False positve rate. NOTE: For smoothing, we use model predictions.
  fpr_Male <- with(
    data,
    (seq(0, n_prog) - n_Male)
    / (sum(Male == 1) - sum(the_pred[Male == 1]))
  )
  fpr_Female <- with(
    data,
    (seq(n_prog, 0) - n_Female)
    / (sum(Male ==0) - sum(the_pred[Male == 0]))
  )
  
  # Calculate the positions of the maximum objective, demographic parity,
  # equalized false positive rate, and equalized false negative rate points.
  # NOTE: Because of one-indexing, these are off by 1.
  n_max <- which((n_Male + n_Female) == max(n_Male + n_Female))
  
  # if(length(n_max) > 1) stop('n_max not unique, please reduce perc')
  n_dp <- which(abs(p_Male - p_Female) == min(abs(p_Male - p_Female)))
  n_fpr <- which(abs(fpr_Male - fpr_Female) == min(abs(fpr_Male - fpr_Female)))
  n_fnr <- which(abs(fnr_Male - fnr_Female) == min(abs(fnr_Male - fnr_Female)))
  
  # Convert them into data frames for easy plotting.
  # NOTE: Because arrays are one-indexed, these are off by one.
  df_dp <- tibble(
    obj = (n_Male + n_Female)[n_dp],
    n_Male = n_dp - 1,
    label = "DP,\nFNR"
  )
  df_fpr <- tibble(
    obj = (n_Male + n_Female)[n_fpr],
    n_Male = n_fpr - 1,
    label = "FPR"
  )
  # NOTE: FNR is almost on top of DP, so we don't plot it.
  df_fnr <- tibble(
    obj = (n_Male + n_Female)[n_fnr],
    n_Male = n_fnr - 1,
    label = "FNR"
  )
  df_right <- tibble(
    # Number of high cost patients admitted
    obj = n_Male + n_Female,
    # Number of Male patients admitted
    n_Male = seq(p_Male) - 1
  )
  
  if(!is.null(manual_vline)){
    the_vline = geom_vline(xintercept = manual_vline, linetype = "dashed", alpha = 0.5) 
  } else {
    the_vline = geom_vline(xintercept = min(n_max) - 1, linetype = "dashed", alpha = 0.5) 
  }
  
  if(!is.null(manual_ylim)){
    the_ylims = manual_ylim
  } else {
    the_ylims = c(floor(min(n_Male + n_Female)) - 2,
                  ceiling(max(n_Male + n_Female)) + 2)
  }
  
  p_pareto_full <- ggplot(mapping = aes(x = n_Male, y = obj)) +
    geom_line(data = df_right, linetype = "solid") +
    # NOTE: Because arrays are one indexed, n_max is off by 1.
    the_vline +
    scale_y_continuous(breaks= scales::pretty_breaks(),
                       limits = the_ylims)+
    xlim(0, nrow(data) * perc) + 
    geom_point(data = df_dp, color = RColorBrewer::brewer.pal(3, "Dark2")[[1]],
               size = 3, alpha = 0.8) +
    geom_point(data = df_fpr, color = RColorBrewer::brewer.pal(3, "Dark2")[[2]],
               size = 3, alpha = 0.8) +
    geom_text(
      aes(label = label),
      data = df_dp,
      size = 3,
      color = RColorBrewer::brewer.pal(3, "Dark2")[[1]],
      nudge_x = 100 * perc,
      nudge_y = n_y[1]
    ) +
    geom_text(
      aes(label = label),
      data = df_fpr,
      size = 3,
      color = RColorBrewer::brewer.pal(3, "Dark2")[[2]],
      nudge_x = 100 * perc,
      nudge_y = n_y[2]
    ) +
    theme(aspect.ratio=1) +
    scale_color_brewer(palette = "Set1") +
    labs(
      x = "Nb. Total d'hommes assurés",
      y = "Nb. coûts élevés"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      legend.text = element_text(size = 9)
    )
  
  
  utility <- data.frame(esp = (n_Male + n_Female),
    diff_par = abs(p_Male - p_Female))
  
  to_return = list('graph' = p_pareto_full,
                   'util' = utility)
  return(to_return)
}

# data.frame(#n_Male,
#            'n' = seq_along(n_Male), 
#            fnr_Male, fnr_Female) %>%
#   reshape2::melt(id.vars = 'n') %>% 
#   ggplot(aes(x = n, y = value, color = variable, fill = variable)) + 
#   geom_point()

Pareto_full(fdat, pred = 'best', real = TRUE,
            manual_ylim = c(25, 50))
Pareto_full(fdat, pred = 'un', real = TRUE,
            manual_ylim = c(25, 50))
Pareto_full(fdat, pred = 'aw', real = TRUE,
            manual_ylim = c(25, 50))

g1 <- Pareto_full(fdat, pred = 'best', real = TRUE,
                  manual_ylim = c(25, 50)) 

g2 <- Pareto_full(fdat, pred = 'un', real = TRUE,
                  manual_ylim = c(25, 50)) 
  
g3 <- Pareto_full(fdat, pred = 'aw', real = TRUE,
                  manual_ylim = c(25, 50)) 

df <- rbind(g1$util, g2$util, g3$util)
df$n_male = seq_along(g1$util$esp)
df$model = c(rep('best', nrow(g1$util)),
             rep('un', nrow(g1$util)),
             rep('aw', nrow(g1$util)))

df$id = 1:nrow(df)
grid = expand.grid('id' = df$id, lam = seq(0, 750, length.out = 100))
df_b <- left_join(df, grid, by = 'id')
df_b$utility <- pmax(df_b$esp - df_b$lam * df_b$diff_par, 0)

library(gganimate)

a <- df_b %>%
  ggplot(aes(x = n_male, y = utility, fill = model, color = model)) + 
  geom_line(size = 2, alpha = 0.8) + 
  theme_bw() + 
  scale_color_brewer(palette = 'Dark2', name = 'Modèle') + 
  transition_time(lam) +
    labs(x = "Nb. d'hommes, sur 500", 
         y = latex2exp::TeX('$u(d)$'),
         title = "Pénalité: {frame_time}") +
  transition_time(lam) 

  
animate(a, renderer = magick_renderer(loop = TRUE),
        nframes = 100)

pareto_stuff <- ggpubr::ggarrange(g1 + ggtitle('Best') +
                                    rremove("xlab"),
                  g2$graph + ggtitle('Un')+
                    rremove("xlab"), 
                  g3 + ggtitle('Aw'), 
                  nrow = 3,
                  common.legend = TRUE) 

p 

ggsave(
  "figures/pareto_stuff.pdf",
  plot = pareto_stuff,
  height = 5.5,
  width = 3.5
)


ggpubr::ggarrange(Pareto_full(fdat  %>% filter(Young == 1), pred = 'bestb_p', real = TRUE,
                                              manual_ylim = c(20, 42)) +
                    ggtitle('Précis') +
                                    rremove("xlab"),
                                  Pareto_full(fdat %>% filter(Young == 1), pred = 'unb_p', real = TRUE,
                                              manual_ylim = c(20, 42)) + ggtitle('Inconscient')+
                                    rremove("xlab"), 
                                  Pareto_full(fdat %>% filter(Young == 1), pred = 'awb_p', real = TRUE,
                                              manual_vline = 199,
                                              manual_ylim = c(20, 42)) + ggtitle('Conscient'), 
                                  nrow = 3,
                                  common.legend = TRUE)

# Check calibration on true label
p_label_bias <- fdat%>%
  mutate(Male = ifelse(Male == 0, "Wht. pat.", "Blk. pat.")) %>%
  ggplot(aes(x = best_p, y = (gagne_sum_t >= 5) + 0.0, color = Male)) +
  geom_smooth(
    formula = y ~ x,
    method = "glm",
    method.args = list(family = binomial),
    linewidth = 1/2
  ) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous("best_p estimate", labels = scales::label_percent()) +
  scale_y_continuous(
    expr(Pr("chronic conditions" >= 5)),
    labels = scales::label_percent()
  ) +
  labs(color = NULL) +
  theme_bw() +
  theme(
    legend.position = c(0.99, 0.01),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = alpha(0, 0)),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 9),
    plot.margin = margin(10, 10, 10, 0)
  )

ggsave("figures/label_bias.pdf", plot = p_label_bias, height = 2.5, width = 2.5)

# Look at heterogeneous treatment effects
m_treat <- fdat%>%
  filter(program_enrolled_t == 1) %>%
  select(gagne_sum_t, gagne_sum_tm1, Male, starts_with("dem_")) %>%
  glm(I(gagne_sum_t >= 5) ~ Male * ., data = ., family = binomial)
m_control <- fdat%>%
  filter(program_enrolled_t == 0) %>%
  select(gagne_sum_t, gagne_sum_tm1, Male, starts_with("dem_"),
         starts_with("cost_"), contains("elixhauser"), contains("romano")) %>%
  # NOTE: Ulcer variable is colinear after interacting with Male.
  select(-ulcer_romano_tm1) %>%
  glm(I(gagne_sum_t >= 5) ~ Male * ., data = ., family = binomial)

# Compare treatment and control
df_treatment_effects <- fdat%>%
  mutate(
    tau = predict(m_treat, df, type = "response")
    - predict(m_control, df, type = "response"),
    # Treatment is probably at least neutral for everyone
    tau = pmax(tau, 0),
    Male = ifelse(Male == 0, "Femalepatients", "Male patients")
  )

df_treatment_effects %>%
  # Print out the correlation between the number of chronic conditions and the
  # treatment effect.
  glue::glue_data(
    "Correlation between number of chronic conditions and treatment effect: ",
    "{cor(gagne_sum_t, tau)}."
  ) %>%
  print()

p_treatment_effects <- df_treatment_effects %>%
  ggplot(aes(x = tau)) +
  geom_histogram(bins = 13) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_continuous(
    labels = scales::label_percent(),
    limits = c(0, 1/2),
    expand = c(0,0)
  ) +
  labs(
    x = "Estimated effect",
    y = "Num. patients"
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha("white", 0)),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 9),
    plot.margin = margin(10, 10, 10, 0)
  )

suppressWarnings(ggsave(
  "figures/treatment_effects.pdf",
  plot = p_treatment_effects,
  height = 2.5,
  width = 2.5
))

