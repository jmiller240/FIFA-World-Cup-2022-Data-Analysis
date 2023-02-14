library(ggplot2)
library(dplyr)
library(tidyr)
library(ggcorrplot)

setwd('/Users/jmiller/Documents/Github/MAT311-Data_Science_Final_Project')


#####################################
## Import Data / Data Manipulation ##
#####################################
wc_passing <- read.csv(file = './Data/wc_passing_p90.csv', skip=1)
wc_shooting <- read.csv(file = './Data/wc_shooting_p90.csv', skip=1)
wc_possession <- read.csv(file = './Data/wc_possession.csv', skip=1)
wc_adv_keep <- read.csv(file = './Data/wc_adv_keeping.csv', skip=1)

wc_group_results <- read.csv(file = './Data/wc_group_results.csv')
wc_group_results <- wc_group_results %>%
  mutate(AdvFrmGrpBinary = ifelse(AdvFrmGrp == 'Y', 1, 0))

# Combine sets
wc_data <- wc_passing %>%
  inner_join(wc_shooting, by = 'Squad') %>%
  inner_join(wc_possession, by = 'Squad') %>%
  inner_join(wc_adv_keep, by = 'Squad') %>%
  inner_join(wc_group_results, by = 'Squad') %>%
  select(Squad, Cmp..x, Cmp.1, Dist, Touches, Att.3rd, Att, PSxG..., AdvFrmGrp, # Select and rename only interesting stats
         AdvFrmGrpBinary) %>%
  rename(Pass_Cmp_Pct = Cmp..x, Short_Pass_Cmp = Cmp.1, Av_Shot_Dist = Dist, 
         TotTouches = Touches, Att_3rd_Touches = Att.3rd, Launch_GK_Pass = Att, 
         PSxG_Plus_Minus = PSxG...)

# Record means for each statistic
wc_data_means <- colMeans(wc_data %>% select(-Squad, -AdvFrmGrp, -AdvFrmGrpBinary))
wc_data_means <- t(data.frame(wc_data_means))
wc_data_means[,7]



##############
## Visuals ##
##############
# Combined set correlation matrix. Use to find statistics that are 1) strongly correlated with advancement from group and 2) aren't
#   correlated with each other
# From https://stackoverflow.com/questions/52554336/plot-the-equivalent-of-correlation-matrix-for-factors-categorical-data-and-mi
model.matrix(~0+., data=wc_data %>% select(-Squad, -AdvFrmGrpBinary)) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

# Simple scatters
ggplot(data = wc_data, aes(x = Short_Pass_Cmp, y = Drib_Att, color = AdvFrmGrp, label = Squad)) + 
  geom_point() + geom_text()

ggplot(data = wc_data, aes(x = Short_Pass_Cmp, y = GKLaunchPct, color = AdvFrmGrp, label = Squad)) +
  geom_point()

#Shot Distance vs. GK Launched Pass Attempts
ggplot(data = wc_data, aes(x = Av_Shot_Dist, y = Launch_GK_Pass, color = AdvFrmGrp)) + 
  geom_point() + geom_vline(xintercept = wc_data_means[,3]) + 
  geom_hline(yintercept = wc_data_means[,6]) + 
  ggtitle('GK Launched Passes vs. Average Shot Distance') +
  annotate('text', x = 20, y = 20, size = 2, label = 'Many Launches,\n Far Shots') + 
  annotate('text', x = 16, y = 20, size = 2, label = 'Many Launches,\n Close Shots') + 
  annotate('text', x = 20, y = 7, size = 2, label = 'Few Launches,\n Far Shots') + 
  annotate('text', x = 16, y = 7, size = 2, label = 'Few Launches,\n Close Shots')
  
#Pass Completion% vs. PSxG+/-
ggplot(data = wc_data, aes(x = Pass_Cmp_Pct, y = PSxG_Plus_Minus, color = AdvFrmGrp, label = Squad)) + 
  geom_point() + geom_vline(xintercept = wc_data_means[,1]) + 
  geom_hline(yintercept = wc_data_means[,7])

#Pass Completion% vs. Short Pass Completions
ggplot(data = wc_data, aes(x = Pass_Cmp_Pct, y = Short_Pass_Cmp, color = AdvFrmGrp, label = Squad)) + 
  geom_point() + 
  ggtitle('Short Pass Completions P90 vs. Pass Completion Percentage') + 
  theme(plot.title = element_text(size = 10))

#Short Pass Completions vs. PSxG+/-
ggplot(data = wc_data, aes(x = Short_Pass_Cmp, y = PSxG_Plus_Minus, color = AdvFrmGrp, label = Squad)) + 
  geom_point() + geom_vline(xintercept = wc_data_means[,2]) + 
  geom_hline(yintercept = wc_data_means[,7])

#Launch GK Pass vs. PSxG+/-
ggplot(data = wc_data, aes(x = Launch_GK_Pass, y = PSxG_Plus_Minus, color = AdvFrmGrp, label = Squad)) + 
  geom_point() + geom_vline(xintercept = wc_data_means[,6]) + 
  geom_hline(yintercept = wc_data_means[,7])

#Short Pass Cmp vs. Total Touches
ggplot(data = wc_data, aes(x = Short_Pass_Cmp, y = TotTouches, color = AdvFrmGrp, label = Squad)) + 
  geom_point() + geom_vline(xintercept = wc_data_means[,2]) + 
  geom_hline(yintercept = wc_data_means[,4])

#GK Launch Pass vs. Total Touches
ggplot(data = wc_data, aes(x = Launch_GK_Pass, y = TotTouches, color = AdvFrmGrp, label = Squad)) + 
  geom_point() + geom_vline(xintercept = wc_data_means[,6]) + 
  geom_hline(yintercept = wc_data_means[,4])

#Short Pass Cmp vs. Att 3rd Touches
ggplot(data = wc_data, aes(x = Short_Pass_Cmp, y = Att_3rd_Touches, color = AdvFrmGrp, label = Squad)) + 
  geom_point() + geom_vline(xintercept = wc_data_means[,2]) + 
  geom_hline(yintercept = wc_data_means[,5])



##############################
## Decision Tree Classifier ##
##############################
library(tidyr)
library(tidymodels)
library(parsnip)
library(yardstick)
library(rpart)
library(rpart.plot)
library(rsample)

wc_data$AdvFrmGrp <- as.factor(wc_data$AdvFrmGrp)

# Test/train splits
set.seed(300)
wc_splits <- wc_data %>%
  initial_split(prop = 0.8)
wc_train <- wc_splits %>% training()
wc_test <- wc_splits %>% testing()

# Configure and train dtree
wc_dtree <- decision_tree(mode = 'classification') %>%
  set_engine('rpart') %>%
  parsnip::fit(AdvFrmGrp ~ Pass_Cmp_Pct + Short_Pass_Cmp + Av_Shot_Dist +
    TotTouches + Att_3rd_Touches + Launch_GK_Pass + PSxG_Plus_Minus,
    data = wc_train)
wc_dtree
wc_dtree$fit %>% rpart.plot()

# Training Predictions
pred_dtree <- wc_train %>%
  select(-Squad, -AdvFrmGrpBinary) %>%
  bind_cols(
    predict(wc_dtree, new_data = wc_train, type = 'class')
  ) %>%
  rename(predicted_result = .pred_class)
accuracy(pred_dtree, AdvFrmGrp, predicted_result)  # Calculate training accuracy

# Training confusion matrix
confusion <- pred_dtree %>%
  conf_mat(truth = AdvFrmGrp, estimate = predicted_result)
confusion

# Test Predictions
test_dtree <- wc_test %>%
  select(-Squad, -AdvFrmGrpBinary) %>%
  bind_cols(
    predict(wc_dtree, new_data = wc_test, type = 'class')
  ) %>%
  rename(predicted_result = .pred_class)
accuracy(test_dtree, AdvFrmGrp, predicted_result)  # Calculate testing accuracy

# Testing confusion matrix
confusion_test <- test_dtree %>%
  conf_mat(truth = AdvFrmGrp, estimate = predicted_result)
confusion_test




