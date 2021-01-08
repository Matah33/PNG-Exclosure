#----------------------------------------------------------#
#
#
#                Exclosure experiment PNG
#
#               Mesopredator compensation
#
#             Ondrej Mottl - Marketa Tahadlova
#                         2020
#
#----------------------------------------------------------#

source("R/00_config.R")

#----------------------------------------------------------#
# 4. calculation of guild ratio -----
#----------------------------------------------------------#

data_con <-
  dataset_fin %>% 
  filter(Treatment == "CON") %>% 
  mutate(
    pred_con = PRE,
    herb_con = CHEW + SUC,
    value_con = log(pred_con/herb_con)) %>% 
  dplyr::select(Hab, Spec, TreeID, value_con)

data_exp <-
  dataset_fin %>% 
  filter(Treatment == "EXP") %>% 
  mutate(
    pred_exp = PRE,
    herb_exp = CHEW + SUC,
    value_exp = log(pred_exp/herb_exp)) %>% 
  dplyr::select(Hab, Spec, TreeID, value_exp)


inner_join(
  data_con,
  data_exp,
  by = c("TreeID","Hab","Spec")) %>% 
  ggplot(
    aes(
      x = value_con,
      y = value_exp,
      col = Hab)) + 
  
  geom_vline(
    xintercept = 0,
    lty = 3,
    size = 0.1,
    col = "gray30") +
  
  geom_hline(
    yintercept = 0,
    lty = 3,
    size = 0.1,
    col = "gray30") +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    lty = 3,
    size = 0.1,
    col = "gray30") +
  
  geom_smooth(
    method = "lm",
    formula = y~x,
    se = F) + 
  
  geom_point() +
  
  coord_fixed() +
  labs(
    x = "ln(predator : herbivore)\nvertebrate present",
    y = "ln(predator : herbivore)\nvertebrate absent"
  )
