#----------------------------------------------------------#
#
#
#                Exclosure experiment PNG
#
#                     guild ratios
#
#             Ondrej Mottl - Marketa Tahadlova
#                         2020
#
#----------------------------------------------------------#
source("R/00_config.R")

#----------------------------------------------------------#
# 4. calculation of guild ratio -----
#----------------------------------------------------------#

dataset_guild_ratio <-
  dataset_fin %>% 
  dplyr::select(
    Plot, Treatment, Hab, Spec, TreeID,
    leaf_area_total, Total_abundance,
    CHEW, NR, PRE, SUC) %>% 
  pivot_longer(
    cols = -c(Plot, Treatment, Hab, Spec,  TreeID, Total_abundance, leaf_area_total),
    names_to = "guild",
    values_to = "abundance"  ) %>% 
  replace(is.na(.), 0) %>% 
  mutate(abundance_per_LA = Total_abundance / leaf_area_total ) %>% 
  mutate(guild_abundance_LA = abundance / leaf_area_total ) %>% 
  mutate(guild_ratio = guild_abundance_LA / abundance_per_LA ) %>% 
  mutate(TreeID = paste(Plot, Treatment, Hab, Spec, TreeID, sep = "_")) %>% 
  dplyr::select(-c(
    leaf_area_total,
    Total_abundance,
    abundance_per_LA,
    guild_abundance_LA,
    abundance)) %>% 
  mutate_if(is.character,as.factor)

summary(dataset_guild_ratio)

#----------------------------------------------------------#
# 5. Exploratory figures -----
#----------------------------------------------------------#

# per treatmetns
(ext_plot_01 <- 
   dataset_guild_ratio %>% 
   group_by(Treatment, guild) %>% 
   summarise(
     guild_ratio = mean(guild_ratio)) %>% 
   ungroup() %>% 
   ggplot(
     aes(
       x = Treatment,
       y = guild_ratio,
       fill = guild,
       col = guild) ) +
   
   geom_bar(
     col = "gray30",
     stat = "identity",
     position = "stack",
   ) +
   
   labs(
     x = "Treatment", 
     y = "Ratio of guild presence") +
   scale_fill_manual(values = pallete_4) +
   scale_y_continuous(
     limits = c(0,1)) +
   scale_color_manual(values = pallete_4) +
   theme(
     text = element_text(size = text_size),
     legend.position = "right"))

 # per haitat
(ext_plot_02 <- 
    dataset_guild_ratio %>% 
    group_by(Hab, guild) %>% 
    summarise(
      guild_ratio = mean(guild_ratio)) %>% 
    ungroup() %>% 
    ggplot(
      aes(
        x = Hab,
        y = guild_ratio,
        fill = guild,
        col = guild) ) +
    
    geom_bar(
      col = "gray30",
      stat = "identity",
      position = "stack",
    ) +
    
    labs(
      x = "Habitat", 
      y = "Ratio of guild presence") +
    scale_fill_manual(values = pallete_4) +
    scale_y_continuous(
      limits = c(0,1)) +
    scale_color_manual(values = pallete_4) +
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))

# per species
(ext_plot_03 <- 
    dataset_guild_ratio %>% 
    group_by(Spec, guild) %>% 
    summarise(
      guild_ratio = mean(guild_ratio)) %>% 
    ungroup() %>% 
    ggplot(
      aes(
        x = Spec,
        y = guild_ratio,
        fill = guild,
        col = guild) ) +
    
    geom_bar(
      col = "gray30",
      stat = "identity",
      position = "stack",
    ) +
    
    labs(
      x = "Ficus species", 
      y = "Ratio of guild presence") +
    scale_fill_manual(values = pallete_4) +
    scale_y_continuous(
      limits = c(0,1)) +
    scale_color_manual(values = pallete_4) +
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))


#----------------------------------------------------------#
# 7. individual guild test -----
#----------------------------------------------------------#

#----------------------------------------------------------#
# 7.2 CHEW -----
#----------------------------------------------------------#

dataset_guild_chew <-
  dataset_guild_ratio %>% 
  filter(guild == "CHEW") %>%
  dplyr::select(TreeID, Hab, Treatment, Spec, guild,  guild_ratio) %>% 
  drop_na() %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(guild_ratio = ifelse(
    guild_ratio == 0,
    guild_ratio + .Machine$double.eps*100,
    guild_ratio))

summary(dataset_guild_chew)

# 7.2.1. model -----

glm_invertebrates_guild_chew_full <-
  glmmTMB(
    guild_ratio ~ Hab * Treatment * Spec, 
    data = dataset_guild_chew,
    family = beta_family(),
    na.action = "na.fail")

summary(glm_invertebrates_guild_chew_full)
check_model(glm_invertebrates_guild_chew_full) 
model_performance(glm_invertebrates_guild_chew_full)
check_distribution(glm_invertebrates_guild_chew_full)
qplot(residuals(glm_invertebrates_guild_chew_full))

# calculate
glm_invertebrates_guild_chew_dd <-  
  pdredge(
    glm_invertebrates_guild_chew_full,
    trace = T)

# save result table
glm_invertebrates_guild_chew_dd %>% 
  as_tibble() %>% 
  write_csv("data/output/invertebrates_ratio_chew_model_result.csv")

# observe the best model
glm_invertebrates_guild_chew_dd %>% 
  as_tibble() %>% 
  filter(delta < 2 ) %>% 
  View()


glm_invertebrates_guild_chew_m1<- 
  glmmTMB(
    guild_ratio ~ Hab + Spec + Hab:Spec,
    data = dataset_guild_chew,
    family = beta_family(),
    na.action = "na.fail")

glm_invertebrates_guild_chew_m2 <- 
  glmmTMB(
    guild_ratio ~ Hab + Spec + Treatment + Hab:Spec,
    data = dataset_guild_chew,
    family = beta_family(),
    na.action = "na.fail")


compare_performance(
  glm_invertebrates_guild_chew_m1, glm_invertebrates_guild_chew_m2,
  rank = T
)

compare_performance(
  glm_invertebrates_guild_chew_m1, glm_invertebrates_guild_chew_m2,
  rank = T) %>% 
  as_tibble() %>% 
  write_csv("data/output/invertebrates_ratio_chew_model_performance_comparison.csv")



glm_invertebrates_guild_chew_select <- glm_invertebrates_guild_chew_m1

summary(glm_invertebrates_guild_chew_select)
check_model(glm_invertebrates_guild_chew_select)
model_performance(glm_invertebrates_guild_chew_select)
check_distribution(glm_invertebrates_guild_chew_select)
qplot(residuals(glm_invertebrates_guild_chew_select))

# 7.2.2. plot -----


(model_plot_chew_01 <-
   dataset_guild_chew %>% 
   ggplot(
     aes(
       x = Treatment,
       y =  guild_ratio,
       col = Treatment,
       fill = Treatment)) +
   
   geom_point(
     alpha = 1,
     position = position_jitterdodge(
       dodge.width = 0.5,
       jitter.width = 0.15)) +
   
   labs(
     x = "Treatment",
     y = "Guild abundance ratio") +
   scale_color_manual(values = pallete_1) +
   scale_fill_manual(values = pallete_1) +
   theme(
     text = element_text(size = text_size),
     legend.position = "none"))

glm_invertebrates_guild_chew_emmeans_HabSpec <-
  emmeans(
    glm_invertebrates_guild_chew_select,
    pairwise ~ Hab + Spec + Hab:Spec,
    type = "response") 


(model_plot_chew_02 <-
    glm_invertebrates_guild_chew_emmeans_HabSpec$emmeans %>%
    as_tibble() %>% 
    ggplot(
      aes(
        x = Hab,
        y =  response,
        col = Spec,
        fill = Spec)) +
    
    geom_point(
      data = dataset_guild_chew,
      aes(y = guild_ratio),
      alpha = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL,
        ymax = upper.CL),
      width = 0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size = 1)+
    
    geom_point(
      shape = 0,
      size = 3,
      position = position_dodge(width = 0.5)) +
    
    labs(
      x = "habitat",
      y = "Guild abundance ratio") +
    scale_color_manual(values = pallete_3) +
    scale_fill_manual(values = pallete_3) +
    scale_y_continuous(limits = c(0,1)) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))


glm_invertebrates_guild_chew_emmeans_HabSpec$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/inv_ratio_chew_pairwise_HabSpec_contrast.csv")

glm_invertebrates_guild_chew_emmeans_HabSpec$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/inv_ratio_chew_pairwise_HabSpec_emmeans.csv")


model_plot_chew_sum <- 
  ggarrange(
    
    model_plot_chew_01 + 
      rremove("ylab"),
    
    model_plot_chew_02 + 
      rremove("ylab"),
    
    nrow = 1,
    common.legend = T,
    labels = c("CHEW",""),
    legend = "none") 


plot(model_plot_chew_sum)

#----------------------------------------------------------#
# 7.3 NR -----
#----------------------------------------------------------#

dataset_guild_nr <-
  dataset_guild_ratio %>% 
  filter(guild == "NR") %>%
  dplyr::select(TreeID, Hab, Treatment, Spec, guild,  guild_ratio) %>% 
  drop_na() %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(guild_ratio = ifelse(
    guild_ratio == 0,
    guild_ratio + .Machine$double.eps*100,
    guild_ratio))

summary(dataset_guild_nr)

# 7.3.1. model -----

glm_invertebrates_guild_nr_full <-
  glmmTMB(
    guild_ratio ~ Hab * Treatment * Spec, 
    data = dataset_guild_nr,
    family = beta_family(),
    na.action = "na.fail")

summary(glm_invertebrates_guild_nr_full)
check_model(glm_invertebrates_guild_nr_full) 
model_performance(glm_invertebrates_guild_nr_full)
check_distribution(glm_invertebrates_guild_nr_full)
qplot(residuals(glm_invertebrates_guild_nr_full))

# calculate
glm_invertebrates_guild_nr_dd <-  
  pdredge(
    glm_invertebrates_guild_nr_full,
    trace = T)


# save result table
glm_invertebrates_guild_nr_dd %>% 
  as_tibble() %>% 
  write_csv("data/output/invertebrates_ratio_nr_model_result.csv")

# observe the best model
glm_invertebrates_guild_nr_dd %>% 
  as_tibble() %>% 
  filter(delta < 2 ) %>% 
  View()

glm_invertebrates_guild_nr_select <- 
  glmmTMB(
    guild_ratio ~ Hab + Spec + Treatment + Hab:Spec,
    data = dataset_guild_nr,
    family = beta_family(),
    na.action = "na.fail")

summary(glm_invertebrates_guild_nr_select)
check_model(glm_invertebrates_guild_nr_select)
model_performance(glm_invertebrates_guild_nr_select)
check_distribution(glm_invertebrates_guild_nr_select)
qplot(residuals(glm_invertebrates_guild_nr_select))

# 7.3.2. plot -----

# calculate emmeans
glm_invertebrates_guild_nr_emmeans_treat <-
  emmeans(
    glm_invertebrates_guild_nr_select,
    pairwise ~ Treatment,
    type = "response") 

(model_plot_nr_01 <-
    glm_invertebrates_guild_nr_emmeans_treat$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = Treatment,
        y = response,
        col = Treatment,
        fill = Treatment)) +
    
    geom_point(
      data = dataset_guild_nr,
      aes(y = guild_ratio),
      alpha = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL,
        ymax = upper.CL),
      width = 0.1,
      size = 1)+
    
    geom_point(
      shape = 0,
      size = 3,
      position = position_dodge(width = 0.5)) +
    
    labs(
      x = "Treatment",
      y = "Guild abundance ratio") +
    scale_color_manual(values = pallete_1) +
    scale_fill_manual(values = pallete_1) +
    scale_y_continuous(limits = c(0,1)) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))

# save the pairwise test 
glm_invertebrates_guild_nr_emmeans_treat$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/inv_ratio_nr_pairwise_treat_contrast.csv")

glm_invertebrates_guild_nr_emmeans_treat$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/inv_ratio_nr_pairwise_treat_emmeans.csv")


glm_invertebrates_guild_nr_emmeans_HabSpec <-
  emmeans(
    glm_invertebrates_guild_nr_select,
    pairwise ~ Hab + Spec + Hab:Spec,
    type = "response") 


(model_plot_nr_02 <-
    glm_invertebrates_guild_nr_emmeans_HabSpec$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = Hab,
        y = response,
        col = Spec,
        fill = Spec)) +
    
    geom_point(
      data = dataset_guild_nr,
      aes(y = guild_ratio),
      alpha = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL,
        ymax = upper.CL),
      width = 0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size = 1)+
    
    geom_point(
      shape = 0,
      size = 3,
      position = position_dodge(width = 0.5)) +
    
    labs(
      x = "habitat",
      y = "Guild abundance ratio") +
    scale_color_manual(values = pallete_3) +
    scale_fill_manual(values = pallete_3) +
    scale_y_continuous(limits = c(0,1)) +
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))



glm_invertebrates_guild_nr_emmeans_HabSpec$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/inv_ratio_nr_pairwise_HabSpec.csv")

glm_invertebrates_guild_nr_emmeans_HabSpec$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/inv_ratio_nr_pairwise_HabSpec_emmeans.csv")


model_plot_nr_sum <- 
  ggarrange(
    
    model_plot_nr_01 + 
      rremove("ylab"),
    
    model_plot_nr_02 + 
      rremove("ylab"),
    
    nrow = 1,
    common.legend = T,
    labels = c("NR",""),
    legend = "none") 

plot(model_plot_nr_sum)



#----------------------------------------------------------#
# 7.3. PRE -----
#----------------------------------------------------------#

dataset_guild_pre <-
  dataset_guild_ratio %>% 
  filter(guild == "PRE") %>%
  dplyr::select(TreeID, Hab, Treatment, Spec, guild,  guild_ratio) %>% 
  drop_na() %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(guild_ratio = ifelse(
    guild_ratio == 0,
    guild_ratio + .Machine$double.eps*100,
    guild_ratio)) %>% 
  mutate(guild_ratio = ifelse(
    guild_ratio == 1,
    guild_ratio - .Machine$double.eps*100,
    guild_ratio))

summary(dataset_guild_pre)

# 7.4.1. model -----

glm_invertebrates_guild_pre_full <-
  glmmTMB(
    guild_ratio ~ Hab * Treatment * Spec, 
    data = dataset_guild_pre,
    family = beta_family(),
    na.action = "na.fail")

summary(glm_invertebrates_guild_pre_full)
check_model(glm_invertebrates_guild_pre_full) 
model_performance(glm_invertebrates_guild_pre_full)
check_distribution(glm_invertebrates_guild_pre_full)
qplot(residuals(glm_invertebrates_guild_pre_full))

# calculate
glm_invertebrates_guild_pre_dd <-  
  pdredge(
    glm_invertebrates_guild_pre_full,
    trace = T)

# save result table
glm_invertebrates_guild_pre_dd %>% 
  as_tibble() %>% 
  write_csv("data/output/invertebrates_ratio_pre_model_result.csv")


# observe the best model
glm_invertebrates_guild_pre_dd %>% 
  as_tibble() %>% 
  filter(delta < 2 ) %>% 
  View()

glm_invertebrates_guild_pre_m1<- 
  glmmTMB(
    guild_ratio ~ Hab + Spec + Treatment + Hab:Treatment + Spec:Treatment,
    data = dataset_guild_pre,
    family = beta_family(),
    na.action = "na.fail")

glm_invertebrates_guild_pre_m2 <- 
  glmmTMB(
    guild_ratio ~ Hab + Spec + Treatment + Spec:Treatment,
    data = dataset_guild_pre,
    family = beta_family(),
    na.action = "na.fail")


compare_performance(
  glm_invertebrates_guild_pre_m1, glm_invertebrates_guild_pre_m2,
  rank = T
)

compare_performance(
  glm_invertebrates_guild_pre_m1, glm_invertebrates_guild_pre_m2,
  rank = T )%>% 
  as_tibble() %>% 
  write_csv("data/output/invertebrates_ratio_pre_model_performance_comparison.csv")

glm_invertebrates_guild_pre_select <- glm_invertebrates_guild_pre_m1

summary(glm_invertebrates_guild_pre_select)
check_model(glm_invertebrates_guild_pre_select)
model_performance(glm_invertebrates_guild_pre_select)
check_distribution(glm_invertebrates_guild_pre_select)
qplot(residuals(glm_invertebrates_guild_pre_select))

# 7.4.2. plot -----

# calculate emmeans
glm_invertebrates_guild_pre_emmeans_HabTreat <-
  emmeans(
    glm_invertebrates_guild_pre_select,
    pairwise ~ Hab + Treatment + Hab:Treatment,
    type = "response") 


(model_plot_pre_01 <-
    glm_invertebrates_guild_pre_emmeans_HabTreat$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = Hab,
        y = response,
        col = Treatment,
        fill = Treatment)) +
    
    geom_point(
      data = dataset_guild_pre,
      aes(y = guild_ratio),
      alpha = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL,
        ymax = upper.CL),
      width = 0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size = 1)+
    
    geom_point(
      shape = 0,
      size = 3,
      position = position_dodge(width = 0.5)) +
    
    labs(
      x = "Habitat",
      y = "Guild abundance ratio") +
    scale_color_manual(values = pallete_1) +
    scale_fill_manual(values = pallete_1) +
    scale_y_continuous(limits = c(0,1)) +
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))


# save the pairwise test 
glm_invertebrates_guild_pre_emmeans_treat$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/invertebrates_ratio_pre_pairwise_test_HabTreat.csv")



# calculate emmeans
glm_invertebrates_guild_pre_emmeans_SpecTreat <-
  emmeans(
    glm_invertebrates_guild_pre_select,
    pairwise ~ Spec + Treatment + + Spec:Treatment,
    type = "response") 


(model_plot_pre_02 <-
    glm_invertebrates_guild_pre_emmeans_SpecTreat$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = Treatment,
        y = response,
        col = Spec,
        fill = Spec)) +
    
    geom_point(
      data = dataset_guild_pre,
      aes(y = guild_ratio),
      alpha = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL,
        ymax = upper.CL),
      width = 0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size = 1)+
    
    geom_point(
      shape = 0,
      size = 3,
      position = position_dodge(width = 0.5)) +
    
    labs(
      x = "Treatment",
      y = "Guild abundance ratio") +
    scale_color_manual(values = pallete_3) +
    scale_fill_manual(values = pallete_3) +
    scale_y_continuous(limits = c(0,1)) +
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))


# save the pairwise test 
glm_invertebrates_guild_pre_emmeans_treat$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/inv_ratio_pre_pairwise_SpecTreat_contrast.csv")


glm_invertebrates_guild_pre_emmeans_treat$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/inv_ratio_pre_pairwise_SpecTreat_emmeans.csv")


model_plot_pre_sum <- 
  ggarrange(
    
    model_plot_pre_02 + 
      rremove("ylab"),
    
    model_plot_pre_01 + 
      rremove("ylab"),
    
    nrow = 1,
    common.legend = T,
    labels = c("PRE",""),
    legend = "none") 

plot(model_plot_pre_sum)



#----------------------------------------------------------#
# 7.5. SUC -----
#----------------------------------------------------------#

dataset_guild_suc <-
  dataset_guild_ratio %>% 
  filter(guild == "SUC") %>%
  dplyr::select(TreeID, Hab, Treatment, Spec, guild,  guild_ratio) %>% 
  drop_na() %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(guild_ratio = ifelse(
    guild_ratio == 0,
    guild_ratio + .Machine$double.eps*100,
    guild_ratio)) %>% 
  mutate(guild_ratio = ifelse(
    guild_ratio == 1,
    guild_ratio - .Machine$double.eps*100,
    guild_ratio))

summary(dataset_guild_suc)

# 7.5.1. model -----

glm_invertebrates_guild_suc_full <-
  glmmTMB(
    guild_ratio ~ Hab * Treatment * Spec, 
    data = dataset_guild_suc,
    family = beta_family(),
    na.action = "na.fail")

summary(glm_invertebrates_guild_suc_full)
check_model(glm_invertebrates_guild_suc_full) 
model_performance(glm_invertebrates_guild_suc_full)
check_distribution(glm_invertebrates_guild_suc_full)
qplot(residuals(glm_invertebrates_guild_suc_full))

# calculate
glm_invertebrates_guild_suc_dd <-  
  pdredge(
    glm_invertebrates_guild_suc_full,
    trace = T)

glm_invertebrates_guild_suc_dd %>% 
  as_tibble() %>% 
  filter(delta < 2 ) %>% 
  View()

# save result table
glm_invertebrates_guild_suc_dd %>% 
  as_tibble() %>% 
  write_csv("data/output/invertebrates_ratio_suc_model_result.csv")


glm_invertebrates_guild_suc_select  <- 
  glmmTMB(
    guild_ratio ~ Hab + Spec + Treatment + Hab:Spec + Spec:Treatment,
    data = dataset_guild_suc,
    family = beta_family(),
    na.action = "na.fail")


summary(glm_invertebrates_guild_suc_select)
check_model(glm_invertebrates_guild_suc_select)
model_performance(glm_invertebrates_guild_suc_select)
check_distribution(glm_invertebrates_guild_suc_select)
qplot(residuals(glm_invertebrates_guild_suc_select))

# 7.5.2. plot -----

# calculate emmeans
glm_invertebrates_guild_suc_emmeans_SpecTreat <-
  emmeans(
    glm_invertebrates_guild_suc_select,
    pairwise ~ Spec + Treatment + + Spec:Treatment,
    type = "response") 


(model_plot_suc_01 <-
    glm_invertebrates_guild_suc_emmeans_SpecTreat$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = Treatment,
        y = response,
        col = Spec,
        fill = Spec)) +
    
    geom_point(
      data = dataset_guild_suc,
      aes(y = guild_ratio),
      alpha = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL,
        ymax = upper.CL),
      width = 0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size = 1)+
    
    geom_point(
      shape = 0,
      size = 3,
      position = position_dodge(width = 0.5)) +
    
    labs(
      x = "Treatment",
      y = "Guild abundance ratio") +
    scale_color_manual(values = pallete_3) +
    scale_fill_manual(values = pallete_3) +
    scale_y_continuous(limits = c(0,1)) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))


# save the pairwise test 
glm_invertebrates_guild_suc_emmeans_SpecTreat$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/inv_ratio_suc_pairwise_SpecTreat_contrast.csv")


glm_invertebrates_guild_suc_emmeans_SpecTreat$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/inv_ratio_suc_pairwise_SpecTreat_emmeans.csv")

glm_invertebrates_guild_suc_emmeans_SpecHab <-
  emmeans(
    glm_invertebrates_guild_suc_select,
    pairwise ~ Spec + Hab + + Spec:Hab,
    type = "response") 


(model_plot_suc_02 <-
    glm_invertebrates_guild_suc_emmeans_SpecHab$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = Hab,
        y = response,
        col = Spec,
        fill = Spec)) +
    
    geom_point(
      data = dataset_guild_suc,
      aes(y = guild_ratio),
      alpha = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL,
        ymax = upper.CL),
      width = 0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size = 1)+
    
    geom_point(
      shape = 0,
      size = 3,
      position = position_dodge(width = 0.5)) +
    
    labs(
      x = "Habitat",
      y = "Guild abundance ratio") +
    scale_color_manual(values = pallete_3) +
    scale_fill_manual(values = pallete_3) +
    scale_y_continuous(limits = c(0,1)) +
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))


# save the pairwise test 
glm_invertebrates_guild_suc_emmeans_SpecHab$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/inv_ratio_suc_pairwise_SpecHab_contrast.csv")

glm_invertebrates_guild_suc_emmeans_SpecHab$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/inv_ratio_suc_pairwise_SpecHab_emmeans.csv")

model_plot_suc_sum <- 
  ggarrange(
    
    model_plot_suc_01 + 
      rremove("ylab"),
    
    model_plot_suc_02 + 
      rremove("ylab"),
    
    nrow = 1,
    common.legend = T,
    labels = c("SUC",""),
    legend = "none") 

plot(model_plot_suc_sum)


#----------------------------------------------------------#
# 7.6 Summary -----
#----------------------------------------------------------#


model_plot_guild_01 <- 
  ggarrange(
    
    model_plot_chew_sum + 
      rremove("ylab"),
    
    model_plot_nr_sum +
      rremove("ylab") +
      rremove("y.text"),
    
    model_plot_pre_sum + 
      rremove("ylab") +
      rremove("y.text"),
    
    model_plot_suc_sum + 
      rremove("ylab") + 
      rremove("y.text"),
    
    ncol = 1,
    common.legend = T,
    legend = "right") %>% 
  annotate_figure(
    left = text_grob(
      "Guild abundance ratio",
      size = text_size,
      rot = 90)
  )

grob_legend_treat <- 
  cowplot::get_legend(model_plot_pre_01)

grob_legend_spec <- 
  cowplot::get_legend(model_plot_nr_02)


legend_grob <-
  ggarrange(
    grob_legend_treat,
    grob_legend_spec,
    ncol = 1
  )
model_plot_guild_01_leg <- 
  ggarrange(
    model_plot_guild_01,
    legend_grob,
    nrow = 1,
    widths = c(1, 0.15)
  )

plot(model_plot_guild_01_leg)

# teoreticky jde rozsekat do více grafů

# save pdf
ggsave(
  "fig/guild_abundance/model_ratio_plot_01.pdf",
  model_plot_guild_01_leg,
  width = PDF_width,
  height = PDF_height,
  units = "in")

