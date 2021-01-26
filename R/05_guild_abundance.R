#----------------------------------------------------------#
#
#
#                Exclosure experiment PNG
#
#                  abundace of guilds
#
#             Ondrej Mottl - Marketa Tahadlova
#                         2020
#
#----------------------------------------------------------#

source("R/00_config.R")

#----------------------------------------------------------#
# 4. calculation of Abundance -----
#----------------------------------------------------------#

dataset_guild_abundance <-
  dataset_fin %>% 
  dplyr::select(
    Plot, Treatment, Hab, Spec, TreeID,
    leaf_area_total,
    CHEW, NR, PRE, SUC) %>% 
  pivot_longer(
    cols = -c(Plot, Treatment, Hab, Spec,  TreeID, leaf_area_total),
    names_to = "guild",
    values_to = "abundance"  ) %>% 
  mutate(abundance_per_leaf_area = abundance / leaf_area_total ) %>% 
  mutate(TreeID = as.character(TreeID)) %>% 
  mutate_if(is.character,as.factor)

summary(dataset_guild_abundance)

#----------------------------------------------------------#
# 5. Exploratory figures -----
#----------------------------------------------------------#

# per treatmetns
(ext_plot_01 <- 
   dataset_guild_abundance %>% 
   ggplot(
     aes(
       x = Treatment,
       y = abundance_per_leaf_area,
       fill = guild,
       col = guild)) +
   
   geom_flat_violin(
     col = "gray30",
     alpha = 1/2,
     trim = TRUE,
     position = position_nudge(
       x = 0.2,
       y = 0)) +
   
   geom_point(
     position = position_jitter(width = 0.15),
     alpha = 1,
     size = 1)+
   
   geom_boxplot(
     width=0.2,
     outlier.shape = NA,
     col = "gray30",
     alpha = 1) +
   
   facet_grid( ~ guild)+
   
   labs(
     x = "Treatment", 
     y = expression(paste("Invertebrates abundance per m" ^ 2))) +
   scale_fill_manual(values = pallete_4) +
   scale_color_manual(values = pallete_4) +
   theme(
     text = element_text(size = text_size),
     legend.position = "none"))

ggsave(
  "fig/guild_abundance/ext_plot_01.pdf",
  ext_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# per habitat
(ext_plot_02 <- 
    dataset_guild_abundance %>% 
    ggplot(
      aes(
        x = Hab,
        y = abundance_per_leaf_area,
        fill = guild,
        col = guild)) +
    
    geom_flat_violin(
      col = "gray30",
      alpha = 1/2,
      trim = TRUE,
      position = position_nudge(
        x = 0.2,
        y = 0)) +
    
    geom_point(
      position = position_jitter(width = 0.15),
      alpha = 1,
      size = 1)+
    
    geom_boxplot(
      width=0.2,
      outlier.shape = NA,
      col = "gray30",
      alpha = 1) +
    
    facet_grid( ~ guild)+
    
    labs(
      x = "Habitat",
      y = expression(paste("Invertebrates abundance per m" ^ 2)) )+
    scale_color_manual(values = pallete_4) +
    scale_fill_manual(values = pallete_4) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))

ggsave(
  "fig/guild_abundance/ext_plot_02.pdf",
  ext_plot_02,
  width = PDF_width,
  height = PDF_height,
  units = "in")


# per species
(ext_plot_03 <- 
    dataset_guild_abundance %>% 
    ggplot(
      aes(
        x = Spec,
        y = abundance_per_leaf_area,
        fill = guild,
        col = guild)) +
    
    geom_flat_violin(
      col = "gray30",
      alpha = 1/2,
      trim = TRUE,
      position = position_nudge(
        x = 0.2,
        y = 0)) +
    
    geom_point(
      position = position_jitter(width = 0.15),
      alpha = 1,
      size = 1)+
    
    geom_boxplot(
      width=0.2,
      outlier.shape = NA,
      col = "gray30",
      alpha = 1) +
    
    facet_grid( ~ guild)+
    
    labs(
      x = "Species",
      y = expression(paste("Invertebrates abundance per m" ^ 2)) )+
    scale_fill_manual(values = pallete_4) +
    scale_color_manual(values = pallete_4) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))

ggsave(
  "fig/guild_abundance/ext_plot_03.pdf",
  ext_plot_03,
  width = PDF_width,
  height = PDF_height,
  units = "in")


#----------------------------------------------------------#
# 6. CHEW -----
#----------------------------------------------------------#

dataset_guild_chew <-
  dataset_guild_abundance %>% 
  filter(guild == "CHEW") %>%
  dplyr::select(TreeID, Hab, Treatment, Spec, guild,  abundance_per_leaf_area) %>% 
  drop_na() %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(abundance_per_leaf_area = ifelse(
    abundance_per_leaf_area == 0,
    abundance_per_leaf_area + .Machine$double.eps*100,
    abundance_per_leaf_area))

summary(dataset_guild_chew)

# 6.1. model -----

glm_invertebrates_guild_chew_full <-
  glmmTMB(
    abundance_per_leaf_area ~ Hab * Treatment * Spec, 
    data = dataset_guild_chew,
    family = tweedie(),
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
  write_csv("data/output/invertebrates_chew_model_result.csv")

# observe the best model
glm_invertebrates_guild_chew_dd %>% 
  as_tibble() %>% 
  filter(delta < 2 ) %>% 
  View()

#changed from Hab+Treatment into Treatment !!!!!!!!!!!!!!

glm_invertebrates_guild_chew_select <- 
  glmmTMB(
    abundance_per_leaf_area ~ Treatment,
    data = dataset_guild_chew,
    family = tweedie(),
    na.action = "na.fail")


summary(glm_invertebrates_guild_chew_select)
check_model(glm_invertebrates_guild_chew_select)
model_performance(glm_invertebrates_guild_chew_select)
check_distribution(glm_invertebrates_guild_chew_select)
qplot(residuals(glm_invertebrates_guild_chew_select))

# 6.2. plot -----

# calculate emmeans
glm_invertebrates_guild_chew_emmeans_treat <-
  emmeans(
    glm_invertebrates_guild_chew_select,
    pairwise ~ Treatment,
    type = "response") 


(model_plot_chew_01 <-
    glm_invertebrates_guild_chew_emmeans_treat$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = Treatment,
        y = response,
        col = Treatment,
        fill = Treatment)) +
    
    geom_point(
      data = dataset_guild_chew,
      aes(y = abundance_per_leaf_area),
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
      y = expression(paste("Invertebrates abundance per m" ^ 2)),
      title = "CHEW") +
    scale_color_manual(values = pallete_1) +
    scale_fill_manual(values = pallete_1) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))



# save the pairwise test 
glm_invertebrates_guild_chew_emmeans_treat$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/inv_chew_pairwise_test_treat_contrast.csv")

glm_invertebrates_guild_chew_emmeans_treat$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/inv_chew_pairwise_treat_emmeans.csv")

##### Habitat analysis removed ####


#glm_invertebrates_guild_chew_emmeans_Hab <-
  emmeans(
    glm_invertebrates_guild_chew_select,
    pairwise ~ Hab,
    type = "response") 


#(model_plot_chew_02 <-
    glm_invertebrates_guild_chew_emmeans_Hab$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = Hab,
        y = response,
        col = Hab,
        fill = Hab)) +
    
    geom_point(
      data = dataset_guild_chew,
      aes(y = abundance_per_leaf_area),
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
      y = expression(paste("Invertebrates abundance per m" ^ 2)),
      title = "CHEW") +
    scale_color_manual(values = pallete_2) +
    scale_fill_manual(values = pallete_2) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))

# save the pairwise test 
#glm_invertebrates_guild_chew_emmeans_Hab$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/inv_chew_pairwise_Hab_contrast.csv")

# save the pairwise test 
#glm_invertebrates_guild_chew_emmeans_Hab$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/inv_chew_pairwise_Hab_emmeans.csv")

#----------------------------------------------------------#
# 7. PRE -----
#----------------------------------------------------------#


dataset_guild_pre <-
  dataset_guild_abundance %>% 
  filter(guild == "PRE") %>%
  dplyr::select(TreeID, Hab, Treatment, Spec, guild,  abundance_per_leaf_area) %>% 
  drop_na() %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(abundance_per_leaf_area = ifelse(
    abundance_per_leaf_area == 0,
    abundance_per_leaf_area + .Machine$double.eps*100,
    abundance_per_leaf_area))

summary(dataset_guild_pre)

# 7.1. model -----

glm_invertebrates_guild_pre_full <-
  glmmTMB(
    abundance_per_leaf_area ~ Hab * Treatment * Spec, 
    data = dataset_guild_pre,
    family = gaussian(link = "log"),
    na.action = "na.fail")

summary(glm_invertebrates_guild_pre_full)
check_model(glm_invertebrates_guild_pre_full) 
model_performance(glm_invertebrates_guild_pre_full)
check_distribution(glm_invertebrates_guild_pre_full)  # Gamma distribution will not converge
qplot(residuals(glm_invertebrates_guild_pre_full))

# calculate
glm_invertebrates_guild_pre_dd <-  
  pdredge(
    glm_invertebrates_guild_pre_full,
    trace = T)

# save result table
glm_invertebrates_guild_pre_dd %>% 
  as_tibble() %>% 
  write_csv("data/output/invertebrates_pre_model_result.csv")

# observe the best model
glm_invertebrates_guild_pre_dd %>% 
  as_tibble() %>% 
  filter(delta < 2 ) %>% 
  View()


glm_invertebrates_guild_pre_m1 <- 
  glm(
    abundance_per_leaf_area ~ Hab,
    data = dataset_guild_pre,
    family = gaussian(link = "log"),
    na.action = "na.fail")

glm_invertebrates_guild_pre_m2 <- 
  glm(
    abundance_per_leaf_area ~ Hab + Treatment,
    data = dataset_guild_pre,
    family = gaussian(link = "log"),
    na.action = "na.fail")

compare_performance(
  glm_invertebrates_guild_pre_m1, glm_invertebrates_guild_pre_m2,
  rank = T
)


compare_performance(
  glm_invertebrates_guild_pre_m1, glm_invertebrates_guild_pre_m2,
  rank = T) %>% 
  as_tibble() %>% 
  write_csv("data/output/inv_pre_model_performance_comparison.csv")

glm_invertebrates_guild_pre_select <- glm_invertebrates_guild_pre_m1


summary(glm_invertebrates_guild_pre_select)
check_model(glm_invertebrates_guild_pre_select)
model_performance(glm_invertebrates_guild_pre_select)
check_distribution(glm_invertebrates_guild_pre_select)
qplot(residuals(glm_invertebrates_guild_pre_select))

# 7.2. plot -----

# calculate emmeans
#glm_invertebrates_guild_pre_emmeans_treat <-
  emmeans(
    glm_invertebrates_guild_pre_select,
    pairwise ~ Treatment,
    type = "response") 


#(model_plot_pre_01 <-
    glm_invertebrates_guild_pre_emmeans_treat$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = Treatment,
        y = response,
        col = Treatment,
        fill = Treatment)) +
    
    geom_point(
      data = dataset_guild_pre,
      aes(y = abundance_per_leaf_area),
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
      y = expression(paste("Invertebrates abundance per m" ^ 2)),
      title = "PRE") +
    scale_color_manual(values = pallete_1) +
    scale_fill_manual(values = pallete_1) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))



# save the pairwise test 
#glm_invertebrates_guild_pre_emmeans_treat$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/inv_pre_pairwise_treat_contrast.csv")

#glm_invertebrates_guild_pre_emmeans_treat$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/inv_pre_pairwise_treat_emmeans.csv")


glm_invertebrates_guild_pre_emmeans_Hab <-
  emmeans(
    glm_invertebrates_guild_pre_select,
    pairwise ~ Hab,
    type = "response") 


(model_plot_pre_02 <-
    glm_invertebrates_guild_pre_emmeans_Hab$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = Hab,
        y = response,
        col = Hab,
        fill = Hab)) +
    
    geom_point(
      data = dataset_guild_pre,
      aes(y = abundance_per_leaf_area),
      alpha = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  asymp.LCL,
        ymax = asymp.UCL),
      width = 0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size = 1)+
    
    geom_point(
      shape = 0,
      size = 3,
      position = position_dodge(width = 0.5)) +
    
    labs(
      x = "Habitat",
      y = expression(paste("Invertebrates abundance per m" ^ 2)),
      title = "PRE") +
    scale_color_manual(values = pallete_2) +
    scale_fill_manual(values = pallete_2) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))

# save the pairwise test 
glm_invertebrates_guild_pre_emmeans_Hab$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/inv_pre_pairwise_hab_constrast.csv")


# save the pairwise test 
glm_invertebrates_guild_pre_emmeans_Hab$contrasts %>% 
  as_tibble() %>% 
  write_csv("data/output/inv_pre_pairwise_hab_emmeans.csv")

#----------------------------------------------------------#
# 8. NR -----
#----------------------------------------------------------#

dataset_guild_nr <-
  dataset_guild_abundance %>% 
  filter(guild == "NR") %>%
  dplyr::select(TreeID, Hab, Treatment, Spec, guild,  abundance_per_leaf_area) %>% 
  drop_na() %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(abundance_per_leaf_area = ifelse(
    abundance_per_leaf_area == 0,
    abundance_per_leaf_area + .Machine$double.eps*100,
    abundance_per_leaf_area))

summary(dataset_guild_nr)

# 8.1. model -----

glm_invertebrates_guild_nr_full <-
  glmmTMB(
    abundance_per_leaf_area ~ Hab * Treatment * Spec, 
    data = dataset_guild_nr,
    family = tweedie(),
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
  write_csv("data/output/invertebrates_nr_model_result.csv")

# observe the best model
glm_invertebrates_guild_nr_dd %>% 
  as_tibble() %>% 
  filter(delta < 2 ) %>% 
  View()


glm_invertebrates_guild_nr_select <- 
  glmmTMB(
    abundance_per_leaf_area ~ Treatment,
    data = dataset_guild_nr,
    family = tweedie(),
    na.action = "na.fail")


summary(glm_invertebrates_guild_nr_select)
check_model(glm_invertebrates_guild_nr_select)
model_performance(glm_invertebrates_guild_nr_select)
check_distribution(glm_invertebrates_guild_nr_select)
qplot(residuals(glm_invertebrates_guild_nr_select))

# 8.2. plot -----

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
      aes(y = abundance_per_leaf_area),
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
      y = expression(paste("Invertebrates abundance per m" ^ 2)),
      title = "NR") +
    scale_color_manual(values = pallete_1) +
    scale_fill_manual(values = pallete_1) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))


# save the pairwise test 
glm_invertebrates_guild_nr_emmeans_treat$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/inv_nr_pairwise_treat_constrast.csv")

# save the pairwise test 
glm_invertebrates_guild_nr_emmeans_treat$emmeans%>% 
  as_tibble() %>% 
  write_csv("data/output/inv_nr_pairwise_treat_emmeans.csv")

(model_plot_nr_02 <-
    dataset_guild_nr %>% 
    ggplot(
      aes(
        x = Hab,
        y = abundance_per_leaf_area,
        col = Hab,
        fill = Hab)) +
    
    geom_point(
      alpha = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    labs(
      x = "Habitat",
      y = expression(paste("Invertebrates abundance per m" ^ 2)),
      title = "NR") +
    scale_color_manual(values = pallete_2) +
    scale_fill_manual(values = pallete_2) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))


#----------------------------------------------------------#
# 9. SUC -----
#----------------------------------------------------------#

dataset_guild_suc <-
  dataset_guild_abundance %>% 
  filter(guild == "SUC") %>%
  dplyr::select(TreeID, Hab, Treatment, Spec, guild,  abundance_per_leaf_area) %>% 
  drop_na() %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(abundance_per_leaf_area = ifelse(
    abundance_per_leaf_area == 0,
    abundance_per_leaf_area + .Machine$double.eps*100,
    abundance_per_leaf_area))

summary(dataset_guild_suc)

# 9.1. model -----

glm_invertebrates_guild_suc_full <-
  glmmTMB(
    abundance_per_leaf_area ~ Hab * Treatment * Spec, 
    data = dataset_guild_suc,
    family = tweedie(),
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

# save result table
glm_invertebrates_guild_suc_dd %>% 
  as_tibble() %>% 
  write_csv("data/output/invertebrates_suc_model_result.csv")

# observe the best model
glm_invertebrates_guild_suc_dd %>% 
  as_tibble() %>% 
  filter(delta < 2 ) %>% 
  View()


glm_invertebrates_guild_suc_m1 <- 
  glmmTMB(
    abundance_per_leaf_area ~ (+1),
    data = dataset_guild_suc,
    family = tweedie(),
    na.action = "na.fail")

glm_invertebrates_guild_suc_m2 <- 
  glmmTMB(
    abundance_per_leaf_area ~ Treatment,
    data = dataset_guild_suc,
    family = tweedie(),
    na.action = "na.fail")

compare_performance(
  glm_invertebrates_guild_suc_m1, glm_invertebrates_guild_suc_m2,
  rank = T
)

compare_performance(
  glm_invertebrates_guild_suc_m1, glm_invertebrates_guild_suc_m2,
  rank = T) %>% 
  as_tibble() %>% 
  write_csv("data/output/invertebrates_suc_model_performance_comparison.csv")

glm_invertebrates_guild_suc_select <- glm_invertebrates_guild_suc_m1

# ->  no precictor !!

# summary(glm_invertebrates_guild_suc_select)
# check_model(glm_invertebrates_guild_suc_select)
# model_performance(glm_invertebrates_guild_suc_select)
# check_distribution(glm_invertebrates_guild_suc_select)
# qplot(residuals(glm_invertebrates_guild_suc_select))

# 9.2. plot -----


(model_plot_suc_01 <-
   dataset_guild_suc %>% 
    ggplot(
      aes(
        x = Treatment,
        y = abundance_per_leaf_area,
        col = Treatment,
        fill = Treatment)) +
    
    geom_point(
      alpha = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    labs(
      x = "Treatment",
      y = expression(paste("Invertebrates abundance per m" ^ 2)),
      title = "SUC") +
    scale_color_manual(values = pallete_1) +
    scale_fill_manual(values = pallete_1) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))

(model_plot_suc_02 <-
    dataset_guild_suc %>% 
    ggplot(
      aes(
        x = Hab,
        y = abundance_per_leaf_area,
        col = Hab,
        fill = Hab)) +
    
    geom_point(
      alpha = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    labs(
      x = "Habitat",
      y = expression(paste("Invertebrates abundance per m" ^ 2)),
      title = "SUC") +
    scale_color_manual(values = pallete_2) +
    scale_fill_manual(values = pallete_2) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))

#----------------------------------------------------------#
# 10. Summary -----
#----------------------------------------------------------#

model_plot_guild_01 <- 
  ggarrange(
    
    model_plot_chew_01 + 
      scale_y_continuous(limits = c(0,150)) +
      rremove("xylab"),
    
    model_plot_pre_01 +
      scale_y_continuous(limits = c(0,150)) +
      rremove("xylab") +
      rremove("y.text"),
    
    model_plot_nr_01 + 
      scale_y_continuous(limits = c(0,150)) +
      rremove("xylab") +
      rremove("y.text"),
    
    model_plot_suc_01 + 
      scale_y_continuous(limits = c(0,150)) +
      rremove("xylab") + 
      rremove("y.text"),
    
    nrow = 1,
    align = "hv" ) %>% 
  annotate_figure(
    left = text_grob(
      expression(paste("Invertebrates abundance per m" ^ 2)),
      size = text_size,
      rot = 90),
    bottom = text_grob(
      "Treatment",
      size = text_size)
  )

plot(model_plot_guild_01)

# save pdf
ggsave(
  "fig/guild_abundance/model_plot_01.pdf",
  model_plot_guild_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")



model_plot_guild_02 <- 
  ggarrange(
    
    model_plot_chew_02 + 
      scale_y_continuous(limits = c(0,150)) +
      rremove("xylab"),
    
    model_plot_pre_02 +
      scale_y_continuous(limits = c(0,150)) +
      rremove("xylab") +
      rremove("y.text"),
    
    model_plot_nr_02 + 
      scale_y_continuous(limits = c(0,150)) +
      rremove("xylab") +
      rremove("y.text"),
    
    model_plot_suc_02 + 
      scale_y_continuous(limits = c(0,150)) +
      rremove("xylab") + 
      rremove("y.text"),
    
    nrow = 1,
    align = "hv" ) %>% 
  annotate_figure(
    left = text_grob(
      expression(paste("Invertebrates abundance per m" ^ 2)),
      size = text_size,
      rot = 90),
    bottom = text_grob(
      "Habitat",
      size = text_size)
  )

plot(model_plot_guild_02)

# save pdf
ggsave(
  "fig/guild_abundance/model_plot_02.pdf",
  model_plot_guild_02,
  width = PDF_width,
  height = PDF_height,
  units = "in")
