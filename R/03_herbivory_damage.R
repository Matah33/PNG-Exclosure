#----------------------------------------------------------#
#
#
#                Exclosure experiment PNG
#
#                     Herbivory damage
#
#             Ondrej Mottl - Marketa Tahadlova
#                         2020
#
#----------------------------------------------------------#

source("R/00_config.R")
#----------------------------------------------------------#
# 4. Herbivory damage exploratory figures -----
#----------------------------------------------------------#


# per treatments
(ext_plot_01 <- 
   dataset_fin %>% 
   ggplot(
     aes(
       x = Treatment,
       y = herbivory_percentage_mean,
       fill = Treatment,
       col = Treatment)) +
   
   geom_flat_violin(
     col = "gray30",
     alpha = 1/2,
     trim = TRUE,
     position = position_nudge(
       x = 0.2,
       y = 0)) +
   
   geom_point(
     position = position_jitter(width = 0.15),
     alpha = 1/2,
     size = 1)+
   
   geom_boxplot(
     width=0.2,
     outlier.shape = NA,
     col = "gray30",
     alpha = 1) +
   
   labs(
     x = "Treatment", 
     y = "Mean herbivory damage (%)") +
   scale_fill_manual(values = pallete_1) +
   scale_color_manual(values = pallete_1) +
   theme(
     text = element_text(size = text_size),
     legend.position = "none"))

ggsave(
  "fig/herbivory_damage/ext_plot_01.pdf",
  ext_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# per habitat
(ext_plot_02 <- 
    dataset_fin %>% 
    ggplot(
      aes(
        x = Hab,
        y = herbivory_percentage_mean,
        fill = Hab,
        col = Hab)) +
    
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
    
    labs(
      x = "Habitat",
      y = "Mean herbivory damage (%)" )+
    scale_fill_manual(values = pallete_2)+
    scale_color_manual(values = pallete_2)+
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))

ggsave(
  "fig/herbivory_damage/ext_plot_02.pdf",
  ext_plot_02,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# per species
(ext_plot_03 <- 
    dataset_fin %>% 
    ggplot(
      aes(
        x = Spec,
        y = herbivory_percentage_mean,
        fill = Spec,
        col = Spec)) +
    
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
    
    labs(
      x = "Species",
      y = "Mean herbivory damage (%)" )+
    scale_fill_manual(values = pallete_3)+
    scale_color_manual(values = pallete_3)+
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))

ggsave(
  "fig/herbivory_damage/ext_plot_03.pdf",
  ext_plot_03,
  width = PDF_width,
  height = PDF_height,
  units = "in")


#----------------------------------------------------------#
# 5. Model build -----
#----------------------------------------------------------#

dataset_herbivory_damage <-
  dataset_fin %>% 
  mutate(herbivory_percentage_mean = herbivory_percentage_mean/100) %>% 
  mutate(herbivory_percentage_mean = ifelse(
    herbivory_percentage_mean == 0,
    herbivory_percentage_mean + .Machine$double.eps*100,
    herbivory_percentage_mean)) %>% 
  mutate_if(is.character,as.factor)


summary(dataset_herbivory_damage)


# create full model with all interaction
glm_herbivory_damage_full <-
  glmmTMB(
    herbivory_percentage_mean ~ Hab * Treatment * Spec,
    data = dataset_herbivory_damage,
    family = "beta_family",
    na.action = "na.fail")

summary(glm_herbivory_damage_full)
check_distribution(glm_herbivory_damage_full)
check_model(glm_herbivory_damage_full)
check_heteroscedasticity(glm_herbivory_damage_full)
model_performance(glm_herbivory_damage_full)

# compute all posible combinations
glm_herbivory_damage_dd <- 
  MuMIn::dredge(glm_herbivory_damage_full,
                                         trace = TRUE)

# save result table
glm_herbivory_damage_dd %>% 
  as_tibble() %>% 
  write_csv("data/output/herbivory_damage_model_result.csv")

# observe the best model
glm_herbivory_damage_dd %>% 
  as_tibble() %>% 
  filter(delta < 2) %>% 
  View()


# fit the best model 
glm_herbivory_damage_select <-
  glmmTMB(
    herbivory_percentage_mean ~ Hab + Spec + Treatment,
    data = dataset_herbivory_damage,
    family = beta_family(),
    na.action = "na.fail")

summary(glm_herbivory_damage_select)
check_model(glm_herbivory_damage_select)
model_performance(glm_herbivory_damage_select)
check_heteroscedasticity(glm_herbivory_damage_select)
qplot(residuals(glm_herbivory_damage_select))

# 5.1 Model plots  -----

# there are no interaction so have to plotted individually

# calculate emmeans hab
glm_herbivory_damage_emmeans_hab <-
  emmeans(
    glm_herbivory_damage_select,
    pairwise ~ Hab,
    type = "response") 

# save the pairwise test 
glm_herbivory_damage_emmeans_hab$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/herbivory_pairwise_hab_contrast.csv")
glm_herbivory_damage_emmeans_hab$emmeans %>% 
  as_tibble() %>% 
    write_csv("data/output/herbivory_pairwise_hab_emmeans.csv")


(model_plot_01 <-
  glm_herbivory_damage_emmeans_hab$emmeans %>% 
  as_tibble() %>% 
  ggplot(
    aes(
      x = Hab,
      y = response,
      col = Hab,
      fill = Hab)) +
  
  geom_point(
    data = dataset_herbivory_damage,
    aes(y = herbivory_percentage_mean),
    alpha = 1,
    position = position_jitterdodge(
      dodge.width = 0.5,
      jitter.width = 0.15)) +
  
  geom_errorbar(
    aes(
      ymin =  lower.CL,
      ymax = upper.CL),
    width=0.2,
    position = position_dodge(width = 0.5, preserve = "single"),
    size=1) +
  
  geom_point(
    shape = 0,
    size = 3,
    position = position_dodge(width = 0.5))+
 
  labs(
    x = "Habitat",
    y = "Mean herbivory damage (%)" )+
  scale_fill_manual(values = pallete_2)+
  scale_color_manual(values = pallete_2)+
  theme(
    text = element_text(size = text_size),
    legend.position = "right"))

# save pdf
ggsave(
  "fig/herbivory_damage/model_plot_01.pdf",
  model_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")



# calculate emmeans Spec
glm_herbivory_damage_emmeans_Spec <-
  emmeans(
    glm_herbivory_damage_select,
    pairwise ~ Spec,
    type = "response") 

glm_herbivory_damage_emmeans_Spec$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/herbivory_pairwise_spec_contrasts.csv")

glm_herbivory_damage_emmeans_Spec$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/herbivory_pairwise_spec_contrasts_emmeans.csv")

(model_plot_02 <-
    glm_herbivory_damage_emmeans_Spec$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = Spec,
        y = response,
        col = Spec,
        fill = Spec)) +
    
    geom_point(
      data = dataset_herbivory_damage,
      aes(y = herbivory_percentage_mean),
      alpha = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL,
        ymax = upper.CL),
      width=0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size=1) +
    
    geom_point(
      shape = 0,
      size = 3,
      position = position_dodge(width = 0.5))+
    
    labs(
      x = "Habitat",
      y = "Mean herbivory damage (%)" )+
    scale_fill_manual(values = pallete_3)+
    scale_color_manual(values = pallete_3)+
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))

# save pdf
ggsave(
  "fig/herbivory_damage/model_plot_02.pdf",
  model_plot_02,
  width = PDF_width,
  height = PDF_height,
  units = "in")


# calculate emmeans Treatment
glm_herbivory_damage_emmeans_Treatment <-
  emmeans(
    glm_herbivory_damage_select,
    pairwise ~ Treatment,
    type = "response") 

# save the pairwise test 
glm_herbivory_damage_emmeans_Treatment$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/herbivory_pairwise_Treatment_contrast.csv")

glm_herbivory_damage_emmeans_Treatment$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/herbivory_pairwise_Treatment_emmeans.csv")

(model_plot_03 <-
    glm_herbivory_damage_emmeans_Treatment$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = Treatment,
        y = response,
        col = Treatment,
        fill = Treatment)) +
    
    geom_point(
      data = dataset_herbivory_damage,
      aes(y = herbivory_percentage_mean),
      alpha = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL,
        ymax = upper.CL),
      width=0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size=1) +
    
    geom_point(
      shape = 0,
      size = 3,
      position = position_dodge(width = 0.5))+
    
    labs(
      x = "Habitat",
      y = "Mean herbivory damage (%)" )+
    scale_fill_manual(values = pallete_1)+
    scale_color_manual(values = pallete_1)+
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))

# save pdf
ggsave(
  "fig/herbivory_damage/model_plot_03.pdf",
  model_plot_03,
  width = PDF_width,
  height = PDF_height,
  units = "in")



