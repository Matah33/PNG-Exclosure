#----------------------------------------------------------#
#
#
#                Exclosure experiment PNG
#
#                   invertebrates abundace
#
#             Ondrej Mottl - Marketa Tahadlova
#                         2020
#
#----------------------------------------------------------#

source("R/00_config.R")

#----------------------------------------------------------#
# 4. calculation of Abundance -----
#----------------------------------------------------------#

# exploraoty figures
(ext_plot_01 <- 
   dataset_fin %>%
   ggplot(
     aes(
       x = leaf_area_total,
       y = Total_abundance,
       col = Treatment,
       fill = Treatment)) +
   
   geom_smooth(
     method = "glm",
     formula = y ~ x,
     method.args = list(family = "poisson")) +
   
   geom_point()+
   
   labs(
     x = expression(paste("Total leaf area per tree individual (m" ^ 2,")")), 
     y = "Total invertebrates abundance") +
   scale_fill_manual(values = pallete_1)+
   scale_color_manual(values = pallete_1)+
   theme(
     text = element_text(size = text_size),
     legend.position = "right"))
 

(ext_plot_02 <- 
    dataset_fin %>%
    dplyr::select(
      Hab, Spec, Treatment, TreeID, leaf_area_total, CHEW, NR, PRE, SUC) %>% 
    pivot_longer(
      cols = -c(Hab, Spec, Treatment, TreeID, leaf_area_total),
      names_to = "Guild",
      values_to = "Abundance") %>% 
    ggplot(
      aes(
        x = leaf_area_total,
        y = Abundance,
        fill = Guild,
        col = Guild)) +
    
    geom_smooth(
      method = "glm",
      formula = y ~ x,
      method.args = list(family = "poisson")) +
    
    geom_point() +
    
    facet_wrap(~Guild)+
    
    labs(
      x = expression(paste("Total leaf area per tree individual (m" ^ 2,")")), 
      y = "Total invertebrates abundance") +
    scale_fill_manual(values = pallete_4) +
    scale_color_manual(values = pallete_4) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))

# both does not look so good !!!

# calculate abundance per leaf area
dataset_abundance <- 
  dataset_fin %>% 
  mutate(abundance_per_leaf_area = Total_abundance / leaf_area_total ) %>% 
  mutate_if(is.character,as.factor)

summary(dataset_abundance)

#----------------------------------------------------------#
# 5. Exploratory figures -----
#----------------------------------------------------------#

# per treatmetns
(ext_plot_03 <- 
  dataset_abundance %>% 
  ggplot(
    aes(
      x = Treatment,
      y = abundance_per_leaf_area,
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
    alpha = 1,
    size = 1)+
  
   geom_boxplot(
    width=0.2,
    outlier.shape = NA,
    col = "gray30",
    alpha = 1) +
  
   labs(
    x = "Treatment", 
    y = expression(paste("Invertebrates abundance per m" ^ 2))) +
  scale_fill_manual(values = pallete_1) +
  scale_color_manual(values = pallete_1) +
  theme(
    text = element_text(size = text_size),
    legend.position = "none"))

ggsave(
  "fig/invertebrates_abundance/ext_plot_03.pdf",
  ext_plot_03,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# per habitat
(ext_plot_04 <- 
    dataset_abundance %>% 
    ggplot(
      aes(
        x = Hab,
        y = abundance_per_leaf_area,
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
      y = expression(paste("Invertebrates abundance per m" ^ 2)) )+
    scale_color_manual(values = pallete_2) +
    scale_fill_manual(values = pallete_2) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))

ggsave(
  "fig/invertebrates_abundance/ext_plot_04.pdf",
  ext_plot_04,
  width = PDF_width,
  height = PDF_height,
  units = "in")


# per species
(ext_plot_05 <- 
    dataset_abundance %>% 
    ggplot(
      aes(
        x = Spec,
        y = abundance_per_leaf_area,
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
      y = expression(paste("Invertebrates abundance per m" ^ 2)) )+
    scale_fill_manual(values = pallete_3) +
    scale_color_manual(values = pallete_3) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))

ggsave(
  "fig/invertebrates_abundance/ext_plot_05.pdf",
  ext_plot_05,
  width = PDF_width,
  height = PDF_height,
  units = "in")


#----------------------------------------------------------#
# 6. Model build -----
#----------------------------------------------------------#

dataset_abundance_model <-
  dataset_abundance %>%
  dplyr::select( Hab, Treatment, Spec, abundance_per_leaf_area) %>% 
  drop_na() %>% 
  mutate_if(is.character,as.factor)

summary(dataset_abundance_model)

# cretae full model with all interaction
glm_invertebrates_abundance_full <-
  glmmTMB(
    abundance_per_leaf_area ~ Hab * Treatment * Spec,
    data = dataset_abundance_model,
    family = gaussian(link = "log"),
    na.action = "na.fail")


summary(glm_invertebrates_abundance_full)
check_model(glm_invertebrates_abundance_full)
check_distribution(glm_invertebrates_abundance_full)  
check_heteroscedasticity(glm_invertebrates_abundance_full) 
qplot(residuals(glm_invertebrates_abundance_full))

# compute all posible combinations
glm_invertebrates_abundance_dd <- 
  MuMIn::dredge(glm_invertebrates_abundance_full,
                trace = TRUE)

# save result table
glm_invertebrates_abundance_dd %>% 
  as_tibble() %>% 
  write_csv("data/output/invertebrates_abundance_model_result.csv")

# observe the best model
glm_invertebrates_abundance_dd %>% 
  as_tibble() %>% 
  filter(delta <2 ) %>% 
  View()


glm_invertebrates_abundance_select <-   
  glmmTMB(
    abundance_per_leaf_area ~ Hab + Treatment ,
    data = dataset_abundance_model,
    family = gaussian(link = "log"),
    na.action = "na.fail")

summary(glm_invertebrates_abundance_select)
check_model(glm_invertebrates_abundance_select)
check_distribution(glm_invertebrates_abundance_select)
model_performance(glm_invertebrates_abundance_select)
check_heteroscedasticity(glm_invertebrates_abundance_select)
qplot(residuals(glm_invertebrates_abundance_select))


# 6.1 Model plots  -----

# there are no interaction so have to plotted individually


# Hab
glm_invertebrates_abundance_emmeans_Hab <-
  emmeans(
    glm_invertebrates_abundance_select,
    pairwise ~ Hab,
    type = "response") 


(model_plot_01 <-
    glm_invertebrates_abundance_emmeans_Hab$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = Hab,
        y = response,
        col = Hab,
        fill = Hab)) +
    
    geom_point(
      data = dataset_abundance_model,
      aes(y = abundance_per_leaf_area),
      alpha = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL,
        ymax = upper.CL),
      position = position_dodge(width = 0.5, preserve = "single"),
      width = 0.2,
      size = 1)+
    
    geom_point(
      shape = 0,
      size = 3,
      position = position_dodge(width = 0.5))+
    
    labs(
      x = "Habitat",
      y = expression(paste("Invertebrates abundance per m" ^ 2)) ) +
    scale_color_manual(values = pallete_2) +
    scale_fill_manual(values = pallete_2) +
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))
  

# save pdf
ggsave(
  "fig/invertebrates_abundance/model_plot_01.pdf",
  model_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# save the pairwise test 
glm_invertebrates_abundance_emmeans_Hab$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/inv_abundance_pairwise_Hab_contrast.csv")


glm_invertebrates_abundance_emmeans_Hab$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/inv_abundance_pairwise_Hab_emmeans.csv")


# Treatment
glm_invertebrates_abundance_emmeans_Treatment <-
  emmeans(
    glm_invertebrates_abundance_select,
    pairwise ~ Treatment,
    type = "response") 


(model_plot_01 <-
    glm_invertebrates_abundance_emmeans_Treatment$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = Treatment,
        y = response,
        col = Treatment,
        fill = Treatment)) +
    
    geom_point(
      data = dataset_abundance_model,
      aes(y = abundance_per_leaf_area),
      alpha = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin = lower.CL,
        ymax = upper.CL),
      position = position_dodge(width = 0.5, preserve = "single"),
      width = 0.2,
      size = 1)+
    
    geom_point(
      shape = 0,
      size = 3,
      position = position_dodge(width = 0.5))+
    
    labs(
      x = "Habitat",
      y = expression(paste("Invertebrates abundance per m" ^ 2)) ) +
    scale_color_manual(values = pallete_1) +
    scale_fill_manual(values = pallete_1) +
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))


# save pdf
ggsave(
  "fig/invertebrates_abundance/model_plot_01.pdf",
  model_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# save the pairwise test 
glm_invertebrates_abundance_emmeans_Treatment$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/invertebrates_abundance_pairwise_test_Treatment.csv")

