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

#----------------------------------------------------------#
# 1. Load libraries and functions -----
#----------------------------------------------------------#

# delete existing workspace to start clean
rm(list = ls())

# Package version control
library(renv)
# renv::init()
# renv::snapshot(lockfile = "data/lock/revn.lock")
renv::restore(lockfile = "data/lock/revn.lock")

# libraries
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(MuMIn)
library(emmeans)
library(glmmTMB)
library(performance)

#----------------------------------------------------------#
# 2. Import data -----
#----------------------------------------------------------#

list_files <-  list.files("data/output/")

if(any(list_files %in% "dataset_fin.csv")) {
  dataset_fin <-  read.csv("data/output/dataset_fin.csv") %>% 
    as_tibble()
} else {
  source("R/01_Data.R")
}

#----------------------------------------------------------#
# 3. graphical properties definition  -----
#----------------------------------------------------------#

theme_set(theme_classic())
text_size <-  10

PDF_width <-  10
PDF_height <-  6


# display.brewer.all()

# Treatment pallete
pallete_1 <-  brewer.pal(3,"Pastel1")
names(pallete_1) <-  
  dataset_fin$Treatment %>% 
  unique()

# habitat pallete
pallete_2 <-  brewer.pal(4,"Set2")
names(pallete_2) <-  
  dataset_fin$Hab %>% 
  unique()

# Species pallete
pallete_3 <-  brewer.pal(4,"Accent")
names(pallete_3) <-  
  dataset_fin$Spec %>% 
  unique()

# Guild pallete
pallete_4 <-  brewer.pal(4,"Set1")
names(pallete_4) <-  c("CHEW", "NR", "PRE", "SUC")


# get the flat violin geom
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

#----------------------------------------------------------#
# 4. Herbivory damage exploratory figures -----
#----------------------------------------------------------#


# per treatmetns
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
      x = "Ficus species",
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


# cretae full model with all interaction
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
    family = "beta_family",
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

# save the pairwise test 
glm_herbivory_damage_emmeans_hab$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/herbivory_damage_pairwise_test_hab.csv")


# calculate emmeans Spec
glm_herbivory_damage_emmeans_Spec <-
  emmeans(
    glm_herbivory_damage_select,
    pairwise ~ Spec,
    type = "response") 

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

# save the pairwise test 
glm_herbivory_damage_emmeans_Spec$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/herbivory_damage_pairwise_test_Spec.csv")


# calculate emmeans Treatment
glm_herbivory_damage_emmeans_Treatment <-
  emmeans(
    glm_herbivory_damage_select,
    pairwise ~ Treatment,
    type = "response") 

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

# save the pairwise test 
glm_herbivory_damage_emmeans_Treatment$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/herbivory_damage_pairwise_test_Treatment.csv")

