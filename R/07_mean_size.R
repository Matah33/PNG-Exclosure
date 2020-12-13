#----------------------------------------------------------#
#
#
#                Exclosure experiment PNG
#
#                   artropods size change
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
library(glmmTMB)
library(MuMIn)
library(emmeans)
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

abundance_log_breaks <-  
  c(0,1,10,100,
    paste0("1e", seq(1:15)) %>% 
      noquote()) %>%  
  as.numeric()

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
# 4. Exporatory figures -----
#----------------------------------------------------------#

# per treatmetns
(ext_plot_01 <- 
   dataset_fin %>% 
   ggplot(
     aes(
       x = Treatment,
       y = Mean_size,
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
     size = 1) +
   
   geom_boxplot(
     width=0.2,
     outlier.shape = NA,
     col = "gray30",
     alpha = 1) +
   
   labs(
     x = "Treatment", 
     y = "Mean size of arthropod") +
   scale_fill_manual(values = pallete_1) +
   scale_color_manual(values = pallete_1) +
   theme(
     text = element_text(size = text_size),
     legend.position = "none"))

ggsave(
  "fig/arthropod_size/ext_plot_01.pdf",
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
        y = Mean_size,
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
      y = "Mean size of arthropod" )+
    scale_fill_manual(values = pallete_2)+
    scale_color_manual(values = pallete_2)+
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))

ggsave(
  "fig/arthropod_size/ext_plot_02.pdf",
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
        y = Mean_size,
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
      y = "Mean size of arthropod" )+
    scale_fill_manual(values = pallete_3)+
    scale_color_manual(values = pallete_3)+
    theme(
      text = element_text(size = text_size),
      legend.position = "none"))

ggsave(
  "fig/arthropod_size/ext_plot_03.pdf",
  ext_plot_03,
  width = PDF_width,
  height = PDF_height,
  units = "in")


#----------------------------------------------------------#
# 5. Model build -----
#----------------------------------------------------------#

dataset_mean_size <-
  dataset_fin %>% 
  mutate_if(is.character,as.factor) %>% 
  dplyr::select( Hab, Treatment, Spec, TreeID, Mean_size) %>% 
  drop_na() %>% 
  mutate_if(is.character,as.factor)


summary(dataset_mean_size)

# cretae full model with all interaction
glm_mean_size_full <-
  glm(Mean_size ~ Hab * Treatment * Spec,
      data = dataset_mean_size,
      family = Gamma(),
      na.action = "na.fail")

summary(glm_mean_size_full)
check_model(glm_mean_size_full) # do not know why it does not work
check_distribution(glm_mean_size_full)
model_performance(glm_mean_size_full)
qplot(residuals(glm_mean_size_full))
check_normality(glm_mean_size_full)
check_heteroscedasticity(glm_mean_size_full)

# compute all posible combinations
glm_mean_size_dd <- 
  MuMIn::dredge(
    glm_mean_size_full,
    trace = T)

# save result table
glm_mean_size_dd %>% 
  as_tibble() %>% 
  write_csv("data/output/mean_size_model_result.csv")

# observe the best model
glm_mean_size_dd %>% 
  as_tibble() %>% 
  filter(delta < 2 ) %>% 
  View()

# fit the all the models with similar parsimony
glm_leaf_m1 <- 
  glm(Mean_size ~ Hab + Treatment,
      data = dataset_mean_size,
      family = Gamma(),
      na.action = "na.fail")

glm_leaf_m2 <- 
  glm(Mean_size ~ Hab + Spec + Treatment,
      data = dataset_mean_size,
      family = Gamma(),
      na.action = "na.fail")

glm_leaf_m3 <- 
  glm(Mean_size ~  Treatment,
      data = dataset_mean_size,
      family = Gamma(),
      na.action = "na.fail")

glm_leaf_m4 <- 
  glm(Mean_size ~ Spec + Treatment,
      data = dataset_mean_size,
      family = Gamma(),
      na.action = "na.fail")


# compare models
compare_performance(
  glm_leaf_m1, glm_leaf_m2, glm_leaf_m3, glm_leaf_m4,
  rank = TRUE)

# m1 is better
glm_mean_size_select <- glm_leaf_m2

summary(glm_mean_size_select)
check_model(glm_mean_size_select)
model_performance(glm_mean_size_select)
check_heteroscedasticity(glm_mean_size_select)
check_normality(glm_mean_size_select)
qplot(residuals(glm_mean_size_select))


# calculate emmeans
glm_mean_size_emmeans <-
  emmeans(
    glm_mean_size_select,
    pairwise ~ Treatment,
    type = "response") 

(model_plot_01 <- 
    glm_mean_size_emmeans$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = Treatment,
        y = response,
        col = Treatment)) + 
    
    geom_point(
      data = dataset_mean_size,
      aes(y = Mean_size),
      alpha = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  asymp.LCL,
        ymax = asymp.UCL),
      width=0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size = 1)+
    
    geom_point(
      shape = 0,
      position = position_dodge(width = 0.5),
      size = 3) +
    
    labs(
      x = "Habitat",
      y = "Mean size of arthropod (mm)" ) +
    scale_color_manual(values = pallete_1) +
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))

# save pdf
ggsave(
  "fig/arthropod_size/model_plot_01.pdf",
  model_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# save the pairwise test 
glm_mean_size_emmeans$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/mean_size_pairwise_test.csv")
