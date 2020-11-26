#----------------------------------------------------------#
#
#
#                Exclosure experiment PNG
#
#                       Leaf area
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
pallete_1 <-  brewer.pal(3,"Set1")
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

#----------------------------------------------------------#
# 4. Leaf area exporatory figures -----
#----------------------------------------------------------#

# per treatmetns
ext_plot_01 <- 
  dataset_fin %>% 
  ggplot(
    aes(
      x = Treatment,
      y = leaf_area_total,
      fill = Treatment )) +
  geom_violin(
    col = "gray30",
    alpha = 1/2,
    trim = FALSE) +
  geom_boxplot(
    width=0.1,
    col = "gray30")+
  labs(
    x = "Treatment", 
    y = expression(paste("Total leaf area per tree individual (m" ^ 2,")"))) +
  scale_fill_manual(values = pallete_1) +
  theme(
    text = element_text(size = text_size),
    legend.position = "none"
  )

plot(ext_plot_01)


ggsave(
  "fig/leaf_area/ext_plot_01.pdf",
  ext_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# per habitat
ext_plot_02 <- 
  dataset_fin %>% 
  ggplot(
    aes(
      x = Hab,
      y = leaf_area_total,
      fill = Hab )) +
  geom_violin(
    col = "gray30",
    alpha = 1/2,
    trim = FALSE) +
  geom_boxplot(
    width=0.1,
    col = "gray30") +
  labs(
    x = "Habitat",
    y = expression(paste("Total leaf area per tree individual (m" ^ 2,")")) )+
  scale_fill_manual(values = pallete_2)+
  theme(
    text = element_text(size = text_size),
    legend.position = "none")

plot(ext_plot_02)

ggsave(
  "fig/leaf_area/ext_plot_02.pdf",
  ext_plot_02,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# combination
ext_plot_03 <- 
  dataset_fin %>% 
  ggplot(
    aes(
      x = Hab,
      y = leaf_area_total,
      fill = Treatment)) +
  geom_violin(
    col = "gray30",
    alpha = 1/2,
    trim = FALSE,
    position = position_dodge(width = 0.5)) +
  geom_boxplot(
    width=0.1,
    col = "gray30",
    position = position_dodge(width = 0.5)) +
  labs(
    x = "Habitat",
    y = expression(paste("Total leaf area per tree individual (m" ^ 2,")")) )+
  scale_fill_manual(values = pallete_1)+
  theme(
    text = element_text(size = text_size),
    legend.position = "right")

plot(ext_plot_03)

ggsave(
  "fig/leaf_area/ext_plot_03.pdf",
  ext_plot_03,
  width = PDF_width*1.2,
  height = PDF_height,
  units = "in")

# per species
ext_plot_04 <- 
  dataset_fin %>% 
  ggplot(
    aes(
      x = Spec,
      y = leaf_area_total,
      fill = Spec )) +
  geom_violin(
    col = "gray30",
    alpha = 1/2,
    trim = FALSE) +
  geom_boxplot(
    width=0.1,
    col = "gray30") +
  labs(
    x = "Ficus species",
    y = expression(paste("Total leaf area per tree individual (m" ^ 2,")")) )+
  scale_fill_manual(values = pallete_3)+
  theme(
    text = element_text(size = text_size),
    legend.position = "none")

plot(ext_plot_04)

ggsave(
  "fig/leaf_area/ext_plot_04.pdf",
  ext_plot_04,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# full
ext_plot_05 <- 
  dataset_fin %>% 
  ggplot(
    aes(
      x = Spec,
      y = leaf_area_total,
      fill = Treatment)) +
  geom_violin(
    col = "gray30",
    alpha = 1/2,
    trim = FALSE,
    position = position_dodge(width = 0.5)) +
  geom_boxplot(
    width=0.1,
    col = "gray30",
    position = position_dodge(width = 0.5)) +
  facet_wrap(~Hab,
             scales = "free_x")+
  labs(
    x = "Habitat",
    y = expression(paste("Total leaf area per tree individual (m" ^ 2,")")) )+
  scale_fill_manual(values = pallete_1)+
  theme(
    text = element_text(size = text_size),
    legend.position = "right") 

# to color facet according to Habitat  
ext_plot_05_e <- ggplot_gtable(ggplot_build(ext_plot_05))
stripr <- which(grepl('strip-t', ext_plot_05_e$layout$name))

for (i in stripr) {
  if (all(class(ext_plot_05_e$grobs[[i]]) != "zeroGrob")){
    j <- which(grepl('rect', ext_plot_05_e$grobs[[i]]$grobs[[1]]$childrenOrder))
    k <- which(grepl('title', ext_plot_05_e$grobs[[i]]$grobs[[1]]$childrenOrder))
    
    ext_plot_05_e$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- 
      pallete_2[ext_plot_05_e$grobs[[i]]$grobs[[1]]$children[[k]]$children[[1]]$label] 
  }
}

plot(ext_plot_05_e)

ggsave(
  "fig/leaf_area/ext_plot_05.pdf",
  ext_plot_05,
  width = PDF_width*1.2,
  height = PDF_height,
  units = "in")


#----------------------------------------------------------#
# 5. Model build -----
#----------------------------------------------------------#

dataset_leaf_area <-
  dataset_fin %>% 
  mutate_if(is.character,as.factor)

# cretae full model with all interaction
glm_leaf_area_full <-
  glm(leaf_area_total ~ Hab*Treatment*Spec,
      data=dataset_leaf_area,
      family = "Gamma",
      na.action = "na.fail")

summary(glm_leaf_area_full)
check_collinearity(glm_leaf_area_full)
check_normality(glm_leaf_area_full)
check_heteroscedasticity(glm_leaf_area_full)

# compute all posible combinations
glm_leaf_area_dd <- MuMIn::dredge(glm_leaf_area_full)

# observe the best model
glm_leaf_area_dd

# save result table
glm_leaf_area_dd %>% 
  as_tibble() %>% 
  write_csv("data/output/leaf_area_model_result.csv")

# fit the best model (Hab,Treatment,Hab:Treatment)
glm_leaf_area_select <-
  glm(leaf_area_total ~ Hab*Treatment,
      data=dataset_fin,
      family = "Gamma",
      na.action = "na.fail")

summary(glm_leaf_area_select)
check_collinearity(glm_leaf_area_select)
check_normality(glm_leaf_area_select)
check_heteroscedasticity(glm_leaf_area_select)
r2(glm_leaf_area_select)

# calculate emmeans
glm_leaf_area_emmeans <-
  emmeans(
    glm_leaf_area_select,
    pairwise~Hab*Treatment,
    type = "response") 

model_plot_01 <- 
glm_leaf_area_emmeans$emmeans %>% 
  as_tibble() %>% 
  ggplot(
    aes(
      x = Hab,
      y = response,
      col = Treatment,
      ymin =  asymp.LCL,
      ymax = asymp.UCL)) + 
  geom_errorbar(
    width=0.2,
    position = position_dodge(width = 0.5),
    size=0.1)+
  geom_point(
    shape = 0,
    position = position_dodge(width = 0.5)) +
  labs(
    x = "Habitat",
    y = expression(paste("Estimated total leaf area per tree individual (m" ^ 2,")")) ) +
  scale_color_manual(values = pallete_1) +
  theme(
    text = element_text(size = text_size),
    legend.position = "right")

# save pdf
ggsave(
  "fig/leaf_area/model_plot_01.pdf",
  model_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# save the pairwise test 
glm_leaf_area_emmeans$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/leaf_area_pairwise_test.csv")


