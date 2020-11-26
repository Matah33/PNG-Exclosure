#----------------------------------------------------------#
#
#
#                Exclosure experiment PNG
#
#                     Arthropod abundace
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
library(mgcv)

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

dataset_abundance <- 
  dataset_fin %>% 
  mutate(abundance_per_leaf_area = total_inv / leaf_area_total )


#----------------------------------------------------------#
# 3. graphical properties definition  -----
#----------------------------------------------------------#

theme_set(theme_classic())
text_size <-  10

PDF_width <-  10
PDF_height <-  6

abundance_log_breaks <-  c(0,1,10,100,
                           paste0("1e",seq(1:15)) %>% 
                             noquote()) %>%  
  as.numeric()

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
# 4. Abundance  exploratory figures -----
#----------------------------------------------------------#

# per treatmetns
ext_plot_01 <- 
  dataset_abundance %>% 
  ggplot(
    aes(
      x = Treatment,
      y = abundance_per_leaf_area,
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
    y = expression(paste("Arthropod abundance per m" ^ 2))) +
  scale_fill_manual(values = pallete_1) +
  scale_y_continuous(trans = "log1p",
                     breaks = abundance_log_breaks)+
  theme(
    text = element_text(size = text_size),
    legend.position = "none"
  )

plot(ext_plot_01)


ggsave(
  "fig/arthropod_abundance/ext_plot_01.pdf",
  ext_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# per habitat
ext_plot_02 <- 
  dataset_abundance %>% 
  ggplot(
    aes(
      x = Hab,
      y = abundance_per_leaf_area,
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
    y = expression(paste("Arthropod abundance per m" ^ 2)) )+
  scale_fill_manual(values = pallete_2) +
  scale_y_continuous(trans = "log1p",
                     breaks = abundance_log_breaks) +
  theme(
    text = element_text(size = text_size),
    legend.position = "none")

plot(ext_plot_02)

ggsave(
  "fig/arthropod_abundance/ext_plot_02.pdf",
  ext_plot_02,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# combination
ext_plot_03 <- 
  dataset_abundance %>% 
  ggplot(
    aes(
      x = Hab,
      y = abundance_per_leaf_area,
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
    y = expression(paste("Arthropod abundance per m" ^ 2)) )+
  scale_fill_manual(values = pallete_1) +
  scale_y_continuous(trans = "log1p",
                     breaks = abundance_log_breaks) +
  theme(
    text = element_text(size = text_size),
    legend.position = "right")

plot(ext_plot_03)

ggsave(
  "fig/arthropod_abundance/ext_plot_03.pdf",
  ext_plot_03,
  width = PDF_width*1.2,
  height = PDF_height,
  units = "in")

# per species
ext_plot_04 <- 
  dataset_abundance %>% 
  ggplot(
    aes(
      x = Spec,
      y = abundance_per_leaf_area,
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
    y = expression(paste("Arthropod abundance per m" ^ 2)) )+
  scale_fill_manual(values = pallete_3) +
  scale_y_continuous(trans = "log1p",
                     breaks = abundance_log_breaks)+
  theme(
    text = element_text(size = text_size),
    legend.position = "none")

plot(ext_plot_04)

ggsave(
  "fig/arthropod_abundance/ext_plot_04.pdf",
  ext_plot_04,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# full
ext_plot_05 <- 
  dataset_abundance %>% 
  ggplot(
    aes(
      x = Spec,
      y = abundance_per_leaf_area,
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
    y = expression(paste("Arthropod abundance per m" ^ 2)) )+
  scale_fill_manual(values = pallete_1) +
  scale_y_continuous(trans = "log1p",
                     breaks = abundance_log_breaks) +
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
  "fig/arthropod_abundance/ext_plot_05.pdf",
  ext_plot_05,
  width = PDF_width*1.2,
  height = PDF_height,
  units = "in")


#----------------------------------------------------------#
# 5. Model build -----
#----------------------------------------------------------#

dataset_abundance_model <-
  dataset_abundance %>% 
  mutate_if(is.character,as.factor)

# cretae full model with all interaction
glm_arthropod_abundance_full <-
  gam(
    abundance_per_leaf_area ~ Hab*Treatment*Spec,
    data = dataset_abundance_model,
    family = tw(),
    method = "REML",
    na.action = "na.fail")


summary(glm_arthropod_abundance_full)
check_collinearity(glm_arthropod_abundance_full)
check_heteroscedasticity(glm_arthropod_abundance_full) 

# data hae high Heteroscedasticity -> transform
glm_arthropod_abundance_full <-
  gam(
    log(abundance_per_leaf_area+1) ~ Hab*Treatment*Spec,
    data = dataset_abundance_model,
    family = tw(),
    method = "REML",
    na.action = "na.fail")

summary(glm_arthropod_abundance_full)
check_collinearity(glm_arthropod_abundance_full)
check_heteroscedasticity(glm_arthropod_abundance_full)

# compute all posible combinations
glm_arthropod_abundance_dd <- MuMIn::dredge(glm_arthropod_abundance_full,
                                         trace = TRUE)

# observe the best model
glm_arthropod_abundance_dd

# save result table
glm_arthropod_abundance_dd %>% 
  as_tibble() %>% 
  write_csv("data/output/arthropod_abundance_model_result.cvs")

# two models have same AIC, lest compare R2
glm_arthropod_abundance_m1 <-
  gam(
    log(abundance_per_leaf_area+1) ~ Hab+Treatment+Spec,
    data = dataset_abundance_model,
    family = tw(),
    method = "REML",
    na.action = "na.fail")

glm_arthropod_abundance_m2 <-
  gam(
    log(abundance_per_leaf_area+1) ~ Hab*Treatment,
    data = dataset_abundance_model,
    family = tw(),
    method = "REML",
    na.action = "na.fail")

compare_performance(
  glm_arthropod_abundance_m1,
  glm_arthropod_abundance_m2)

# m2 has sligthly higher R2 -> using m2

summary(glm_arthropod_abundance_m2)
check_collinearity(glm_arthropod_abundance_m2)
check_heteroscedasticity(glm_arthropod_abundance_m2)
r2(glm_arthropod_abundance_m2)

# calculate emmeans
glm_arthropod_abundance_emmeans <-
  emmeans(
    glm_arthropod_abundance_m2,
    pairwise~Hab*Treatment,
    type = "response") 


model_plot_01 <-
  glm_arthropod_abundance_emmeans$emmeans %>% 
  as_tibble() %>% 
  ggplot(
    aes(
      x = Hab,
      y = exp(response)-1,
      col = Treatment,
      ymin =  exp(lower.CL)-1,
      ymax = exp(upper.CL)-1,
      fill = Treatment)) +
  geom_errorbar(
    width=0.2,
    position = position_dodge(width = 0.5),
    size=0.1)+
  geom_point(
    shape = 0,
    position = position_dodge(width = 0.5)) +
  labs(
    x = "Habitat",
    y = expression(paste("Estimated arthropod abundance per m" ^ 2)) ) +
  scale_color_manual(values = pallete_1) +
  scale_y_continuous(trans = "log1p",
                     breaks = abundance_log_breaks) +
  theme(
    text = element_text(size = text_size),
    legend.position = "right")

plot(model_plot_01)

# save pdf
ggsave(
  "fig/arthropod_abundance/model_plot_01.pdf",
  model_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# save the pairwise test 
glm_arthropod_abundance_emmeans$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/arthropod_abundance_pairwise_test.cvs")
