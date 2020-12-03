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

abundance_log_breaks <-  c(0,1,10,100,
                           paste0("1e",seq(1:15)) %>% 
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
# 4. Abundance  exploratory figures -----
#----------------------------------------------------------#


ext_plot_01 <- 
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
    y = "Total arthropod abundance") +
  scale_fill_manual(values = pallete_1)+
  scale_color_manual(values = pallete_1)+
  theme(
    text = element_text(size = text_size),
    legend.position = "right"
  )

plot(ext_plot_01) # does not look so good

ggsave(
  "fig/arthropod_abundance/ext_plot_01.pdf",
  ext_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")


ext_plot_02 <- 
  dataset_fin %>%
  dplyr::select(c(Hab, Spec, Treatment, Tree, leaf_area_total, CHEW, NR, PRE, SUC)) %>% 
  pivot_longer(
    cols = -c(Hab, Spec, Treatment, Tree, leaf_area_total),
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
  labs(
    x = expression(paste("Total leaf area per tree individual (m" ^ 2,")")), 
    y = "Total arthropod abundance") +
  geom_point()+
  facet_wrap(~Guild)+
  scale_fill_manual(values = pallete_4) +
  scale_color_manual(values = pallete_4) +
  theme(
    text = element_text(size = text_size),
    legend.position = "none"
  )

plot(ext_plot_02) # this looks even worse

ggsave(
  "fig/arthropod_abundance/ext_plot_02.pdf",
  ext_plot_02,
  width = PDF_width,
  height = PDF_height,
  units = "in")


dataset_abundance <- 
  dataset_fin %>% 
  mutate(abundance_per_leaf_area = Total_abundance / leaf_area_total )


# per treatmetns
ext_plot_03 <- 
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
    alpha = 1/2,
    size = 1)+
  geom_boxplot(
    width=0.2,
    outlier.shape = NA,
    col = "gray30",
    alpha = 1/2) +
  labs(
    x = "Treatment", 
    y = expression(paste("Arthropod abundance per m" ^ 2))) +
  scale_fill_manual(values = pallete_1) +
  scale_color_manual(values = pallete_1) +
  scale_y_continuous(trans = "log1p",
                     breaks = abundance_log_breaks)+
  theme(
    text = element_text(size = text_size),
    legend.position = "none"
  )

plot(ext_plot_03)


ggsave(
  "fig/arthropod_abundance/ext_plot_03.pdf",
  ext_plot_03,
  width = PDF_width,
  height = PDF_height,
  units = "in")

# per habitat
ext_plot_04 <- 
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
    alpha = 1/2,
    size = 1)+
  geom_boxplot(
    width=0.2,
    outlier.shape = NA,
    col = "gray30",
    alpha = 1/2) +
  labs(
    x = "Habitat",
    y = expression(paste("Arthropod abundance per m" ^ 2)) )+
  scale_color_manual(values = pallete_2) +
  scale_fill_manual(values = pallete_2) +
  scale_y_continuous(trans = "log1p",
                     breaks = abundance_log_breaks) +
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


# per species
ext_plot_05 <- 
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
    alpha = 1/2,
    size = 1)+
  geom_boxplot(
    width=0.2,
    outlier.shape = NA,
    col = "gray30",
    alpha = 1/2) +
  labs(
    x = "Ficus species",
    y = expression(paste("Arthropod abundance per m" ^ 2)) )+
  scale_fill_manual(values = pallete_3) +
  scale_color_manual(values = pallete_3) +
  scale_y_continuous(trans = "log1p",
                     breaks = abundance_log_breaks)+
  theme(
    text = element_text(size = text_size),
    legend.position = "none")

plot(ext_plot_05)

ggsave(
  "fig/arthropod_abundance/ext_plot_05.pdf",
  ext_plot_05,
  width = PDF_width,
  height = PDF_height,
  units = "in")


#----------------------------------------------------------#
# 5. Model build -----
#----------------------------------------------------------#

dataset_abundance_model <-
  dataset_abundance %>% 
  mutate_if(is.character,as.factor) %>% 
  dplyr::select( Hab, Treatment, Spec, abundance_per_leaf_area) %>% 
  drop_na()

# cretae full model with all interaction
glm_arthropod_abundance_full <-
  glm(
    abundance_per_leaf_area ~ Hab*Treatment*Spec,
    data = dataset_abundance_model,
    family = "Gamma",
    na.action = "na.fail")


summary(glm_arthropod_abundance_full)
check_model(glm_arthropod_abundance_full) # do not know why does not work
check_collinearity(glm_arthropod_abundance_full)
check_heteroscedasticity(glm_arthropod_abundance_full) 
check_normality(glm_arthropod_abundance_full)
qplot(residuals(glm_arthropod_abundance_full))

# compute all posible combinations
glm_arthropod_abundance_dd <- MuMIn::dredge(glm_arthropod_abundance_full,
                                         trace = TRUE)

# observe the best model
glm_arthropod_abundance_dd

# save result table
glm_arthropod_abundance_dd %>% 
  as_tibble() %>% 
  write_csv("data/output/arthropod_abundance_model_result.csv")

# two models have same AIC, lest compare R2
glm_arthropod_abundance_select <-
  glm(
    abundance_per_leaf_area ~ Hab + Spec + Treatment,
    data = dataset_abundance_model,
    family = "Gamma",
    na.action = "na.fail")

summary(glm_arthropod_abundance_select)
r2(glm_arthropod_abundance_select)
check_model(glm_arthropod_abundance_select)
check_collinearity(glm_arthropod_abundance_select)
check_heteroscedasticity(glm_arthropod_abundance_select)
check_normality(glm_arthropod_abundance_select)
qplot(residuals(glm_arthropod_abundance_select))

# calculate emmeans
glm_arthropod_abundance_emmeans <-
  emmeans(
    glm_arthropod_abundance_select,
    pairwise~Hab + Spec + Treatment,
    type = "response") 


model_plot_01 <-
  glm_arthropod_abundance_emmeans$emmeans %>% 
  as_tibble() %>% 
  ggplot(
    aes(
      x = Spec,
      y = response,
      col = Treatment,
      fill = Treatment)) +
  geom_point(
    data = dataset_abundance_model,
    aes(y = abundance_per_leaf_area),
    alpha = 1/2,
    position = position_jitterdodge(
      dodge.width = 0.5,
      jitter.width = 0.15)) +
  geom_errorbar(
    aes(
      ymin =  asymp.LCL,
      ymax = asymp.UCL),
    width=0.2,
    position = position_dodge(width = 0.5, preserve = "single"),
    size=1
  )+
  geom_point(
    shape = 0,
    size = 3,
    position = position_dodge(width = 0.5)) +
  facet_wrap(~Hab) + 
  labs(
    x = "Habitat",
    y = expression(paste("Arthropod abundance per m" ^ 2)) ) +
  scale_color_manual(values = pallete_1) +
  scale_fill_manual(values = pallete_1) +
  theme(
    text = element_text(size = text_size),
    legend.position = "right")

# to color facet according to Habitat  
model_plot_01_e <- ggplot_gtable(ggplot_build(model_plot_01))
stripr <- which(grepl('strip-t', model_plot_01_e$layout$name))

for (i in stripr) {
  if (all(class(model_plot_01_e$grobs[[i]]) != "zeroGrob")){
    j <- which(grepl('rect', model_plot_01_e$grobs[[i]]$grobs[[1]]$childrenOrder))
    k <- which(grepl('title', model_plot_01_e$grobs[[i]]$grobs[[1]]$childrenOrder))
    
    model_plot_01_e$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- 
      pallete_2[model_plot_01_e$grobs[[i]]$grobs[[1]]$children[[k]]$children[[1]]$label] 
  }
}

plot(model_plot_01_e)

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
  write_csv("data/output/arthropod_abundance_pairwise_test.csv")
