#----------------------------------------------------------#
#
#
#                Exclosure experiment PNG
#
#                         Data
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

#----------------------------------------------------------#
# 2. Import data and merge -----
#----------------------------------------------------------#

dataset_tree_spec <-  
  readxl::read_xlsx("data/input/Tree_main.xlsx")  %>% 
  mutate(Tree = as.character(Tree))

dataset_leaf <-  
  readxl::read_xlsx("data/input/LeafHerbivory.xlsx") %>% 
  rename(
    Age_Leaf = `Age Leaf`,
    Ideal_area = `Ideal Area`) %>% 
  mutate(Tree = as.character(Tree))

# sum leaf area per tree
dataset_leaf_sum <-
  dataset_leaf %>% 
    mutate(Percentage = (Herbivory/Ideal_area)*100) %>% 
    group_by(Plot, Treatment, Tree) %>% 
    summarise(
      .groups = "keep",
      leaf_area_total = sum(Ideal_area)/10e3,
      herbivory_percentage_median = median(Percentage),
      herbivory_percentage_mean = mean(Percentage)
    )

dataset_arth <-  
  readxl::read_xlsx("data/input/INV_clean.xlsx") %>% 
  rename(Tree = Sample,
         Size = 'Size (mm)') %>% 
  mutate(Tree = as.character(Tree))

# arthropod abunance
dataset_arth_sum_abund <-
  dataset_arth %>% 
  group_by(Site, Treatment, Tree, Guild) %>% 
  summarise(
    .groups = "keep",
    Abundance = n()
  ) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Guild, values_from = Abundance) %>% 
  replace(is.na(.), 0)
  
dataset_arth_sum_abund <-
  dataset_arth_sum_abund %>% 
  mutate(Total_abundance = dataset_arth_sum_abund %>% 
           dplyr::select(
             dataset_arth$Guild %>% 
               unique()) %>% 
           rowSums())

# artropod sizes
dataset_arth_mean_size_guild <-
  dataset_arth %>% 
  group_by(Site, Treatment, Tree, Guild) %>% 
  summarise(
    .groups = "keep",
    Mean_size = mean(Size)
  ) %>% 
  ungroup() %>%
  mutate(Guild = paste0("size_",Guild)) %>% 
  pivot_wider(names_from = Guild, values_from = Mean_size)

dataset_arth_mean_size_total <-
dataset_arth %>% 
  group_by(Site, Treatment, Tree) %>% 
  summarise(
    .groups = "keep",
    Mean_size = mean(Size)
  ) %>% 
  ungroup()


# merging artropods data
dataset_arth_final <-
  dataset_arth_sum_abund %>% 
  left_join(
    dataset_arth_mean_size_guild,
    by = c("Site", "Treatment", "Tree")) %>% 
  left_join(
    dataset_arth_mean_size_total,
    by = c("Site", "Treatment", "Tree")) %>% 
  mutate(
    Plot = case_when(
      Site == "Baiteta_" ~ "BAI",
      Site == "Wanang 1_" ~ "WA1",
      Site == "Wanang 3_" ~ "WA3",
      Site == "YAW_" ~ "YAW"
    )
  )


# merge all together
dataset_fin <-
  dataset_tree_spec %>% 
  left_join(
    dataset_leaf_sum,
    by = c("Plot", "Treatment", "Tree")) %>% 
  left_join(
    dataset_arth_final,
    by = c("Plot", "Treatment", "Tree") 
  )



#----------------------------------------------------------#
# 3. save data  -----
#----------------------------------------------------------#

write_csv(
  dataset_fin,
  "data/output/dataset_fin.csv")
