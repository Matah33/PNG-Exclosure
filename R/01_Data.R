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

dataset_arth <-  
  readxl::read_xlsx("data/input/PNGmerge.xlsx")  %>% 
  mutate(Tree = as.character(Tree))

dataset_leaf <-  
  readxl::read_xlsx("data/input/LeafHerbivory.xlsx") %>% 
  rename(
    Age_Leaf = `Age Leaf`,
    Ideal_area = `Ideal Area`) %>% 
  mutate(Tree = as.character(Tree))

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

dataset_fin <-
  dataset_arth %>% 
  left_join(
    dataset_leaf_sum,
    by = c("Plot", "Treatment", "Tree")
  )

#----------------------------------------------------------#
# 3. save data  -----
#----------------------------------------------------------#

write_csv(
  dataset_fin,
  "data/output/dataset_fin.csv")
