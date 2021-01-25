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
# 1. Import data -----
#----------------------------------------------------------#

# 1.1 desing data-----
dataset_desing <-  
  readxl::read_xlsx("data/input/data_input_clean.xlsx","Design_data")  %>% 
  mutate(TreeID = as.character(TreeID))


# calculate total tree leaf area
dataset_desing <-  
  dataset_desing %>% 
  mutate(
    LA_cm = LeafAreaF1 / 10e3,
    LA_W_ratio = LA_cm / WeightFrame,
    leaf_area_total = WeightTot * LA_W_ratio
  ) 

# 1.2 herbivory data -----
dataset_herbivory <-  
  readxl::read_xlsx("data/input/data_input_clean.xlsx","Leaf_frames")  %>% 
  rename(
    Age_Leaf = `Age Leaf`) %>% 
  mutate(TreeID = as.character(Tree_ID)) %>% 
  dplyr::select(-Tree_ID)

# sum herbivory per tree
dataset_herbivory_sum <-
  dataset_herbivory %>% 
    mutate(
      Percentage = (HerbivoryArea / LeafAreaIdeal) * 100) %>%  # recalculate 
    group_by(Plot, Treatment, TreeID) %>% 
    summarise(
      .groups = "keep",
    #  leaf_area_total = sum(LeafAreaIdeal)/10e3,
      herbivory_percentage_median = median(Percentage),
      herbivory_percentage_mean = mean(Percentage)
    )

write_csv(
  dataset_desing,
  "data/output/dataset_desing.csv")

# 1.3 invertebrates data -----
dataset_invertebrates <-  
  readxl::read_xlsx("data/input/data_input_clean.xlsx","Invertebrates")  %>% 
  rename(
    Size = 'Size (mm)') %>% 
  mutate(
    TreeID = as.character(TreeID),
    Plot = case_when(
      Plot == "BAI" ~ "BAI", 
      Plot == "WAN1" ~ "WA1",
      Plot == "WAN3" ~ "WA3",
      Plot == "YAW" ~ "YAW" 
    ))



# invertebrates abunance
dataset_invertebrates_sum_abund <-
  dataset_invertebrates %>% 
  group_by(Plot, Treatment, TreeID, Guild) %>% 
  summarise(
    .groups = "keep",
    Abundance = n()
  ) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Guild, values_from = Abundance) %>% 
  replace(is.na(.), 0)


# The rest is the same;

dataset_invertebrates_sum_abund <-
  dataset_invertebrates_sum_abund %>% 
  mutate(Total_abundance = dataset_invertebrates_sum_abund %>% 
           dplyr::select(
             dataset_invertebrates$Guild %>% 
               unique()) %>% 
           rowSums())

# artropod sizes
dataset_invertebrates_mean_size_guild <-
  dataset_invertebrates %>% 
  group_by(Plot, Treatment, TreeID, Guild) %>% 
  summarise(
    .groups = "keep",
    Mean_size = mean(Size)
  ) %>% 
  ungroup() %>%
  mutate(Guild = paste0("size_",Guild)) %>% 
  pivot_wider(names_from = Guild, values_from = Mean_size)

dataset_invertebrates_mean_size_total <-
dataset_invertebrates %>% 
  group_by(Plot, Treatment, TreeID) %>% 
  summarise(
    .groups = "keep",
    Mean_size = mean(Size)
  ) %>% 
  ungroup()

# problem with NA creating rows so again manualy



# merging artropods data
dataset_invertebrates_final <-
  dataset_invertebrates_sum_abund %>% 
  left_join(
    dataset_invertebrates_mean_size_guild,
    by = c("Plot", "Treatment", "TreeID")) %>% 
  left_join(
    dataset_invertebrates_mean_size_total,
    by = c("Plot", "Treatment", "TreeID"))

dataset_invertebrates_final <- read.delim ("clipboard")
dataset_invertebrates_final$TreeID<-as.character(dataset_invertebrates_final$TreeID)




# 1.4 summary -----
# merge all together
dataset_fin <-
  dataset_desing %>% 
  left_join(
    dataset_herbivory_sum,
    by = c("Plot", "Treatment", "TreeID")) %>% 
  left_join(
    dataset_invertebrates_final,
    by = c("Plot", "Treatment", "TreeID") 
  )


dataset_fin[is.na(dataset_fin)] <- 0

# This created artificial values in the table, therefore I created the dataset manually->



#merged dataset_desing from "output"+ manually created inv_data
dataset_fin<-read.delim("clipboard") 

#----------------------------------------------------------#
# 2. save data  -----
#----------------------------------------------------------#

write_csv(
  dataset_fin,
  "data/output/dataset_fin.csv")

