#### Analyses ####

library(gt)
library(naniar)
library(readxl)
library(janitor)
library(lubridate)
library(tidyverse)
`%!in%` <- negate(`%in%`)


# Read --------------------------------------------------------------------

df_pkm <- read_rds("data/backup_df_pkm.RDS")

df <- df_pkm %>%
  filter(
    extra %!in% c("Individuals", "Others")
    )


# Explore -----------------------------------------------------------------

### Missing values
# df %>% gg_miss_var(show_pct = T)
# df %>% Hmisc::describe()
# df %>% vis_miss()


### Types (pokemon-wise)
mat1 <- df %>% 
  distinct(name, type_1, .keep_all = T) %>%
  tabyl(gen, type_1) %>%
  select(-1) %>% as.matrix()

mat2 <- df %>%
  distinct(name, type_2, .keep_all = T) %>%
  tabyl(gen, type_2) %>% select(-NA_) %>%
  select(-1) %>% as.matrix()
dim(mat1) == dim(mat2)

tab_type_gen <- mat1+mat2 %>% as_tibble()
tab_type_gen %>% gt

### Types (line-wise)
mat1 <- df %>% 
  distinct(line, type_1, .keep_all = T) %>%
  tabyl(gen, type_1) %>%
  select(-1) %>% as.matrix()

mat2 <- df %>%
  distinct(line, type_2, .keep_all = T) %>%
  tabyl(gen, type_2) %>% select(-NA_) %>%
  select(-1) %>% as.matrix()
dim(mat1) == dim(mat2)

tab_type_gen <- mat1+mat2 %>% as_tibble()
tab_type_gen %>% gt






# To-do -------------------------------------------------------------------

#Git repository
#Check sex percentage of rare mons
#Add Mythical mons


