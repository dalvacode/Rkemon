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
  tabyl(gen, type_1) %>% select(-NA_) %>%
  select(-1) %>% as.matrix()

mat2 <- df %>%
  distinct(name, type_2, .keep_all = T) %>%
  tabyl(gen, type_2) %>% select(-NA_) %>%
  select(-1) %>% as.matrix()
dim(mat1) == dim(mat2)

tab_type_ind <- mat1+mat2
tab_type_ind <- t(tab_type_ind)
colnames(tab_type_ind) <- tabyl(df, gen)[[1]]
tab_type_ind

tab_type_ind_html <- tab_type_ind %>%
  as_tibble(rownames = "Type") %>%
  adorn_totals(where = "col") %>%
  gt %>%
  tab_spanner(label = "Generation", columns = c(2:11)) %>%
  cols_align(columns = c(2:11), align = "center") %>%
  cols_label(H = "H*") %>%
  data_color(
    columns = c(2:11),
    colors = scales::col_numeric(
      palette = pals::cividis(n = 10),#pals::ocean.haline(n = 10),
      reverse = T,
      domain = NULL
    ), #alpha = 0.85
  ) %>%
  # data_color(
  #   columns = c(12),
  #   colors = scales::col_numeric(
  #     palette = pals::cividis(n = 20)[1:10],#pals::ocean.haline(n = 10),
  #     reverse = T,
  #     domain = NULL
  #   ), #alpha = 0.85
  # ) %>%
  tab_source_note(source_note = "*Hisui")

gtsave(tab_type_ind_html, filename = "out/Types by generation (n of pokémons).html")

### Types (line-wise)
mat1 <- df %>% 
  distinct(line, type_1, .keep_all = T) %>%
  tabyl(gen, type_1) %>% select(-NA_) %>%
  select(-1) %>% as.matrix()

mat2 <- df %>%
  distinct(line, type_2, .keep_all = T) %>%
  tabyl(gen, type_2) %>% select(-NA_) %>%
  select(-1) %>% as.matrix()
dim(mat1) == dim(mat2)

tab_type_gen <- mat1+mat2
tab_type_gen <- t(tab_type_gen)
colnames(tab_type_gen) <- tabyl(df, gen)[[1]]
tab_type_gen

input <- tab_type_gen %>% as_tibble(rownames = "Type") %>% adorn_totals(where = "col") %>% gt

input %>% data_color(
  columns = c(2:11),
  colors = scales::col_numeric(
    palette = pals::cividis(n = 10),#pals::ocean.haline(n = 10),
    reverse = T,
    domain = NULL
  ),
  #alpha = 0.85
)

