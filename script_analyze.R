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


# Explore missing values --------------------------------------------------

### Missing values
# df %>% gg_miss_var(show_pct = T)
# df %>% Hmisc::describe()
# df %>% vis_miss()



# Tab types x gen, pokémon-wise -------------------------------------------

input <- df_pkm %>%
  filter(
    extra %!in% c("Individuals", "Others", "Mega"),
    !str_detect(as.character(id), "\\.5")
    )

mat1 <- input %>% 
  distinct(name, regional, type_1, .keep_all = T) %>%
  tabyl(gen, type_1) %>% select(-NA_) %>%
  select(-1) %>% as.matrix()

mat2 <- input %>%
  distinct(name, regional, type_2, .keep_all = T) %>%
  tabyl(gen, type_2) %>% select(-NA_) %>%
  select(-1) %>% as.matrix()
dim(mat1) == dim(mat2)

tab_type_ind <- mat1+mat2
tab_type_ind <- t(tab_type_ind)
colnames(tab_type_ind) <- tabyl(input, gen)[[1]]
tab_type_ind

tab_type_ind_html <- tab_type_ind %>%
  as_tibble(rownames = "Type") %>%
  adorn_totals(where = "col") %>%
  gt %>%
  tab_spanner(label = "Generation", columns = c(2:11)) %>%
  cols_align(columns = c(2:11), align = "center") %>%
  cols_label(H = "H*") %>%
  data_color(
    columns = c(2:12),
    colors = scales::col_numeric(
      #palette = pals::coolwarm(n = 3),
      palette = pals::cividis(n = 10),
      #pals::ocean.haline(n = 10),
      reverse = T,
      domain = NULL #c(0, max(tab_type_ind))
    ),
    #alpha = 0.85
  ) %>%
  tab_style(
    style = list(
      #cell_fill(color = "lightcyan"),
      cell_text(weight = "bold")
      ),
    locations = list(
      cells_body(columns = c(Type, Total)),
      cells_column_labels(),
      cells_column_spanners()
      )
    ) %>%
  cols_width(
    c(str_c(1:9), "H") ~ px(50),
    Total ~ px(80)
  ) %>%
  tab_header(
    title = html("<b>Frequency of each type by generation</b>"),
    subtitle = html("<b>(based on individual pokémons)</b>")
  ) %>%
  tab_source_note(source_note = html(
    "*Hisui<br>
    Color intensity by column."))

list_cols <- c(str_c(1:9), "H")
for (i in seq_along(list_cols)) {
  tab_type_ind_html <- tab_type_ind_html %>%
    tab_style(
      style = list(cell_text(color = "red", weight = "bold")),
      locations = cells_body(
        columns = list_cols[i],
        rows = tab_type_ind_html$`_data`[[list_cols[i]]] <= 1
      )
    )
}

tab_type_ind_html

gtsave(tab_type_ind_html, filename = "out/Types by generation (n of pokémons).html")



# Tab types x gen, line-wise ----------------------------------------------

input <- df_pkm %>%
  filter(
    extra %!in% c("Individuals", "Others", "Mega")
    )

midput <- input %>%
  group_by(line, gen) %>%
  summarize(
    across(starts_with("d_"), ~ sum(.x, na.rm = T)),
    across(starts_with("d_"), ~ if_else(.x > 0, 1, .x))
    )

output <- midput %>%
  group_by(gen) %>%
  summarize(
    across(starts_with("d_"), ~ sum(.x, na.rm = T))
  )

tab_type_line <- output %>%
  column_to_rownames(var = "gen") %>%
  t()

tab_type_line_html <- tab_type_line %>%
  as_tibble(rownames = "Type") %>%
  mutate(Type = str_replace(Type, "d_", "") %>% str_to_sentence()) %>%
  adorn_totals(where = "col") %>%
  gt %>%
  tab_spanner(label = "Generation", columns = c(2:11)) %>%
  cols_align(columns = c(2:11), align = "center") %>%
  cols_label(H = "H*") %>%
  data_color(
    columns = c(2:12),
    colors = scales::col_numeric(
      #palette = pals::coolwarm(n = 3),
      #palette = pals::cividis(n = 10),
      pals::ocean.haline(n = 30),
      reverse = T,
      domain = NULL
      #domain = c(0, max(tab_type_line))
    ),
    #alpha = 0.85
  ) %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = list(
      cells_body(columns = c(Type, Total)),
      cells_column_labels(),
      cells_column_spanners()
    )
  ) %>%
  cols_width(
    c(str_c(1:9), "H") ~ px(50),
    Total ~ px(80)
    ) %>%
  tab_header(
    title = html("<b>Frequency of each type by generation</b>"),
    subtitle = html("<b>(based on evolution lines)</b>")
    ) %>%
  tab_source_note(
    source_note = html(
      "*Hisui.<br>
      Lines present across generations are over-represented (e.g. Pikachu in gen 1 and Pichu in gen 2).<br>
      Color intensity by column."))

list_cols <- c(str_c(1:9), "H")
for (i in seq_along(list_cols)) {
  tab_type_line_html <- tab_type_line_html %>%
    tab_style(
      style = list(cell_text(color = "red", weight = "bold")),
      locations = cells_body(
        columns = list_cols[i],
        rows = tab_type_line_html$`_data`[[list_cols[i]]] <= 1
        )
      )
}

tab_type_line_html

gtsave(tab_type_line_html, filename = "out/Types by generation (n of evo lines).html")

