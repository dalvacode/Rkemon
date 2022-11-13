#### POKEMON ANALYSIS ####

library(gt)
library(naniar)
library(readxl)
library(janitor)
library(lubridate)
library(tidyverse)
`%!in%` <- negate(`%in%`)

# Read --------------------------------------------------------------------

# raw_list <- read_excel("data/old/pokemon_list.xlsx", col_types = "text")

#raw_fam <- read_excel("data/pokemon_lines.xlsx", col_types = "text")
raw_lines <- read_excel("data/pokemon_lines_extended.xlsx", col_types = "text")

raw_dex <- read_delim("data/pokemon_dex.csv", delim = ",", col_types = c(.default = "c"))

#raw_dex_new <- read_excel("data/pokemon_dex_hisui.xlsx", col_types = "text")
raw_dex_new <- read_excel("data/pokemon_dex_hisui_paldea.xlsx", col_types = "text")

# Tidy evolution line -----------------------------------------------------

## Clean and filter
df_lines <- raw_lines %>%
  clean_names() %>%
  filter(!is.na(line))

df_lines <- df_lines %>%
  pivot_longer(
    cols = c(stage1, stage2, stage3),
    names_to = "test",
    values_to = "name"
    ) %>%
  distinct(species, line, name, .keep_all = T) %>%
  filter(!is.na(name)) %>%
  select(species, line, name, region_based)

# df_lines <- df_lines %>%
#   mutate(
#     regional = case_when(
#       str_detect(line, "Red Stripe|Blue Stripe") ~ NA_character_,
#       str_detect(line, "White Stripe") ~ "Hisuian",
#       str_detect(line, "\\(") ~ str_sub(
#         line,
#         str_locate(line, "\\(")[,1]+1,
#         -2
#         ),
#       TRUE ~ NA_character_
#       ),
#   )
# df_lines %>% tabyl(regional)


# Tidy dex ----------------------------------------------------------------

#### Clean, select and parse
df_dex <- raw_dex %>%
  clean_names() %>%
  bind_rows(raw_dex_new) %>% # Join Hisui and Paldea dex
  select(
    id = x1,
    ndex = pokedex_number,
    name,
    gen = generation,
    rarity = status,
    category = species,
    type_1,
    type_2,
    pc_male = percentage_male,
    egg_type_1,
    egg_type_2
  ) %>%
  mutate(
    across(c(id, ndex), ~ as.numeric(.x))
    )


#### Filter and format
df_dex <- df_dex %>%
  mutate(
    name_extended = name,
    name_extended = str_replace(name_extended, "^Hoopa ", ""),
    name = str_replace(name, "Mega |Partner |Alolan |Galarian |Hisuian |Paldean ", ""),
    name = str_replace(name, " X| Y|Black |White |Primal ", ""),
    name = case_when(
      str_detect(name, "Mr.|Jr.|Tapu|Null") ~ name,
      str_detect(name, "Rotom") ~ "Rotom",
      str_detect(name, "Necrozma") ~ "Necrozma",
      str_detect(name, "Ash-Greninja") ~ "Greninja",
      str_detect(name, "Rockruff") ~ "Rockruff",
      ndex %in% c(978:993) ~ name,
      str_detect(name, " ") ~ str_sub(name, 1, str_locate(name, " ")[,1]-1),
      TRUE ~ name
      )
    )



# Joint df ----------------------------------------------------------------

#### New variables
### Species
df_pkm <- df_lines %>%
  select(species, line, name) %>%
  left_join(x = df_dex, y = ., by = "name")


### Region
df_pkm <- df_pkm %>%
  mutate(
    regional = case_when(
      name_extended %in% c("Basculin (White Stripe)", "Basculegion") ~ "Hisuian",
      str_detect(name_extended, "Alolan|Galarian|Hisuian|Paldean") ~ str_sub(
        name_extended, 1, str_locate(name_extended, " ")[,1]-1
        ),
      # str_detect(line, "\\(") ~ str_sub(
      #   name_extended, 1, str_locate(name_extended, " ")[,1]-1
      #  ),
      TRUE ~ "Original"
      ),
    regional = if_else(regional %in% c("Basculin", "Darmanitan", "Mega"), "Original", regional)
    )

## Regional evolutions
df_pkm <- df_pkm %>%
  mutate(
    reg_evo = case_when(
      name %in% c("Perrserker", "Sirfetch'd", "Mr. Rime", "Cursola", "Obstagoon", "Runerigus") ~ "Galarian",
      name %in% c("Overqwil", "Sneasler") ~ "Hisuian",
      name %in% c("Clodsire") ~ "Paldean",
      TRUE ~ "No"
    ),
    regional = if_else(reg_evo != "No", reg_evo, regional)
  )

df_pkm %>% tabyl(regional)


### Line
## Regional lines
df_pkm <- df_pkm %>%
  filter(name %in% c("Pichu", "Pikachu", "Exeggcute", "Cubone")) %>%
  filter(name_extended != "Partner Pikachu") %>%
  mutate(
    id = id + 0.5,
    regional = "Alolan"
  ) %>%
  bind_rows(df_pkm)

df_pkm <- df_pkm %>%
  filter(name %in% c("Koffing", "Mime Jr.")) %>%
  mutate(
    id = id + 0.5,
    regional = "Galarian"
  ) %>%
  bind_rows(df_pkm)

df_pkm <- df_pkm %>%
  filter(name %in% c(
    "Cyndaquil", "Quilava", "Oshawott", "Dewott", "Rowlet", "Dartrix",
    "Petilil", "Rufflet"
    )) %>%
  mutate(
    id = id + 0.5,
    regional = "Hisuian"
  ) %>%
  bind_rows(df_pkm)

## Rest
df_pkm <- df_pkm %>%
  mutate(
    line = if_else(
      regional != "Original",
      str_c(regional, " ", species),
      species
      )
  ) %>% distinct(.keep_all = T)


tab_spec_lines <- df_pkm %>%
  group_by(species) %>%
  summarize(
    n = n(),
    n_lines = length(unique(line)),
    description = str_c(unique(line), collapse = ", ")
    )

tab_spec_lines %>% filter(n_lines > 1) %>% print(n = 50) #38 species with lines >= 2


### Extra forms
df_pkm <- df_pkm %>%
  mutate(
    extra = case_when(
      ## Specific invididuals:
      str_detect(name_extended, "^Partner |^Own Tempo|Eternamax$") ~ "Individuals", #to be excluded
      ## Temporal forms:
      str_detect(name_extended, "^Mega ") ~ "Mega",
      str_detect(name_extended, "Ash-|Aegislash Blade|^Primal |Zen Mode|School|Resolute ") ~ "Battle",
      str_detect(name_extended, "Meteor|Noice Face|Hangry Mode|Crowned |Therian |Pirouette ") ~ "Battle",
      str_detect(name_extended, "Necrozma|Calyrex ") ~ "Fusion",
      str_detect(name_extended, "Zygarde 10%|Zygarde Complete| Origin ") ~ "Others", #to be excluded
      name == "Deoxys" & !str_detect(name_extended, "Deoxys Normal") ~ "Others", #to be excluded
      #name %in% c("Shaymin", "Kyurem", "Hoopa", "Oricorio") ~ "Normal",
      TRUE ~ "Base"
      )
    )

df_pkm %>% tabyl(extra)

df_pkm %>% filter(!is.na(extra))

df_pkm %>% filter(is.na(line))


# ## Type number
# df_pkm <- df_pkm %>%
#   mutate(type_n = if_else(!is.na(type_2), 2, 1))
# df_pkm %>% tabyl(type_n)

## Sexed
df_pkm <- df_pkm %>%
  mutate(
    pc_male = as.numeric(pc_male),
    pc_male = case_when(
      species %in% c("Nincada") ~ 50,
      TRUE ~ pc_male
      )
  )

df_pkm <- df_pkm %>%
  mutate(
    sexed = case_when(
      !is.na(pc_male) ~ "Sexual",
      species %in% c("Voltorb", "Magnemite", "Staryu", "Ditto", "Porygon", "Unown",
                     "Shedinja", "Lunatone", "Solrock", "Baltoy", "Beldum", "Bronzor",
                     "Rotom", "Klink", "Cryogonal", "Golett", "Carbink", "Minior", "Dhelmise",
                     "Sinistea", "Falinks", "Dracozolt", "Arctozolt", "Dracovish", "Arctovish") ~ "Asexual",
      rarity %in% c("Legendary", "Sub Legendary", "Mythical") ~ "Unknown",
      TRUE ~ NA_character_)
    )

# Fix galarian forms
input <- df_pkm %>%
  filter(is.na(sexed)) %>%
  select(-sexed, -pc_male)
output <- df_pkm %>%
  filter(species %in% input$species) %>%
  distinct(species, .keep_all = T) %>%
  select(species, sexed, pc_male) %>%
  left_join(x = input, by = "species")

df_pkm <- df_pkm %>% filter(!is.na(sexed)) %>% bind_rows(output) %>% arrange(id)

df_pkm %>% tabyl(sexed)


### Rarity
df_pkm <- df_pkm %>%
  mutate(
    rarity = case_when(
      species %in% c("Bulbasaur", "Charmander", "Squirtle",
                     "Chikorita", "Cyndaquil", "Totodile",
                     "Treecko", "Torchic", "Mudkip",
                     "Turtwig", "Chimchar", "Piplup",
                     "Snivy", "Tepig", "Oshawott",
                     "Chespin", "Fennekin", "Froakie",
                     "Rowlet", "Litten", "Popplio",
                     "Grookey", "Scorbunny", "Sobble",
                     "Sprigatito", "Fuecoco", "Quaxly") ~ "Starter",
      ndex %in% c(793:799, 803:806) ~ "Ultra Beasts",
      species %in% c("Dratini", "Larvitar", "Bagon", "Beldum",
                     "Gible", "Deino", "Goomy",
                     "Jangmo-o", "Dreepy", "Frigibax") ~ "Pseudo-Legendary",
      rarity == "Normal" ~ "Regular",
      rarity == "Sub Legendary" ~ "Sub-legendary",
      TRUE ~ rarity
      ) %>% factor(levels = c("Regular", "Starter", "Pseudo-Legendary",
                              "Sub-legendary", "Legendary", "Mythical",
                              "Ultra Beasts", "Paradox"))
    )
df_pkm %>% tabyl(rarity)

### Gen
df_pkm <- df_pkm %>%
  mutate(
    gen = case_when(
      extra == "Mega" ~ "6",
      regional == "Alolan" ~ "7",
      regional %in% c("Galarian") ~ "8",
      regional %in% c("Hisuian") ~ "8.5",
      regional == "Paldean" ~ "9",
      TRUE ~ gen
    ),
    gen_name = case_when(
      gen == "1" ~ str_c(gen, "-Kanto"),
      gen == "2" ~ str_c(gen, "-Johto"),
      gen == "3" ~ str_c(gen, "-Hoenn"),
      gen == "4" ~ str_c(gen, "-Sinnoh"),
      gen == "5" ~ str_c(gen, "-Unova"),
      gen == "6" ~ str_c(gen, "-Kalos"),
      gen == "7" ~ str_c(gen, "-Alola"),
      gen == "8" ~ str_c(gen, "-Galar"),
      gen == "8.5" ~ str_c(gen, "-Hisui"),
      gen == "9" ~ str_c(gen, "-Paldea"),
      TRUE ~ NA_character_
      )
    ) %>%
  relocate(gen_name, .after = gen)
df_pkm %>% tabyl(gen)


### Types (dummy)
df_pkm %>% tabyl(type_1) %>% arrange(1)# %>% nrow
df_pkm %>% tabyl(type_2) %>% arrange(1)# %>% nrow

mat1 <- df_pkm %>%
  mutate(val = 1) %>%
  select(id, type_1, val) %>%
  pivot_wider(names_from = type_1, values_from = val, values_fill = 0) %>%
  select(-id, -`NA`)
mat1 <- mat1 %>% select(order(colnames(.))) %>% as.matrix()

mat2 <- df_pkm %>%
  mutate(val = 1) %>%
  select(id, type_2, val) %>%
  pivot_wider(names_from = type_2, values_from = val, values_fill = 0) %>%
  select(-id, -`NA`)
mat2 <- mat2 %>% select(order(colnames(.))) %>% as.matrix()

mat <- mat1 + mat2 %>% as_tibble()
colnames(mat) <- str_c("d_", str_to_lower(colnames(mat)))

df_pkm <- df_pkm %>% cbind(mat) %>% as_tibble()


# Backup ------------------------------------------------------------------

# write_rds(df_pkm, file = "data/backup_df_pkm.RDS")

