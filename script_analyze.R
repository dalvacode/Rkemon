#### Analyses ####

library(RColorBrewer)
library(pals)
library(fmsb)
library(gt)
#library(naniar)
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



# types x gen, pokémon-wise -----------------------------------------------

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
    title = html("<b>Number of pokémons</b>"),
    subtitle = html("<b>by type and generation</b>")
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



# types x gen, line-wise --------------------------------------------------

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
    title = html("<b>Number of evolution lines</b>"),
    subtitle = html("<b>by type and generation</b>")
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



# stats by type -----------------------------------------------------------

input <- df_pkm %>%
  filter(
    extra %!in% c("Individuals", "Others", "Mega"),
    !is.na(st_hp)
  )

## Prepare data
list_i = tabyl(input, type_1)[,1]
output <- list()
params <- bind_rows(
  min = summarize(
    input,
    across(starts_with("st_"), min)
  ),
  max = summarize(
    input,
    across(starts_with("st_"), max)
  ),
  #.id = "ref"
)

for (i in list_i) {
  midput <- input %>% filter(type_1 == i | type_2 == i) %>%
    summarize(
      across(starts_with("st_"), ~ mean(.x))
      )
  output[[i]] <- midput*(-100/(1-params[2,]/params[1,]))/params[1,] + 100 / (1-params[2,]/params[1,])
}
output <- output %>%
  bind_rows(.id = "Type") #%>% column_to_rownames("Type")

## Prepare palette
pal_pkm <- tibble(
    values = tableau20(n = 20),
    names = c(
    # Dunkelblo
    "Dragon", "Ice",
    # Orange
    "Ground", "Normal",
    # Gréng
    "Grass", "drop",
    # Rout
    "Fire", "drop",
    # Mof
    "Psychic", "Ghost",
    # Brong
    "Rock", "Fighting",
    # Rosa
    "Poison", "Fairy",
    # Gro
    "Dark", "Steel",
    # Giel
    "Bug", "Electric",
    # Hellblo
    "Water", "Flying"
    )
  ) %>%
  filter(names != "drop") %>%
  mutate(values = if_else(names == "Electric", "yellow", values))

## Prepare graph
input_fig <- output
output_fig <- list()

for (i in list_i) {
  midput <- input_fig %>%
    filter(Type == i) %>% select(-1)
  midput <- rbind(
    rep(100,6),
    rep(0,6),
    midput
    )
  
  png(filename = str_c("out/stats_x_type_", i, ".png"),
      width = 10, height = 10, res = 300, units = "cm" 	# only for non-pdf files
  )
  
  radarchart(
    midput,
    #axistyp = 1,
    # polygon
    pcol = filter(pal_pkm, names == i)[[1]],
    pfcol = alpha(filter(pal_pkm, names == i)[[1]], 0.8),
    plwd = 4,
    # grid
    cglcol="grey",
    cglty = 1,
    axislabcol = "grey",
    caxislabels = seq(0,100,25),
    cglwd = 0.8,
    # labels
    vlcex = 0.8 
  )
  output_fig[[i]] <- recordPlot()
  dev.off()
  
  
}




# types network -----------------------------------------------------------

library(rgl)
library(networkD3)
library(ggraph)
library(igraph)

raw_type <- read_excel("data/pokemon_types_simple.xlsx")

mat <- raw_type %>%
  column_to_rownames("Attack type") %>%
  mutate(across(everything(), ~ case_when(
    #.x == 0 ~ 10,
    .x == 0 ~ 1,
    .x == 1 ~ 0,
    .x == 0.5 ~ 2,
    .x == 2 ~ 3,
    TRUE ~ .x
  ))) %>%
  as.matrix()

#mat <- mat * 10
midput <- graph_from_adjacency_matrix(adjmatrix = mat, weighted = T, add.rownames = "code")

midput$name <- "Pkm types matrix"
V(midput)$id <- 1:vcount(midput)
#E(g)$weight

#as_adjacency_matrix(g, attr = "weight")
ds_type <- igraph::as_data_frame(midput)
ds_type <- ds_type %>%
  mutate(
    effect = case_when(
      weight == filter(ds_type, from == "Normal", to == "Ghost")$weight ~ "Immune",
      weight == filter(ds_type, from == "Normal", to == "Rock")$weight ~ "Ineffective",
      weight == filter(ds_type, from == "Fighting", to == "Normal")$weight ~ "Supereffective",
      TRUE ~ NA_character_
      ),
    color = case_when(
      effect == "Immune" ~ "black",
      effect == "Ineffective" ~ "darkred",
      effect == "Supereffective" ~ "darkgreen",
      TRUE ~ NA_character_
      ),
    group = case_when(
      if_all(c("from", "to"), ~ .x %in% c("Water", "Fire", "Grass")) ~ "A",
      if_all(c("from", "to"), ~ .x %in% c("Electric", "Flying", "Bug")) ~ "B",
      if_all(c("from", "to"), ~ .x %in% c("Ice", "Steel", "Ground", "Rock")) ~ "C",
      if_all(c("from", "to"), ~ .x %in% c("Psychic", "Poison", "Fairy", "Dark", "Ghost")) ~ "D",
      if_all(c("from", "to"), ~ .x %in% c("Normal", "Fighting")) ~ "E",
      if_all(c("from", "to"), ~ .x %in% c("Dragon")) ~ "F",
      TRUE ~ NA_character_
      ),
    group_color = case_when(
      group == "A" ~ "red",
      group == "B" ~ "orange",
      group == "C" ~ "brown",
      group == "D" ~ "blue",
      group == "E" ~ "grey50",
      group == "A" ~ "purple",
      TRUE ~ "darkgreen"
      )
    ) %>% filter(
      effect == "Supereffective",
      #effect != "Ineffective",
      from %in% c("Psychic", "Poison", "Fairy", "Dark", "Ghost", "Fighting"),
      to %in% c("Psychic", "Poison", "Fairy", "Dark", "Ghost", "Fighting"),
      # from %!in% c("Normal", "Dragon"),
      # to %!in% c("Normal", "Dragon"),
      # from %!in% c("Normal", "Dragon"),
      # to %!in% c("Normal", "Dragon")
      )

g <- graph_from_data_frame(ds_type)






# ggraph(ds_type, layout = "auto") + 
#   geom_edge_link(aes(color = effect, edge_width = weight), arrow = arrow()) + 
#   geom_node_point() +
#   geom_node_text(aes(label = name), repel = T) +
#   scale_edge_width_continuous(range = c(1, 3)) +
#   scale_color_brewer(palette = "Set1")
#   scale_color_manual(values = c("grey30", "red", "green"))
# 
# plot(g,
#      layout = layout.circle,
#      vertex.size = 8,
#      edge.arrow.size = 0.2,
#      #vertex.label = NA,
#      mark.shape = 1
#      )
# 
# simpleNetwork(ds_type, height="100px", width="100px")

# mypal <- tibble(type = V(g)$name) %>%
#   mutate(
#     group = case_when(
#       type %in% c("Water", "Fire", "Grass") ~ "A",
#       type %in% c("Electric", "Flying", "Bug") ~ "B",
#       type %in% c("Ice", "Steel", "Ground", "Rock") ~ "C",
#       type %in% c("Psychic", "Poison", "Fairy", "Dark", "Ghost") ~ "D",
#       type %in% c("Normal", "Fighting") ~ "E",
#       type %in% c("Dragon") ~ "F",
#       TRUE ~ NA_character_
#     ),
#     group_color = case_when(
#       group == "A" ~ "red",
#       group == "B" ~ "orange",
#       group == "C" ~ "brown",
#       group == "D" ~ "blue",
#       group == "E" ~ "grey50",
#       group == "F" ~ "purple",
#       TRUE ~ NA_character_
#       )
#     )
# 
# #par(bg="white")
# 
# plot(g, 
#      #layout = layout.sugiyama,
#      layout = layout_as_tree,
#      
#      # === vertex
#      vertex.color = mypal$group_color,          # Node color
#      vertex.frame.color = "grey30",                 # Node border color
#      vertex.shape="circle",                        # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
#      vertex.size=14,                               # Size of the node (default is 15)
#      vertex.size2=NA,                              # The second size of the node (e.g. for a rectangle)
#      
#      # === vertex label
#      vertex.label= mypal$type,                   # Character vector used to label the nodes
#      vertex.label.color="black",
#      #vertex.label.family="Times",                  # Font family of the label (e.g.“Times”, “Helvetica”)
#      vertex.label.font=2,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
#      vertex.label.cex=0.5,                           # Font size (multiplication factor, device-dependent)
#      vertex.label.dist=0,                          # Distance between the label and the vertex
#      vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
#      
#      # === Edge
#      edge.color = E(g)$group_color,                           # Edge color
#      edge.width = ds_type$weight,                                 # Edge width, defaults to 1
#      edge.arrow.size = 0.3,                            # Arrow size, defaults to 1
#      edge.arrow.width = 0.5,                           # Arrow width, defaults to 1
#      edge.lty = "solid",                             # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
#      edge.curved = 0.3    ,                          # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
# )
# 
# 
# 
# 
# # Compute node degrees
# degree(g)
# # Compute node strengths
# strength(g)
# # Compute betweenness
# dist_weight = 1 / E(g)$weight
# edge_betweenness(g, weights = dist_weight)
# 
# # ties_centrality <- ds_type %>% mutate(
# #   degree = degree(g),
# #   strength = strength(g),
# #   betweenness = edge_betweenness(g, weights = dist_weight)
# #   )
# 
# # Make is_weak TRUE whenever the tie is weak
# is_weak <- ifelse(E(g)$weight == 1, T, F)
# # Plot
# ggraph(g, layout = "with_kk") +
#   geom_edge_link(aes(color = is_weak))
# 
# # Calculate node strength
# A <- mat
# rowSums(A)
# # Calculate node degree
# B <- A > 0
# rowSums(B)
# 
# 
# 
# # Compute a distance matrix
# d <- as.dist(1-mat)
# # Perform hierarchical clustering
# clusters <- hclust(d, method = "average")
# plot(clusters)
# # Add cluster information to nodes (cut them in 4)
# nodes$cluster <- cutree(clusters, k = 4)



# Build 3D network --------------------------------------------------------

## Prepare palette
mypal <- tibble(type = V(g)$name) %>%
  mutate(
    group = case_when(
      type %in% c("Water", "Fire", "Grass") ~ "A",
      type %in% c("Electric", "Flying", "Bug") ~ "B",
      type %in% c("Ice", "Steel", "Ground", "Rock") ~ "C",
      type %in% c("Psychic", "Poison", "Fairy", "Dark", "Ghost") ~ "D",
      type %in% c("Normal", "Fighting") ~ "E",
      type %in% c("Dragon") ~ "F",
      TRUE ~ NA_character_
    ),
    group_color = case_when(
      group == "A" ~ "red",
      group == "B" ~ "orange",
      group == "C" ~ "brown",
      group == "D" ~ "blue",
      group == "E" ~ "grey50",
      group == "F" ~ "purple",
      TRUE ~ NA_character_
    )
  )

## Prepare input
input <- simplify(g)
#coord3D <- layout.fruchterman.reingold(input, dim = 3)
coord3D <- igraph::layout_nicely(input, dim = 3) # best for Supereffective + Immune)

n <- cbind(mypal, coord3D)
colnames(n) <- c("type", "group", "color", "x", "y", "z")

e <- igraph::as_data_frame(input) %>%
  left_join(
    x = .,
    y = mypal %>% select(from = type, from.group = group_color),
    by = "from"
    ) %>%
  left_join(
    x = .,
    y = mypal %>% select(to = type, to.group = group_color),
    by = "to"
    ) %>%
  mutate(
    from.weight = if_else(weight == 1, "black", "green"),
    to.weight = if_else(weight == 1, "black", "red"),
    color_weight = if_else(weight == 1, "darkred", "darkgreen"),
    color_group = if_else(from.group == to.group, from.group, "grey80")
    )

e <- e %>%
  left_join(
    x = .,
    y = n %>% select(from = type, from.x = x, from.y = y, from.z = z),
    by = "from"
    ) %>%
  left_join(
    x = .,
    y = n %>% select(to = type, to.x = x, to.y = y, to.z = z),
    by = "to"
  ) #%>% select(-from, -to)
  
for (i in 1:nrow(e)) {
  mid <- e[i,]
  plot3d(
    x = c(mid$from.x, mid$to.x), y = c(mid$from.y, mid$to.y), z = c(mid$from.z, mid$to.z),
    #col = mid$color_group,
    #col = c(mid$from.group, mid$to.group),
    col = c(mid$from.weight, mid$to.weight),
    type = "l",
    lwd = mid$weight,
    add = T,
    decorate = F,
    xlab = "", ylab = "", zlab = ""
  )  
}
plot3d(
  x = n$x, y = n$y, z = n$z,
  #col = n$color,
  type = "s", radius = .1,
  add = T,
  xlab = "", ylab = "", zlab = ""
)

# htmlwidgets::saveWidget(rglwidget(width = 520, height = 520), 
#                         file = "out/type_3d.html",
#                         libdir = "libs",
#                         selfcontained = FALSE
# )




