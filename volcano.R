library(sf)
library(rvest)
library(ggdist)
library(hrbragg) # remotes::install_github("hrbrmstr/hrbragg") # or use another theme
library(ggsflabel) # remotes::install_github("yutannihilation/ggsflabel")
library(tidyverse)

# this uses the R native pipe, so if folks are on older R versions, swap that out
# for the {magrittr} pipe.

# Ísland shapefile
# https://geodata.lib.berkeley.edu/download/file/stanford-dq155fd3935-shapefile.zip
ice <- st_read("./stanford-dq155fd3935-shapefile/dq155fd3935.shp")

# Ísland volcano geo information
# https://kmlexport.toolforge.org/?article=List_of_volcanoes_in_Iceland
vol <- st_read("doc.kml")

# Ísland volcano height info
pg <- read_html("https://en.wikipedia.org/wiki/List_of_volcanoes_in_Iceland") 

html_table(pg)[[2]] |> 
  select(name=Name, elev=2) |> 
  filter(elev != "(m)") |> 
  mutate(
    elev = as.numeric(stri_replace_all_regex(elev, "[^[:digit:]]", ""))
  ) |> 
  filter(elev > 0) -> height

# make the raincloud distribution chart

ggplot(
  data = height,
  aes(elev)
) +
  stat_halfeye(
    adjust = 0.5,
    justification = -0.1,
    .width = 0,
    point_colour = NA,
    fill = "#db7f33"
  ) +
  geom_boxplot(
    width = 0.1,
    fill = "#db7f33"
  ) +
  stat_dots(
    side = "bottom",
    justification = 1.125,
    dotsize = 1/6,
    fill = "#db7f33",
    stroke = 0.125,
    color = "black"
  ) +
  scale_x_comma(
  ) +
  scale_y_continuous(
    expand = c(0, 0, 0, 0)
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Volcano Height Distribution (m)"
  ) +
  theme_cs(grid="XY", plot_title_size = 14) +
  theme(
    axis.text.y.left = element_blank(),
    plot.background =  element_rect(fill = "transparent", color = "transparent"),
    panel.background = element_rect(fill = "transparent", color = "transparent")
  ) -> gg

# make the final map
ggplot() +
  geom_sf( # gray base layer for the outline
    data = ice,
    size = 2,
    fill = "white",
    color = "gray90"
  ) +
  geom_sf( # the ísland map
    data = ice,
    size = 0.125,
    fill = "white"
  ) +
  geom_sf_text( # the volcano emoji's (only the ones that are on the raised continent)
    data = vol |> filter(!(Name %in% c("Kolbeinsey", "Reykjaneshryggur", "Helgafell", "Jólnir", "Eldfell", "Surtsey"))),
    aes(label = "⛰")
  ) +
  geom_sf_text_repel( # the volcano names (only the ones that are on the raised continent)
    data = vol |> filter(!(Name %in% c("Kolbeinsey", "Reykjaneshryggur", "Helgafell", "Jólnir", "Eldfell","Surtsey"))),
    aes(
      label = Name
    ),
    family = clear_sans_pkg$bold # if {hrbragg} is not usable, choose a diff font
  ) +
  annotation_custom( # place the raincloud distribution chart
    grob = ggplotGrob(gg2),
    xmin = -22, 
    xmax = -18,
    ymin = 66.06,
    ymax = 67.04
  ) +
  coord_sf( # crop the map and remove the graticules
    xlim = c(-24.5, -13.25),
    ylim = c(63.25, 66.75),
    datum = NA
  ) +
  annotate( # add the "title"
    geom = "text",
    x = -13,
    y = 63.4,
    vjust = 0,
    hjust = 1,
    label = "The Volcanoes of Ísland",
    family = clear_sans_pkg$bold,
    size = 10
  ) +
  labs(
    x = NULL, y = NULL
  ) +
  theme_cs(
    grid = ""
  ) +
  theme(
    panel.background = element_rect( # "ocean blue"
      color = NA, 
      fill = "#9dcaca"
    )
  )

