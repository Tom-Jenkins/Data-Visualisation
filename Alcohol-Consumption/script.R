# ============================ #
#
# Global Alcohol Consumption
#
# Data from Tidy Tuesday:
# https://github.com/rfordatascience/tidytuesday
# 2018-06-26
#
# ============================ #

# In RStudio set working directory to the path where this R script is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load packages
library(ggplot2)
library(ggfun)
library(magick)
library(grid)
library(RColorBrewer)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(patchwork)

# Font setup
library(showtext)
font_add_google("Roboto", "roboto")
showtext_auto()

# ---------- #
# Data Preparation
# ---------- #

# Read in data
url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/refs/heads/main/data/2018/2018-06-26/week13_alcohol_global.csv"
df <- read.csv(url)
df$country |> sort()

# Get country polygons from rnaturalworld in sf class
countries_sf <- rnaturalearthdata::countries110[, "name_long"]
countries_sf$name_long |> sort()

# Find out which countries do not match between datasets
df$country[which(!df$country %in% countries_sf$name_long)] |> sort()

# Create a name mapping vector
name_map <- c(
  "Antigua & Barbuda" = "Antigua and Barbuda",
  "Bosnia-Herzegovina" = "Bosnia and Herzegovina",
  "Brunei" = "Brunei Darussalam",
  "Cote d'Ivoire" = "Côte d'Ivoire",
  "Cabo Verde" = "Republic of Cabo Verde",
  "Congo" = "Democratic Republic of the Congo",
  "Gambia" = "The Gambia",
  "Laos" = "Lao PDR",
  "Micronesia" = "Federated States of Micronesia",
  "Macedonia" = "North Macedonia",
  "North Korea" = "Dem. Rep. Korea",
  "Sao Tome & Principe" = "São Tomé and Principe",
  "South Korea" = "Republic of Korea",
  "St. Lucia" = "Saint Lucia",
  "St. Kitts & Nevis" = "Saint Kitts and Nevis",
  "St. Vincent & the Grenadines" = "Saint Vincent and the Grenadines",
  "Swaziland" = "Eswatini",
  "USA" = "United States",
  "Trinidad & Tobago" = "Trinidad and Tobago"
)

# Change the country names to the name map
df$country <- ifelse(df$country %in% names(name_map), name_map[df$country], df$country)
df$country[which(!df$country %in% countries_sf$name_long)] |> sort()

# Join polygons to data.frame and transform to Robinson projection
world_df <- df |> 
  full_join(countries_sf, by = c("country" = "name_long")) |> 
  st_as_sf() |> 
  st_transform("ESRI:54030") |> 
  filter(country != "Antarctica")

# ---------- #
# Parameters
# ---------- #

# Parameters
white <- "#FDFDFD"
black <- "black"
grey <- "#F5F5F5"
border_width <- 0.02

# Bounding box
bbox <- st_bbox(c(xmin = -120, xmax = 155, ymin = -50, ymax = 80), crs = st_crs(4326)) |> 
  st_transform("ESRI:54030")

# TJ Data Visualisation logo in SVG format
logo <- image_read_svg("../misc/logo-darkblue.svg")
logo_grob <- rasterGrob(logo, interpolate = TRUE)

# Theme
maptheme <- theme(
  text = element_text(family = "roboto"),
  panel.background = element_roundrect(fill = grey),
  panel.grid.major = element_line(linewidth = 0.2),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  plot.title = element_text(size = 60),
  plot.subtitle = element_text(size = 40),
  plot.caption = element_text(size = 30),
  legend.text = element_text(size = 40),
  legend.title = element_text(size = 45),
  legend.key.size = unit(15, "pt"),
  plot.margin = unit(c(l = 5, r = 5, b = 5, t = 5), "pt"),
)

# ---------- #
# Beer Map
# ---------- #

# Country with highest consumption
world_df[which.max(world_df$beer_servings),]

# Plot beer can servings map
(beer_plt <- ggplot(data = world_df, aes(fill = beer_servings))+
  geom_sf(colour = black, linewidth = border_width)+
  coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax))+
  labs(
    title = "Beer Consumption by Country",
    subtitle = "The average number of cans of beer consumed per person in 2010",
    caption = "Data source: FiveThirtyEight.com"
  )+
  scale_fill_gradientn(
    name = "Cans of Beer",
    colours = brewer.pal(7, "OrRd")
  )+
  maptheme
)

# Overlay TJ Data Visualisation logo at top-right
(beer_plt_1 <- beer_plt+
  inset_element(logo_grob, left = 0.92, right = 1, bottom = 0.90, top = 1, align_to = "full")
)

# Beer image
beer_img <- image_read("Beer.png")
beer_img_grob <- rasterGrob(beer_img, interpolate = TRUE)

# Overlay beer image at bottom-left
(beer_plt_2 <- beer_plt_1+
    inset_element(
      p = beer_img_grob,
      left = 0.04, right = 0.09,
      bottom = 0.16, top = 0.26,
      align_to = "full"
    )
)

# ---------- #
# Wine Map
# ---------- #

# Country with highest consumption
world_df[which.max(world_df$wine_servings),]

# Plot wine glass servings map
(wine_plt <- ggplot(data = world_df, aes(fill = wine_servings))+
    geom_sf(colour = black, linewidth = border_width)+
    coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax))+
    labs(
      title = "Wine Consumption by Country",
      subtitle = "The average number of glasses of wine consumed per person in 2010",
      caption = "Data source: FiveThirtyEight.com"
    )+
    scale_fill_gradientn(
      name = "Glasses of Wine",
      colours = brewer.pal(7, "RdPu")
    )+
    maptheme
)

# Overlay TJ Data Visualisation logo at top-right
(wine_plt_1 <- wine_plt+
    inset_element(logo_grob, left = 0.92, right = 1, bottom = 0.90, top = 1, align_to = "full")
)

# wine image
wine_img <- image_read("Wine.jpg")
wine_img_grob <- rasterGrob(wine_img, interpolate = TRUE)

# Overlay wine image at bottom-left
(wine_plt_2 <- wine_plt_1+
    inset_element(
      p = wine_img_grob,
      left = 0.04, right = 0.09,
      bottom = 0.16, top = 0.26,
      align_to = "full"
    )
)

# ---------- #
# Spirit Map
# ---------- #

# Country with highest consumption
world_df[which.max(world_df$spirit_servings),]

# Plot spirits glass servings map
(spirits_plt <- ggplot(data = world_df, aes(fill = spirit_servings))+
    geom_sf(colour = black, linewidth = border_width)+
    coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax))+
    labs(
      title = "Spirits Consumption by Country",
      subtitle = "The average number of shots of spirits consumed per person in 2010",
      caption = "Data source: FiveThirtyEight.com"
    )+
    scale_fill_gradientn(
      name = "Shots of Spirit",
      colours = brewer.pal(7, "GnBu")
    )+
    maptheme
)

# Overlay TJ Data Visualisation logo at top-right
(spirits_plt_1 <- spirits_plt+
    inset_element(logo_grob, left = 0.92, right = 1, bottom = 0.90, top = 1, align_to = "full")
)

# spirits image
spirits_img <- image_read("Spirit.jpg")
spirits_img_grob <- rasterGrob(spirits_img, interpolate = TRUE)

# Overlay spirits image at bottom-left
(spirits_plt_2 <- spirits_plt_1+
    inset_element(
      p = spirits_img_grob,
      left = 0.04, right = 0.09,
      bottom = 0.16, top = 0.26,
      align_to = "full"
    )
)

# ---------- #
# Export Graphics
# ---------- #

# Output file names
beer_output_png <- "Beer_graphic.png"
wine_output_png <- "Wine_graphic.png"
spirit_output_png <- "Spirit_graphic.png"

# Save graphics
ggsave(beer_output_png, plot = beer_plt_1, dpi = 600)
ggsave(beer_output_png, plot = beer_plt_1, dpi = 600)
ggsave(wine_output_png, plot = wine_plt_1, dpi = 600)
ggsave(wine_output_png, plot = wine_plt_1, dpi = 600)
ggsave(spirit_output_png, plot = spirits_plt_1, dpi = 600)
ggsave(spirit_output_png, plot = spirits_plt_1, dpi = 600)

# Crop unecessary whitespace from images
crop_image <- function (image_path) {
  image_path |>
    image_read() |> 
    image_trim() |> 
    image_border(color = "white", geometry = "10x10") |>
    image_write(image_path, density = "600x600")
}
images <- c(beer_output_png, wine_output_png, spirit_output_png)
lapply(images, FUN = crop_image)
