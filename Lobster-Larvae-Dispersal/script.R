# ============================ #
#
# Lobster Larvae Dispersal Map
#
# Data from Tom Jenkins PhD Thesis:
# http://hdl.handle.net/10871/35541
# 2018-10-28
#
# ============================ #

# In RStudio set working directory to the path where this R script is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load packages
library(ggplot2)
library(gganimate)
library(gifski)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(ggspatial)
library(vroom)
library(dplyr)
library(tidyr)
library(scales)
library(magick)
library(grid)
library(patchwork)

# ---------- #
# Data Preparation
# ---------- #

# Coordinate reference system
crs <- 27700

# Read in release sites data
release_sites <- vroom("release_sites.csv")

# Project coordinates to CRS
release_sites_sf <- release_sites |> 
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) |> 
  st_transform(crs)

# Read in particle trajectory data
trajectory_df <- drop_na(vroom("trajectory_data.csv"))

# Print number of particles per release site (n=100)
trajectory_df |> 
  group_by(Location) |> 
  count(Particle_ID) |> 
  count(Location)

# Project coordinates to CRS
trajectory_sf <- trajectory_df |> 
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) |> 
  st_transform(crs)

# Convert back to data.frame for geom_path() to work
trajectory_df <- tibble(
  Location = trajectory_sf$Location,
  Particle_ID = trajectory_sf$Particle_ID,
  Time = trajectory_sf$Time,
  Longitude = st_coordinates(trajectory_sf)[,"X"],
  Latitude = st_coordinates(trajectory_sf)[,"Y"]
)

# Import map of Europe
subregions <- c("Western Europe","Northern Europe","Southern Europe")
europe <- rnaturalearthhires::countries10 |> 
  filter(SUBREGION %in% subregions) |> 
  select(ADMIN) |> 
  st_transform(crs)

# ---------- #
# Create Animation
# ---------- #

# Parameters
white <- "#FDFDFD"
black <- "black"
lightgrey <- "#F5F5F5"
grey <- "grey20"
palette_func = colorRampPalette(c("#FF13F0","purple","#0096FF","cyan", "#7FFF7F", "yellow", "#FF7F00", "red"))
site_cols <- palette_func(length(release_sites$Release_sites))
show_col(site_cols)

# Bounding box
bbox <- st_bbox(c(xmin = -11, xmax = 7, ymin = 48.5, ymax = 56), crs = st_crs(4326)) |> 
  st_transform(crs)

# TJ Data Visualisation logo in SVG format
logo <- image_read_svg("../misc/logo-darkblue.svg")
logo_grob <- rasterGrob(logo, interpolate = TRUE)

# Theme
maptheme <- theme(
  panel.background = element_rect(fill = lightgrey),
  panel.grid.major = element_line(linewidth = 0.2),
  axis.title.y = element_text(vjust = 5),
  plot.title = element_text(size = 18),
  plot.subtitle = element_text(size = 15),
  plot.caption = element_text(size = 10),
  plot.margin = margin(t = 10, r = 0, b = 5, l = 0, unit = "pt"),
)

# Static plot
static_plt <- ggplot()+
  geom_path(
    data = trajectory_df,
    aes(x = Longitude, y = Latitude, group = Particle_ID, colour = Location),
    linewidth = 0.4,
    show.legend = FALSE,
  )+
  geom_sf(data = europe, fill = grey, colour = black)+
  geom_sf(
    data = release_sites_sf,
    col = black,
    fill = site_cols,
    shape = 24,
    size = 3.5,
    stroke = 0.8
  )+
  annotation_north_arrow(
    data = europe,
    which_north = "true",
    location = "tl",
    height = grid::unit(0.8, "cm"),
    width = grid::unit(0.8, "cm"),
    style = north_arrow_orienteering(
      text_size = 5,
      line_width = 0.5
    )
  )+
  annotation_scale(
    data = europe,
    location = "br",
    style = "bar",
    width_hint = 0.25,
    bar_cols = c(white, white, white),
    line_width = 2,
    line_col = grey,
    height = unit(0.40, "cm"),
    pad_y = unit(0.25, "cm"),
    text_cex = 0.75,
    text_col = white,
    text_face = "bold",
  )+
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]))+
  scale_colour_manual(values = site_cols)+
  maptheme+
  # Overlay TJ Data Visualisation logo at top-right
  annotation_custom(
    grob = logo_grob,
    xmin = bbox["xmin"] + (bbox["xmax"] - bbox["xmin"]) * 0.1,
    xmax = bbox["xmin"] + (bbox["xmax"] - bbox["xmin"]) * 0.01,
    ymin = bbox["ymin"] + (bbox["ymax"] - bbox["ymin"]) * 0.1,
    ymax = bbox["ymin"] + (bbox["ymax"] - bbox["ymin"]) * 0.01
  )
static_plt

# Animate plot
animate_plt <- static_plt+
  transition_reveal(Time)+
  ease_aes("linear")+
  labs(
    title = "Simulated Dispersal of Lobster Larvae from Release Sites",
    subtitle = "Day: {floor(frame_along)} of 30",
    caption = "Data source: http://hdl.handle.net/10871/35541"
  )

# Render animation
animation <- animate(
  plot = animate_plt,
  nframes = 150, start_pause = 5, end_pause = 20,
  width = 10, height = 7, units = "in",
  res = 300
)
animation

# Save animation as GIF
anim_save(animation = animation, filename = "Dispersal.gif", path = ".")
 