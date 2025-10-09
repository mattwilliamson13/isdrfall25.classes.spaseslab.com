library(FedData)
library(sf)
library(elevatr)
library(NatParksPalettes)
library(rayshader)
library(colorspace)
library(NatParksPalettes)
library(MetBrewer)
library(scico)
library(glue)
height_shade2 <- function (heightmap, 
                           heightmap2 = NULL,
                           texture1, 
                           texture2, 
                           split, 
                           keep_user_par = TRUE) 
{
  
  t1 <- texture1
  t2 <- texture2
  
  if (!is.null(heightmap2)) {
    # Sea level and above
    
    tempfilename = tempfile()
    
    grDevices::png(tempfilename, width = nrow(heightmap), height = ncol(heightmap))
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::image(rayshader:::fliplr(heightmap), axes = FALSE, col = t1, 
                    useRaster = TRUE)
    graphics::image(rayshader:::fliplr(heightmap2), axes = FALSE, col = t2, 
                    useRaster = TRUE, add = TRUE)
    grDevices::dev.off()
    tempmap = png::readPNG(tempfilename)
  } else {
    range1 <- c(min(heightmap, na.rm = TRUE), split)
    range2 <- c(split, max(heightmap, na.rm = TRUE))
    
    # Sea level and above
    
    tempfilename = tempfile()
    
    grDevices::png(tempfilename, width = nrow(heightmap), height = ncol(heightmap))
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::image(rayshader:::fliplr(heightmap), axes = FALSE, col = t1, 
                    useRaster = TRUE, zlim = range1)
    graphics::image(rayshader:::fliplr(heightmap), axes = FALSE, col = t2, 
                    useRaster = TRUE, zlim = range2, add = TRUE)
    grDevices::dev.off()
    tempmap = png::readPNG(tempfilename)
  }
  
  return(tempmap)
}


map <- "sawtooth"
protected_areas <- get_padus(template = "Sawtooth Wilderness",
                             label = "SWA") 

pa_buff <- protected_areas[[1]] %>% 
  st_buffer(., dist = 1)
z <- 11
zelev <- get_elev_raster(pa_buff, z = z, clip = "location")
mat <- raster_to_matrix(zelev)

pal <- "gray_arches2"

c1 <- scico(palette = "grayC", n = 5)
c2 <- natparks.pals(name = "Arches2", n = 7)
colors <- c(c1[2:3], rev(c2[4:7]))
# Calculate the aspect ratio of the plot so you can translate the dimensions

w <- nrow(mat)
h <- ncol(mat)

# Scale so longer side is 1

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))
rgl::rgl.close()

# Create the initial 3D object
shadow_depth <- min(mat, na.rm = TRUE)

# setting resolution to about 5x for height
res <- mean(round(terra::res(zelev))) / 8

try(rgl::rgl.close())

# Create the initial 3D object

mat %>%
  # This adds the coloring, we're passing in our `colors` object
  height_shade(texture = grDevices::colorRampPalette(c("white", "grey90", colors), bias = .5)(256))  %>%
  plot_3d(heightmap = mat,
          # This is my preference, I don't love the `solid` in most cases
          solid = FALSE,
          # You might need to hone this in depending on the data resolution;
          # lower values exaggerate the height
          z = res,
          # Set the location of the shadow, i.e. where the floor is.
          # This is on the same scale as your data, so call `zelev` to see the
          # min/max, and set it however far below min as you like.
          shadowdepth = shadow_depth,
          # Set the window size relatively small with the dimensions of our data.
          # Don't make this too big because it will just take longer to build,
          # and we're going to resize with `render_highquality()` below.
          windowsize = c(1200,1200), 
          # This is the azimuth, like the angle of the sun.
          # 90 degrees is directly above, 0 degrees is a profile view.
          phi = 90, 
          zoom = 1, 
          # `theta` is the rotations of the map. Keeping it at 0 will preserve
          # the standard (i.e. north is up) orientation of a plot
          theta = 0, 
          background = "white") 

# Use this to adjust the view after building the window object
render_camera(phi = 36, zoom = 0.65, theta = 60)

###############################
# Create High Quality Graphic #
###############################

# You should only move on if you have the object set up
# as you want it, including colors, resolution, viewing position, etc.
library(tidyverse)
# Ensure dir exists for these graphics
if (!dir.exists(glue("images/{map}"))) {
  dir.create(glue("images/{map}"))
}

# Set up outfile where graphic will be saved.
# Note that I am not tracking the `images` directory, and this
# is because these files are big enough to make tracking them on
# GitHub difficult. 
outfile <- str_to_lower(glue("images/{map}/{map}_{pal}_z{z}.png"))

# Now that everything is assigned, save these objects so we
# can use then in our markup script
saveRDS(list(
  map = map,
  pal = pal,
  z = z,
  colors = colors,
  outfile = outfile
), "images/fchurch/fchurch.rds")

# Wrap this in brackets so it runs as chunk
{
  # Test write a PNG to ensure the file path is good.
  # You don't want `render_highquality()` to fail after it's 
  # taken hours to render.
  png::writePNG(matrix(1), outfile)
  # I like to track when I start the render
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"), "\n")
  render_highquality(
    # We test-wrote to this file above, so we know it's good
    outfile, 
    # See rayrender::render_scene for more info, but best
    # sample method ('sobol') works best with values over 256
    samples = 300, 
    # Turn light off because we're using environment_light
    light = FALSE, 
    # All it takes is accidentally interacting with a render that takes
    # hours in total to decide you NEVER want it interactive
    interactive = FALSE,
    # HDR lighting used to light the scene
    #environment_light = "../bathybase/env/phalzer_forest_01_4k.hdr",
    # Adjust this value to brighten or darken lighting
    intensity_env = 1.75,
    # Rotate the light -- positive values move it counter-clockwise
    rotate_env = 90,
    # This effectively sets the resolution of the final graphic,
    # because you increase the number of pixels here.
    width = round(6000 * wr), height = round(6000 * hr)
  )
  end_time <- Sys.time()
  cat(glue("Total time: {end_time - start_time}"))
}


library(tidyverse)
library(maps)
library(geosphere)

# Example flow data
df <- read_csv("data/original/fec_reciepts.csv")%>% 
  group_by(contributor_state) %>% 
  summarize(contributions = sum(contribution_receipt_amount)/1000,
            proportion = (contributions/sum(contributions))) %>%
  mutate(to = "ID") %>% 
  rename("from" = contributor_state)

# State centroid lookup
state_locs <- tibble(
  state = st$NAME,
  abb   = st$STUSPS,
  lon   = st$x,
  lat   = st$y
) %>%
  filter(!abb %in% c("AK","HI"))


st <- tigris::states() 
st_coords <- st %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  rename(x = "X", y = "Y")
st <- cbind(st, st_coords)

# Attach coordinates
flows_coords <- df %>%
  left_join(state_locs, by = c("from" = "abb")) %>%
  rename(lon_from = lon, lat_from = lat) %>%
  left_join(state_locs, by = c("to" = "abb")) %>%
  rename(lon_to = lon, lat_to = lat) %>%
  filter(!is.na(lon_from), !is.na(lat_from),
         !is.na(lon_to),   !is.na(lat_to),
         !(from == to)) 

make_arc <- function(from_lon, from_lat, to_lon, to_lat) {
  if (is.na(from_lon) | is.na(from_lat) | is.na(to_lon) | is.na(to_lat)) {
    return(NULL)
  }
  if (from_lon == to_lon & from_lat == to_lat) {
    return(NULL)
  }
  mat <- tryCatch(
    geosphere::gcIntermediate(c(from_lon, from_lat),
                              c(to_lon, to_lat),
                              n = 100, addStartEnd = TRUE),
    error = function(e) NULL
  )
  if (is.null(mat)) return(NULL)
  st_linestring(as.matrix(mat))
}


flow_lines <- flows_coords %>%
  rowwise() %>%
  mutate(geometry = list(make_arc(lon_from, lat_from, lon_to, lat_to))) %>%
  ungroup() %>%
  filter(!sapply(geometry, is.null)) %>%   # drop failed arcs
  st_as_sf(crs = 4326)


albers_crs <- st_crs("+proj=aea +lat_1=39 +lat_2=45 +lat_0=37 +lon_0=-96")

flow_proj   <- st_transform(flow_lines, albers_crs)

states_sf <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
  filter(!ID %in% c("alaska","hawaii")) %>%
  st_transform(albers_crs)


ggplot() +
  geom_sf(data = states_sf, fill = "gray10", color = "gray50") +
  geom_sf(data = flow_proj, aes(size = contributions, alpha = contributions),
            color = "darkorchid4", lineend = "round") +
  scale_size(range = c(0.1, 1.5), guide = "none") +
  scale_alpha_continuous(range = c(0.5,1)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))




# Generate great-circle paths
flow_paths <- flows_coords %>%
  rowwise() %>%
  mutate(path = list(as_tibble(
    geosphere::gcIntermediate(
      c(lon_from, lat_from),
      c(lon_to, lat_to),
      n = 100,            # smooth curve
      addStartEnd = TRUE,
      breakAtDateLine = FALSE
    )
  ) %>% mutate(t = seq(0, 1, length.out = n()))) ) %>%  # add position along path
  ungroup() %>%
  select(from, to, contributions, proportion, path) %>%
  unnest(path) %>%
  group_by(from, to) %>%
  mutate(group_id = cur_group_id()) %>%
  ungroup()

# Base map
states_map <- map_data("state") %>%
  filter(!region %in% c("alaska", "hawaii"))

id_val <- states_map %>% 
  filter(region == "idaho") %>% 
  mutate(dollars = )

# Plot
flow_plot <- ggplot() +
  geom_polygon(
    data = states_map,
    aes(x = long, y = lat, group = group),
    fill = "black", color = "gray50"
  ) +
  geom_path(
    data = flow_paths,
    aes(x = lon, y = lat, group = group_id, color = contributions, 
        size = contributions),
    lineend = "round",
    alpha = 0.6
  ) +
  scale_size(range = c(0.1, 1.5), guide="none") +
  scale_color_gradient(low = "white",high = "darkorchid4") +
  coord_map("gilbert", xlim = c(-125, -66), ylim = c(24, 50)) +
  labs(color = "Contributions \n(in 1000s of USD)") +
  theme_void() +
  theme(panel.background = element_rect(fill = "transparent", color = NA),  legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(0.5, "in"), legend.title.position = "top", legend.title = element_text(color = "white", face = "bold"),legend.text = element_text(colour = "white"))

ggsave("images/flow_plot.png",flow_plot)
