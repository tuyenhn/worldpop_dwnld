library(sf)
library(fs)
library(stars)
library(tidyverse)

# long printing for debug
options(max.print = 3000)

# download "gadm41_VNM_1_pk.rds" with `geodata::gadm()`
hcmc_shp <- read_rds("gadm41_VNM_1_pk.rds") %>%
  terra::unwrap() %>%
  st_as_sf() %>%
  filter(GID_1 == "VNM.25_1")

# get buffered bounding box of HCMC
hcmc_buff_bbox <- hcmc_shp %>%
  st_buffer(dist = units::set_units(5, "km")) %>%
  st_bbox()

# cropped each raster with `st_intersects()` to HCMC shape
walk(
  dir_ls("Global_2000_2020_1km_UNadj"),
  \(path){
    tiff <- read_stars(path)

    # initial crop to reduce tiff size
    init_crop <- st_crop(tiff, hcmc_buff_bbox)

    # subsetting intersecting cells and actual polygon
    # `as_points = FALSE` means considering raster cells as polygons instead of points
    intersected_tiff <- st_intersects(init_crop, hcmc_shp, as_points = FALSE)

    # get the raster cell indices and values
    intersecting_indices <- which(lengths(intersected_tiff) > 0)
    intersecting_values <- init_crop[[1]][intersecting_indices]

    # replace all other values of cropped tiff with NA
    init_crop[[1]][-intersecting_indices] <- NA

    write_rds(init_crop, paste0("hcmc_1km_cropped/", tools::file_path_sans_ext(basename(path)), ".rds"))
  },
  .progress = TRUE
)

# VIZ test
path <- dir_ls("Global_2000_2020_1km_UNadj")[1]
tiff <- read_stars(path)
init_crop <- st_crop(tiff, hcmc_buff_bbox)

ggplot() +
  geom_stars(data = init_crop) +
  geom_sf(data = hcmc_shp, fill = NA, linewidth = 0.5, color = "red")
# geom_sf(data = st_buffer(hcmc_shp, dist = units::set_units(5, "km")), fill = NA) +
# geom_sf(data = hcmc_buff_bbox %>% st_as_sfc(), fill = NA)
