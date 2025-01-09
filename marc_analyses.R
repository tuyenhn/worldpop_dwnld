library(fs)
library(sf)
library(stars)
library(units)
library(furrr)
library(future)
library(purrr)

plan(multicore, workers = 7)

tempor_range <- 2020

vnm_ppp_1km_UNadj_urls <- paste0(
  "ftp://ftp.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/", tempor_range, "/VNM/vnm_ppp_", tempor_range, "_1km_Aggregated_UNadj.tif"
)
npl_ppp_1km_UNadj_urls <- paste0(
  "ftp://ftp.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/", tempor_range, "/NPL/npl_ppp_", tempor_range, "_1km_Aggregated_UNadj.tif"
)
idn_ppp_1km_UNadj_urls <- paste0(
  "ftp://ftp.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/", tempor_range, "/IDN/idn_ppp_", tempor_range, "_1km_Aggregated_UNadj.tif"
)

urls <- c(vnm_ppp_1km_UNadj_urls, npl_ppp_1km_UNadj_urls, idn_ppp_1km_UNadj_urls)

# downloads
fnames <- urls %>% path_file()
download.file(urls, map(fnames, ~ path_join(c("marc_analyses/", .x))) %>% list_c(), method = "libcurl")

# shpfiles
walk(c("VNM", "NPL", "IDN"), ~ geodata::gadm(country = .x, level = 1, path = "marc_analyses/"))

# load files
vnm_ppp <- read_stars("marc_analyses/vnm_ppp_2020_1km_Aggregated_UNadj.tif") %>% set_names("pop_count")
npl_ppp <- read_stars("marc_analyses/npl_ppp_2020_1km_Aggregated_UNadj.tif") %>% set_names("pop_count")
idn_ppp <- read_stars("marc_analyses/idn_ppp_2020_1km_Aggregated_UNadj.tif") %>% set_names("pop_count")

vnm_shp <- read_rds("marc_analyses/gadm/gadm41_VNM_1_pk.rds") %>%
  terra::unwrap() %>%
  st_as_sf()
npl_shp <- read_rds("marc_analyses/gadm/gadm41_NPL_1_pk.rds") %>%
  terra::unwrap() %>%
  st_as_sf()
idn_shp <- read_rds("marc_analyses/gadm/gadm41_IDN_1_pk.rds") %>%
  terra::unwrap() %>%
  st_as_sf()

# test viz
ggplot() +
  geom_sf(data = vnm_shp) +
  geom_stars(data = vnm_ppp, mapping = aes(fill = pop_count), alpha = 0.5)

# areal intersection weighted population count for each admin unit
processor <- function(ppp_rast, shp_file) {
  ppp_sf <- ppp_rast %>%
    st_as_sf()

  weighted_ppp_df <- future_map(1:nrow(shp_file), \(p_idx){
    # get current province shape
    curr_p_shp <- shp_file[p_idx, ]

    # areal intersecting pop count map to province shape by `st_intersection()`
    weighted_ppp_sf <- ppp_sf %>%
      mutate(orig_cell_area = st_area(geometry) %>% drop_units()) %>%
      st_intersection(curr_p_shp) %>%
      mutate(
        areal_intersected_cell_area = st_area(geometry) %>% drop_units(),
        areal_weighted_pop_count = pop_count * areal_intersected_cell_area / orig_cell_area
      )
    sum_weighted_ppp <- sum(weighted_ppp_sf$areal_weighted_pop_count)

    tibble_row(
      GID_1 = curr_p_shp$GID_1,
      sum_weighted_ppp = sum_weighted_ppp
    )
  }) %>% list_rbind()
}

vnm_weighted_ppp_df <- processor(vnm_ppp, vnm_shp)
npl_weighted_ppp_df <- processor(npl_ppp, npl_shp)
idn_weighted_ppp_df <- processor(idn_ppp, idn_shp)
