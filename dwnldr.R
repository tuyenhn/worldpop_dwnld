library(tidyverse)
library(fs)

options(timeout = max(300, getOption("timeout")))

# datasets metadata
dataset_chksm_url <- "ftp://ftp.worldpop.org/assets/wpgpDatasets.md5"
if (!file_exists("wpgpDatasets.md5")) {
  download.file(dataset_chksm_url, "wpgpDatasets.md5")
}
dataset_chksm <- readLines("wpgpDatasets.md5")

dataset_url <- "ftp://ftp.worldpop.org/assets/wpgpDatasets.csv"
if (!file_exists("wpgpDatasets.csv")) {
  download.file(dataset_url, "wpgpDatasets.csv")
}

if (tools::md5sum("wpgpDatasets.csv") != dataset_chksm) {
  stop("Datasets metadata MD5 checksum not match")
}

datasets_metadat <- read_csv("wpgpDatasets.csv")
View(datasets_metadat)

metadat_iso3s <- datasets_metadat %>%
  pull(ISO3) %>%
  unique() %>%
  sort()

country_iso3 <- "VNM"
resolution <- "100m"

get_ppp_urls <- function(iso3, res, years, UNadj = TRUE) {
  if (!(toupper(iso3) %in% metadat_iso3s)) {
    stop(printf("ISO3 `%s` doesn't exist", iso3))
  }

  if (!(res %in% c("100m", "1km"))) {
    stop("Only supports \"100m\" and \"1km\"")
  }

  if (!all(years > 1999, years < 2021)) {
    stop("`years` should be between 2000 and 2020 (inclusive)")
  }

  suffix <- ifelse(UNadj, "_UNadj", "")

  paste0(
    "ftp://ftp.worldpop.org/",
    datasets_metadat %>%
      filter(ISO3 == iso3, Covariate %in% paste0("ppp_", years, suffix)) %>%
      pull(PathToRaster)
  )
}

downloader <- function(urls, dirs = NA, parallel = FALSE) {
  if (is.na(dirs)) {
    if(!dir_exists("Population_Global_2000_2020")){
      dir_create("Population_Global_2000_2020")
    }

    dirs <- map2(
      rep("Population_Global_2000_2020", length(urls)),
      path_file(urls),
      ~ path_join(c(.x, .y))
    ) %>% list_c()
  }

  if (length(urls) != length(dirs)) {
    stop("`urls` and `dirs` must have the same length")
  }

  if (parallel) {
    download.file(urls, dirs, method = "libcurl")
  } else {
    for (i in 1:length(urls)) {
      if (!file_exists(dirs[i])) {
        download.file(urls[i], dirs[i])
      }
    }
  }
}

vnm_ppp_100m_UNadj_urls <- get_ppp_urls("VNM", "100m", 2003:2020)

vnm_ppp_1km_UNadj_urls <- paste0(
  "ftp://ftp.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/", 2003:2020, "/VNM/vnm_ppp_", 2003:2020, "_1km_Aggregated_UNadj.tif"
)

downloader(vnm_ppp_100m_UNadj_urls)
