library(tidyverse)
library(fs)

options(timeout = max(300, getOption("timeout")))

# datasets metadata
dataset_chksm_url <- "ftp://ftp.worldpop.org/assets/wpgpDatasets.md5"
if (!file_exists("wpgpDatasets.md5")) {
  download.file(dataset_chksm_url, "wpgpDatasets.md5")
  dataset_chksm <<- readLines("wpgpDatasets.md5")
}

dataset_url <- "ftp://ftp.worldpop.org/assets/wpgpDatasets.csv"
if (!file_exists("wpgpDatasets.csv")) {
  download.file(dataset_url, "wpgpDatasets.csv")
}

if (tools::md5sum("wpgpDatasets.csv") != dataset_chksm) {
  stop("Datasets metadata MD5 checksum not match")
}


datasets_metadat <- read_csv("wpgpDatasets.csv")

vnm_ppp_100m_UNadj_urls <- paste0(
  "ftp://ftp.worldpop.org/",
  datasets_metadat %>%
    filter(ISO3 == "VNM", Covariate %in% paste0("ppp_", 2003:2020, "_UNadj")) %>%
    pull(PathToRaster)
)
vnm_ppp_100m_UNadj_dirs <- map2(
  rep("Global_2000_2020", length(vnm_ppp_100m_UNadj_urls)),
  path_file(vnm_ppp_100m_UNadj_urls),
  ~ path_join(c(.x, .y))
) %>% list_c()

vnm_ppp_1km_UNadj_urls <- paste0(
  "ftp://ftp.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/", 2003:2020, "/VNM/vnm_ppp_", 2003:2020, "_1km_Aggregated_UNadj.tif"
)
vnm_ppp_1km_UNadj_dirs <- map2(
  rep("Global_2000_2020_1km_UNadj", length(vnm_ppp_1km_UNadj_urls)),
  path_file(vnm_ppp_1km_UNadj_urls),
  ~ path_join(c(.x, .y))
) %>% list_c()

downloader <- function(urls, dirs, parallel = FALSE) {
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

downloader(vnm_ppp_100m_UNadj_urls, vnm_ppp_100m_UNadj_dirs, TRUE)
