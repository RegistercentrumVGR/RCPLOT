# Source code for downloading and cleaning up map data.

library(magrittr)
requireNamespace("readxl")
requireNamespace("curl")
requireNamespace("dplyr")
requireNamespace("stringr")
requireNamespace("sf")

se4 <- tempfile()
se7 <- tempfile()

download.file(url = "http://api.thenmap.net/v2/se-4/geo/2020-06-03",
              destfile = se4)
download.file(url = "http://api.thenmap.net/v2/se-7/geo/2020-06-03",
              destfile = se7)

counties <- sf::read_sf(se4)
municipalities <- sf::read_sf(se7)

# Pad id with 0
counties[["id"]] <- stringr::str_pad(
    counties[["id"]],
    width = 2,
    side = "left",
    pad = "0"
)

# Pad id with 0
municipalities[["id"]] <- stringr::str_pad(
  municipalities[["id"]],
  width = 4,
  side = "left",
  pad = "0"
)

# Get names for county and municipality
#___________________________________________________

# Download codes from SCB
xlsxfile = "./data-raw/codes.xls"
curl::curl_download(
  url = paste0(
    "https://www.scb.se/contentassets/7a89e48960f",
    "741e08918e489ea36354a/kommunlankod_20211229.xls"
  ),
  destfile = xlsxfile
)

# Clean-up data
df <- readxl::read_excel(xlsxfile)
df <- df[6:nrow(df),]
names(df) <- c("Code", "Name")

# Split code into Region and municipality code
codes <- df %>%
  dplyr::mutate(
    Code = stringr::str_pad(
      as.character(Code),
      width = 4,
      side = "left",
      pad = "0"
    ),
    RegionID = stringr::str_pad(
      as.character(substr(Code, 1,2)),
      width = 2,
      side = "left",
      pad = "0"
    ),
    MunicipID = stringr::str_pad(
      as.character(substr(Code, 3,4)),
      width = 2,
      side = "left",
      pad = "0"
    ),
    RegionID_tmp = ifelse(RegionID == "00", MunicipID, RegionID),
    MunicipID = ifelse(RegionID == "00", NA_character_, MunicipID),
    RegionID = RegionID_tmp,
    Name = gsub("(s län| län)", "", Name),
    Name = stringi::stri_enc_toutf8(Name)
  ) %>%
  dplyr::select(-RegionID_tmp, -Code)

# Separate data into county and municipality
county_names <- codes[is.na(codes$MunicipID),c("Name", "RegionID")]

municip_names <- codes[!is.na(codes$MunicipID),c("Name", "RegionID", "MunicipID")]

municip_names[["id"]] <- paste0(municip_names[["RegionID"]], municip_names[["MunicipID"]])

counties <- dplyr::full_join(
    counties,
    county_names,
    by = c("id" = "RegionID")
)


municipalities <- dplyr::full_join(
  municipalities,
  municip_names,
  by = "id"
)
# Save data to correct folder
save(counties, file = file.path("data", "counties.rda"),
     compress = "bzip2", ascii = TRUE)
save(municipalities, file = file.path("data", "municipalities.rda"),
     compress = "bzip2", ascii = TRUE)

#usethis::use_data(counties, overwrite = TRUE)
#usethis::use_data(municipalities, overwrite = TRUE)
# Remove xls-file if it exists
if(file.exists(xlsxfile)){
  file.remove(xlsxfile)
}
