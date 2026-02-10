## Code to prepare example datasets
library(data.table)
library(terra)

sample_start_date <- "2020-05-01"
sample_end_date <- "2020-05-31"

# ===== 1. Process EC data =====
halmyran <- fread('D:/EC/R_codes_organized/FFP_R/data/Halmyran/Halmyran_2021_05.csv')
halmyran[halmyran == -9999] <- NA

# Convert date column to Date class
halmyran$date <- as.Date(halmyran$date, format = "%d/%m/%y")
halmyran$datetime <- as.POSIXct(halmyran$datetime)

# Filter by date range using the date column
ec_data_example <- halmyran[
  date >= sample_start_date & date <= sample_end_date,
  c('datetime', 'wind_speed', 'v_var', 'u*', 'wind_dir', 'L')
]

# Remove incomplete cases
ec_data_example <- ec_data_example[complete.cases(ec_data_example), ]

message("EC data: ", nrow(ec_data_example), " observations from ",
        min(ec_data_example$datetime), " to ", max(ec_data_example$datetime))

# ===== 2. Site coordinates =====
site_coords_example <- data.frame(
  long = 19.56924,
  lat = 64.159996
)

# ===== 3. Copy GRIB file as-is =====
blh_grib_path <- 'D:/EC/Data_organized/dynamic footprint/blh_data/degfert_blh.grib'

# ===== 4. Save to package =====
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

# Copy the GRIB file directly (no subsetting)
file.copy(blh_grib_path,
          "inst/extdata/degfert_blh.grib",
          overwrite = TRUE)

# Save CSV files
write.csv(ec_data_example, "inst/extdata/ec_data.csv", row.names = FALSE)
write.csv(site_coords_example, "inst/extdata/site_coords.csv", row.names = FALSE)

# Save as .rda (only EC and site coords)
usethis::use_data(ec_data_example, overwrite = TRUE)
usethis::use_data(site_coords_example, overwrite = TRUE)

message("Example datasets created successfully!")
message("Files created:")
message("  - inst/extdata/ec_data.csv (", nrow(ec_data_example), " rows)")
message("  - inst/extdata/site_coords.csv")
message("  - inst/extdata/degfert_blh.grib (copied as-is, ~5 MB)")
