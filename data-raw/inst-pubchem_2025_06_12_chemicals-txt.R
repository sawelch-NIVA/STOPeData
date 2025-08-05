## code to prepare `inst/ecotox_2025_06_12_chemicals.txt` dataset goes here

# downloaded from EPA ECOTOX

# usethis::use_data(
#   "inst/data/raw/ecotox_2025_06_12_chemicals.txt",
#   overwrite = TRUE
# )

chemicals <- readr::read_delim(
  file = "inst/data/raw/ecotox_2025_06_12_chemicals.txt",
  delim = "|",
  col_names = TRUE,
  col_types = "cccc",
  trim_ws = TRUE,
) |>
  dplyr::mutate(
    # Use iconv with substitution for problematic characters
    PARAMETER_NAME = iconv(chemical_name, to = "UTF-8", sub = "") |>
      # ASCII apostrophes cause problems?
      gsub(pattern = "'", replacement = "'"),
    CAS_RN = iconv(cas_number, to = "UTF-8", sub = ""),
    .keep = "none"
  ) |>
  arrow::write_parquet(
    sink = "inst/data/clean/ecotox_2025_06_12_chemicals.parquet"
  )
