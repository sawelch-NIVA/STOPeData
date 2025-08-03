## code to prepare `inst/pubchem_2025_06_12_chemicals.txt` dataset goes here

# downloaded from

usethis::use_data(inst/data/raw/pubchem_2025_06_12_chemicals.txt, overwrite = TRUE)
chemicals <- read.delim(file = "inst/data/raw/ecotox_2025_06_12_chemicals.txt", sep = "|", header = TRUE, encoding = "ASCII") |>
  dplyr::mutate(PARAMETER_NAME = stringi::stri_enc_toutf8(chemical_name),
                CAS_RN = stringi::stri_enc_toutf8(cas_number), .keep = "none") |>
  arrow::write_parquet(sink = "inst/data/clean/ecotox_2025_06_12_chemicals.parquet")
