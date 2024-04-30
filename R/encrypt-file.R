library(here)
library(openssl)

encrypt_file <- function(file, new_file){
  pubkey <- readRDS(here("fc-dashboard", "app-inputs", "rsa-pubkey.RDS"))

  raw_data <- charToRaw(read_file(file))

  envelope <- encrypt_envelope(raw_data, pubkey)

  saveRDS(envelope, new_file)
}
