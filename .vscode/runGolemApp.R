usage <- "Usage: Rscript runGolemApp.R <path> <port> [--devmode]"
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop(usage)
}
path <- args[1]
port <- as.integer(args[2])
stopifnot(is.integer(port))
devmode <- "--devmode" %in% args

# Set working directory to the project root (Golem expects this)
setwd(path)

if (devmode) {
  shiny::devmode()
} else {
  options(shiny.autoreload = TRUE)
}


message("Running Golem app")
message("-----------------")
message(sprintf('golem::run_dev()\n'))

options(shiny.port = port)
# browseURL("http://127.0.0.1:1206")
golem::run_dev()
