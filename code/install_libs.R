# Set pkgs
pkgs = c(
  "tidyverse",
  "tm",
  "tidytext",
  "sentimentr",
  "treemap",
  "quanteda",
  "SentimentAnalysis",
  "RTextTools",
  "e1071",
  "spacyr"
)

# Install pkgs not yet installed
installed_packages = pkgs %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}

# Load pkgs
invisible(lapply(pkgs, library, character.only = TRUE))

#Load dev version of academictwitteR
#devtools::install_github("cjbarrie/academictwitteR", build_vignettes = TRUE)
require(academictwitteR)
