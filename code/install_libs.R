pkgs = c("tidyverse", "academictwitteR", "tm", 
         "tidytext", "sentimentr", "treemap"
         )

# Install packages not yet installed
installed_packages = pkgs %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}

# Packages loading
invisible(lapply(pkgs, library, character.only = TRUE))
