# # Grab cited packages from Rmd file
# lines <- readLines("rjournal/greenwell-boehmke.Rmd")
# z <- sapply(lines, FUN = function(x) {
#   stringi::stri_extract(x, pattern = "pkg\\{[:alnum:]*\\}", regex = TRUE)
# })
# z <- unname(sort(unique(z)))
# z <- gsub("^pkg\\{", replacement = "", x = z)
# z <- gsub("\\}$", replacement = "", x = z)

# Remove current bib files, if they exist
files <- c("rjournal/greenwell.bib", "rjournal/packages.bib")
for (f in files) {
  if (file.exists(f)) {
    file.remove(f)
  }
}

# List of cited packages to include in the bibliography
pkgs <- c(
  "iml",
  "Rcpp"
)

# Make sure the packages listed above are installed and up to date
required_pkgs <- setdiff(pkgs, installed.packages()[, "Package"])
install.packages(required_pkgs)

# Generate bibliography
knitr::write_bib(pkgs, file = "rjournal/greenwell.bib", tweak = TRUE,
                 width = NULL, prefix = "R-")

# Create new bib file
file.append("rjournal/greenwell.bib", "rjournal/general.bib")
