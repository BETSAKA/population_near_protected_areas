suppressPackageStartupMessages({
  library(utils)
})

cran_repo <- "https://cloud.r-project.org"

required_packages <- c(
  "dplyr",
  "purrr",
  "readr",
  "stringr",
  "tidyr",
  "sf",
  "terra",
  "tibble"
)

missing_base <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_base) > 0) {
  install.packages(missing_base, repos = cran_repo)
}

if (!requireNamespace("exactextractr", quietly = TRUE)) {
  install.packages(
    "https://cloud.r-project.org/src/contrib/exactextractr_0.10.1.tar.gz",
    repos = NULL,
    type = "source"
  )
}

cat("R replication environment ready\n")
cat("Installed package library:\n")
print(.libPaths())
cat("Package availability:\n")
for (pkg in c(required_packages, "exactextractr")) {
  cat(pkg, requireNamespace(pkg, quietly = TRUE), "\n")
}