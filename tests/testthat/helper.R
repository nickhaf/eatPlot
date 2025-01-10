# Function for sourcing Rcode from .Rmd file
ksource <- function(x, ...) {
  source(knitr::purl(x, output = tempfile()), ...)
}
