
template_resources = function(name, package, ...) {
  system.file('rmarkdown', 'templates', name, 'resources', ..., package = package)
}

gsub_fixed = function(...) gsub(..., fixed = TRUE)

pandoc2.0 = function() rmarkdown::pandoc_available('2.0')

generate_id <- function() paste0(sample(c(letters, LETTERS), size = 8, replace = TRUE), collapse = "")