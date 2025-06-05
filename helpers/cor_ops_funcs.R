get_cor_ops_nlme <- function() {
  cor_ops <- list()
  cor_ops$cor_structs <- c("corARMA", "corARMA", "corCompSymm")
  cor_ops$params <- list(list(value = c(0.1, 0.5, 0.9), p = c(1), q = c(0), fixed = TRUE), list(p = c(0), q = c(1)), list(value = c(0)))
  cor_ops$cor_group <- ""
  cor_ops$cor_data <- "ind"
  cor_ops$group_ops <- list(none = c(""))
  return(cor_ops)
}

get_cor_ops_inla <- function() {
  a <- "f(ind, model = \"ar1\")"
  b <- "f(ind, model = \"seasonal\", season.length = 10)"
  c <- "f(ind, model = \"iid\")"
  ops_vec <- c(a, b, c)
  return(ops_vec)
}