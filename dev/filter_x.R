library(dplyr)
library(MSnbase)

e_data <- matrix(
  data = c(NA_real_, 1:5),
  nrow = 3,
  ncol = 2,
  dimnames = list(c("A", "B", "C"), c("sample1", "sample2"))
)

filter_x <- function(x, type = c("na", "zero", "sum"),
                     pattern, op = c("==", "<=", "<", ">=", ">", "!="), value, ...) {
  # check arguments
  match.arg(op)
  match.arg(type)

  # get number of NA/zero, or sum of values in column group
  if (type == "na") {
    test_values <- apply(
      X = is.na(x[, c(grep(pattern = pattern, x = colnames(x), ...)), drop = FALSE]),
      MARGIN = 1,
      FUN = sum
    )
  } else if (type == "zero") {
    test_values <- apply(
      X = x[, c(grep(pattern = pattern, x = colnames(x), ...)), drop = FALSE],
      MARGIN = 1,
      FUN = function(y) sum(y == 0, na.rm = TRUE)
    )
  } else if (type == "sum") {
    test_values <- rowSums(
      x = x[, c(grep(pattern = pattern, x = colnames(x), ...)), drop = FALSE],
      na.rm = TRUE
    )
  }

  # get operator function
  op <- match.fun(op)

  # subset x using operator to compare calculated values versus given value
  subset(x, op(test_values, value))
}

aaa <- filter_x(e_data, "sum", "sample1", ">", 0)
bbb <- filter_x(e_data, "sum", "sample2", "<", 5)


ccc <- mapply(
  FUN = function(p, o, v) filter_x(e_data, "sum", p, o, v),
  c("sample1", "sample2"),
  c(">", "<"),
  c(0, 5),
  SIMPLIFY = FALSE,
  USE.NAMES = FALSE
)

Reduce(union, ccc)
matrix(Reduce(setdiff, ccc), ncol = 2)

zzz <- function(o) {
  op <- as.name(o)
  op
}
zzz("<")
