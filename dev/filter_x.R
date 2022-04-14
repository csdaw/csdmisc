library(dplyr)
library(MSnbase)

e_data <- matrix(
  data = c(NA_real_, 1:5),
  nrow = 3,
  ncol = 2,
  dimnames = list(c("A", "B", "C"), c("sample1", "sample2"))
)

rowsum2 <- function(x, type = c("na", "zero", "sum"),
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

  # compare calculated values for each row versus the desired value
  op(test_values, value)
}

aaa <- rowsum2(e_data, "sum", "sample", "<", 6)
bbb <- rowsum2(e_data, "sum", "sample", ">", 3)


filter_x <- function(x, type = c("na", "zero", "sum"),
                     pattern, op, value, setop = NULL, ...) {
  # check arguments
  match.arg(type)
  if(!any(class(x) %in% c("data.frame", "matrix")))
    stop("x must be a data.frame or a matrix")

  # repeat if args are longer than single string
  if (any(lengths(list(pattern, op, value)) > 1)) {
    # check argument
    match.arg(setop, c("&", "|", "xor"))
    if(missing(setop)) stop("You must specify 'setop'")

    # generate list of logical vectors indicating which rows pass threshold
    passing_rows <- mapply(function(p, o, v) rowsum2(x, type, p, o, v, ...),
                           pattern, op, value, SIMPLIFY = FALSE)

    # combine list of logical vectors by the specified setop
    rows_to_keep <- Reduce(as.name(setop), passing_rows)
  } else {
    rows_to_keep <- rowsum2(x, type, pattern, op, value, ...)
  }

  # output filtered matrix/data.frame
  x[rows_to_keep, , drop = FALSE]
}

microbenchmark::microbenchmark(
  filter_x(e_data, "sum", "sample", "==", c(5, 7, 8, 9, 19, 11), setop = "xor"),
  filter_x(e_data, "sum", "sample", ">", 3),
  filterNA(e_data, pNA = 0, pattern = "01"),
  times = 1000L
)

microbenchmark::microbenchmark(
  filter_x(e_data, "na", "sample", "<", 1),
  filterNA(e_data, pNA = 0, pattern = "01"),
  times = 1000L
)


e_data3 <- filter_x(e_data, "sum", "sample", "==", c(5, 7), setop = "xor")

e_data2 <- e_data[aaa&bbb, , drop = FALSE]

test <- function(a, b, c) {
  print(any(lengths(list(a, b, c)) > 1))
}

test(NULL, 1, 1)

any(lengths(list(1:3, 4:9, 1)))

# example of the output from: op(test_values, value).
# You'll need to 'apply' it to get multiple vectors
ccc <- c(F, T, T)
ddd <- c(T, F, T)

ccc & ddd # intersection
ccc | ddd # union
xor(ccc, ddd) # setdiff

# Then just subset the matrix a single time with the output vector!!!!
