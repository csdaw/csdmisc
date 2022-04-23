library(dplyr)
library(MSnbase)

e_data <- matrix(
  data = c(NA_real_, 1:5),
  nrow = 3,
  ncol = 2,
  dimnames = list(c("A", "B", "C"), c("sample1", "sample2"))
)

<<<<<<< HEAD
rowsum2 <- function(x, type = c("na", "zero", "sum"),
=======
rowsum2 <- function(data, x = c("na", "zero", "sum"),
>>>>>>> origin
                    pattern, op = c("==", "<=", "<", ">=", ">", "!="), value, ...) {
  # check arguments
  if (!any(class(data) %in% c("data.frame", "matrix")))
    stop("data must be a data.frame or a matrix")
  match.arg(x)
  stopifnot(is.character(pattern))
  match.arg(op)
  stopifnot(is.numeric(value))

  # get colnames
  coln <- colnames(data)

  # get number of NA/zero, or sum of values in column group
  if (x == "na") {
    test_values <- rowSums(
      is.na(data[, grep(pattern = pattern, x = coln, ...), drop = FALSE])
    )
  } else if (x == "zero") {
    test_values <- rowSums(
      data[, grep(pattern = pattern, x = coln, ...), drop = FALSE] == 0,
      na.rm = TRUE
    )
  } else if (x == "sum") {
    test_values <- rowSums(
      data[, grep(pattern = pattern, x = coln, ...), drop = FALSE],
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

filter_x <- function(data, x = c("na", "zero", "sum"),
                     pattern, op, value, setop = c("&", "|", "xor"), ...) {
  # check arguments
  if (!any(class(data) %in% c("data.frame", "matrix")))
    stop("data must be a data.frame or a matrix")
  match.arg(x)
  stopifnot(is.character(pattern))
  stopifnot(is.character(op))
  stopifnot(is.numeric(value))
  match.arg(setop)

  # check argument lengths
  args <- list(pattern, op, value)
  max_arg_len <- max(lengths(args))

  if (max_arg_len > 1) {
    # recycle arguments to length of longest
    recycled_args <- lapply(args, rep, length.out = max_arg_len)

    # allocate list to hold results
    rows_to_keep <- vector(mode = "list", length = max_arg_len)

    # loop over recycled arguments
    for (i in seq_len(max_arg_len)) {
      rows_to_keep[[i]] <- rowsum2(data, x, recycled_args[[1]][i], recycled_args[[2]][i], recycled_args[[3]][i], ...)
    }

    # output x with only rows of interest
    data[Reduce(setop, rows_to_keep), ]

  } else {
    # output x with only rows of interest
    data[rowsum2(data, x, pattern, op, value, ...), ]
  }
}

filter_x <- function(data, x = c("na", "zero", "sum"),
                     pattern, op, value, setop = c("&", "|", "xor"), ...) {
  # check arguments
  if (!any(class(data) %in% c("data.frame", "matrix")))
    stop("data must be a data.frame or a matrix")
  match.arg(x)
  stopifnot(is.character(pattern))
  stopifnot(is.character(op))
  stopifnot(is.numeric(value))
  match.arg(setop)

  # check argument lengths
  args <- list(pattern, op, value)
  max_arg_len <- max(lengths(args))

  if (max_arg_len > 1) {
    rows_to_keep <- mapply(
      function(p, o, v) {
        rowsum2(data, x, p, o, v, ...)
      },
      pattern,
      op,
      value,
      SIMPLIFY = FALSE
    )

    data[Reduce(setop, rows_to_keep), ]

  } else {
    data[rowsum2(data, x, pattern, op, value, ...), ]
  }
}

library(MSnbase)

e_data <- data.frame(
  "sample1" = sample(0:20, 100000L, replace = TRUE),
  "sample2" = sample(0:20, 100000L, replace = TRUE)
) %>%
  as.matrix()

microbenchmark::microbenchmark(
  filter_x(e_data, "sum", c("sample1", "sample2"), "==", 1:5, "|"),
  filterNA(e_data, pNA = 0, pattern = "01"),
  times = 100L
)





patterns <- c("sample", "sample2")
values <- c(1, 3, 4, 6, 7)

my_fun <- function(p, v) {
  args <- list(p, v)
  max_len <- max(lengths(args))
  out <- lapply(args, rep, length.out = max_len)
  out
}

my_fun(patterns, values)

aaa <- vector(mode = "list", length = 2)

for (i in seq_along(patterns)) {
  aaa[[i]] <- filter_x(e_data, "sum", patterns[i], "==", values[i])
}

e_data[Reduce("|", aaa), ]

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
