#' Flexible filtering of matrix/data.frames
#'
#' @description Filter a matrix or data.frame based on the sum of values, sum
#' of zeros, or sum of `NA`s in each row. Optionally filter based on only
#' certain columns. Also optionally perform multiple filtering steps on
#' different sets of columns, wherein the function is vectorised over `op`,
#' and `value`, and `pattern` (see examples).
#'
#' @inheritParams rowsum2
#' @param op `string` or `character` vector describing operator to use for
#' comparing row sums to a desired value.
#' Possible values: `"=="`, `"!="`, `"<="`, `">="`, `"<"`, `">"`.
#' @param value `numeric` value or vector to compare the row sums against.
#' @param pattern Optional `string` or `character` vector containing regular
#' expression(s) to match column names via \code{\link[base]{grep}}.
#' @param setop Optional `string` to indicate the method to combine the rows
#' returned by multiple filtering steps. This must be specified if using a
#' vector for `op`, `value`, or `pattern`.
#'
#' @return Returns `data` containing only the rows matching the
#' specified condition(s).
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(NA, 1:10, 0), nrow = 4, ncol = 3,
#' dimnames = list(NULL, c("sample1", "sample2", "sample3")))
#'
#' df <- data.frame(mat)
#'
#' # works with data.frame or matrix
#' filter_x(
#'   data = mat,
#'   x = "na",
#'   op = "==",
#'   value = 0
#' )
#' filter_x(
#'   data = df,
#'   x = "na",
#'   op = "==", value = 0
#' )
#'
#' # filter based on sum, sum of NA, or sum of zeros
#' filter_x(mat, "na", ">=", 1)
#' filter_x(mat, "sum", ">", 5)
#' filter_x(mat, "zero", "==", 1)
#'
#' # perform multiple filtering steps at the same time
#' # using column name pattern matching, the results being combined with 'setop'
#' # ('setop' can be &, |, xor which corresponds to AND, OR, SYMMETRIC DIFFERENCE)
#' # (you can supply multiple 'op', 'value', and 'pattern' values in a vector)
#' filter_x(mat, "sum", ">", c(4, 12), c("sample[1-2]", "sample[2-3]"), setop = "&")
#' filter_x(mat, "sum", ">", c(4, 12), c("sample[1-2]", "sample[2-3]"), setop = "|")
#' filter_x(mat, "sum", ">", c(4, 12), c("sample[1-2]", "sample[2-3]"), setop = "xor")
#'
filter_x <- function(data, x = c("na", "zero", "sum"),
                     op = c("==", "!=", "<=", ">=", "<", ">"),
                     value, pattern, ..., setop) {
  # check arguments
  if (!any(class(data) %in% c("data.frame", "matrix")))
    stop("data must be a data.frame or a matrix")
  match.arg(x)
  match.arg(op, several.ok = TRUE)
  if(!is.numeric(value)) stop("'value' must be numeric")

  # check argument lengths
  if (!missing(pattern)) args <- list(op, value, pattern) else args <- list(op, value)
  max_arg_len <- max(lengths(args))

  if (!missing(setop)) {
    match.arg(setop, c("&", "|", "xor"))

    # recycle arguments to length of longest
    recycled_args <- lapply(args, rep, length.out = max_arg_len)

    # allocate list to hold results
    rows_to_keep <- vector(mode = "list", length = max_arg_len)

    # loop over recycled arguments
    for (i in seq_len(max_arg_len)) {
      rows_to_keep[[i]] <- rowsum2(data, x, recycled_args[[1]][i], recycled_args[[2]][i], recycled_args[[3]][i], ...)
    }

    # output x with only rows of interest
    data[Reduce(setop, rows_to_keep), , drop = FALSE]

  } else {
    if (max_arg_len > 1) warning("Some arguments have length > 1 but 'setop' is missing. You should specify 'setop'")

    data[rowsum2(data, x, op, value, pattern, ...), , drop = FALSE]
  }
}
