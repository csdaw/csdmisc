#' Flexible filtering of matrix/data.frames
#'
#' @description Filter a matrix or data.frame based on the sum of values, zero,
#' or `NA` in each row. Optionally filter based on only certain columns.
#' Also optionally perform multiple filtering steps on different sets of columns,
#' wherein the function is vectorised over `op`, and `value`, and `pattern`
#' (see examples).
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
#' @return Returns `data` with the rows not matching the specified condition(s)
#' removed.
#'
#' @export
#'
#' @examples
#' # need to add some examples
filter_x <- function(data, x = c("na", "zero", "sum"),
                     op = c("==", "!=", "<=", ">=", "<", ">"),
                     value, pattern, ..., setop) {
  # check arguments
  if (!any(class(data) %in% c("data.frame", "matrix")))
    stop("data must be a data.frame or a matrix")
  match.arg(x)
  match.arg(op, several.ok = TRUE)
  if(!is.numeric(value)) stop("'value' must be numeric")

  if (!missing(setop)) {
    match.arg(setop, c("&", "|", "xor"))

    # check argument lengths
    args <- list(op, value, pattern)
    max_arg_len <- max(lengths(args))

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
    data[rowsum2(data, x, op, value, pattern, ...), , drop = FALSE]
  }
}
