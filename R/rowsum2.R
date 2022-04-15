#' Flexible summation of matrix/data.frame rows
#'
#' @description Count the sum of values, zero, or `NA` in the each row of a
#' data.frame or matrix. Optionally count only certain columns and/or compare
#' the row sums to a desired value.
#'
#' @param data `data.frame` or `matrix`.
#' @param x `string` summation method, one of either: `"na"`, `"zero"`, or `"sum"`.
#' @param op Optional `string` describing operator to use for comparing row sums
#' to a desired value. One of either: `"=="`, `"!="`, `"<="`, `">="`, `"<"`, `">"`.
#' @param value Optional `numeric` value to compare the row sums against.
#' @param pattern Optional `string` containing a regular expression to match
#' column names via \code{\link[base]{grep}}.
#' @param ... Other arguments to be passed into \code{\link[base]{grep}}.
#'
#' @return Returns a `logical` vector if 'op' and 'value' have been specified.
#' Otherwise returns a `numeric` vector.
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(NA, 1:6, 0), nrow = 4, ncol = 2,
#'               dimnames = list(NULL, c("sample1", "sample2")))
#' df <- data.frame(mat)
#'
#' # works with data.frame or matrix
#' rowsum2(mat, "na")
#' rowsum2(df, "na")
#'
#' # get just the sums (should use rowSums directly in this particular case),
#' # or sum of na/zero for each row
#' rowsum2(mat, "sum")
#' rowsum2(mat, "na")
#' rowsum2(mat, "zero")
#'
#' # compare the row sums against a value of interest to
#' # see which rows 'pass'
#' rowsum2(mat, "sum", ">", 5)
#'
#' # get row sums for only certain columns based on
#' # matching columns with grep
#' rowsum2(mat, "na", pattern = "sample1")
#'
#' # combine row sum comparison and column matching
#' rowsum2(df, "na", "==", 0, "sample1")
#'
rowsum2 <- function(data, x = c("na", "zero", "sum"),
                    op, value, pattern, ...) {
  # check arguments
  if (!any(class(data) %in% c("data.frame", "matrix")))
    stop("data must be a data.frame or a matrix")
  match.arg(x)
  if (!missing(op)) match.arg(op, choices = c("==", "!=", "<=", ">=", "<", ">"))
  if (!missing(value))
    if(!is.numeric(value)) stop("'value' must be numeric")

  # get colnames if pattern isn't missing
  if (!missing(pattern)) {
    if(is.null(colnames(data))) stop("'data' must have colnames if 'pattern' is used")
    cols <- grep(pattern = pattern, x = colnames(data), ...)
  } else {
    cols <- seq_len(ncol(data))
  }

  # get number of NA/zero, or sum of values in column group
  if (x == "na") {
    test_values <- rowSums(
      is.na(data[, cols, drop = FALSE])
    )
  } else if (x == "zero") {
    test_values <- rowSums(
      data[, cols, drop = FALSE] == 0,
      na.rm = TRUE
    )
  } else if (x == "sum") {
    test_values <- rowSums(
      data[, cols, drop = FALSE],
      na.rm = TRUE
    )
  }

  if(!missing(value)) {
    # get operator function
    op <- match.fun(op)

    # compare calculated values versus the desired value
    op(test_values, value)
  } else {
    # output just the sums
    test_values
  }
}
