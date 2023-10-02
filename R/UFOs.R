
# old
# Deprecated version of %pre%
#'
#' @param .data
#' @param prefix
#'
#' @return
#' @export
#'
#' @examples
'%#%' <- function(.data, prefix) {
  prefix <- as.character(substitute(prefix))
  .data[startsWith(colnames(.data), prefix)]
}

'%pre%' <- function(.data, prefix) {
  string.prefix <- substitute(prefix) |>
    as.character()
  .data[startsWith(names(.data), string.prefix)]
}

'%!pre%' <- function(.data, prefix) {
  string.prefix <- substitute(prefix) |>
    as.character()
  .data[!startsWith(names(.data), string.prefix)]
}

'%match%' <- function(.data, pattern) {
  names <- names(.data)
  string.pattern <- substitute(pattern) |>
    as.character()
  extractedNames <- grep(string.pattern, names, value = TRUE)
  .data[extractedNames]
}

'%!match%' <- function(.data, pattern) {
  names <- names(.data)
  string.pattern <- substitute(pattern) |>
    as.character()
  extractedNames <- grep(string.pattern, names, value = TRUE)
  .data[!(names %in% extractedNames)]
}

'%rgx%' <- function(.data, regex) {
  # FUNCTIONS: regexpr and gregexpr
  names <- names(.data)
  .data[grep(regex,names, value = TRUE)]

}

'%!rgx%' <- function(.data, regex) {
  # FUNCTIONS: regexpr and gregexpr
  names <- names(.data)
  extractedNames <- grep(regex,names, value = TRUE)
  .data[!(names %in% extractedNames)]

}



'%suf%' <- function(.data, suffix) {
  string.suffix <- substitute(suffix) |>
    as.character()
  .data[endsWith(names(.data), string.suffix)]
}

'%!suf%' <- function(.data, suffix) {
  string.suffix <- substitute(suffix) |>
    as.character()
  .data[!endsWith(names(.data), string.suffix)]
}

'%by%' <- function(.data, by.variable) {
  # String with varname
  string.varname <- substitute(by.variable) |>
    as.character()

  # output: list with dataframes
  list.output <- list()

  # Unique vallues to split dataframe by
  unique.values <- unique(.data[string.varname]) |>
    unlist() |>
    as.vector()

  # Number of unique values
  n.values <- length(unique.values)


  for (i in 1:n.values){
    # Subsetting the dataset by ith unique value
    data.subset <- subset(.data, .data[string.varname] == unique.values[i])

    # name for split object
    name <- paste0(string.varname, " = ", unique.values[i])

    # adding subset to list
    list.output[[name]] <- data.subset
  }
  list.output
}

'%byPre%' <- function(.data, prefix) {

  string.prefix <- substitute(prefix) |>
    as.character()
  var <- .data[startsWith(names(.data), string.prefix)][,1]

  # String with varname
  string.varname <- names(var)
  print(string.varname)
  # output: list with dataframes
  list.output <- list()

  # Unique vallues to split dataframe by
  unique.values <- unique(.data[string.varname]) |>
    unlist() |>
    as.vector()

  # Number of unique values
  n.values <- length(unique.values)


  for (i in 1:n.values){
    # Subsetting the dataset by ith unique value
    data.subset <- subset(.data, .data[string.varname] == unique.values[i])

    # name for split object
    name <- paste0(string.varname, " = ", unique.values[i])

    # adding subset to list
    list.output[[name]] <- data.subset
  }
  list.output
}
