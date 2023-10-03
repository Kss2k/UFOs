
# old
# Deprecated version of %pre%
#'
#' @param .data array like object, or list
#' @param prefix prefix to gather elements based upon
#'
#' @return
#' @export
#'
#' @examples
#'
'%#%' <- function(.data, prefix) {
  prefix <- as.character(substitute(prefix))
  .data[startsWith(colnames(.data), prefix)]
}

#' Select Variable by prefixoperator
#'
#' @param .data array-like object, or list
#' @param prefix prefix to find variables by
#'
#' @return
#' @export
#'
#' @examples iris %pre% Sepal
'%pre%' <- function(.data, prefix) {
  string.prefix <- substitute(prefix) |>
    as.character()
  .data[startsWith(names(.data), string.prefix)]
}

#' Select variables matching prefix
#'
#' @param .data array-like object, or list
#' @param prefix prefix to find variables by
#'
#' @return
#' @export
#'
#' @examples prefix(iris, Sepal)
prefix <- function(.data, prefix) {
  string.prefix <- substitute(prefix) |>
    as.character()
  .data[startsWith(names(.data), string.prefix)]
}

#' Select varibles not matching prefix
#'
#' @param .data
#' @param prefix
#'
#' @return
#' @export
#'
#' @examples antiPrefix(iris, Sepal)
antiPrefix <- function(.data, prefix) {
  string.prefix <- substitute(prefix) |>
    as.character()
  .data[!startsWith(names(.data), string.prefix)]
}

#' Select varibles not matching prefix
#'
#' @param .data
#' @param prefix
#'
#' @return
#' @export
#'
#' @examples iris %!pre% Sepal
'%!pre%' <- function(.data, prefix) {
  string.prefix <- substitute(prefix) |>
    as.character()
  .data[!startsWith(names(.data), string.prefix)]
}

#' Select variables containing pattern
#'
#' @param .data
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples iris %match% Len
'%match%' <- function(.data, pattern) {
  names <- names(.data)
  string.pattern <- substitute(pattern) |>
    as.character()
  extractedNames <- grep(string.pattern, names, value = TRUE)
  .data[extractedNames]
}

#' Select variables not containing pattern
#'
#' @param .data
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples iris %!match% Len
'%!match%' <- function(.data, pattern) {
  names <- names(.data)
  string.pattern <- substitute(pattern) |>
    as.character()
  extractedNames <- grep(string.pattern, names, value = TRUE)
  .data[!(names %in% extractedNames)]
}

#' Select variables matching regular expression
#'
#' @param .data
#' @param regex
#'
#' @return
#' @export
#'
#' @examples iris %rgx% "(Sepal|Petal).(Length)"
#'
'%rgx%' <- function(.data, regex) {
  # FUNCTIONS: regexpr and gregexpr
  names <- names(.data)
  .data[grep(regex,names, value = TRUE)]

}

#' Select variables not matching regular expression
#'
#' @param .data
#' @param regex
#'
#' @return
#' @export
#'
#' @examples iris %!rgx% "^Petal"
'%!rgx%' <- function(.data, regex) {
  # FUNCTIONS: regexpr and gregexpr
  names <- names(.data)
  extractedNames <- grep(regex,names, value = TRUE)
  .data[!(names %in% extractedNames)]

}



#' Select variables ending with pattern
#'
#' @param .data
#' @param suffix
#'
#' @return
#' @export
#'
#' @examples iris %suf% Length
'%suf%' <- function(.data, suffix) {
  string.suffix <- substitute(suffix) |>
    as.character()
  .data[endsWith(names(.data), string.suffix)]
}

#' Select variables not ending with pattern
#'
#' @param .data
#' @param suffix
#'
#' @return
#' @export
#'
#' @examples iris %!suf% Length
'%!suf%' <- function(.data, suffix) {
  string.suffix <- substitute(suffix) |>
    as.character()
  .data[!endsWith(names(.data), string.suffix)]
}

#' Split data by variables
#'
#' @param .data
#' @param by.variable
#'
#' @return
#' @export
#'
#' @examples
'%by%' <- function(.data, by.variable, string = FALSE) {
  # String with varname
    # Since by can be called recursively and it accepts the varname unevaluated/not in string-format
    # it becomes tricky to call the function recursively, if we for example called it again using by.variable,
    # the new variablename would become "by.variable", which is a problem.

  # If we call %by% on dataframe
  if (string == FALSE) {
    string.varname <- substitute(by.variable) |>
      as.character()
  }
  # If %by% is called within byList()
  else if (string == TRUE) {
    string.varname <- by.variable
  }

  # Check depth of object to see whether we need to call byList
  depth <- listDepth(.data)
  if (depth > 1) return(byList(.data, string.varname))

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

#' split dataset by prefix of variable.
#' pby is short for "prefix by"
#' @param .data
#' @param prefix the funciton assumes the prefix uniquely identifies the variable
#'
#' @return
#' @export
#'
#' @examples iris %#by% Spe
#' iris %#by% Spe %#by% Petal.Len
#'
'%pby%' <- function(.data, prefix) {

  string.prefix <- substitute(prefix) |>
    as.character()
  regex <- paste0("^", string.prefix)

  variableNames <- names(lowestLayer(.data))
  string.varname <- variableNames[grep(regex, variableNames)[1]]
  print(string.varname)

  '%by%'(.data, string.varname, string = TRUE)

}

listDepth <- function(list, count = 0) {
  # This function has a very specific usecase, where the depth of the list is uniformly distributed (hopefully?)
  # I can therefore avoid searching the whole thing, for the deepest node.
  if (is.null(list)|(is.list(list) == FALSE)) return(count)
  listDepth(list[[1]], count + 1)
}

lowestLayer <- function(list, count = 0) {
  layers <- listDepth(list)
  if (layers <= 1) return(list)

  lowestLayer(list[[1]])
}


byList <- function(list, string.varname) {
  lapply(list, '%by%', string.varname, string = TRUE)
}
