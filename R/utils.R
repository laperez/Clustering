#'
#'Method that format a number with four digits
#'
#'@param x number
#'@param k number of decimals
#'
#'@return a number convert to string with four digits
#'
#'@keywords internal
#'

specify_decimal <-
  function(x, k)
    trimws(format(round(x, k), nsmall = k))



#' Method that return a list of files that exists in a directory
#'
#' @param directory of the directory
#'
#' @return a vector with the files existing into of a directory
#'
#' @keywords internal
#'

path_dataset = function (directory) {
  on.exit(setwd(directory))

  files <-
    list.files(path = directory,
               pattern = c("\\.dat|\\.arff|\\.csv"))

  if (!is.null(files) && length(files) > 0)
    return (files)

  else
    return (CONST_NULL)
}


#' Method that fill vector
#'
#' @param data matriz or dataframe with dataset
#' @param appcluster data with the information of the appcluster object
#'
#' @return a vector fill with information
#'
#' @keywords internal
#'

fill_cluster_vector = function(data, appcluster) {
  cluster_vector = array(dim = nrow(data))
  for (iterator_row in 1:length(appcluster@clusters)) {
    for (iterator_col in 1:length(appcluster@clusters[[iterator_row]])) {
      array_cluster = appcluster@clusters[[iterator_row]]
      pos = array_cluster[iterator_col]

      cluster_vector[[pos]] = iterator_row

    }
  }

  return (cluster_vector)

}

#' Method in charge of detecting the limit of a dataset header.
#'
#' @param path of the dataset
#'
#' @return The row where the dataset attributes definition ends
#'
#' @keywords internal
#'

detect_definition_attribute = function(path) {
  con = file(path, "r")
  number_rows = 1
  while (T) {
    line = readLines(con, n = 1)
    if (!grepl("@", line)) {
      break

    }
    number_rows = number_rows + 1
  }

  close(con)

  return (number_rows - 1)
}

#'
#'Method that returns the number of variables in a dataset directory
#'
#'@param path dataset directory
#'
#'@return returns the number of variables in a dataset directory
#'
#'@keywords internal
#'

number_variables_dataset <- function(path) {
  files <- path_dataset(path)

  number_variables <- 0

  if (!is.null(files)) {
    for (pos in 1:length(files)) {
      if (extension_file(files[pos]) == CONST_FORMAT_ARFF_FILE) {
        df <- read_arff(files[pos])
      } else {
        df <- as.matrix(
          read.csv(
            file = files[pos],
            header = CONST_FALSE,
            comment.char = '@',
            stringsAsFactors = F,
            encoding = "UTF-8"
          )
        )
      }
      number_variables <- number_variables + ncol(df)
    }
  }

  return (number_variables)

}

#'
#'Method that return the extension of a file
#'
#'@param path dataset directory
#'
#'@return return the extension of file
#'
#'@keywords internal
#'

extension_file <- function(path) {
  return (file_ext(path))
}

#'
#'Method that converts a dataset into a matrix
#'
#'@param path dataset directory
#'
#'@return returns a matrix whose content is the dataset received as a parameter
#'
#'@keywords internal
#'

read_file <- function(path) {
  result <- CONST_NULL

  if (extension_file(path) == CONST_FORMAT_ARFF_FILE) {
    result <- read_arff(path)
  } else {
    result <- as.matrix(read.csv(
      file = path,
      header = CONST_FALSE,
      comment.char = '@',
      stringsAsFactors = F,
      encoding = "UTF-8",
      strip.white = T
    ))
  }

  return (result)
}

dataframe_by_metrics_evaluation <- function(data, external = T) {

  columns <- colnames(data)
  index <- 2
  cols_remove <- NULL

  if (external){
    cols_remove <- c('timeInternal')
  } else cols_remove <- c('timeExternal')

  for (number_col in 6:length(columns)){

    if (external){
      if (is_Internal_Metrics(columns[number_col])){
        cols_remove[index]= columns[number_col]
        index <- index+1
      }
    } else {
      if (is_External_Metrics(columns[number_col])){
        cols_remove[index]= columns[number_col]
        index <- index+1
      }
    }
  }

  return (select(data, -cols_remove))

}

#'
#'Method that converts a matrix into numerical format
#'
#'@param datas information matrix
#'
#'@return return a matrix in numeric format
#'
#'@keywords internal
#'

convert_numeric_matrix <- function(datas){

  ncol(datas)

  for (iterator in 1:ncol(datas)){

    if (class(datas[,iterator])=="character"){

      character_numeric <- ifelse(!is.na(as.numeric(datas[,iterator])), 1, 0)

      if(character_numeric > 0){
        datas[,iterator] = as.numeric(datas[,iterator])
      } else {
        datas[,iterator]=as.numeric(as.factor(datas[,iterator]))
      }

    } else {
      datas[,iterator] = as.numeric(datas[,iterator])
    }
  }

  return (datas)
}
