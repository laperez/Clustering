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



#'
#'  Method that return a list of files that exists in a directory
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

fill_cluster = function(data, clusters) {
  cluster_vector = array(dim = nrow(data))
  for (iterator_row in 1:length(clusters)) {
    for (iterator_col in 1:length(clusters[[iterator_row]])) {
      array_cluster = clusters[[iterator_row]]
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
            stringsAsFactors = FALSE,
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
      stringsAsFactors = FALSE,
      encoding = "UTF-8",
      strip.white = TRUE
    ))
  }

  return (result)
}

#'
#'
#'Method to filter only the external measurement columns
#'
#'@param data information matrix.
#'
#'@param external boolean indicating whether it is an external measurement.
#'
#'@return returns a data frame with the filtered columns.
#'
#'@keywords internal
#'

dataframe_by_metrics_evaluation <- function(data, external = TRUE) {

  if (external) {
    data <- transform_dataset(data)
  } else {
    data <- transform_dataset_internal(data)
  }

  columns <- colnames(data)
  index <- 2
  cols_remove <- NULL

  cols_remove <- c('Time')

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
#'Method that converts a matrix into numerical format.
#'
#'@param datas information matrix.
#'
#'@return return a matrix in numeric format.
#'
#'@keywords internal
#'

convert_numeric_matrix <- function(datas){

  ncol(datas)

  for (iterator in 1:ncol(datas)){

    if (is(datas[,iterator],"character")) {

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

#'
#'
#'Method for filtering clustering results.
#'
#'@param result data.frame with clustering results.
#'
#'@return a matrix with the filtered columns.
#'
#'@keywords internal
#'

resultClustering <- function(result) {

  colname <- colnames(result)
  colname <- colname[!colname %in% c(CONST_PRECISION_METRIC_ATTR,
                                     CONST_RECALL_METRIC_ATTR,
                                     CONST_ENTROPY_METRIC_ATTR,
                                     CONST_VARIATION_INFORMATION_METRIC_ATTR,
                                     CONST_F_MEASURE_METRIC_ATTR,
                                     CONST_FOWLKES_MALLOWS_INDEX_METRIC_ATTR,
                                     CONST_DUNN_METRIC,
                                     CONST_DUNN_METRIC_ATTR,
                                     CONST_SILHOUETTE_METRIC,
                                     CONST_SILHOUETTE_METRIC_ATTR,
                                     CONST_CONNECTIVITY_METRIC,
                                     CONST_CONNECTIVITY_METRIC_ATTR,
                                     CONST_TIME_INTERNAL_ATTR)]

  return (select(result, colname))

}

#'
#'Method to convert columns to ordinal.
#'
#'@param df data frame with the results.
#'
#'@return convert data frame to Ordinal.
#'
#'

convert_toOrdinal <- function(df) {

  nameColumns <- colnames(df)

  for (iterate in 1:length(nameColumns)) {

    if (tolower(nameColumns[iterate]) == tolower(CONST_RANKING)){
      df$Var = toOrdinal(as.numeric(df$Var))
    }

    if (tolower(nameColumns[iterate]) == tolower(CONST_PRECISION_METRIC_ATTR)){
      df$PrecisionAtt = toOrdinal(as.numeric(df$PrecisionAtt))
    }

    if (tolower(nameColumns[iterate]) == tolower(CONST_RECALL_METRIC_ATTR)){
      df$RecallAtt = toOrdinal(as.numeric(df$RecallAtt))
    }

    if (tolower(nameColumns[iterate]) == tolower(CONST_ENTROPY_METRIC_ATTR)){
      df$EntropyAtt = toOrdinal(as.numeric(df$EntropyAtt))
    }

    if (tolower(nameColumns[iterate]) == tolower(CONST_VARIATION_INFORMATION_METRIC_ATTR)){
      df$Variation_informationAtt = toOrdinal(as.numeric(df$Variation_informationAtt))
    }

    if (tolower(nameColumns[iterate]) == tolower(CONST_F_MEASURE_METRIC_ATTR)){
      df$F_measureAtt = toOrdinal(as.numeric(df$F_measureAtt))
    }

    if (tolower(nameColumns[iterate]) == tolower(CONST_FOWLKES_MALLOWS_INDEX_METRIC_ATTR)){
      df$Fowlkes_mallows_indexAtt = toOrdinal(as.numeric(df$Fowlkes_mallows_indexAtt))
    }

    if (tolower(nameColumns[iterate]) == tolower(CONST_DUNN_METRIC_ATTR)){
      df$DunnAtt = toOrdinal(as.numeric(df$DunnAtt))
    }

    if (tolower(nameColumns[iterate]) == tolower(CONST_SILHOUETTE_METRIC_ATTR)){
      df$SilhouetteAtt = toOrdinal(as.numeric(df$SilhouetteAtt))
    }

    if (tolower(nameColumns[iterate]) == tolower(CONST_CONNECTIVITY_METRIC_ATTR)){
      df$ConnectivityAtt = toOrdinal(as.numeric(df$ConnectivityAtt))
    }

    if (tolower(nameColumns[iterate]) == tolower(CONST_TIME_EXTERNAL_ATTR)){
      df$TimeAtt = toOrdinal(as.numeric(df$TimeAtt))
    }
  }

  return (df)
}

#'
#'
#'Method for refactoring the distance measurement name.
#'
#'@param nameMeasure name of the distance measure
#'
#'@return a string with the refactored measure name
#'
#'@keywords internal
#'

refactorName <- function(nameMeasure) {

    return (switch(nameMeasure,
                 kmeans_rcpp={"-"},
                 kmeans_arma={"-"},
                 mini_kmeans={"-"},
                 pearson_correlation={"correlation"},
                 hclust_euclidean={"euclidean"},
                 apclusterK_euclidean={"euclidean"},
                 apclusterK_manhattan={"manhattan"},
                 apclusterK_minkowski={"minkowski"},
                 agnes_euclidean={"euclidean"},
                 agnes_manhattan={"manhattan"},
                 clara_euclidean={"euclidean"},
                 clara_manhattan={"manhattan"},
                 daisy_manhattan={"manhattan"},
                 daisy_gower={"gower"},
                 daisy_euclidean={"euclidean"},
                 diana_euclidean={"euclidean"},
                 fanny_euclidean={"euclidean"},
                 fanny_manhattan={"fanny_manhattan"},
                 mona={"-"},
                 pam_euclidean={"euclidean"},
                 pam_manhattan={"manhattan"},
                 gmm_euclidean={"euclidean"},
                 gmm_manhattan={"manhattan"},
                 pvclust_euclidean={"euclidean"},
                 pvclust_correlation={"pvclust_correlation"}
                 ));
}
