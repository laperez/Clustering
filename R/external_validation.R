#'
#' Method in charge of obtaining those metrics that are external from those
#' indicated.
#'
#' @param metrics Array with the metrics used in the calculation.
#'
#' @return Return an array with the metrics that are external.
#'
#' @keywords internal
#'

row_name_df_external = function(metrics) {
  result = c()

  result = c(result, CONST_ALGORITHM)
  result = c(result, CONST_DISTANCE)
  result = c(result, CONST_CLUSTERS)
  result = c(result, CONST_DATASET)
  result = c(result, CONST_RANKING)
  result = c(result, CONST_TIME_EXTERNAL)

  insertTime <- 0

  for (iterate in 1:length(metrics)) {

    if (tolower(metrics[iterate]) == tolower(CONST_ENTROPY_METRIC)) {
      result = c(result, CONST_ENTROPY_METRIC)
    }

    if (tolower(metrics[iterate]) == tolower(CONST_VARIATION_INFORMATION_METRIC)) {
      result = c(result, CONST_VARIATION_INFORMATION_METRIC)
    }

    if (tolower(metrics[iterate]) == tolower(CONST_PRECISION_METRIC)) {
      result = c(result, CONST_PRECISION_METRIC)
    }

    if (tolower(metrics[iterate]) == tolower(CONST_RECALL_METRIC)) {
      result = c(result, CONST_RECALL_METRIC)
    }

    if (tolower(metrics[iterate]) == tolower(CONST_F_MEASURE_METRIC)) {
      result = c(result, CONST_F_MEASURE_METRIC)
    }

    if (tolower(metrics[iterate]) == tolower(CONST_FOWLKES_MALLOWS_INDEX_METRIC)) {
      result = c(result, CONST_FOWLKES_MALLOWS_INDEX_METRIC)
    }

    # Varibles
    if (tolower(metrics[iterate]) == tolower(CONST_TIME_EXTERNAL_ATTR)) {
      if (insertTime == 0) {
        result = c(result, CONST_TIME_EXTERNAL_ATTR)
        insertTime <- 1
      }
    }

    if (tolower(metrics[iterate]) == tolower(CONST_ENTROPY_METRIC_ATTR)) {
      result = c(result, CONST_ENTROPY_METRIC_ATTR)
    }

    if (tolower(metrics[iterate]) ==
        tolower(CONST_VARIATION_INFORMATION_METRIC_ATTR)) {
      result = c(result, CONST_VARIATION_INFORMATION_METRIC_ATTR)
    }

    if (tolower(metrics[iterate]) == tolower(CONST_PRECISION_METRIC_ATTR)) {
      result = c(result, CONST_PRECISION_METRIC_ATTR)
    }

    if (tolower(metrics[iterate]) == tolower(CONST_RECALL_METRIC_ATTR)) {
      result = c(result, CONST_RECALL_METRIC_ATTR)
    }

    if (tolower(metrics[iterate]) == tolower(CONST_F_MEASURE_METRIC_ATTR)) {
      result = c(result, CONST_F_MEASURE_METRIC_ATTR)
    }

    if (tolower(metrics[iterate]) ==
        tolower(CONST_FOWLKES_MALLOWS_INDEX_METRIC_ATTR)) {
      result = c(result, CONST_FOWLKES_MALLOWS_INDEX_METRIC_ATTR)
    }

  }

  return (result)
}


#'
#' Method that applicate differents external metrics about a data frame or
#' matrix, for example precision, recall etc
#'
#' @param column_dataset_label Array containing the distribution of the data in
#' the cluster.
#'
#' @param clusters_vector Array that containe tha data grouped in cluster.
#'
#' @param metric Array with external metric types.
#'
#' @return Return a list of the external results initialized to zero.
#'
#' @keywords internal
#'

external_validation = function(column_dataset_label,
                               clusters_vector,
                               metric = CONST_NULL) {
  start.time = Sys.time()

  precision = 0.0

  recall = 0.0

  entropy = 0.0

  f_measure = 0.0

  variation_information = 0.0

  fowlkes_mallows_index = 0.0

  if (!is.vector(column_dataset_label) ||
      !is.numeric(column_dataset_label))
    stop('The column_dataset_label field must be numeric')

  if (!is.numeric(clusters_vector))
    stop ('Vector_cluster field must be numeric')

  if (length(column_dataset_label) != length(clusters_vector))
    stop('The length of column_dataset_label must be the same
         as vector_clusters')

  if (is.integer(column_dataset_label))
    column_dataset_label = as.numeric(column_dataset_label)

  if (is.integer(column_dataset_label))
    column_dataset_label = as.numeric(column_dataset_label)

  if (is.integer(clusters_vector))
    clusters_vector = as.numeric(clusters_vector)

  if (!is.null(metric) && !is.vector(metric))
    stop('The metric field must be a vector')

  table_convert =
    convert_table (clusters_vector, column_dataset_label)

  conversion_data_frame = as.data.frame.matrix(table_convert)

  true_positive_plus_false_positive =
    sum(asNumeric(chooseZ(rowSums(
      conversion_data_frame
    ), 2)))

  true_positive_plus_false_negative =
    sum(asNumeric(chooseZ(colSums(
      conversion_data_frame
    ), 2)))

  true_positive = sum(asNumeric(chooseZ(as.vector(
    as.matrix(conversion_data_frame)
  ), 2)))

  false_positive =
    true_positive_plus_false_positive - true_positive

  false_positive =
    true_positive_plus_false_positive - true_positive

  false_negative =
    true_positive_plus_false_negative - true_positive

  if (is.null(metric)) {
    precision = precision_metric (true_positive, false_positive)

    recall = recall_metric (true_positive, false_negative)

    entropy =
      entropy_metric (conversion_data_frame,
                      table_convert,
                      column_dataset_label)

    variation_information =
      variation_information_metric(conversion_data_frame, table_convert)

    f_measure =
      fmeasure_metric (true_positive, false_positive, false_negative)

    fowlkes_mallows_index =
      fowlkes_mallows_index_metric(true_positive, false_positive,
                                   false_negative)

  } else {
    for (me in metric) {
      if (me == CONST_PRECISION_METRIC) {
        precision =
          precision_metric (true_positive, false_positive)
      }

      if (me == CONST_RECALL_METRIC)
        recall = recall_metric (true_positive, false_negative)
      if (me == CONST_ENTROPY_METRIC)
        entropy =
          entropy_metric (conversion_data_frame,
                          table_convert,
                          column_dataset_label)
      if (me == CONST_VARIATION_INFORMATION_METRIC)
        variation_information =
        variation_information_metric(conversion_data_frame, table_convert)
      if (me == CONST_F_MEASURE_METRIC)
        f_measure =
          fmeasure_metric (true_positive, false_positive, false_negative)
      if (me == CONST_FOWLKES_MALLOWS_INDEX_METRIC)
        fowlkes_mallows_index =
          fowlkes_mallows_index_metric(true_positive, false_positive,
                                       false_negative)
    }
  }


  end.time = Sys.time()

  time = end.time - start.time

  time_external = round(as.numeric(time),4)

  if (is.infinite(as.numeric(entropy))) entropy <- 0
  if (is.na(as.numeric(entropy))) entropy <- 0

  if (is.infinite(as.numeric(variation_information))) variation_information <- 0
  if (is.na(as.numeric(variation_information))) variation_information <- 0

  if (is.infinite(as.numeric(precision))) precision <- 0
  if (is.na(as.numeric(precision))) precision <- 0

  if (is.infinite(as.numeric(recall))) recall <- 0
  if (is.na(as.numeric(recall))) recall <- 0

  if (is.infinite(as.numeric(f_measure))) f_measure <- 0
  if (is.na(as.numeric(f_measure))) f_measure <- 0

  if (is.infinite(as.numeric(fowlkes_mallows_index))) fowlkes_mallows_index <- 0
  if (is.na(as.numeric(fowlkes_mallows_index))) fowlkes_mallows_index <- 0

  resultadoValores = list(
    "entropy" = format(round(as.numeric(entropy), digits = 4),scientific = F),
    "variation_information" = format(round(
      as.numeric(variation_information),
      digits = 4
    ),scientific = F),
    "precision" = format(round(as.numeric(precision), digits = 4),
                         scientific = F),
    "recall" = format(round(as.numeric(recall), digits = 4),scientific = F),
    "f_measure" = format(round(as.numeric(f_measure), digits = 4),
                         scientific = F),
    "fowlkes_mallows_index" = format(round(
      as.numeric(fowlkes_mallows_index),
      digits = 4
    ),scientific = F),
    "time" = round(as.numeric(time_external), digits = 4)
  )

  return (resultadoValores)
}

#'
#' Method in charge of creating a table from an array with the values of the
#' variable used as a sample and another with the classification of the values.
#'
#' @param clusters_vector Array of the variable used for the classification.
#'
#' @param column_dataset_label Array with the grouping of the values.
#'
#' @return Return a table with the grouping of both arrays.
#'
#' @keywords internal
#'

convert_table = function (clusters_vector, column_dataset_label) {
  return (table (clusters_vector, column_dataset_label))
}

#'
#' Method to calculate the entropy.
#'
#' @param conversion_data_frame A double with the result of the entropy
#' calculation.
#'
#' @param table_convert Table conversion (variable - cluster).
#'
#' @param column_dataset_label Array with the calculation of the clustering
#' algorithm.
#'
#' @return Return a double with the result of the entropy calculation.
#'
#' @keywords internal
#'

entropy_metric =
  function (conversion_data_frame,
            table_convert,
            column_dataset_label) {
    entropy = 0

    dividend_entropy =
      sum(apply(conversion_data_frame, 2, function(x)
        entropy_formula(x)))

    result_entropy = -(1 / (sum(table_convert) * log2(length(
      unique(column_dataset_label)
    )))) * dividend_entropy

    entropy = round(as.numeric(result_entropy), 4)

    return (entropy)

  }


#'
#' Method to calculate the variation information.
#'
#' @param conversion_data_frame Return a double with the result of the entropy
#' calculation.
#'
#' @param table_convert Table conversion (variable - cluster).
#'
#' @return Returns a double with the result of the variation information
#' calculation.
#'
#' @keywords internal
#'

variation_information_metric =
  function(conversion_data_frame, table_convert) {
    share_information = 0.0

    join_entropy = 0.0

    tmp_variation_information = 0.0

    variation_information = 0.0

    for (iterate_row in 1:nrow(conversion_data_frame)) {
      for (iterate_col in 1:ncol(conversion_data_frame)) {
        if (conversion_data_frame[iterate_row, iterate_col] > 0.0) {
          join_entropy = join_entropy + (-((conversion_data_frame[iterate_row,
              iterate_col] / sum(table_convert)) *
                log2(conversion_data_frame[iterate_row, iterate_col] /
                       sum(table_convert))
          ))

          share_information = share_information +
            ((conversion_data_frame[iterate_row, iterate_col] /
                sum(table_convert)) * log2(as.numeric(
            as.bigz(
              as.numeric(sum(table_convert)) *
                as.numeric(conversion_data_frame[iterate_row, iterate_col])
            ) / as.bigz(as.numeric(sum(
              conversion_data_frame[iterate_row,]
            )) * as.numeric(sum(
              conversion_data_frame[, iterate_col]
            )))
          )))
        }
      }
    }

    entropy_class = sum(apply(conversion_data_frame, 2, function(x)
      - (sum(x) / sum(table_convert)) * log2(sum(x) / sum(table_convert))))

    entropy_cluster = sum(apply(conversion_data_frame, 1, function(x)
      - (sum(x) / sum(table_convert)) * log2(sum(x) / sum(table_convert))))

    tmp_variation_information = (entropy_cluster + entropy_class) - 2.0 *
      share_information

    variation_information = ifelse(is.nan(round(tmp_variation_information, 4)),
                                   0, round(tmp_variation_information, 4))

    return (variation_information)
  }

#'
#' Method to calculate the precision.
#'
#' @param true_positive Array with matching elements of B is in the same cluster.
#'
#' @param false_positive Array with non matching element of B is in the same
#' cluster.
#'
#' @return Returns a double with the precision calculation.
#'
#' @keywords internal
#'

precision_metric = function (true_positive, false_positive) {
  precision = 0.0

  precision  = ifelse(is.nan(true_positive / (true_positive + false_positive)),
                      0, round(true_positive / (true_positive + false_positive),
                               4))

  return (precision)
}


#'
#' Method to calculate the recall.
#'
#' @param true_positive Array with matching elements of B is in the same cluster.
#'
#' @param false_negative Array with matching elements of B is not in the same
#' cluster.
#'
#' @return Returns a double with the recall calculation.
#'
#' @keywords internal
#'

recall_metric = function(true_positive, false_negative) {
  recall = 0.0

  recall = ifelse(is.nan(true_positive / (true_positive + false_negative)), 0,
                  round(true_positive / (true_positive + false_negative), 4))

  return (recall)

}

#'
#' Method to calculate the f_measure.
#'
#' @param true_positive Array with matching elements of B is in the same cluster.
#'
#' @param false_positive Array with non matching element of B is in the same
#' cluster.
#'
#' @param false_negative Array with matching elements of B is not in the same
#' cluster.
#'
#' @return Returns a double with the f_measure calculation.
#'
#' @keywords internal
#'

fmeasure_metric =
  function(true_positive,
           false_positive,
           false_negative) {
    fmeasure = 0.0

    precision = precision_metric(true_positive, false_positive)

    recall = recall_metric(true_positive, false_negative)

    fmeasure =
      ifelse(is.nan(round(2.0 * ((precision * recall) / (precision + recall)
      ), 4)), 0, round(2.0 * ((precision * recall) / (precision + recall)
      ), 4))

    return (fmeasure)
  }

#'
#' Method to calculate the fowlkes and mallows.
#'
#'
#' @param true_positive Array with matching elements of B is in the same cluster.
#'
#' @param false_positive Array with non matching element of B is in the same
#' cluster.
#'
#' @param false_negative Array with matching elements of B is not in the same
#' cluster.
#'
#' @return Returns a double with the fowlkes_mallows_index calculation.
#'
#' @keywords internal
#'

fowlkes_mallows_index_metric =
  function(true_positive,
           false_positive,
           false_negative) {
    fowlkes_mallows_index = 0.0

    fowlkes_mallows_index = sqrt((true_positive / ((true_positive +
                                                      false_positive)
    )) * (
      true_positive / (true_positive + false_negative)
    ))

    if (is.na(fowlkes_mallows_index)) fowlkes_mallows_index = 0;

    return (fowlkes_mallows_index)

  }

#'
#'Method that return a list of internal validation initialized to zero.
#'
#'@return A list of all values set to zero.
#'
#'@keywords internal
#'

initializeExternalValidation = function() {
  entropy = 0

  variation_information = 0

  precision = 0

  recall = 0

  f_measure = 0

  jaccard_index = 0

  fowlkes_mallows_index = 0

  time = 0


  resultadoValores = list(
    "entropy" = format(round(as.numeric(entropy), digits = 4),scientific = F),
    "variation_information" = format(round(
      as.numeric(variation_information),
      digits = 4
    ),scientific = F),
    "precision" = format(round(as.numeric(precision), digits = 4),
                         scientific = F),
    "recall" = format(round(as.numeric(recall), digits = 4),scientific = F),
    "f_measure" = format(round(as.numeric(f_measure), digits = 4),
                         scientific = F),
    "fowlkes_mallows_index" = format(round(
      as.numeric(fowlkes_mallows_index),
      digits = 4
    ),scientific = F),
    "time" = round(as.numeric(time), digits = 4)
  )

  return (resultadoValores)

}

#'
#'Method for calculating entropy.
#'
#'@param x_vec With datas to calculate entropy.
#'
#'@return An array with the calculate.
#'
#'@keywords internal
#'

entropy_formula = function(x_vec) {
  vec = rep(NA, length(x_vec))

  for (iterate in 1:length(x_vec)) {
    if (x_vec[iterate] == 0.0) {
      vec[iterate] = 0.0
    }

    else {
      vec[iterate] = ((x_vec[iterate]) * log2(x_vec[iterate] / sum(x_vec)))
    }
  }

  return(vec)
}

#'
#'Method for filtering external columns of a dataset.
#'
#'@param df Data frame with clustering results.
#'
#'@return Dafa frame filtered with the columns of the external measurements.
#'

transform_dataset = function(df) {

  nameColumns <- colnames(df)

  numberOcurrenceTime <- which(nameColumns == CONST_TIME_EXTERNAL)

  numberOcurrenceTimeAttr <- which(nameColumns == CONST_TIME_EXTERNAL_ATTR)

  if (length(numberOcurrenceTime) > 1){

    c <- c()

    #' Exists internal measure

    for (iterate in 1:length(nameColumns)) {

      if (nameColumns[iterate] %in% c(CONST_ALGORITHM,CONST_DISTANCE,CONST_CLUSTERS,CONST_DATASET,CONST_RANKING)){
        c <- append(c,iterate)
      } else {

        if (iterate < numberOcurrenceTime[2]){
          c <- append(c,iterate)
        } else {
          if (iterate >= numberOcurrenceTimeAttr[1] && iterate < numberOcurrenceTimeAttr[2]){
            c <- append(c,iterate)
          }
        }
      }
    }

    nameColumns <- c
  }

  return (select(df, nameColumns))
}
