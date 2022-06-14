#'
#' Method that returns the value or variable depending on where it is in the
#' calculated metrics.
#'
#' @param algorith Algorithm name.
#'
#' @param distance Name of the metric used to calculate the distance between
#' points.
#'
#' @param cluster Number of clusters.
#'
#' @param dataset Name of dataset.
#'
#' @param ranking Position we want to obtain from the list of variables.
#'
#' @param timeExternal Array with the external validation calculation times of
#' the clustering.
#'
#' @param entropy Array with the calculation of the entropy for each of the
#' variables.
#'
#' @param variation_information Array with the calculation of the
#' variation_information for each of the variables.
#'
#' @param precision Array with the calculation of the precision for each of the
#' variables.
#'
#' @param recall Array with the calculation of the recall for each of the
#' variables.
#'
#' @param fowlkes_mallows_index Array with the calculation of the
#' fowlkes_mallows_index for each of the variables.
#'
#' @param f_measure Array with the calculation of the f_measure for each of the
#' variables.
#'
#' @param dunn Array with the calculation of the dunn for each of the variables.
#'
#' @param connectivity Array with the calculation of the connectivity for each
#' of the variables.
#'
#' @param silhouette Array with the calculation of the silhouette for each of
#' the variables.
#'
#' @param timeInternal Array with the internal validation calculation times of
#' the clustering.
#'
#' @param variables True if we want to show the value of the metric calculation
#' and false if we want to show the variable.
#'
#' @return Returns an array with the calculation of each metric based on the
#' indicated position.
#'
#' @keywords internal
#'

calculate_result <-
  function(algorith,
           distance,
           cluster,
           dataset,
           ranking,
           timeExternal,
           entropy,
           variation_information,
           precision,
           recall,
           fowlkes_mallows_index,
           f_measure,
           timeInternal,
           dunn,
           connectivity,
           silhouette,
           variables) {

    resultadoValores = list()

    if (variables) {
      sort_timeExternal <-
        sort(x = as.vector(unlist(timeExternal)),
             decreasing = FALSE,
             index.return = TRUE)$ix
    } else {
      sort_timeExternal <-
        sort(x = as.vector(unlist(timeExternal)),
             decreasing = FALSE,
             index.return = TRUE)$x
    }
    if (variables) {
      sort_entropy <-
        if (!is.null(entropy))
          sort(x = as.vector(unlist(entropy)),
               decreasing = TRUE,
               index.return = TRUE)$ix
      else
        NULL
    } else {
      sort_entropy <-
        if (!is.null(entropy))
          sort(x = as.vector(unlist(entropy)),
               decreasing = TRUE,
               index.return = TRUE)$x
      else
        NULL
    }

    if (variables) {
      sort_variation_information <-
        if (!is.null(variation_information))
          sort(
            x = as.vector(unlist(variation_information)),
            decreasing = TRUE,
            index.return = TRUE
          )$ix
      else
        NULL
    } else {
      sort_variation_information <-
        if (!is.null(variation_information))
          sort(
            x = as.vector(unlist(variation_information)),
            decreasing = TRUE,
            index.return = TRUE
          )$x
      else
        NULL
    }

    if (variables) {
      sort_precision <-
        if (!is.null(precision))
          sort(x = as.vector(unlist(precision)),
               decreasing = TRUE,
               index.return = TRUE)$ix
      else
        NULL
    } else {
      sort_precision <-
        if (!is.null(precision))
          sort(x = as.vector(unlist(precision)),
               decreasing = TRUE,
               index.return = TRUE)$x
      else
        NULL
    }

    if (variables) {
      sort_recall <-
        if (!is.null(recall))
          sort(x = as.vector(unlist(recall)),
               decreasing = TRUE,
               index.return = TRUE)$ix
      else
        NULL
    } else {
      sort_recall <-
        if (!is.null(recall))
          sort(x = as.vector(unlist(recall)),
               decreasing = TRUE,
               index.return = TRUE)$x
      else
        NULL
    }

    if (variables) {
      sort_fowlkes_mallows_index <-
        if (!is.null(fowlkes_mallows_index))
          sort(
            unlist(x = as.vector(fowlkes_mallows_index)),
            decreasing = TRUE,
            index.return = TRUE
          )$ix
      else
        NULL
    } else {
      sort_fowlkes_mallows_index <-
        if (!is.null(fowlkes_mallows_index))
          sort(
            x = as.vector(unlist(fowlkes_mallows_index)),
            decreasing = TRUE,
            index.return = TRUE
          )$x
      else
        NULL
    }

    if (variables) {
      sort_f_measure <-
        if (!is.null(f_measure))
          sort(x = as.vector(unlist(f_measure)),
               decreasing = TRUE,
               index.return = TRUE)$ix
      else
        NULL
    } else {
      sort_f_measure <-
        if (!is.null(f_measure))
          sort( x = as.vector(unlist(f_measure)),
               decreasing = TRUE,
               index.return = TRUE)$x
      else
        NULL
    }

    if (variables) {
      sort_timeInternal <-
       sort(  x = as.vector(unlist(timeInternal)),
             decreasing = FALSE,
             index.return = TRUE)$ix
    } else {
      sort_timeInternal <-
        sort(x = as.vector(unlist(timeInternal)),
             decreasing = FALSE,
             index.return = TRUE)$x
    }

    if (variables) {
      sort_dunn <-
        if (!is.null(dunn))
          sort(x = as.vector(unlist(dunn)),
               decreasing = TRUE,
               index.return = TRUE)$ix
      else
        NULL
    } else {
      sort_dunn <-
        if (!is.null(dunn))
          sort(x = as.vector(unlist(dunn)),
               decreasing = TRUE,
               index.return = TRUE)$x
      else
        NULL
    }

    if (variables) {
      sort_connectivity <-
        if (!is.null(connectivity))
          sort(x = as.vector(unlist(connectivity)),
               decreasing = TRUE,
               index.return = TRUE)$ix
      else
        NULL
    } else {
      sort_connectivity <-
        if (!is.null(connectivity))
          sort(x = as.vector(unlist(connectivity)),
               decreasing = TRUE,
               index.return = TRUE)$x
      else
        NULL
    }

    if (variables) {
      sort_silhouette <-
        if (!is.null(silhouette))
          sort(x = as.vector(unlist(silhouette)),
               decreasing = TRUE,
               index.return = TRUE)$ix
      else
        NULL
    } else {
      sort_silhouette <-
        if (!is.null(silhouette))
          sort(x = as.vector(unlist(silhouette)),
               decreasing = TRUE,
               index.return = TRUE)$x
      else
        NULL
    }

    if (is.null(sort_silhouette) && is.null(sort_dunn) && is.null(sort_connectivity)) sort_timeInternal <- NULL
    if (is.null(sort_entropy) && is.null(sort_variation_information) && is.null(sort_precision) && is.null(sort_recall) && is.null(sort_f_measure) && is.null(sort_fowlkes_mallows_index)) sort_timeExternal <- NULL
    if (is.null(distance)) distance <- '-'


    resultadoValores$algorith = algorith
    resultadoValores$distance = distance
    resultadoValores$cluster = cluster
    resultadoValores$dataset = dataset
    resultadoValores$ranking = ranking

    if (!is.null(sort_timeExternal))
      if (variables)
        resultadoValores$timeExternal = match(ranking, sort_timeExternal)
    else
      resultadoValores$timeExternal =
      format(round(as.numeric(sort_timeExternal[ranking]), digits = 4),
             scientific = FALSE)

    if (!is.null(sort_entropy))
      if (variables)
        resultadoValores$entropy = sort_entropy[ranking]
    else
      resultadoValores$entropy =format(round(as.numeric(sort_entropy[ranking]),
                                             digits = 4),scientific = FALSE)
    if (!is.null(sort_variation_information))
      if (variables)
      resultadoValores$variation_information =
      sort_variation_information[ranking]
    else
      resultadoValores$variation_information =
      format(round(as.numeric(sort_variation_information[ranking]), digits = 4),
             scientific = FALSE)
    if (!is.null(sort_precision))
      if (variables)  resultadoValores$precision = sort_precision[ranking]
    else resultadoValores$precision =
      format(round(as.numeric(sort_precision[ranking]), digits = 4),
             scientific = FALSE)
    if (!is.null(sort_recall))
      if (variables)  resultadoValores$recall = sort_recall[ranking]
    else resultadoValores$recall =
      format(round(as.numeric(sort_recall[ranking]),
                                                digits = 4),scientific = FALSE)
    if (!is.null(sort_f_measure))
      if (variables) resultadoValores$f_measure = sort_f_measure[ranking]
    else resultadoValores$f_measure =
      format(round(as.numeric(sort_f_measure[ranking]), digits = 4),scientific = FALSE)
    if (!is.null(sort_fowlkes_mallows_index))
      if (variables)  resultadoValores$fowlkes_mallows_index =
      sort_fowlkes_mallows_index[ranking]
    else resultadoValores$fowlkes_mallows_index =
      format(round(as.numeric(sort_fowlkes_mallows_index[ranking]),
                   digits = 4),scientific = FALSE)
    if (!is.null(sort_timeInternal))
      if (variables)  resultadoValores$timeInternal =
      match(ranking, sort_timeInternal)
    else resultadoValores$timeInternal =
      format(round(as.numeric(sort_timeInternal[ranking]),
                   digits = 4),scientific = FALSE)
    if (!is.null(sort_connectivity))
      if (variables)  resultadoValores$connectivity = sort_connectivity[ranking]
    else resultadoValores$connectivity =
      format(round(as.numeric(sort_connectivity[ranking]), digits = 4),
             scientific = FALSE)
    if (!is.null(sort_dunn))
      if (variables)  resultadoValores$dunn = sort_dunn[ranking]
    else resultadoValores$dunn = format(round(as.numeric(sort_dunn[ranking]),
                                              digits = 4),scientific = FALSE)
    if (!is.null(sort_silhouette))
      if (variables)  resultadoValores$silhouette = sort_silhouette[ranking]
    else resultadoValores$silhouette =
      format(round(as.numeric(sort_silhouette[ranking]), digits = 4),
             scientific = FALSE)

    return (resultadoValores)

  }

#'
#' Method that returns the value or variable depending on where it is in the
#' calculated metrics.
#'
#' @param algorith Algorithm name.
#'
#' @param distance Name of the metric used to calculate the distance between
#' points.
#'
#' @param cluster Number of clusters.
#'
#' @param dataset Name of dataset.
#'
#' @param dunn Array with the calculation of the dunn for each of the variables.
#'
#' @param connectivity Array with the calculation of the connectivity for each
#' of the variables.
#'
#' @param silhouette Array with the calculation of the silhouette for each of
#' the variables.
#'
#' @param timeInternal Array with the internal validation calculation times of
#' the clustering.
#'
#' @param variables True if we want to show the value of the metric calculation
#' and false if we want to show the variable.
#'
#' @return Returns an array with the calculation of each metric based on the
#' indicated position.
#'
#' @keywords internal
#'

calculate_result_internal <-
  function(algorith,
           distance,
           cluster,
           dataset,
           ranking,
           timeInternal,
           dunn,
           connectivity,
           silhouette,
           variables) {

    resultadoValores = list()

    if (variables) {
      sort_timeInternal <-
        sort(  x = as.vector(unlist(timeInternal)),
               decreasing = FALSE,
               index.return = TRUE)$ix
    } else {
      sort_timeInternal <-
        sort(x = as.vector(unlist(timeInternal)),
             decreasing = FALSE,
             index.return = TRUE)$x
    }

    if (variables) {
      sort_dunn <-
        if (!is.null(dunn))
          sort(x = as.vector(unlist(dunn)),
               decreasing = TRUE,
               index.return = TRUE)$ix
      else
        NULL
    } else {
      sort_dunn <-
        if (!is.null(dunn))
          sort(x = as.vector(unlist(dunn)),
               decreasing = TRUE,
               index.return = TRUE)$x
      else
        NULL
    }

    if (variables) {
      sort_connectivity <-
        if (!is.null(connectivity))
          sort(x = as.vector(unlist(connectivity)),
               decreasing = TRUE,
               index.return = TRUE)$ix
      else
        NULL
    } else {
      sort_connectivity <-
        if (!is.null(connectivity))
          sort(x = as.vector(unlist(connectivity)),
               decreasing = TRUE,
               index.return = TRUE)$x
      else
        NULL
    }

    if (variables) {
      sort_silhouette <-
        if (!is.null(silhouette))
          sort(x = as.vector(unlist(silhouette)),
               decreasing = TRUE,
               index.return = TRUE)$ix
      else
        NULL
    } else {
      sort_silhouette <-
        if (!is.null(silhouette))
          sort(x = as.vector(unlist(silhouette)),
               decreasing = TRUE,
               index.return = TRUE)$x
      else
        NULL
    }

    if (is.null(sort_silhouette) && is.null(sort_dunn) && is.null(sort_connectivity)) sort_timeInternal <- NULL
    if (is.null(distance)) distance <- '-'


    resultadoValores$algorith = algorith
    resultadoValores$distance = distance
    resultadoValores$cluster = cluster
    resultadoValores$dataset = dataset


    if (!is.null(sort_timeInternal))
      if (variables)  resultadoValores$timeInternal =
      match(ranking, sort_timeInternal)
    else resultadoValores$timeInternal =
      format(round(as.numeric(sort_timeInternal[ranking]),
                   digits = 4),scientific = FALSE)
    if (!is.null(sort_connectivity))
      if (variables)  resultadoValores$connectivity = sort_connectivity[ranking]
    else resultadoValores$connectivity =
      format(round(as.numeric(sort_connectivity[ranking]), digits = 4),
             scientific = FALSE)
    if (!is.null(sort_dunn))
      if (variables)  resultadoValores$dunn = sort_dunn[ranking]
    else resultadoValores$dunn = format(round(as.numeric(sort_dunn[ranking]),
                                              digits = 4),scientific = FALSE)
    if (!is.null(sort_silhouette))
      if (variables)  resultadoValores$silhouette = sort_silhouette[ranking]
    else resultadoValores$silhouette =
      format(round(as.numeric(sort_silhouette[ranking]), digits = 4),
             scientific = FALSE)

    return (resultadoValores)

  }

#'
#' Method that calculates the best rated external metrics.
#'
#' @param df Data matrix or data frame.
#'
#' @return Return a table with the external metrics that has the best rating.
#'
#' @keywords internal
#'

calculate_best_external_variables_by_metrics <- function (df) {

  df <- transform_dataset(df)

  table_res <- data.table(df)

  fields <-
    paste(row_name_df_external(colnames(table_res)), collapse = ",")

  query <-
    paste("select", fields, "from table_res where Var=1", collapse = "")

  calculate_by_metrics <-
    sqldf(query)


  return (calculate_by_metrics)
}

#'
#' Method that calculates the best rated internal metrics.
#'
#' @param df Data matrix or data frame.
#'
#' @return Return a table with the internal metrics that has the best rating.
#'
#' @keywords internal
#'

calculate_best_internal_variables_by_metrics <- function (df) {

  df <- transform_dataset_internal(df)

  table_res <- data.table(df)

  fields <-
    paste(row_name_df_internal(colnames(table_res)), collapse = ",")

  query <-
    paste("select", fields, "from table_res where Var=1", collapse = "")

  calculate_by_metrics <-
    sqldf(query)


  return (calculate_by_metrics)
}

#'
#' Method that calculates which algorithm behaves best for the datasets provided.
#'
#' @param df Data matrix or data frame.
#'
#' @return Return a table with the best performing algorithm for the provided
#' datasets.
#'
#' @keywords internal
#'

calculate_validation_external_by_metrics <-
  function (df) {

    df <- transform_dataset(df)

    table_res <- data.table(df)

    fields <-
      paste(row_name_df_external(colnames(table_res)), collapse = ",")

    metrics_external <- unlist(strsplit(fields,","));

    query_fields <- "";

    for (i in 6:length(metrics_external)){
      query_fields <- paste(query_fields,", MAX(",metrics_external[i],") as ",
                            metrics_external[i],sep = "")
    }

    query <-
      paste("select Algorithm ", query_fields, "from table_res
            group by Algorithm order by Algorithm desc", collapse = "")


    calculate_external_by_metrics <-
      sqldf(
        query
      )


    return (calculate_external_by_metrics)


  }

#'
#' Method that calculates which algorithm and which metric behaves best for the
#' datasets provided.
#'
#' @param df Data matrix or data frame.
#'
#' @param metric String with the metric.
#'
#' @return Return a table with the algorithm and the best performing metric for
#' the datasets.
#'
#' @keywords internal
#'

calculate_best_validation_external_by_metrics <-
  function (df,metric) {

    df <- transform_dataset(df)

    table_res <- data.table(df)

    fields <-
      paste(row_name_df_external(colnames(table_res)), collapse = ",")

    metrics_external <- unlist(strsplit(fields,","));

    query_fields <- "";

    for (i in 6:length(metrics_external)){
      query_fields <- paste(query_fields,metrics_external[i],sep = ", ")
    }

    query <-
      paste("select Algorithm,Distance,Clusters", query_fields, "from table_res group by
            Algorithm, Distance HAVING ",metric," = MAX(",metric,") order by Algorithm desc", collapse = "")

    calculate_best_external_by_metrics <-
      sqldf(
        query
      )

    return (calculate_best_external_by_metrics)
  }

#'
#' Method that calculates which algorithm behaves best for the datasets provided.
#'
#' @param df Data matrix or data frame.
#'
#' @return Return a table with the best performing algorithm for the provided
#' datasets.
#'
#' @keywords internal
#'

calculate_validation_internal_by_metrics <-
  function (df) {

    df <- transform_dataset_internal(df)

    table_res <- data.table(df)

    fields <-
      paste(row_name_df_internal(colnames(table_res)), collapse = ",")

    metrics_internal <- unlist(strsplit(fields,","));

    query_fields <- "";

    for (i in 6:length(metrics_internal)){
      query_fields <- paste(query_fields,", MAX(",metrics_internal[i],") as ",
                            metrics_internal[i],sep = "")
    }

    query <-
      paste("select Algorithm ", query_fields, "from table_res group by
            Algorithm order by Algorithm desc", collapse = "")


    calculate_internal_by_metrics <-
      sqldf(
        query
      )


    return (calculate_internal_by_metrics)

  }

#'
#' Method that calculates which algorithm and which metric behaves best for the
#' datasets provided.
#'
#' @param df Data matrix or data frame.
#'
#' @param metric String with the metric.
#'
#' @return Return a table with the algorithm and the best performing metric for
#' the datasets.
#'
#' @keywords internal
#'

calculate_best_validation_internal_by_metrics <-
  function (df, metric) {

    df <- transform_dataset_internal(df)

    table_res <- data.table(df)

    fields <-
      paste(row_name_df_internal(colnames(table_res)), collapse = ",")

    metrics_internal <- unlist(strsplit(fields,","));

    query_fields <- "";

    for (i in 6:length(metrics_internal)){
      query_fields <- paste(query_fields,metrics_internal[i],sep = ", ")
    }

    query <-
      paste("select Algorithm,Distance,Clusters ", query_fields, "from table_res group by
            Algorithm, Distance HAVING ",metric," = MAX(",metric,") order by Algorithm desc", collapse = "")

    calculate_best_internal_by_metrics <-
      sqldf(
        query
      )


    return (calculate_best_internal_by_metrics)


  }

#'
#' Method in charge of obtaining a table with the results of the algorithms
#' grouped by clusters, calculating the maximum value of each external metrics.
#'
#' @param df Data matrix or data frame.
#'
#' @return Return a table with the algorithms and the clusters.
#'
#' @keywords internal
#'

show_result_external_algorithm_group_by_clustering <-
    function (df) {

      df <- transform_dataset(df)

      table_res <- data.table(df)

      fields <-
        paste(row_name_df_external(colnames(table_res)), collapse = ",")

      metrics_external <- unlist(strsplit(fields,","));

      query_fields <- "";

      for (i in 6:length(metrics_external)){
        query_fields <- paste(query_fields,", MAX(",metrics_external[i],") as ",
                              metrics_external[i],sep = "")
      }

      query <-
        paste("select Algorithm,Clusters ", query_fields, "from table_res group
              by Algorithm, Clusters order by Algorithm desc", collapse = "")

      calculate_result_external_algorithm_by_clusters <-
        sqldf(
          query
        )

      return (calculate_result_external_algorithm_by_clusters)
    }


#'
#' Method in charge of obtaining a table with the results of the algorithms
#' grouped by clusters, calculating the maximum value of each internal metrics.
#'
#' @param df Data matrix or data frame.
#'
#' @return Return a table with the algorithms and the clusters.
#'
#' @keywords internal
#'

show_result_internal_algorithm_group_by_clustering <-
  function (df) {

    df <- transform_dataset_internal(df)

    table_res <- data.table(df)

    fields <-
      paste(row_name_df_internal(colnames(table_res)), collapse = ",")

    metrics_internal <- unlist(strsplit(fields,","));

    query_fields <- "";

    for (i in 6:length(metrics_internal)){
      query_fields <- paste(query_fields,", MAX(",metrics_internal[i],") as ",
                            metrics_internal[i],sep = "")
    }

    query <-
      paste("select Algorithm,Clusters ", query_fields, "from table_res group
            by Algorithm, Clusters order by Algorithm desc", collapse = "")

    calculate_result_internal_algorithm_by_clusters <-
      sqldf(
        query
      )

    return (calculate_result_internal_algorithm_by_clusters)
  }


#'
#' Method that returns a table with the algorithm and the metric indicated as
#' parameters.
#'
#' @param df Data matrix or data frame.
#'
#' @param metric String with the metric.
#'
#' @return Return a table with the algorithm and the metric indicated as
#' parameter.
#'
#' @keywords internal
#'

show_result_external_algorithm_by_metric <-
  function(df, metric) {

    df <- transform_dataset(df)

    table_res <- data.table(df)

    fields <-
      paste(row_name_df_external(colnames(table_res)), collapse = ",")

    metrics_external <- unlist(strsplit(fields,","));

    query_fields <- "";

    for (i in 6:length(metrics_external)){
      query_fields <- paste(query_fields,metrics_external[i],sep = ", ")
    }

    query <-
      paste("select Algorithm, Distance, Clusters ", query_fields, " from table_res group by Algorithm HAVING ",metric," = MAX(",metric,") order by Algorithm desc", collapse = "", sep = "")

    show_result_external_by_metrics <- sqldf(query)

    return (show_result_external_by_metrics)

  }

#'
#' Method that return max value of metric.
#'
#' @param df Data matrix or data frame.
#'
#' @param metric Metric to evaluate.
#'
#' @return A value with maximum column.
#'
#' @keywords internal
#'

max_value_metric <- function(df,metric,isExternalMetric) {

  if (isExternalMetric) {
    df <- transform_dataset(df)
  } else {
    df <- transform_dataset_internal(df)
  }

  table_res <- data.table(df)

  query <-
    paste("select MAX(",metric,") as maxi from table_res ");

  return (sqldf(query));

}

#'
#' Method that returns a table with the algorithm and the metric indicated as
#' parameters.
#'
#' @param df Data matrix or data frame.
#'
#' @param metric An which we will calculate the results.
#'
#' @return Return a table with the algorithm and the metric indicated as
#' parameter.
#'
#' @keywords internal
#'

show_result_internal_algorithm_by_metric <-
  function(df, metric) {

    df <- transform_dataset_internal(df)

    table_res <- data.table(df)

    fields <-
      paste(row_name_df_internal(colnames(table_res)), collapse = ",")

    metrics_internal <- unlist(strsplit(fields,","));

    query_fields <- "";

    for (i in 6:length(metrics_internal)){
      query_fields <- paste(query_fields,metrics_internal[i],sep = ", ")
    }

    query <-
      paste("select Algorithm, Distance, Clusters ", query_fields, " from table_res group by Algorithm HAVING ",metric," = MAX(",metric,") order by Algorithm desc", collapse = "", sep = "")

    show_result_internal_by_metrics <- sqldf(query)

    return (show_result_internal_by_metrics)

  }
