library(Clustering)

metric_bad <- c("precision","recalll");
metric_good <- c("precision","recall")
package_bad <- c("ClusterR","clusterr")
package_good <- c("ClusterR","cluster")
metrics_good <- c("entropy")
metrics_bad <- c("entropyy")
df <- Clustering::clustering(df = cluster::agriculture, algorithm = c('gmm','clara'), min = 4, max = 5, metrics = c('Precision','Dunn'));


test_that("validates that the input parameters are correct",{

  expect_error(clustering(path = NULL,df = NULL, packages = package_good, algorithm = NULL, min = 3, max = 4, metrics = metric_good))

  expect_error(clustering(path = NULL,df = ClusterR::agriculture, packages = package_good, algorithm = c('gmm'), min = 3, max = 4, metrics = NULL))

  expect_error(clustering(path = NULL,df = ClusterR::agriculture, packages = NULL, algorithm = c('gmm'), min = 4, max = 4, metrics = NULL))

  expect_error(clustering(path = '/Users/datasets/',df = ClusterR::agriculture, packages = NULL, algorithm = c('gmm'), min = 4, max = 4, metrics = NULL))

  expect_error(clustering(path = NULL, df = ClusterR::agriculture, packages = NULL, algorithm = c('gmm'), min = 3, max = 4, metrics = metrics_bad))

  expect_error(clustering(path = NULL, df = ClusterR::agriculture, packages = NULL, algorithm = c('gmm'), min = 5, max = 4, metrics = metrics_good))

})

test_that("validates that it correctly executes the dataset",{


  expect_equal(ncol(df$result),13)

  expect_equal(nrow(df$result),16)

  expect_equal(as.numeric(df$result[1,7]),0.0435)

  expect_equal(as.numeric(Clustering::best_ranked_external_metrics(df)$result[1,7]),0.0435)

  expect_equal(as.numeric(Clustering::best_ranked_internal_metrics(df)$result[1,7]), 0.482)

  expect_equal(as.numeric(Clustering::evaluate_validation_external_by_metrics(df)$result[1,3]),0.0714)

  expect_equal(as.numeric(Clustering::evaluate_validation_internal_by_metrics(df)$result[1,3]),0.4)

  expect_equal(as.numeric(Clustering::evaluate_best_validation_external_by_metrics(df,'Precision')$result[1,5]),0.0667)

  expect_equal(as.numeric(Clustering::evaluate_best_validation_internal_by_metrics(df,'Dunn')$result[1,5]),0.4000)

  expect_equal(as.numeric(Clustering::result_external_algorithm_by_metric(df,"Precision")$result[1,5]),0.0714)

  expect_equal(as.numeric(Clustering::result_internal_algorithm_by_metric(df,"Dunn")$result[2,7]),1)

})


