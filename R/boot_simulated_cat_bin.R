#' Confidence-interval bootstraps on simulated independent variables
#'
#' Create a defined number of simulated independent random variables of a given \code{size}
#' according to \code{type} : 2 ordinal variables, 2 binary variables,
#' 1 binary and 1 ordinal variable.
#' A number of bootstraps are then performed on the sample to calculate a confidence interval
#' of the bootstrap distribution of the chosen method : mutual information or the maximal
#' information coefficient. The percentile method is used to calculate this interval.
#'
#' @param type : the type of the simulated variables: \code{cat} is for 2 ordinal variables,
#'  \code{bin} is for 2 binary variables, \code{bincat} is for 1 binary and 1 ordinal variable.
#' @param method : the method used to calculate the association : mutual information (\code{mi}),
#'  or the maximal information coefficient.
#' @param simu : the number of simulated pairs of variables. For each pair, the confidence-interval
#'  bootstrap is calculated from the bootstrap distribution of the MI/MIC of between the two pairs.
#'  At the end of the program, the mean of the chose percentile is given. Default is 10.
#' @param boots : the number of bootstraps per simulation. Default is 5000.
#' @param size : the size of the sample. Default is 500.
#' @param percentile : the percentile kept. Default is 0.99 (the 99th percentile).
#'
#' @return The mean of the percentile values.
#'
#' @examples
#' boot_simulated_cat_bin("cat", "mic")
#'
#' @importFrom stats rbinom rmultinom runif
#' @importFrom minerva cstats
#' @export
boot_simulated_cat_bin <- function(type = c("cat", "bin", "bincat"),
                                   method = c("mic", "mi"),
                                   simu = 10,
                                   boots = 5000,
                                   size = 500,
                                   percentile = 0.99) {
  type <- match.arg(type)
  method <- match.arg(method)

  if (type == "cat") {
    var1 <- rmultinom(1, size, runif(size))
    var2 <- rmultinom(1, size, runif(size))
  } else if (type == "bin") {
    var1 <- rbinom(size, 1, runif(1))
    var2 <- rbinom(size, 1, runif(1))
  } else {
    var1 <- rmultinom(1, size, runif(size))
    var2 <- rbinom(size, 1, runif(1))
  }

  print(paste("Confidence-interval bootstrap on simulated independent variables of type:", type))
  print(paste("Number of simulations:", simu))
  print(paste("Number of bootstraps per simulations:", boots))
  print(paste("Sample size for each simulation:", size))

  print("Contigency table of the simulated data:")
  print(table(var1, var2))

  res_percentile <- rep(NA, simu)
  for (simu_no in 1:simu) {
    estimate_star <- rep(NA, boots)
    for (boot_no in 1:boots) {
      boot_sample1 <- sample(var1, size, replace = TRUE)
      boot_sample2 <- sample(var2, size, replace = TRUE)

      if (method == "mi") {
        estimate_star[boot_no] <- infotheo::mutinformation(boot_sample1, boot_sample2, method = "mm")
      } else {
        estimate_star[boot_no] <- cstats(as.matrix(boot_sample1), as.matrix(boot_sample2))[,3]
      }
    }
    res_percentile[simu_no] <- quantile(estimate_star, percentile)
    print(paste("Simulation", simu_no, ":", res_percentile[simu_no]))
  }

  print("Mean of the percentiles :")
  mean_percentile <- mean(res_percentile)
  mean_percentile
}
