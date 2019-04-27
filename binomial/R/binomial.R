# Binomial Distribution Classes

# test if an input prob is a valid probability value
check_prob <- function(prob) {
  if (prob >= 0 & prob <= 1) {
    return(TRUE)
  } else {
    stop("invalid prob value")
  }
}

# test if an input trials is a valid value for number of trials
check_trials <- function(trials) {
  if (trials == floor(trials) & trials >= 0) {
    return(TRUE)
  } else {
    stop("invalid trials value")
  }
}

# test if an input success is a valid value for number of successes
check_success <- function(success, trials) {
  for (s in success) {
    if (s != floor(s) | s < 0 | s > trials) {
      stop("invalid success value")
    }
  }
  return(TRUE)
}

# Compute the expected value of a binomial distribution
aux_mean <- function(trials, prob) {
  return(trials * prob)
}

# Compute the variance of a binomial distribution
aux_variance <- function(trials, prob) {
  return(trials * prob * (1 - prob))
}

# Compute the mode of a binomial distribution
aux_mode <- function(trials, prob) {
  m <- trials * prob + prob
  if (m == floor(m)) {
    return(c(m, m - 1))
  }
  return(floor(m))
}

# Compute the skewness of a binomial distribution
aux_skewness <- function(trials, prob) {
  return((1 - 2 * prob) / sqrt(trials * prob * (1 - prob)))
}

# Compute the kurtosis of a binomial distribution
aux_kurtosis <- function(trials, prob) {
  return((1 - 6 * prob * (1 - prob)) / (trials * prob * (1 - prob)))
}

#' @title Binomial Choose
#' @description Calculates the number of combinations in which k successes can occur in n trials
#' @param n Total number of trials
#' @param k Number of successes
#' @return Calculates the number of combinations in which k successes can occur in n trials
#' @export
#' @examples
#' # Calculate number of possible combinations
#' # How many ways to choose 2 successes from 5 trials?
#' comb1 <- bin_choose(n = 5, k = 2)
#' # How many ways to choose 3 successes from 7 trials?
#' comb2 <- bin_choose(n = 7, k = 3)
#'
bin_choose <- function(n, k) {
  for (v in k) {
    if (v > n) {
      stop("k cannot be greater than n")
    }
  }
  return(factorial(n) / (factorial(k) * factorial(n - k)))
}

#' @title Binomial Probability
#' @description Calculates the probability of having k successes in n trials with probability p
#' @param success Number of successes
#' @param trials Total number of trials
#' @param prob Probability of success
#' @return Calculates the probability of having k successes in n trials with probability p
#' @export
#' @examples
#' # Calculate single binomial probability
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#' # Calculate multiple binomial probabilities using a vector
#' bin_probability(success = c(1, 2, 3, 4, 5), trials = 5, prob = 0.5)
#'
bin_probability <- function(success, trials, prob) {
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  return(bin_choose(trials, success) * prob^success * (1 - prob)^(trials - success))
}


#' @title Binomial Distribution
#' @description Creates an object of binomial distribution
#' @param trials Total number of trials
#' @param prob Probability of success
#' @return An object representing the binomial distribution
#' @export
#' @examples
#' # Create a binomial distribution object
#' dis1 <- bin_distribution(trials = 5, prob = 0.5)
#' # Create a bar plot of the binomial distribution
#' plot(dis1)
#'
bin_distribution <- function(trials, prob) {
  success <- 0:trials
  probability <- bin_probability(success, trials, prob)
  df <- data.frame(success, probability)
  class(df) <- c("bindis", "data.frame")
  return(df)
}

#' @export
plot.bindis <- function(x, ...) {
  barplot(x$probability,
          xlab = "successes", ylab = "probability",
          names.arg = x$success)
}

#' @title Cumulative Binomial Distribution
#' @description Creates an object of cumulative binomial distribution
#' @param trials Total number of trials
#' @param prob Probability of success
#' @return An object representing the cumulative binomial distribution
#' @export
#' @examples
#' # Create a binomial cumulative distribution object
#' dis2 <- bin_cumulative(trials = 5, prob = 0.5)
#' # Create a line graph of the cumulative distribution
#' plot(dis2)
#'
bin_cumulative <- function(trials, prob) {
  df <- bin_distribution(trials, prob)
  cumulative <- c()
  for (i in 1:(trials + 1)) {
    cumulative[i] <- sum(df$probability[1:i])
  }
  df$cumulative <- cumulative
  class(df) <- c("bincum", "data.frame")
  return(df)
}

#' @export
plot.bincum <- function(x, ...) {
  plot(x$success, x$cumulative, type = "p",
       xlab = "successes", ylab = "probability")
  lines(x$success, x$cumulative)
}

#' @title Binomial Variable
#' @description Creates an object of binomial variable
#' @param trials Total number of trials
#' @param prob Probability of success
#' @return An object of binomial variable
#' @export
#' @examples
#' # Create a binomial variable
#' var <- bin_variable(trials = 5, prob = 0.5)
#' # Get summary statistics of the binomial variable
#' sum_var <- summary(var)
#' # Print summary statistics
#' sum_var
#'
bin_variable <- function(trials, prob) {
  check_prob(prob)
  check_trials(trials)
  object <- list(
    trials = trials,
    prob = prob)
  class(object) <- "binvar"
  return(object)
}

#' @export
print.binvar <- function(x, ...) {
  cat('"Binomial variable"\n\n')
  cat('Parameters\n')
  cat(sprintf('- number of trials: %s', x$trials), "\n")
  cat(sprintf('- prob of success: %s', x$prob), "\n")
  invisible(x)
}

#' @export
summary.binvar <- function(x, ...) {
  object <- list(
    trials = x$trials,
    prob = x$prob,
    mean = aux_mean(x$trials, x$prob),
    variance = aux_variance(x$trials, x$prob),
    mode = aux_mode(x$trials, x$prob),
    skewness = aux_skewness(x$trials, x$prob),
    kurtosis = aux_kurtosis(x$trials, x$prob)
  )
  class(object) <- "summary.binvar"
  return(object)
}

#' @export
print.summary.binvar <- function(x, ...) {
  cat('"Summary Binomial"\n\n')
  cat('Parameters\n')
  cat(sprintf('- number of trials: %s', x$trials), "\n")
  cat(sprintf('- prob of success: %s', x$prob), "\n\n")
  cat('Measures\n')
  cat(sprintf('- mean    : %s', x$mean), "\n")
  cat(sprintf('- variance: %s', x$variance), "\n")
  cat(sprintf('- mode    : %s', x$mode), "\n")
  cat(sprintf('- skewness: %s', x$skewness), "\n")
  cat(sprintf('- kurtosis: %s', x$kurtosis), "\n")
  invisible(x)
}

#' @title Binomial Mean
#' @description Calculates the mean of a binomial variable
#' @param trials Total number of trials
#' @param prob Probability of success
#' @return Calculates the mean of a binomial variable
#' @export
#' @examples
#' # Calculate the mean of a binomial distribution
#' bin_mean(trials = 5, prob = 0.5)
#'
bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

#' @title Binomial Variance
#' @description Calculates the variance of a binomial variable
#' @param trials Total number of trials
#' @param prob Probability of success
#' @return Calculates the variance of a binomial variable
#' @export
#' @examples
#' # Calculate the variance of a binomial distribution
#' bin_variance(trials = 5, prob = 0.5)
#'
bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}

#' @title Binomial Mode
#' @description Calculates the mode of a binomial variable
#' @param trials Total number of trials
#' @param prob Probability of success
#' @return Calculates the mode of a binomial variable
#' @export
#' @examples
#' # Calculate the mode(s) of a binomial distribution
#' bin_mode(trials = 5, prob = 0.5)
#'
bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

#' @title Binomial Skewness
#' @description Calculates the skewness of a binomial variable
#' @param trials Total number of trials
#' @param prob Probability of success
#' @return Calculates the skewness of a binomial variable
#' @export
#' @examples
#' # Calculate the skewness of a binomial distribution
#' bin_skewness(trials = 5, prob = 0.5)
#'
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}

#' @title Binomial Kurtosis
#' @description Calculates the kurtosis of a binomial variable
#' @param trials Total number of trials
#' @param prob Probability of success
#' @return Calculates the kurtosis of a binomial variable
#' @export
#' @examples
#' # Calculate the kurtosis of a binomial distribution
#' bin_kurtosis(trials = 5, prob = 0.5)
#'
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}

