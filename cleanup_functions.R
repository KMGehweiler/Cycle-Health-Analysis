library(dplyr)

### based on function in library(AppleHealthAnalysis)

data_summarize_mean <- function (health_data, type_filter = NULL, summary_period = "day") 
{
  if (is.null(health_data)) 
    stop("You must specify data frame containing health data.")
  if (is.null(type_filter)) 
    stop("You need to select one filter for this function to be useful.")
  if (!summary_period %in% c("hour", "day")) 
    stop("You must specify a valid period of time to summarise by.")
  if (summary_period == "hour") {
    summary_data <- health_data %>% dplyr::filter(.data$type == type_filter) %>%
      dplyr::group_by(.data$date, .data$hour, .data$sourceName, .data$unit, .data$type) %>% 
      dplyr::summarise(value = mean(.data$value))
  }
  if (summary_period == "day") {
    summary_data <- health_data %>% dplyr::filter(.data$type == type_filter) %>%
      dplyr::group_by(.data$date, .data$sourceName, .data$unit, .data$type) %>% 
      dplyr::summarise(value = mean(.data$value))
  }
  return(summary_data)
}

data_summarize_median <- function (health_data, type_filter = NULL, summary_period = "day") 
{
  if (is.null(health_data)) 
    stop("You must specify data frame containing health data.")
  if (is.null(type_filter)) 
    stop("You need to select one filter for this function to be useful.")
  if (!summary_period %in% c("hour", "day")) 
    stop("You must specify a valid period of time to summarise by.")
  if (summary_period == "hour") {
    summary_data <- health_data %>% dplyr::filter(.data$type == type_filter) %>%
      dplyr::group_by(.data$date, .data$hour, .data$sourceName, .data$unit, .data$type) %>% 
      dplyr::summarise(value = median(.data$value))
  }
  if (summary_period == "day") {
    summary_data <- health_data %>% dplyr::filter(.data$type == type_filter) %>%
      dplyr::group_by(.data$date, .data$sourceName, .data$unit, .data$type) %>% 
      dplyr::summarise(value = median(.data$value))
  }
  return(summary_data)
}


data_summarize_99 <- function (health_data, type_filter = NULL, summary_period = "day") 
{
  if (is.null(health_data)) 
    stop("You must specify data frame containing health data.")
  if (is.null(type_filter)) 
    stop("You need to select one filter for this function to be useful.")
  if (!summary_period %in% c("hour", "day")) 
    stop("You must specify a valid period of time to summarise by.")
  if (summary_period == "hour") {
    summary_data <- health_data %>% dplyr::filter(.data$type == type_filter) %>%
      dplyr::group_by(.data$date, .data$hour, .data$sourceName, .data$unit, .data$type) %>% 
      dplyr::summarise(value =quantile(.data$value, probs =0.99))
  }
  if (summary_period == "day") {
    summary_data <- health_data %>% dplyr::filter(.data$type == type_filter) %>%
      dplyr::group_by(.data$date, .data$sourceName, .data$unit, .data$type) %>% 
      dplyr::summarise(value = quantile(.data$value, probs =0.99))
  }
  return(summary_data)
}

data_summarize_max <- function (health_data, type_filter = NULL, summary_period = "day") 
{
  if (is.null(health_data)) 
    stop("You must specify data frame containing health data.")
  if (is.null(type_filter)) 
    stop("You need to select one filter for this function to be useful.")
  if (!summary_period %in% c("hour", "day")) 
    stop("You must specify a valid period of time to summarise by.")
  if (summary_period == "hour") {
    summary_data <- health_data %>% dplyr::filter(.data$type == type_filter) %>%
      dplyr::group_by(.data$date, .data$hour, .data$sourceName, .data$unit, .data$type) %>% 
      dplyr::summarise(value = max(.data$value))
  }
  if (summary_period == "day") {
    summary_data <- health_data %>% dplyr::filter(.data$type == type_filter) %>%
      dplyr::group_by(.data$date, .data$sourceName, .data$unit, .data$type) %>% 
      dplyr::summarise(value = max(.data$value))
  }
  return(summary_data)
}
