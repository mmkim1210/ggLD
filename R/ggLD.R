#' Visualize correlation with the diagonal on the x-axis
#'
#' @param data input correlation matrix
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' ggLD(data = abs(cor(data.frame(a = rnorm(20), b = rnorm(20), c = rnorm(20)))))
#'
#' @importFrom rlang .data
ggLD <- function(data){
  data <- tidyr::as_tibble(data)
  n <- length(data)

  values <- data %>%
    dplyr::mutate(idx1 = colnames(data)) %>%
    tidyr::pivot_longer(!.data$idx1, names_to = "idx2", values_to = "LD") %>%
    dplyr::filter(!duplicated(paste0(pmax(.data$idx1, .data$idx2), pmin(.data$idx1, .data$idx2)))) %>%
    tidyr::unite("id", .data$idx1:.data$idx2, remove = FALSE)

  positions <- tidyr::tibble(id = rep(values$id, each = 4),
                             x = 0,
                             y = 0)
  positions$x[(nrow(positions) - 3):nrow(positions)] = c(10,
                                                         10 - 10 / (2 * n),
                                                         10 - 10 / n,
                                                         10 - 10 / (2 * n))
  positions$y[(nrow(positions) - 3):nrow(positions)] = c(5,
                                                         5 + 10 / (2 * n),
                                                         5,
                                                         5 - 10 / (2 * n))
  fibonacci <- 0
  for (i in 1:(n - 1)) {
    fibonacci <- fibonacci + i
    for (j in 1:(i + 1)) {
      positions$x[((nrow(positions) - 3) - fibonacci * 4 - (j - 1) * 4):(nrow(positions) - fibonacci * 4 - (j - 1) * 4)] =
        c(10 - 10 / n * i / 2 - (j - 1) * 10 / n / 2,
          10 - 10 / (2 * n) - 10 / n * i / 2 - (j - 1) * 10 / n / 2,
          10 - 10 / n - 10 / n * i / 2 - (j - 1) * 10 / n / 2,
          10 - 10 / (2 * n) - 10 / n * i / 2 - (j - 1) * 10 / n / 2)
      positions$y[((nrow(positions) - 3) - fibonacci * 4 - (j - 1) * 4):(nrow(positions) - fibonacci * 4 - (j - 1) * 4)] =
        c(5 - 10 / n * i / 2 + (j - 1) * 10 / n / 2,
          5 + 10 / (2 * n) - 10 / n * i / 2 + (j - 1) * 10 / n / 2,
          5 - 10 / n * i / 2 + (j - 1) * 10 / n / 2,
          5 - 10 / (2 * n) - 10 / n * i / 2 + (j - 1) * 10 / n / 2)
    }
  }

  merge(values, positions, by = c("id")) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_polygon(ggplot2::aes(fill = .data$LD, group = .data$id)) +
    ggplot2::theme_void() +
    ggplot2::scale_fill_distiller(type = "seq", palette = 1, direction = 1)
}


