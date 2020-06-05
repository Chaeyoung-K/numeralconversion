#' Decimal to Binary conversion
#'
#' This function changes Decimal number to Binary number.
#'
#' @examples
#'
#' dectoBin(25)
dectoBin <- function(n) {
  count <- 1
  vec <- c()
  while (n > 0)
  {
    vec <- c(vec, n%%2)
    n <- n %/% 2
    count <- count + 1
  }

  while (count != 0)
  {
    cat(vec[count - 1])
    count <- count - 1
  }

}
