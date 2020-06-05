#' Decimal to Octal conversion
#'
#' This function changes Decimal number to Octal number.
#'
#' @examples
#'
#' dectoOctal(25)
dectoOctal <- function(n) {
  count <- 1
  vec <- c()
  while (n > 0)
  {
    vec <- c(vec, n%%8)
    n <- n %/% 8
    count <- count + 1
  }

  while (count != 0)
  {
    cat(vec[count - 1])
    count <- count - 1
  }
}
