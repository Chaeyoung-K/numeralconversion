#' Binary to Decimal conversion
#'
#' This function changes Binary number to Decimal number.
#'
#' @examples
#'
#' bintoDec(11001)
bintoDec <- function(n) {

  Decimal <- 0
  count <- 0

  while (TRUE) {
    if (n == 1)
    {
      Decimal <- Decimal + 1
      break
    }

    rest <- n %% 10

    if (rest == 1)
    {
      Decimal <- Decimal + rest*(2^count)
    }

    count <- count + 1
    n <- n %/% 10

    if (n %/% 10 == 0)
    {
      Decimal <- Decimal + 2^count
      break
    }
  }
  cat(Decimal)
}
