#' Tablice przykladu kontrolnego
#'
#' Rozklad obserwacyjny i rozklady interwencyjne dla dwoch poziomow zabiegu
#' i dwoch poziomow wyniku, z wynikiem korzystnym w drugiej kolumnie.
#' Przyklad pozwala odtworzyc recznie granice prawdopodobienstwa
#' koniecznosci, wystarczalnosci oraz koniecznosci i wystarczalnosci.
#'
#' @format Lista z dwiema macierzami \eqn{2 \times 2} o wierszach
#'   \code{x1}, \code{x2} (poziomy zabiegu) i kolumnach \code{y1},
#'   \code{y2} (poziomy wyniku):
#' \describe{
#'   \item{obserwacje}{wspolny rozklad \eqn{P(X, Y)}: wiersz \code{x1}
#'     rowny (0,3; 0,2), wiersz \code{x2} rowny (0,1; 0,4)}
#'   \item{interwencje}{rozklady \eqn{P(Y_{x_j})}: wiersz \code{x1} rowny
#'     (0,6; 0,4), wiersz \code{x2} rowny (0,2; 0,8)}
#' }
#'
#' @source Przyklad liczbowy z opisu tematu pracy; skrypt
#'   \code{data-raw/generuj_dane.R}.
#'
#' @examples
#' tablice_kontrolne$obserwacje
#' rowSums(tablice_kontrolne$interwencje)
"tablice_kontrolne"
