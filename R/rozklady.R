#' Przygotowanie rozkladu obserwacyjnego i rozkladow interwencyjnych
#'
#' Funkcja laczy tablice obserwacyjna i interwencyjna w jeden obiekt,
#' dopasowuje poziomy zabiegu i wyniku po nazwach oraz wyznacza rozklady
#' brzegowe \eqn{u_j = P(X = x_j)} i \eqn{v_i = P(Y = y_i)}.
#'
#' Tablica \code{obserwacje} zawiera \eqn{o_{ji} = P(X = x_j, Y = y_i)}:
#' wiersze odpowiadaja poziomom zabiegu, kolumny poziomom wyniku, a wszystkie
#' elementy sumuja sie do jednosci. Tablica \code{interwencje} zawiera
#' \eqn{a_{ji} = P(Y_{x_j} = y_i)}, czyli rozklad wyniku po hipotetycznym
#' ustawieniu zabiegu na \eqn{x_j}; kazdy wiersz sumuje sie do jednosci.
#' Rozklad interwencyjny jest innym obiektem niz rozklad warunkowy
#' \eqn{P(Y \mid X)}.
#'
#' Zgodnosc wyniku obserwowanego z wynikiem potencjalnym dla faktycznie
#' otrzymanego zabiegu wymaga warunku
#' \eqn{o_{ji} \le a_{ji} \le o_{ji} + 1 - u_j}. Wynik sprawdzenia warunku
#' trafia do diagnostyki obiektu.
#'
#' @param obserwacje Macierz \eqn{m \times n} rozkladu obserwacyjnego
#'   z nazwami poziomow zabiegu (wiersze) i wyniku (kolumny).
#' @param interwencje Macierz \eqn{m \times n} rozkladow interwencyjnych
#'   z tymi samymi nazwami poziomow, w dowolnej kolejnosci.
#' @param tolerancja Tolerancja numeryczna sprawdzania sum, warunku zgodnosci
#'   i zerowych mianownikow.
#'
#' @return Obiekt klasy \code{rozklady_przyczynowe}, lista ze skladnikami:
#'   \describe{
#'     \item{obserwacje, interwencje}{macierze z poziomami w kolejnosci
#'       tablicy obserwacyjnej}
#'     \item{zabieg}{wektor \eqn{u_j = P(X = x_j)}}
#'     \item{wynik}{wektor \eqn{v_i = P(Y = y_i)}}
#'     \item{poziomy}{lista z nazwami poziomow \code{zabieg} i \code{wynik}}
#'     \item{tolerancja}{przyjeta tolerancja}
#'     \item{diagnostyka}{suma tablicy obserwacyjnej, sumy wierszy tablicy
#'       interwencyjnej, wynik sprawdzenia warunku zgodnosci oraz informacja
#'       o nadaniu nazw domyslnych}
#'   }
#'
#' @examples
#' obserwacje <- matrix(c(0.3, 0.1, 0.2, 0.4), nrow = 2,
#'                      dimnames = list(c("x1", "x2"), c("y1", "y2")))
#' interwencje <- matrix(c(0.6, 0.2, 0.4, 0.8), nrow = 2,
#'                       dimnames = list(c("x1", "x2"), c("y1", "y2")))
#' przygotuj_rozklady(obserwacje, interwencje)
#'
#' @export
przygotuj_rozklady <- function(obserwacje, interwencje, tolerancja = 1e-10) {
  # TODO: pelna kontrola wejscia zgodna z kontraktem bledow (SPEC.md):
  #   - wymiary m x n z m, n >= 2, wspolne i jednoznaczne nazwy poziomow,
  #     komunikat "Tablice musza miec zgodne poziomy i wymiary",
  #   - elementy skonczone z przedzialu [0, 1], bez NA, NaN i Inf, komunikat
  #     "Prawdopodobienstwa musza byc skonczone i nalezec do [0, 1]",
  #   - suma tablicy obserwacyjnej i kazdego wiersza tablicy interwencyjnej
  #     rowna 1 z tolerancja, komunikat "Rozklad nie sumuje sie do jednosci",
  #   - warunek zgodnosci o_ji <= a_ji <= o_ji + 1 - u_j, komunikat
  #     "Rozklady naruszaja warunek zgodnosci",
  #   - `tolerancja` jako pojedyncza dodatnia liczba.
  #   Decyzja do opisania w SPEC.md: czy tablice bez nazw sa odrzucane, czy
  #   otrzymuja nazwy domyslne wedlug kolejnosci (obecne zachowanie).
  #   Komunikaty bez znakow diakrytycznych, stop(..., call. = FALSE).
  #   Testy: tests/testthat/test-kontrakt-bledow.R (obecnie pomijane).
  if (!is.matrix(obserwacje) || !is.numeric(obserwacje) ||
      !is.matrix(interwencje) || !is.numeric(interwencje)) {
    stop("Argumenty `obserwacje` i `interwencje` musza byc macierzami ",
         "liczbowymi.", call. = FALSE)
  }
  if (!identical(dim(obserwacje), dim(interwencje))) {
    stop("Tablice musza miec zgodne poziomy i wymiary", call. = FALSE)
  }

  pelne_nazwy <- function(x) !is.null(rownames(x)) && !is.null(colnames(x))
  bez_nazw <- function(x) is.null(rownames(x)) && is.null(colnames(x))
  nazwy_domyslne <- FALSE
  if (pelne_nazwy(obserwacje) && pelne_nazwy(interwencje)) {
    if (!setequal(rownames(obserwacje), rownames(interwencje)) ||
        !setequal(colnames(obserwacje), colnames(interwencje)) ||
        anyDuplicated(rownames(obserwacje)) ||
        anyDuplicated(colnames(obserwacje))) {
      stop("Tablice musza miec zgodne poziomy i wymiary", call. = FALSE)
    }
    interwencje <- interwencje[rownames(obserwacje), colnames(obserwacje),
                               drop = FALSE]
  } else if (bez_nazw(obserwacje) && bez_nazw(interwencje)) {
    nazwy <- list(paste0("x", seq_len(nrow(obserwacje))),
                  paste0("y", seq_len(ncol(obserwacje))))
    dimnames(obserwacje) <- nazwy
    dimnames(interwencje) <- nazwy
    nazwy_domyslne <- TRUE
  } else {
    stop("Tablice musza miec zgodne poziomy i wymiary", call. = FALSE)
  }

  u <- rowSums(obserwacje)
  v <- colSums(obserwacje)
  dolny <- obserwacje <= interwencje + tolerancja
  gorny <- interwencje <= obserwacje + (1 - u) + tolerancja

  structure(
    list(
      obserwacje = obserwacje,
      interwencje = interwencje,
      zabieg = u,
      wynik = v,
      poziomy = list(zabieg = rownames(obserwacje),
                     wynik = colnames(obserwacje)),
      tolerancja = tolerancja,
      diagnostyka = list(
        suma_obserwacji = sum(obserwacje),
        sumy_interwencji = rowSums(interwencje),
        zgodnosc = all(dolny & gorny),
        nazwy_domyslne = nazwy_domyslne
      )
    ),
    class = "rozklady_przyczynowe"
  )
}
