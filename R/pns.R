#' Granice prawdopodobienstwa koniecznosci i wystarczalnosci
#'
#' Dla dwoch poziomow zabiegu funkcja wyznacza granice prawdopodobienstwa
#' wspolnego zdarzenia \eqn{P(Y_{x_j} = y_i, Y_{x_p} = y_k)}: wynik
#' \eqn{y_i} po zabiegu \eqn{x_j} i wynik \eqn{y_k} po zabiegu \eqn{x_p}.
#' Binarne prawdopodobienstwo koniecznosci i wystarczalnosci (PNS) jest
#' przypadkiem, w ktorym \eqn{y_i} oznacza sukces po zabiegu, a \eqn{y_k}
#' porazke bez zabiegu.
#'
#' Obliczenie dzieli populacje wedlug faktycznie otrzymanego zabiegu. W grupie
#' \eqn{X = x_p} wynik \eqn{Y_{x_p}} jest obserwowany, wiec zdarzenie
#' sprowadza sie do \eqn{P(Y_{x_j} = y_i, X = x_p, Y = y_k)}; analogicznie
#' w grupie \eqn{X = x_j}. Suma granic obu grup, polaczona z ograniczeniami
#' Frecheta dla rozkladow interwencyjnych, daje
#' \deqn{L = \max\{0,\ a_{ji} + a_{pk} - 1,\ L_{pk,ji} + L_{ji,pk}\},}
#' \deqn{U = \min\{a_{ji},\ a_{pk},\ U_{pk,ji} + U_{ji,pk}\},}
#' gdzie \eqn{L_{ji,pk}} i \eqn{U_{ji,pk}} sa granicami zdarzenia
#' \eqn{P(Y_{x_j} = y_i, X = x_p, Y = y_k)}. Dla dwoch poziomow zabiegu
#' granice twierdzenia 8 Li i Pearla (2024) pokrywaja sie z ostrymi
#' granicami twierdzenia 2 Shu, Wanga i Li (2026); argument \code{wariant}
#' zmienia wylacznie opis zrodla.
#'
#' @inheritParams granice_zdarzenia
#' @param wynik_po Poziom wyniku po zabiegu \code{zabieg}: nazwa albo numer.
#' @param wynik_bez Poziom wyniku po drugim poziomie zabiegu: nazwa albo
#'   numer.
#' @param zabieg Poziom zabiegu, ktorego dotyczy \code{wynik_po}. Domyslnie
#'   drugi poziom zabiegu.
#'
#' @return Obiekt klasy \code{granice_przyczynowe} z jednym wierszem.
#'
#' @examples
#' dane <- przygotuj_rozklady(tablice_kontrolne$obserwacje,
#'                            tablice_kontrolne$interwencje)
#' # Sukces (y2) po zabiegu x2 i porazka (y1) przy x1: PNS w [0,4; 0,6]
#' granice_pns(dane, wynik_po = "y2", wynik_bez = "y1")
#'
#' @export
granice_pns <- function(dane, wynik_po, wynik_bez, zabieg = NULL,
                        wariant = c("ostre", "artykul")) {
  wariant <- match.arg(wariant)
  x <- dane$poziomy$zabieg
  y <- dane$poziomy$wynik
  if (length(x) != 2L) {
    stop("PNS w tym wariancie wymaga dwoch poziomow zabiegu", call. = FALSE)
  }
  j <- if (is.null(zabieg)) 2L else indeks_poziomu(zabieg, x, "zabieg")
  p <- 3L - j
  i <- indeks_poziomu(wynik_po, y, "wynik_po")
  k <- indeks_poziomu(wynik_bez, y, "wynik_bez")

  o <- dane$obserwacje
  a <- dane$interwencje
  u <- dane$zabieg
  grupa_p <- granica_grupa(o, a, u, j, i, p, k)  # P(Y_xj = y_i, X = x_p, Y = y_k)
  grupa_j <- granica_grupa(o, a, u, p, k, j, i)  # P(Y_xp = y_k, X = x_j, Y = y_i)
  dolna <- max(0, a[j, i] + a[p, k] - 1, grupa_p[1] + grupa_j[1])
  gorna <- min(a[j, i], a[p, k], grupa_p[2] + grupa_j[2])
  if (dolna > gorna + dane$tolerancja) {
    stop("Rozklady naruszaja warunek zgodnosci: dolna granica przekracza ",
         "gorna.", call. = FALSE)
  }

  przedzialy <- data.frame(
    zdarzenie = opis_zdarzenia("pns", x[j], y[i], x[p], y[k]),
    rodzaj = "pns",
    warunkowe = FALSE,
    dolna = dolna,
    gorna = gorna,
    status = "zdefiniowana",
    twierdzenie = zrodlo_twierdzenia("pns", wariant),
    stringsAsFactors = FALSE
  )
  nowe_granice(przedzialy, dane,
               list(wariant = wariant, rodzaj = "pns",
                    tolerancja = dane$tolerancja))
}
