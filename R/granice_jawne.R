# Granice zdarzen z jednym wynikiem hipotetycznym.
#
# Oznaczenia: o = obserwacje (o_ji = P(X = x_j, Y = y_i)), a = interwencje
# (a_ji = P(Y_xj = y_i)), u_j = P(X = x_j), v_i = P(Y = y_i).
# Kazda funkcja zwraca wektor c(L, U) granic wspolnego zdarzenia.
#
# Wariant "artykul": twierdzenia 4-7 Li i Pearla (2024).
# Wariant "ostre": twierdzenia 3-5 Shu, Wanga i Li (2026) dla jednego
# wyniku hipotetycznego (k = 1). Warianty roznia sie wylacznie dolna granica
# zachowania i zastapienia wyniku.

# P(Y_xj = y_i, Y = y_i)
granica_zachowanie <- function(o, a, u, v, j, i, wariant) {
  if (wariant == "artykul") {
    dolna <- max(o[j, i], a[j, i] + v[i] - 1)
  } else {
    # Ograniczenie Frecheta w warstwie X != x_j:
    # P(Y_xj = y_i, X != x_j) = a_ji - o_ji, P(X != x_j, Y = y_i) = v_i - o_ji
    dolna <- max(o[j, i], a[j, i] + v[i] - 1 + u[j] - o[j, i])
  }
  c(dolna, min(a[j, i], v[i]))
}

# P(Y_xj = y_i, Y = y_k), i != k
granica_zastapienie <- function(o, a, u, v, j, i, k, wariant) {
  if (wariant == "artykul") {
    skladniki <- pmax(0, a[j, i] + o[-j, k] - 1 + u[j] - o[j, i])
    dolna <- max(0, a[j, i] + v[k] - 1, sum(skladniki))
  } else {
    dolna <- max(0, a[j, i] - o[j, i] + u[j] + v[k] - o[j, k] - 1)
  }
  c(dolna, min(a[j, i] - o[j, i], v[k] - o[j, k]))
}

# P(Y_xj = y_i, X = x_p), j != p
granica_zmiana_zabiegu <- function(o, a, u, j, i, p) {
  c(max(0, a[j, i] - o[j, i] - 1 + u[j] + u[p]),
    min(a[j, i] - o[j, i], u[p]))
}

# P(Y_xj = y_i, X = x_p, Y = y_k), j != p
granica_grupa <- function(o, a, u, j, i, p, k) {
  c(max(0, a[j, i] + o[p, k] - 1 + u[j] - o[j, i]),
    min(a[j, i] - o[j, i], o[p, k]))
}

zrodlo_twierdzenia <- function(rodzaj, wariant) {
  if (wariant == "artykul") {
    numer <- c(zachowanie = 4, zastapienie = 5, zmiana_zabiegu = 6,
               grupa = 7, pns = 8)[[rodzaj]]
    return(sprintf("Li i Pearl (2024), tw. %d", numer))
  }
  switch(rodzaj,
         zachowanie = "Shu, Wang i Li (2026), tw. 4 (PRep)",
         zastapienie = "Shu, Wang i Li (2026), tw. 4 (PRep)",
         zmiana_zabiegu = "Shu, Wang i Li (2026), tw. 3 (PSub)",
         grupa = "Shu, Wang i Li (2026), tw. 5 (PN)",
         pns = "Shu, Wang i Li (2026), tw. 2 (PNS)")
}

opis_zdarzenia <- function(rodzaj, xj, yi, xp = NULL, yk = NULL,
                           warunkowe = FALSE) {
  sep <- if (warunkowe) " | " else ", "
  switch(rodzaj,
         zachowanie = sprintf("P(Y_%s = %s%sY = %s)", xj, yi, sep, yi),
         zastapienie = sprintf("P(Y_%s = %s%sY = %s)", xj, yi, sep, yk),
         zmiana_zabiegu = sprintf("P(Y_%s = %s%sX = %s)", xj, yi, sep, xp),
         grupa = sprintf("P(Y_%s = %s%sX = %s, Y = %s)", xj, yi, sep, xp, yk),
         pns = sprintf("P(Y_%s = %s, Y_%s = %s)", xj, yi, xp, yk))
}

indeks_poziomu <- function(wartosc, poziomy, argument) {
  indeks <- if (is.character(wartosc)) {
    match(wartosc, poziomy)
  } else if (is.numeric(wartosc)) {
    as.integer(wartosc)
  } else {
    NA_integer_
  }
  if (length(indeks) != 1L || is.na(indeks) || indeks < 1L ||
      indeks > length(poziomy)) {
    stop(sprintf("Argument `%s` musi wskazywac jeden z poziomow: %s.",
                 argument, paste(poziomy, collapse = ", ")), call. = FALSE)
  }
  indeks
}

# Wiersz (lub dwa wiersze, z granicami warunkowymi) tabeli przedzialow.
wiersze_granic <- function(dane, rodzaj, j, i, p = NULL, k = NULL,
                           warunkowe, wariant) {
  o <- dane$obserwacje
  a <- dane$interwencje
  u <- dane$zabieg
  v <- dane$wynik
  x <- dane$poziomy$zabieg
  y <- dane$poziomy$wynik
  tolerancja <- dane$tolerancja

  punktowa <- rodzaj %in% c("zmiana_zabiegu", "grupa") && p == j
  if (punktowa) {
    # Zgodnosc: przy X = x_j wynik potencjalny Y_xj jest wynikiem obserwowanym
    wartosc <- if (rodzaj == "zmiana_zabiegu") o[j, i] else
      if (i == k) o[j, i] else 0
    granice <- c(wartosc, wartosc)
    twierdzenie <- "zgodnosc: Y = Y_x przy X = x"
    status <- "identyfikacja_punktowa"
  } else {
    granice <- switch(
      rodzaj,
      zachowanie = granica_zachowanie(o, a, u, v, j, i, wariant),
      zastapienie = granica_zastapienie(o, a, u, v, j, i, k, wariant),
      zmiana_zabiegu = granica_zmiana_zabiegu(o, a, u, j, i, p),
      grupa = granica_grupa(o, a, u, j, i, p, k)
    )
    twierdzenie <- zrodlo_twierdzenia(rodzaj, wariant)
    status <- "zdefiniowana"
  }
  if (granice[1] > granice[2] + tolerancja) {
    # Koncow przedzialu sie nie zamienia: L > U oznacza niespojne dane
    stop("Rozklady naruszaja warunek zgodnosci: dolna granica przekracza ",
         "gorna dla zdarzenia ", opis_zdarzenia(rodzaj, x[j], y[i], x[p], y[k]),
         ".", call. = FALSE)
  }

  wspolne <- data.frame(
    zdarzenie = opis_zdarzenia(rodzaj, x[j], y[i], x[p], y[k]),
    rodzaj = rodzaj,
    warunkowe = FALSE,
    dolna = granice[1],
    gorna = granice[2],
    status = status,
    twierdzenie = twierdzenie,
    stringsAsFactors = FALSE
  )
  if (!warunkowe) {
    return(wspolne)
  }

  mianownik <- switch(rodzaj,
                      zachowanie = v[i],
                      zastapienie = v[k],
                      zmiana_zabiegu = u[p],
                      grupa = o[p, k])
  warunkowy <- wspolne
  warunkowy$zdarzenie <- opis_zdarzenia(rodzaj, x[j], y[i], x[p], y[k],
                                        warunkowe = TRUE)
  warunkowy$warunkowe <- TRUE
  if (mianownik <= tolerancja) {
    warunkowy$dolna <- NA_real_
    warunkowy$gorna <- NA_real_
    warunkowy$status <- "zerowe_warunkowanie"
  } else {
    warunkowy$dolna <- granice[1] / mianownik
    warunkowy$gorna <- granice[2] / mianownik
  }
  rbind(wspolne, warunkowy)
}

nowe_granice <- function(przedzialy, dane, parametry) {
  rownames(przedzialy) <- NULL
  structure(
    list(przedzialy = przedzialy, poziomy = dane$poziomy,
         parametry = parametry),
    class = "granice_przyczynowe"
  )
}


#' Granice prawdopodobienstwa zdarzenia z jednym wynikiem hipotetycznym
#'
#' Funkcja wyznacza dolna i gorna granice prawdopodobienstwa jednego zdarzenia
#' z wynikiem hipotetycznym \eqn{Y_{x_j} = y_i}, czyli wynikiem, ktory
#' pojawilby sie po ustawieniu zabiegu na \eqn{x_j}. Granice opisuja zbior
#' wartosci zgodnych z rozkladem obserwacyjnym i interwencyjnym; ich
#' szerokosc wynika z niepelnej identyfikacji wielkosci.
#'
#' Rodzaje zdarzen:
#' \describe{
#'   \item{\code{"zachowanie"}}{\eqn{P(Y_{x_j} = y_i, Y = y_i)}: wynik po
#'     ustawieniu zabiegu pozostaje rowny obserwowanemu,}
#'   \item{\code{"zastapienie"}}{\eqn{P(Y_{x_j} = y_i, Y = y_k)},
#'     \eqn{i \ne k}: obserwowany wynik \eqn{y_k} zostaje zastapiony przez
#'     \eqn{y_i},}
#'   \item{\code{"zmiana_zabiegu"}}{\eqn{P(Y_{x_j} = y_i, X = x_p)}: wynik
#'     interwencji w grupie, ktora otrzymala zabieg \eqn{x_p},}
#'   \item{\code{"grupa"}}{\eqn{P(Y_{x_j} = y_i, X = x_p, Y = y_k)}: wynik
#'     interwencji w grupie o obserwowanym zabiegu i wyniku (uogolnione
#'     prawdopodobienstwo koniecznosci).}
#' }
#' Dla \eqn{p = j} zgodnosc wyniku obserwowanego z potencjalnym wyznacza
#' wartosc punktowo, ze statusem \code{"identyfikacja_punktowa"}.
#'
#' Granice warunkowe powstaja przez podzielenie granic wspolnych przez
#' prawdopodobienstwo zdarzenia warunkujacego: \eqn{v_i}, \eqn{v_k},
#' \eqn{u_p} albo \eqn{o_{pk}}. Przy zerowym mianowniku granice warunkowe
#' przyjmuja wartosc \code{NA} ze statusem \code{"zerowe_warunkowanie"},
#' a granice wspolne pozostaja w wyniku.
#'
#' @param dane Obiekt klasy \code{rozklady_przyczynowe}, wynik
#'   \code{\link{przygotuj_rozklady}}.
#' @param rodzaj Rodzaj zdarzenia: \code{"zachowanie"}, \code{"zastapienie"},
#'   \code{"zmiana_zabiegu"} albo \code{"grupa"}.
#' @param wynik Poziom wyniku hipotetycznego \eqn{y_i}: nazwa albo numer.
#' @param zabieg Poziom zabiegu \eqn{x_j} w interwencji: nazwa albo numer.
#' @param wynik_obserwowany Obserwowany poziom wyniku \eqn{y_k}, wymagany
#'   dla rodzajow \code{"zastapienie"} i \code{"grupa"}.
#' @param zabieg_obserwowany Obserwowany poziom zabiegu \eqn{x_p}, wymagany
#'   dla rodzajow \code{"zmiana_zabiegu"} i \code{"grupa"}.
#' @param warunkowe Czy dolaczyc granice warunkowe wzgledem zdarzenia
#'   obserwowanego.
#' @param wariant \code{"ostre"} (granice ostre, Shu, Wang i Li, 2026) albo
#'   \code{"artykul"} (twierdzenia 4--7 Li i Pearla, 2024).
#'
#' @return Obiekt klasy \code{granice_przyczynowe}, lista ze skladnikami:
#'   \describe{
#'     \item{przedzialy}{ramka danych z kolumnami \code{zdarzenie},
#'       \code{rodzaj}, \code{warunkowe}, \code{dolna}, \code{gorna},
#'       \code{status} i \code{twierdzenie}}
#'     \item{poziomy}{nazwy poziomow zabiegu i wyniku}
#'     \item{parametry}{lista z wariantem, rodzajem i tolerancja}
#'   }
#'
#' @references Li, A., Pearl, J. (2024). Probabilities of causation with
#'   nonbinary treatment and effect. Proceedings of the AAAI Conference on
#'   Artificial Intelligence, 38(18), 20465--20472.
#'   \doi{10.1609/aaai.v38i18.30030}
#'
#'   Shu, X., Wang, S., Li, A. (2026). Identification of probabilities of
#'   causation: from recursive to closed-form bounds. Proceedings of IJCAI-26,
#'   4063--4070. \doi{10.24963/ijcai.2026/452}
#'
#' @seealso \code{\link{granice_wszystkie}}, \code{\link{granice_pns}}
#'
#' @examples
#' dane <- przygotuj_rozklady(tablice_kontrolne$obserwacje,
#'                            tablice_kontrolne$interwencje)
#'
#' # Prawdopodobienstwo koniecznosci: porazka przy x1 w grupie X = x2, Y = y2
#' granice_zdarzenia(dane, "grupa", wynik = "y1", zabieg = "x1",
#'                   wynik_obserwowany = "y2", zabieg_obserwowany = "x2",
#'                   warunkowe = TRUE)
#'
#' # Zachowanie wyniku w obu wariantach
#' granice_zdarzenia(dane, "zachowanie", wynik = "y1", zabieg = "x1")
#' granice_zdarzenia(dane, "zachowanie", wynik = "y1", zabieg = "x1",
#'                   wariant = "artykul")
#'
#' @export
granice_zdarzenia <- function(dane,
                              rodzaj = c("zachowanie", "zastapienie",
                                         "zmiana_zabiegu", "grupa"),
                              wynik, zabieg,
                              wynik_obserwowany = NULL,
                              zabieg_obserwowany = NULL,
                              warunkowe = FALSE,
                              wariant = c("ostre", "artykul")) {
  # TODO: kontrola, ze `dane` jest obiektem klasy rozklady_przyczynowe,
  #   a `warunkowe` pojedyncza wartoscia logiczna.
  rodzaj <- match.arg(rodzaj)
  wariant <- match.arg(wariant)
  x <- dane$poziomy$zabieg
  y <- dane$poziomy$wynik

  j <- indeks_poziomu(zabieg, x, "zabieg")
  i <- indeks_poziomu(wynik, y, "wynik")
  p <- k <- NULL
  if (rodzaj %in% c("zastapienie", "grupa")) {
    if (is.null(wynik_obserwowany)) {
      stop(sprintf("Rodzaj `%s` wymaga argumentu `wynik_obserwowany`.",
                   rodzaj), call. = FALSE)
    }
    k <- indeks_poziomu(wynik_obserwowany, y, "wynik_obserwowany")
  }
  if (rodzaj %in% c("zmiana_zabiegu", "grupa")) {
    if (is.null(zabieg_obserwowany)) {
      stop(sprintf("Rodzaj `%s` wymaga argumentu `zabieg_obserwowany`.",
                   rodzaj), call. = FALSE)
    }
    p <- indeks_poziomu(zabieg_obserwowany, x, "zabieg_obserwowany")
  }
  if (rodzaj == "zastapienie" && i == k) {
    stop("Zastapienie wymaga roznych poziomow `wynik` i ",
         "`wynik_obserwowany`; dla rownych poziomow wlasciwy jest rodzaj ",
         "`zachowanie`.", call. = FALSE)
  }

  przedzialy <- wiersze_granic(dane, rodzaj, j, i, p, k, warunkowe, wariant)
  nowe_granice(przedzialy, dane,
               list(wariant = wariant, rodzaj = rodzaj,
                    tolerancja = dane$tolerancja))
}


#' Granice wszystkich zdarzen z jednym wynikiem hipotetycznym
#'
#' Funkcja wyznacza granice dla kazdej kombinacji poziomow w czterech
#' rodzajach zdarzen opisanych w \code{\link{granice_zdarzenia}}: zachowania
#' wyniku (\eqn{mn} zdarzen), zastapienia wyniku (\eqn{mn(n-1)}), zmiany
#' zabiegu (\eqn{mn(m-1)}) i wyniku hipotetycznego w grupie
#' (\eqn{mn(m-1)n}). Pojedyncze zdarzenie kosztuje \eqn{O(m)} operacji,
#' a liczba wszystkich zdarzen rosnie jak \eqn{O(m^2 n^2)}.
#'
#' @inheritParams granice_zdarzenia
#'
#' @return Obiekt klasy \code{granice_przyczynowe} z jednym wierszem na
#'   zdarzenie (oraz wierszem granic warunkowych, gdy
#'   \code{warunkowe = TRUE}).
#'
#' @examples
#' dane <- przygotuj_rozklady(tablice_kontrolne$obserwacje,
#'                            tablice_kontrolne$interwencje)
#' wszystkie <- granice_wszystkie(dane)
#' summary(wszystkie)
#'
#' @export
granice_wszystkie <- function(dane, warunkowe = FALSE,
                              wariant = c("ostre", "artykul")) {
  wariant <- match.arg(wariant)
  m <- length(dane$poziomy$zabieg)
  n <- length(dane$poziomy$wynik)
  wiersze <- list()
  dodaj <- function(...) {
    wiersze[[length(wiersze) + 1L]] <<- wiersze_granic(
      dane, ..., warunkowe = warunkowe, wariant = wariant
    )
  }
  for (j in seq_len(m)) {
    for (i in seq_len(n)) {
      dodaj("zachowanie", j, i)
      for (k in setdiff(seq_len(n), i)) dodaj("zastapienie", j, i, k = k)
      for (p in setdiff(seq_len(m), j)) {
        dodaj("zmiana_zabiegu", j, i, p = p)
        for (k in seq_len(n)) dodaj("grupa", j, i, p = p, k = k)
      }
    }
  }
  nowe_granice(do.call(rbind, wiersze), dane,
               list(wariant = wariant, rodzaj = "wszystkie",
                    tolerancja = dane$tolerancja))
}
