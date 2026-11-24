#' Generowanie modelu przyczynowego o znanej wartosci prawdziwej
#'
#' Procedura tworzy pelny rozklad stanow \eqn{(X, Y_{x_1}, \ldots, Y_{x_m})}
#' dla zabiegu o \eqn{m} poziomach i wyniku o \eqn{n} poziomach. Stanow jest
#' \eqn{m n^m}; dla \eqn{m = n = 3} jest ich 81. Masa kazdego stanu jest
#' losowana niezaleznie z rozkladu \eqn{\mathrm{Gamma}(\kappa, 1)}, a nastepnie
#' normalizowana do sumy jeden, co daje rozklad Dirichleta o parametrze
#' \eqn{\kappa} na zbiorze stanow. Wynik obserwowany jest wynikiem
#' potencjalnym dla faktycznie otrzymanego zabiegu, \eqn{Y = Y_X}.
#'
#' Z rozkladu stanow wynikaja dokladnie: tablica obserwacyjna
#' \eqn{o_{ji} = P(X = x_j, Y_{x_j} = y_i)}, tablica interwencyjna
#' \eqn{a_{ji} = P(Y_{x_j} = y_i)} oraz wartosc prawdziwa kazdego zdarzenia
#' z jednym wynikiem hipotetycznym, a dla \eqn{m = 2} takze kazdego zdarzenia
#' typu PNS. Granice wyznaczone z dokladnych tablic musza zawierac wartosc
#' prawdziwa.
#'
#' Domyslne ustawienia (\eqn{m = n = 3}, \eqn{\kappa = 2}) odpowiadaja
#' procedurze z opisu tematu. Masy o ksztalcie 2 sa zblizone do siebie, wiec
#' dolne granice zwykle przyjmuja wartosci trywialne. Ksztalt mniejszy od
#' jednosci (na przyklad \eqn{\kappa} rowne 0,3) koncentruje mase na nielicznych
#' stanach i uruchamia nietrywialne galezie wzorow; ustawienie
#' \code{poziomy_zabiegu = 2} daje model dla granic PNS.
#'
#' Probka obserwacyjna liczy \code{liczebnosc} jednostek losowanych ze
#' stanow z prawdopodobienstwami rownymi masom. Probka eksperymentalna liczy
#' tyle samo nowych jednostek, ktorym zabieg przydzielono losowo z rownymi
#' prawdopodobienstwami; ich wynik jest wynikiem potencjalnym dla
#' przydzielonego zabiegu. Kolejnosc losowania: masy stanow, jednostki
#' probki obserwacyjnej, jednostki probki eksperymentalnej, przydzial
#' zabiegu.
#'
#' @param liczebnosc Liczba jednostek w kazdej z dwoch probek.
#' @param ziarno Ziarno generatora liczb losowych. Stan generatora w sesji
#'   uzytkownika zostaje przywrocony po zakonczeniu losowania.
#' @param poziomy_zabiegu Liczba poziomow zabiegu \eqn{m}.
#' @param poziomy_wyniku Liczba poziomow wyniku \eqn{n}.
#' @param ksztalt Parametr ksztaltu \eqn{\kappa} rozkladu Gamma mas stanow.
#'
#' @return Obiekt klasy \code{model_przyczynowy}, lista ze skladnikami:
#'   \describe{
#'     \item{stany}{ramka danych stanow z kolumnami \code{X},
#'       \code{Y_x1}, ..., \code{Y_xm} (numery poziomow) i \code{masa}}
#'     \item{rozklady}{obiekt \code{rozklady_przyczynowe} z dokladnymi
#'       tablicami}
#'     \item{prawda}{ramka danych z kolumnami \code{zdarzenie},
#'       \code{rodzaj} i \code{wartosc}: wartosc prawdziwa kazdego zdarzenia
#'       z jednym wynikiem hipotetycznym oraz, dla \eqn{m = 2}, kazdego
#'       zdarzenia PNS}
#'     \item{proba}{probka obserwacyjna: ramka danych z kolumnami
#'       \code{zabieg} i \code{wynik}}
#'     \item{eksperyment}{probka eksperymentalna: ramka danych z kolumnami
#'       \code{zabieg} (przydzielony) i \code{wynik}}
#'     \item{tablice_proby}{lista z czestosciami \code{obserwacje} (wspolne)
#'       i \code{interwencje} (warunkowe wzgledem przydzialu) z obu probek}
#'     \item{parametry}{lista argumentow wywolania}
#'   }
#'
#' @examples
#' model <- generuj_model(liczebnosc = 1000)
#' model
#' head(model$prawda)
#'
#' # Model dla PNS z masami skupionymi na nielicznych stanach
#' model2 <- generuj_model(liczebnosc = 500, poziomy_zabiegu = 2,
#'                         poziomy_wyniku = 3, ksztalt = 0.3)
#' model2$rozklady
#'
#' @export
generuj_model <- function(liczebnosc = 5000, ziarno = 202701,
                          poziomy_zabiegu = 3, poziomy_wyniku = 3,
                          ksztalt = 2) {
  # TODO: kontrola argumentow (dodatnie liczby calkowite, m, n >= 2,
  #   `ksztalt` dodatni i skonczony); dla duzych m, n liczba stanow m * n^m
  #   rosnie szybko i wymaga ograniczenia.
  m <- as.integer(poziomy_zabiegu)
  n <- as.integer(poziomy_wyniku)
  x <- paste0("x", seq_len(m))
  y <- paste0("y", seq_len(n))

  typy <- as.matrix(expand.grid(rep(list(seq_len(n)), m)))
  colnames(typy) <- paste0("Y_", x)
  liczba_typow <- nrow(typy)
  zabieg_stanu <- rep(seq_len(m), each = liczba_typow)
  typy_stanu <- typy[rep(seq_len(liczba_typow), m), , drop = FALSE]
  liczba_stanow <- length(zabieg_stanu)

  withr::with_seed(ziarno, {
    masa <- stats::rgamma(liczba_stanow, shape = ksztalt, rate = 1)
    jednostki_obs <- sample.int(liczba_stanow, liczebnosc, replace = TRUE,
                                prob = masa)
    jednostki_eksp <- sample.int(liczba_stanow, liczebnosc, replace = TRUE,
                                 prob = masa)
    przydzial <- sample.int(m, liczebnosc, replace = TRUE)
  })
  masa <- masa / sum(masa)

  # Zgodnosc: wynik obserwowany jest wynikiem potencjalnym dla zabiegu X
  wynik_stanu <- typy_stanu[cbind(seq_len(liczba_stanow), zabieg_stanu)]

  obserwacje <- matrix(0, m, n, dimnames = list(x, y))
  interwencje <- matrix(0, m, n, dimnames = list(x, y))
  for (j in seq_len(m)) {
    for (i in seq_len(n)) {
      obserwacje[j, i] <- sum(masa[zabieg_stanu == j & wynik_stanu == i])
      interwencje[j, i] <- sum(masa[typy_stanu[, j] == i])
    }
  }

  prawda <- prawda_modelu(masa, zabieg_stanu, typy_stanu, wynik_stanu, x, y)

  proba <- data.frame(
    zabieg = x[zabieg_stanu[jednostki_obs]],
    wynik = y[wynik_stanu[jednostki_obs]],
    stringsAsFactors = FALSE
  )
  eksperyment <- data.frame(
    zabieg = x[przydzial],
    wynik = y[typy_stanu[cbind(jednostki_eksp, przydzial)]],
    stringsAsFactors = FALSE
  )
  czestosci_obs <- table(factor(proba$zabieg, x), factor(proba$wynik, y))
  czestosci_eksp <- table(factor(eksperyment$zabieg, x),
                          factor(eksperyment$wynik, y))
  tablice_proby <- list(
    obserwacje = matrix(czestosci_obs / liczebnosc, m, n,
                        dimnames = list(x, y)),
    interwencje = matrix(prop.table(czestosci_eksp, 1), m, n,
                         dimnames = list(x, y))
  )

  stany <- data.frame(X = zabieg_stanu, typy_stanu, masa = masa,
                      row.names = NULL)

  structure(
    list(
      stany = stany,
      rozklady = przygotuj_rozklady(obserwacje, interwencje),
      prawda = prawda,
      proba = proba,
      eksperyment = eksperyment,
      tablice_proby = tablice_proby,
      parametry = list(liczebnosc = liczebnosc, ziarno = ziarno,
                       poziomy_zabiegu = m, poziomy_wyniku = n,
                       ksztalt = ksztalt)
    ),
    class = "model_przyczynowy"
  )
}

# Wartosci prawdziwe wszystkich zdarzen z jednym wynikiem hipotetycznym
# (i zdarzen PNS dla m = 2), w tej samej postaci opisu co w granicach.
prawda_modelu <- function(masa, zabieg_stanu, typy_stanu, wynik_stanu, x, y) {
  m <- length(x)
  n <- length(y)
  wiersze <- list()
  dodaj <- function(rodzaj, zdarzenie, warunek) {
    wiersze[[length(wiersze) + 1L]] <<- data.frame(
      zdarzenie = zdarzenie, rodzaj = rodzaj, wartosc = sum(masa[warunek]),
      stringsAsFactors = FALSE
    )
  }
  for (j in seq_len(m)) {
    for (i in seq_len(n)) {
      hipotetyczny <- typy_stanu[, j] == i
      dodaj("zachowanie", opis_zdarzenia("zachowanie", x[j], y[i]),
            hipotetyczny & wynik_stanu == i)
      for (k in setdiff(seq_len(n), i)) {
        dodaj("zastapienie",
              opis_zdarzenia("zastapienie", x[j], y[i], yk = y[k]),
              hipotetyczny & wynik_stanu == k)
      }
      for (p in setdiff(seq_len(m), j)) {
        dodaj("zmiana_zabiegu",
              opis_zdarzenia("zmiana_zabiegu", x[j], y[i], xp = x[p]),
              hipotetyczny & zabieg_stanu == p)
        for (k in seq_len(n)) {
          dodaj("grupa",
                opis_zdarzenia("grupa", x[j], y[i], x[p], y[k]),
                hipotetyczny & zabieg_stanu == p & wynik_stanu == k)
        }
      }
      if (m == 2L) {
        p <- 3L - j
        for (k in seq_len(n)) {
          dodaj("pns", opis_zdarzenia("pns", x[j], y[i], x[p], y[k]),
                hipotetyczny & typy_stanu[, p] == k)
        }
      }
    }
  }
  wynik <- do.call(rbind, wiersze)
  rownames(wynik) <- NULL
  wynik
}
