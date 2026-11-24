#' Metody dla granic prawdopodobienstw przyczynowosci
#'
#' Wypisanie, podsumowanie i wykres obiektu klasy \code{granice_przyczynowe}.
#' Podsumowanie zestawia liczbe zdarzen, srednia i najwieksza szerokosc
#' przedzialow (\code{szer_srednia}, \code{szer_maks}) oraz liczbe granic
#' nieokreslonych wedlug rodzaju zdarzenia i rodzaju granic, a takze liczbe
#' wierszy o kazdym statusie. Wykres pokazuje przedzialy jako odcinki na osi
#' prawdopodobienstwa; szerokosc odcinka opisuje niepelna identyfikacje
#' wielkosci.
#'
#' @param x,object Obiekt klasy \code{granice_przyczynowe} albo
#'   \code{summary.granice_przyczynowe}.
#' @param cyfry Liczba cyfr znaczacych w wydruku.
#' @param maks_wierszy Najwieksza liczba wierszy wypisywanych przez
#'   \code{print()}.
#' @param warunkowe Czy wykres pokazuje granice warunkowe (\code{TRUE}) czy
#'   wspolne (\code{FALSE}).
#' @param ... Argumenty przekazywane dalej, obecnie nieuzywane.
#'
#' @return \code{print()} niewidocznie zwraca \code{x}. \code{summary()}
#'   zwraca obiekt klasy \code{summary.granice_przyczynowe} z tabela
#'   zestawienia wedlug rodzaju zdarzenia. \code{plot()} niewidocznie zwraca
#'   ramke danych z przedzialami pokazanymi na wykresie.
#'
#' @examples
#' dane <- przygotuj_rozklady(tablice_kontrolne$obserwacje,
#'                            tablice_kontrolne$interwencje)
#' wynik <- granice_wszystkie(dane, warunkowe = TRUE)
#' print(wynik, maks_wierszy = 6)
#' summary(wynik)
#' plot(granice_zdarzenia(dane, "grupa", wynik = "y1", zabieg = "x1",
#'                        wynik_obserwowany = "y2",
#'                        zabieg_obserwowany = "x2"))
#'
#' @name metody_granice
NULL

#' @rdname metody_granice
#' @export
print.granice_przyczynowe <- function(x, cyfry = 4, maks_wierszy = 20, ...) {
  przedzialy <- x$przedzialy
  cat(sprintf("Granice prawdopodobienstw przyczynowosci (wariant: %s)\n",
              x$parametry$wariant))
  cat(sprintf("  zabieg: %s\n", paste(x$poziomy$zabieg, collapse = ", ")))
  cat(sprintf("  wynik:  %s\n\n", paste(x$poziomy$wynik, collapse = ", ")))
  kolumny <- c("zdarzenie", "dolna", "gorna", "status", "twierdzenie")
  jedno_zrodlo <- length(unique(przedzialy$twierdzenie)) == 1L
  if (jedno_zrodlo) {
    kolumny <- setdiff(kolumny, "twierdzenie")
  }
  pokaz <- przedzialy[seq_len(min(nrow(przedzialy), maks_wierszy)), kolumny]
  pokaz$dolna <- signif(pokaz$dolna, cyfry)
  pokaz$gorna <- signif(pokaz$gorna, cyfry)
  print(pokaz, row.names = FALSE)
  if (nrow(przedzialy) > maks_wierszy) {
    cat(sprintf("... oraz %d dalszych wierszy\n",
                nrow(przedzialy) - maks_wierszy))
  }
  if (jedno_zrodlo) {
    cat(sprintf("\nZrodlo wzoru: %s\n", przedzialy$twierdzenie[1]))
  }
  invisible(x)
}

#' @rdname metody_granice
#' @export
summary.granice_przyczynowe <- function(object, ...) {
  p <- object$przedzialy
  szerokosc <- p$gorna - p$dolna
  granice <- ifelse(p$warunkowe, "warunkowe", "wspolne")
  grupy <- interaction(p$rodzaj, granice, drop = TRUE, sep = "|")
  podzial <- do.call(rbind, strsplit(levels(grupy), "|", fixed = TRUE))
  zestawienie <- data.frame(
    rodzaj = podzial[, 1],
    granice = podzial[, 2],
    liczba = as.integer(table(grupy)),
    szer_srednia = as.numeric(tapply(szerokosc, grupy, mean, na.rm = TRUE)),
    szer_maks = as.numeric(tapply(szerokosc, grupy, function(s) {
      if (all(is.na(s))) NA_real_ else max(s, na.rm = TRUE)
    })),
    nieokreslone = as.integer(tapply(is.na(szerokosc), grupy, sum)),
    stringsAsFactors = FALSE
  )
  structure(
    list(zestawienie = zestawienie,
         statusy = table(p$status),
         parametry = object$parametry,
         poziomy = object$poziomy),
    class = "summary.granice_przyczynowe"
  )
}

#' @rdname metody_granice
#' @export
print.summary.granice_przyczynowe <- function(x, cyfry = 4, ...) {
  cat(sprintf("Granice prawdopodobienstw przyczynowosci (wariant: %s)\n",
              x$parametry$wariant))
  cat(sprintf("Poziomy zabiegu: %d, poziomy wyniku: %d\n\n",
              length(x$poziomy$zabieg), length(x$poziomy$wynik)))
  zestawienie <- x$zestawienie
  zestawienie$szer_srednia <- signif(zestawienie$szer_srednia, cyfry)
  zestawienie$szer_maks <- signif(zestawienie$szer_maks, cyfry)
  print(zestawienie, row.names = FALSE)
  cat("\nStatusy:\n")
  print(x$statusy)
  invisible(x)
}

#' @rdname metody_granice
#' @export
plot.granice_przyczynowe <- function(x, warunkowe = FALSE, ...) {
  p <- x$przedzialy[x$przedzialy$warunkowe == warunkowe, , drop = FALSE]
  p <- p[!is.na(p$dolna), , drop = FALSE]
  if (!nrow(p)) {
    stop("Brak okreslonych przedzialow do pokazania.", call. = FALSE)
  }
  p <- p[rev(seq_len(nrow(p))), , drop = FALSE]
  wiersze <- seq_len(nrow(p))

  stare <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(stare), add = TRUE)
  szerokosc_etykiet <- max(graphics::strwidth(p$zdarzenie, units = "inches",
                                              cex = 0.8))
  graphics::par(mai = c(0.9, szerokosc_etykiet + 0.3, 0.6, 0.3))

  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0.5, nrow(p) + 0.5))
  graphics::abline(v = seq(0, 1, by = 0.2), col = "grey90")
  graphics::segments(p$dolna, wiersze, p$gorna, wiersze, lwd = 4,
                     col = "#2E5E9E", lend = 1)
  graphics::points(c(p$dolna, p$gorna), c(wiersze, wiersze), pch = "|",
                   col = "#2E5E9E")
  graphics::axis(1, at = seq(0, 1, by = 0.2))
  graphics::axis(2, at = wiersze, labels = p$zdarzenie, las = 1,
                 tick = FALSE, cex.axis = 0.8)
  graphics::title(
    main = sprintf("Granice %s (wariant: %s)",
                   if (warunkowe) "warunkowe" else "wspolne",
                   x$parametry$wariant),
    xlab = "Prawdopodobienstwo"
  )
  invisible(p[rev(wiersze), , drop = FALSE])
}


#' Metody dla rozkladow i modelu przyczynowego
#'
#' Wypisanie obiektu klasy \code{rozklady_przyczynowe} (tablice, rozklady
#' brzegowe i diagnostyka) oraz obiektu klasy \code{model_przyczynowy}
#' (parametry procedury generowania i dokladne tablice).
#'
#' @param x Obiekt klasy \code{rozklady_przyczynowe} albo
#'   \code{model_przyczynowy}.
#' @param cyfry Liczba cyfr znaczacych w wydruku.
#' @param ... Argumenty przekazywane dalej, obecnie nieuzywane.
#'
#' @return Niewidocznie zwraca \code{x}.
#'
#' @examples
#' przygotuj_rozklady(tablice_kontrolne$obserwacje,
#'                    tablice_kontrolne$interwencje)
#' generuj_model(liczebnosc = 200)
#'
#' @name metody_rozklady
NULL

#' @rdname metody_rozklady
#' @export
print.rozklady_przyczynowe <- function(x, cyfry = 4, ...) {
  cat("Rozklad obserwacyjny P(X = x, Y = y):\n")
  print(signif(x$obserwacje, cyfry))
  cat("\nRozklady interwencyjne P(Y_x = y), wiersz = ustawiony zabieg:\n")
  print(signif(x$interwencje, cyfry))
  cat("\nP(X):", paste(names(x$zabieg), signif(x$zabieg, cyfry), sep = " = ",
                       collapse = ", "), "\n")
  cat("P(Y):", paste(names(x$wynik), signif(x$wynik, cyfry), sep = " = ",
                     collapse = ", "), "\n")
  cat(sprintf("Warunek zgodnosci o <= a <= o + 1 - u: %s\n",
              if (x$diagnostyka$zgodnosc) "spelniony" else "NARUSZONY"))
  invisible(x)
}

#' @rdname metody_rozklady
#' @export
print.model_przyczynowy <- function(x, cyfry = 4, ...) {
  parametry <- x$parametry
  cat(sprintf(paste0("Model przyczynowy: %d poziomy zabiegu, %d poziomy ",
                     "wyniku, %d stanow, ksztalt %s, ziarno %s\n"),
              parametry$poziomy_zabiegu, parametry$poziomy_wyniku,
              nrow(x$stany), format(parametry$ksztalt),
              format(parametry$ziarno)))
  cat(sprintf("Probki: obserwacyjna i eksperymentalna po %d jednostek\n",
              as.integer(parametry$liczebnosc)))
  cat(sprintf("Zdarzenia o znanej wartosci prawdziwej: %d\n\n",
              nrow(x$prawda)))
  print(x$rozklady, cyfry = cyfry)
  invisible(x)
}
