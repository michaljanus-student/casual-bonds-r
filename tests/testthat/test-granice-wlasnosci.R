# Warstwa druga: wlasnosci, ktore musza zachodzic niezaleznie od danych.

test_that("granice mieszcza sie w [0, 1] i spelniaja L <= U", {
  withr::local_seed(11)
  for (powtorzenie in 1:20) {
    m <- sample(2:4, 1)
    n <- sample(2:4, 1)
    tab <- losowe_tablice(m, n)
    dane <- przygotuj_rozklady(tab$obserwacje, tab$interwencje)
    for (wariant in c("ostre", "artykul")) {
      p <- granice_wszystkie(dane, warunkowe = TRUE, wariant = wariant)$przedzialy
      p <- p[!is.na(p$dolna), ]
      expect_true(all(p$dolna >= -1e-12))
      expect_true(all(p$gorna <= 1 + 1e-12))
      expect_true(all(p$dolna <= p$gorna + 1e-12))
    }
  }
})

test_that("granice ostre zawieraja sie w granicach artykulu", {
  withr::local_seed(12)
  for (powtorzenie in 1:20) {
    tab <- losowe_tablice(sample(2:4, 1), sample(2:4, 1))
    dane <- przygotuj_rozklady(tab$obserwacje, tab$interwencje)
    ostre <- granice_wszystkie(dane)$przedzialy
    artykul <- granice_wszystkie(dane, wariant = "artykul")$przedzialy
    expect_identical(ostre$zdarzenie, artykul$zdarzenie)
    expect_true(all(ostre$dolna >= artykul$dolna - 1e-12))
    # Granice gorne obu wariantow sa identyczne
    expect_equal(ostre$gorna, artykul$gorna)
  }
})

test_that("granice warunkowe sa granicami wspolnymi podzielonymi przez mianownik", {
  withr::local_seed(13)
  tab <- losowe_tablice(3, 3)
  dane <- przygotuj_rozklady(tab$obserwacje, tab$interwencje)
  o <- dane$obserwacje
  w <- granice_zdarzenia(dane, "grupa", wynik = "y2", zabieg = "x1",
                         wynik_obserwowany = "y3", zabieg_obserwowany = "x2",
                         warunkowe = TRUE)$przedzialy
  expect_equal(w$dolna[2], w$dolna[1] / o["x2", "y3"])
  expect_equal(w$gorna[2], w$gorna[1] / o["x2", "y3"])
  z <- granice_zdarzenia(dane, "zachowanie", wynik = "y2", zabieg = "x3",
                         warunkowe = TRUE)$przedzialy
  expect_equal(z$gorna[2], z$gorna[1] / sum(o[, "y2"]))
})

test_that("permutacja poziomow z dopasowaniem nazw nie zmienia wyniku", {
  withr::local_seed(14)
  tab <- losowe_tablice(3, 4)
  dane <- przygotuj_rozklady(tab$obserwacje, tab$interwencje)
  rzedy <- c(3, 1, 2)
  kolumny <- c(2, 4, 1, 3)
  # Tablica interwencyjna w innej kolejnosci niz obserwacyjna
  przestawione <- przygotuj_rozklady(
    tab$obserwacje[rzedy, kolumny],
    tab$interwencje[rev(rzedy), rev(kolumny)]
  )
  for (wariant in c("ostre", "artykul")) {
    a <- granice_zdarzenia(dane, "zastapienie", wynik = "y1", zabieg = "x2",
                           wynik_obserwowany = "y4", warunkowe = TRUE,
                           wariant = wariant)$przedzialy
    b <- granice_zdarzenia(przestawione, "zastapienie", wynik = "y1",
                           zabieg = "x2", wynik_obserwowany = "y4",
                           warunkowe = TRUE, wariant = wariant)$przedzialy
    expect_equal(a, b)
  }
  wszystkie_a <- granice_wszystkie(dane)$przedzialy
  wszystkie_b <- granice_wszystkie(przestawione)$przedzialy
  wszystkie_b <- wszystkie_b[match(wszystkie_a$zdarzenie,
                                   wszystkie_b$zdarzenie), ]
  rownames(wszystkie_b) <- NULL
  expect_equal(wszystkie_a, wszystkie_b)
})

test_that("liczba zdarzen odpowiada liczbie kombinacji poziomow", {
  tab <- losowe_tablice(3, 2)
  p <- granice_wszystkie(przygotuj_rozklady(tab$obserwacje,
                                            tab$interwencje))$przedzialy
  m <- 3
  n <- 2
  liczby <- table(factor(p$rodzaj, c("zachowanie", "zastapienie",
                                     "zmiana_zabiegu", "grupa")))
  expect_equal(as.integer(liczby),
               c(m * n, m * n * (n - 1), m * n * (m - 1), m * n * (m - 1) * n))
})
