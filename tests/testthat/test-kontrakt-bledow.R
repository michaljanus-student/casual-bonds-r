# Warstwa trzecia: dane, przy ktorych metoda nie ma sensu.
#
# Testy opisuja kontrakt bledow z opisu tematu. Oczekiwanym wynikiem jest
# zatrzymanie wykonania z komunikatem, a nie liczba. Kontrola wejscia
# w przygotuj_rozklady() (R/rozklady.R) nie jest jeszcze kompletna, wiec
# testy sa pomijane; po jej implementacji nalezy usunac wywolania pomin().

pomin <- function() {
  skip("Walidacja wejscia do implementacji (kontrakt bledow, SPEC.md).")
}

poziomy <- list(c("x1", "x2"), c("y1", "y2"))
o_dobre <- matrix(c(0.3, 0.1, 0.2, 0.4), 2, dimnames = poziomy)
a_dobre <- matrix(c(0.6, 0.2, 0.4, 0.8), 2, dimnames = poziomy)

test_that("tablice o niezgodnych poziomach zatrzymuja wykonanie", {
  pomin()
  inne <- a_dobre
  rownames(inne) <- c("x1", "x3")
  expect_error(przygotuj_rozklady(o_dobre, inne),
               "Tablice musza miec zgodne poziomy i wymiary")
  expect_error(przygotuj_rozklady(matrix(1, 1, 1), matrix(1, 1, 1)),
               "Tablice musza miec zgodne poziomy i wymiary")
})

test_that("prawdopodobienstwa spoza [0, 1] i nieskonczone zatrzymuja wykonanie", {
  pomin()
  ujemne <- o_dobre
  ujemne[1, 1] <- -0.01
  ujemne[1, 2] <- 0.21
  expect_error(przygotuj_rozklady(ujemne, a_dobre),
               "Prawdopodobienstwa musza byc skonczone i nalezec do \\[0, 1\\]")
  nieskonczone <- a_dobre
  nieskonczone[1, 1] <- Inf
  expect_error(przygotuj_rozklady(o_dobre, nieskonczone),
               "Prawdopodobienstwa musza byc skonczone i nalezec do \\[0, 1\\]")
  braki <- o_dobre
  braki[2, 2] <- NA
  expect_error(przygotuj_rozklady(braki, a_dobre),
               "Prawdopodobienstwa musza byc skonczone i nalezec do \\[0, 1\\]")
})

test_that("rozklad niesumujacy sie do jednosci zatrzymuje wykonanie", {
  pomin()
  expect_error(przygotuj_rozklady(o_dobre * 0.9, a_dobre),
               "Rozklad nie sumuje sie do jednosci")
  zle_wiersze <- a_dobre
  zle_wiersze[1, ] <- c(0.6, 0.5)
  expect_error(przygotuj_rozklady(o_dobre, zle_wiersze),
               "Rozklad nie sumuje sie do jednosci")
})

test_that("sprzeczne marginesy zatrzymuja wykonanie w konstruktorze", {
  pomin()
  # a11 = 0,2 < o11 = 0,3 narusza warunek o_ji <= a_ji
  sprzeczne <- matrix(c(0.2, 0.2, 0.8, 0.8), 2, dimnames = poziomy)
  expect_error(przygotuj_rozklady(o_dobre, sprzeczne),
               "Rozklady naruszaja warunek zgodnosci")
})

test_that("tablice bez nazw poziomow nie sa dopasowywane domyslnie", {
  pomin()
  expect_error(przygotuj_rozklady(unname(o_dobre), a_dobre),
               "Tablice musza miec zgodne poziomy i wymiary")
})
