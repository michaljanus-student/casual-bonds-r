# Procedura generowania danych o znanej strukturze: pelny rozklad stanow
# wyznacza dokladne tablice i wartosc prawdziwa kazdego zdarzenia.

zawiera_prawde <- function(model, wariant) {
  granice <- granice_wszystkie(model$rozklady, wariant = wariant)$przedzialy
  prawda <- model$prawda[model$prawda$rodzaj != "pns", ]
  granice <- granice[match(prawda$zdarzenie, granice$zdarzenie), ]
  expect_false(anyNA(granice$dolna))
  expect_true(all(prawda$wartosc >= granice$dolna - 1e-12))
  expect_true(all(prawda$wartosc <= granice$gorna + 1e-12))
  granice
}

test_that("generator jest powtarzalny i nie zmienia stanu generatora", {
  set.seed(5)
  przed <- .Random.seed
  a <- generuj_model(liczebnosc = 100)
  expect_identical(.Random.seed, przed)
  b <- generuj_model(liczebnosc = 100)
  expect_identical(a, b)
})

test_that("model domyslny ma 81 stanow i spojne tablice", {
  model <- generuj_model(liczebnosc = 500)
  expect_equal(nrow(model$stany), 3 * 3^3)
  expect_equal(sum(model$stany$masa), 1)
  expect_equal(sum(model$rozklady$obserwacje), 1)
  expect_equal(unname(rowSums(model$rozklady$interwencje)), rep(1, 3))
  expect_true(model$rozklady$diagnostyka$zgodnosc)
  # wartosci prawdziwe dla 99 zdarzen z jednym wynikiem hipotetycznym
  expect_equal(nrow(model$prawda), 9 + 18 + 18 + 54)
  expect_equal(nrow(model$proba), 500)
  expect_equal(sum(model$tablice_proby$obserwacje), 1)
  expect_equal(unname(rowSums(model$tablice_proby$interwencje)), rep(1, 3))
})

test_that("granice z dokladnych tablic zawieraja wartosc prawdziwa", {
  modele <- list(
    generuj_model(liczebnosc = 100),
    generuj_model(liczebnosc = 100, ksztalt = 0.3, ziarno = 1),
    generuj_model(liczebnosc = 100, poziomy_zabiegu = 2, poziomy_wyniku = 3,
                  ksztalt = 0.3, ziarno = 2),
    generuj_model(liczebnosc = 100, poziomy_zabiegu = 3, poziomy_wyniku = 2,
                  ksztalt = 0.5, ziarno = 3)
  )
  for (model in modele) {
    zawiera_prawde(model, "ostre")
    zawiera_prawde(model, "artykul")
  }
})

test_that("granice PNS zawieraja wartosc prawdziwa w modelu z m = 2", {
  for (ziarno in 1:5) {
    model <- generuj_model(liczebnosc = 100, poziomy_zabiegu = 2,
                           poziomy_wyniku = 3, ksztalt = 0.3, ziarno = ziarno)
    x <- model$rozklady$poziomy$zabieg
    y <- model$rozklady$poziomy$wynik
    prawda <- model$prawda[model$prawda$rodzaj == "pns", ]
    expect_equal(nrow(prawda), 2 * 3 * 3)
    for (j in 1:2) {
      for (i in 1:3) {
        for (k in 1:3) {
          w <- granice_pns(model$rozklady, wynik_po = y[i], wynik_bez = y[k],
                           zabieg = x[j])$przedzialy
          wartosc <- prawda$wartosc[prawda$zdarzenie == w$zdarzenie]
          expect_length(wartosc, 1)
          expect_gte(wartosc, w$dolna - 1e-12)
          expect_lte(wartosc, w$gorna + 1e-12)
        }
      }
    }
  }
})

test_that("masy skupione uruchamiaja nietrywialne dolne granice", {
  model <- generuj_model(liczebnosc = 100, ksztalt = 0.3, ziarno = 1)
  g <- zawiera_prawde(model, "ostre")
  trywialna <- ifelse(g$rodzaj == "zachowanie",
                      model$rozklady$obserwacje[cbind(
                        match(sub("^P\\(Y_(x[0-9]+) .*", "\\1", g$zdarzenie),
                              model$rozklady$poziomy$zabieg),
                        match(sub("^P\\(Y_x[0-9]+ = (y[0-9]+),.*", "\\1",
                                  g$zdarzenie),
                              model$rozklady$poziomy$wynik))],
                      0)
  expect_gt(sum(g$dolna > trywialna + 1e-9), 0)
})
