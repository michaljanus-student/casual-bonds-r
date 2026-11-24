test_that("metody granic dzialaja i zwracaja obiekty", {
  dane <- przygotuj_rozklady(tablice_kontrolne$obserwacje,
                             tablice_kontrolne$interwencje)
  w <- granice_wszystkie(dane, warunkowe = TRUE)
  expect_output(print(w, maks_wierszy = 3), "dalszych wierszy")
  s <- summary(w)
  expect_s3_class(s, "summary.granice_przyczynowe")
  expect_equal(sum(s$zestawienie$liczba), nrow(w$przedzialy))
  expect_output(print(s), "Statusy")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  pokazane <- plot(w)
  expect_equal(nrow(pokazane), sum(!w$przedzialy$warunkowe))
  expect_s3_class(plot(w, warunkowe = TRUE), "data.frame")
})

test_that("wypisanie rozkladow i modelu dziala", {
  dane <- przygotuj_rozklady(tablice_kontrolne$obserwacje,
                             tablice_kontrolne$interwencje)
  expect_output(print(dane), "spelniony")
  expect_output(print(generuj_model(liczebnosc = 50)), "81 stanow")
})
