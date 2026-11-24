# Warstwa pierwsza: przypadki o wyniku policzonym recznie ze wzorow.
# Oznaczenia: o_ji = P(X = x_j, Y = y_i), a_ji = P(Y_xj = y_i),
# u_j = P(X = x_j), v_i = P(Y = y_i).

kontrolne <- function() {
  przygotuj_rozklady(tablice_kontrolne$obserwacje,
                     tablice_kontrolne$interwencje)
}

test_that("prawdopodobienstwo koniecznosci w przykladzie kontrolnym", {
  # o = ((0,3; 0,2), (0,1; 0,4)), a = ((0,6; 0,4), (0,2; 0,8)), u = (0,5; 0,5)
  # PN: P(Y_x1 = y1, X = x2, Y = y2), j = 1, i = 1, p = 2, k = 2
  # L = max(0, a11 + o22 - 1 + u1 - o11) = max(0, 0,6 + 0,4 - 1 + 0,5 - 0,3)
  #   = 0,2
  # U = min(a11 - o11, o22) = min(0,3; 0,4) = 0,3
  # warunkowo przez o22 = 0,4: [0,5; 0,75]
  w <- granice_zdarzenia(kontrolne(), "grupa", wynik = "y1", zabieg = "x1",
                         wynik_obserwowany = "y2", zabieg_obserwowany = "x2",
                         warunkowe = TRUE)
  p <- w$przedzialy
  expect_equal(p$dolna, c(0.2, 0.5))
  expect_equal(p$gorna, c(0.3, 0.75))
  expect_equal(p$warunkowe, c(FALSE, TRUE))
  expect_equal(p$twierdzenie[1], "Shu, Wang i Li (2026), tw. 5 (PN)")
})

test_that("prawdopodobienstwo wystarczalnosci w przykladzie kontrolnym", {
  # PS: P(Y_x2 = y2, X = x1, Y = y1), j = 2, i = 2, p = 1, k = 1
  # L = max(0, a22 + o11 - 1 + u2 - o22) = max(0, 0,8 + 0,3 - 1 + 0,5 - 0,4)
  #   = 0,2
  # U = min(a22 - o22, o11) = min(0,4; 0,3) = 0,3
  # warunkowo przez o11 = 0,3: [2/3; 1]
  w <- granice_zdarzenia(kontrolne(), "grupa", wynik = "y2", zabieg = "x2",
                         wynik_obserwowany = "y1", zabieg_obserwowany = "x1",
                         warunkowe = TRUE, wariant = "artykul")
  p <- w$przedzialy
  expect_equal(p$dolna, c(0.2, 2 / 3))
  expect_equal(p$gorna, c(0.3, 1))
  expect_equal(p$twierdzenie[1], "Li i Pearl (2024), tw. 7")
})

test_that("PNS w przykladzie kontrolnym", {
  # P(Y_x2 = y2, Y_x1 = y1): L = max(0, a22 + a11 - 1, L_A + L_B)
  # grupa X = x1: P(Y_x2 = y2, X = x1, Y = y1) w [0,2; 0,3]
  # grupa X = x2: P(Y_x1 = y1, X = x2, Y = y2) w [0,2; 0,3]
  # L = max(0, 0,8 + 0,6 - 1, 0,4) = 0,4; U = min(0,8; 0,6; 0,6) = 0,6
  for (wariant in c("ostre", "artykul")) {
    w <- granice_pns(kontrolne(), wynik_po = "y2", wynik_bez = "y1",
                     wariant = wariant)
    expect_equal(w$przedzialy$dolna, 0.4)
    expect_equal(w$przedzialy$gorna, 0.6)
  }
})

test_that("wysokie ryzyka sukcesu ograniczaja PNS z gory", {
  # a = ((0,01; 0,99), (0,01; 0,99)): sukces niemal pewny przy obu zabiegach,
  # wiec P(Y_x2 = sukces, Y_x1 = porazka) <= P(Y_x1 = porazka) = 0,01
  poziomy <- list(c("x1", "x2"), c("y1", "y2"))
  o <- matrix(c(0.005, 0.005, 0.495, 0.495), 2, dimnames = poziomy)
  a <- matrix(c(0.01, 0.01, 0.99, 0.99), 2, dimnames = poziomy)
  w <- granice_pns(przygotuj_rozklady(o, a), wynik_po = "y2", wynik_bez = "y1")
  expect_lte(w$przedzialy$gorna, 0.01 + 1e-12)
  expect_equal(w$przedzialy$dolna, 0, tolerance = 1e-12)
})

test_that("deterministyczny sukces po zabiegu i porazka bez niego daja PNS 1", {
  # a_x2 = (0, 1), a_x1 = (1, 0): kazda jednostka odnosi sukces po x2
  # i porazke po x1, wiec PNS = 1
  poziomy <- list(c("x1", "x2"), c("y1", "y2"))
  o <- matrix(c(0.5, 0, 0, 0.5), 2, dimnames = poziomy)
  a <- matrix(c(1, 0, 0, 1), 2, dimnames = poziomy)
  w <- granice_pns(przygotuj_rozklady(o, a), wynik_po = "y2", wynik_bez = "y1")
  expect_equal(w$przedzialy$dolna, 1)
  expect_equal(w$przedzialy$gorna, 1)
})

test_that("twierdzenie 4 artykulu daje granice nieostra", {
  # o = ((0,3; 0,2), (0,1; 0,4)), a_x1 = (0,8; 0,2), u = (0,5; 0,5),
  # v = (0,4; 0,6). Zdarzenie P(Y_x1 = y1, Y = y1).
  # Artykul: L = max(o11, a11 + v1 - 1) = max(0,3; 0,2) = 0,3,
  #          U = min(a11, v1) = 0,4.
  # Ostre:   L = max(o11, a11 + v1 - 1 + u1 - o11) = max(0,3; 0,4) = 0,4.
  # Uzasadnienie: a11 - o11 = 0,5 = u2, wiec kazda jednostka z X = x2 ma
  # Y_x1 = y1, a zdarzenie ma mase o11 + o21 = 0,4.
  poziomy <- list(c("x1", "x2"), c("y1", "y2"))
  o <- matrix(c(0.3, 0.1, 0.2, 0.4), 2, dimnames = poziomy)
  a <- matrix(c(0.8, 0.2, 0.2, 0.8), 2, dimnames = poziomy)
  dane <- przygotuj_rozklady(o, a)
  artykul <- granice_zdarzenia(dane, "zachowanie", wynik = "y1",
                               zabieg = "x1", wariant = "artykul")
  ostre <- granice_zdarzenia(dane, "zachowanie", wynik = "y1", zabieg = "x1")
  expect_equal(c(artykul$przedzialy$dolna, artykul$przedzialy$gorna),
               c(0.3, 0.4))
  expect_equal(c(ostre$przedzialy$dolna, ostre$przedzialy$gorna), c(0.4, 0.4))
})

test_that("twierdzenie 5 artykulu daje granice nieostra przy m = n = 3", {
  # u = (0,3; 0,35; 0,35), v = (0,2; 0,6; 0,2); P(Y_x1 = y1, Y = y2)
  # Artykul: L = max(0, a11 + v2 - 1, sum_p max(0, a11 + o_p2 - 1 + u1 - o11))
  #            = max(0; 0,3; 0,15 + 0,15) = 0,3
  # Ostre:   L = max(0, a11 - o11 + u1 + v2 - o12 - 1)
  #            = 0,7 - 0,1 + 0,3 + 0,6 - 0,1 - 1 = 0,4
  # U = min(a11 - o11, v2 - o12) = min(0,6; 0,5) = 0,5
  poziomy <- list(paste0("x", 1:3), paste0("y", 1:3))
  o <- matrix(c(0.10, 0.05, 0.05, 0.10, 0.25, 0.25, 0.10, 0.05, 0.05), 3,
              dimnames = poziomy)
  a <- matrix(c(0.70, 0.20, 0.20, 0.15, 0.60, 0.60, 0.15, 0.20, 0.20), 3,
              dimnames = poziomy)
  dane <- przygotuj_rozklady(o, a)
  artykul <- granice_zdarzenia(dane, "zastapienie", wynik = "y1",
                               zabieg = "x1", wynik_obserwowany = "y2",
                               wariant = "artykul")$przedzialy
  ostre <- granice_zdarzenia(dane, "zastapienie", wynik = "y1",
                             zabieg = "x1", wynik_obserwowany = "y2")$przedzialy
  expect_equal(c(artykul$dolna, artykul$gorna), c(0.3, 0.5))
  expect_equal(c(ostre$dolna, ostre$gorna), c(0.4, 0.5))
})

test_that("zerowa masa grupy warunkujacej daje NA i zachowuje granice wspolne", {
  # o12 = P(X = x1, Y = y2) = 0, wiec warunkowanie na X = x1, Y = y2
  # nie jest okreslone; granica gorna wspolna min(a21 - o21, o12) = 0
  poziomy <- list(c("x1", "x2"), c("y1", "y2"))
  o <- matrix(c(0.5, 0.2, 0, 0.3), 2, dimnames = poziomy)
  a <- matrix(c(0.7, 0.4, 0.3, 0.6), 2, dimnames = poziomy)
  w <- granice_zdarzenia(przygotuj_rozklady(o, a), "grupa", wynik = "y1",
                         zabieg = "x2", wynik_obserwowany = "y2",
                         zabieg_obserwowany = "x1", warunkowe = TRUE)
  p <- w$przedzialy
  expect_equal(c(p$dolna[1], p$gorna[1]), c(0, 0))
  expect_true(all(is.na(c(p$dolna[2], p$gorna[2]))))
  expect_equal(p$status, c("zdefiniowana", "zerowe_warunkowanie"))
})

test_that("zabieg obserwowany rowny ustawionemu daje wartosc punktowa", {
  # Zgodnosc: P(Y_x1 = y1, X = x1) = P(X = x1, Y = y1) = o11 = 0,3
  w <- granice_zdarzenia(kontrolne(), "zmiana_zabiegu", wynik = "y1",
                         zabieg = "x1", zabieg_obserwowany = "x1")
  expect_equal(c(w$przedzialy$dolna, w$przedzialy$gorna), c(0.3, 0.3))
  expect_equal(w$przedzialy$status, "identyfikacja_punktowa")
  g <- granice_zdarzenia(kontrolne(), "grupa", wynik = "y1", zabieg = "x1",
                         wynik_obserwowany = "y2", zabieg_obserwowany = "x1")
  expect_equal(c(g$przedzialy$dolna, g$przedzialy$gorna), c(0, 0))
})

test_that("rdzen zatrzymuje obliczenie przy danych bez sensu", {
  expect_error(
    granice_pns(przygotuj_rozklady(diag(1 / 3, 3), diag(3)), "y1", "y2"),
    "PNS w tym wariancie wymaga dwoch poziomow zabiegu"
  )
  expect_error(
    granice_zdarzenia(kontrolne(), "zastapienie", wynik = "y1",
                      zabieg = "x1", wynik_obserwowany = "y1"),
    "zachowanie"
  )
  # a11 = 0,2 < o11 = 0,3 narusza zgodnosc: L = 0,3 > U = 0,2
  poziomy <- list(c("x1", "x2"), c("y1", "y2"))
  o <- matrix(c(0.3, 0.1, 0.2, 0.4), 2, dimnames = poziomy)
  a <- matrix(c(0.2, 0.2, 0.8, 0.8), 2, dimnames = poziomy)
  expect_error(
    granice_zdarzenia(przygotuj_rozklady(o, a), "zachowanie", wynik = "y1",
                      zabieg = "x1"),
    "naruszaja warunek zgodnosci"
  )
  expect_error(
    przygotuj_rozklady(tablice_kontrolne$obserwacje, diag(3)),
    "Tablice musza miec zgodne poziomy i wymiary"
  )
})
