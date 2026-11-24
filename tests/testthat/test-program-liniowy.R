# Niezalezna kontrola programem liniowym po typach odpowiedzi
# (helper-lp.R). Wariant "ostre" musi dawac dokladnie granice programu
# liniowego, a wariant "artykul" przedzial zawierajacy te granice.

porownaj_z_lp <- function(m, n, powtorzenia) {
  for (r in seq_len(powtorzenia)) {
    tab <- losowe_tablice(m, n)
    dane <- przygotuj_rozklady(tab$obserwacje, tab$interwencje)
    lp <- lp_wszystkie(dane$obserwacje, dane$interwencje)
    ostre <- granice_wszystkie(dane)$przedzialy
    artykul <- granice_wszystkie(dane, wariant = "artykul")$przedzialy
    expect_equal(ostre$dolna, lp[, 1], tolerance = 1e-7)
    expect_equal(ostre$gorna, lp[, 2], tolerance = 1e-7)
    expect_true(all(artykul$dolna <= lp[, 1] + 1e-9))
    expect_true(all(artykul$gorna >= lp[, 2] - 1e-9))
  }
}

test_that("wariant ostry pokrywa sie z programem liniowym (2 x 2, 2 x 3)", {
  skip_if_not_installed("lpSolve")
  withr::local_seed(21)
  porownaj_z_lp(2, 2, 8)
  porownaj_z_lp(2, 3, 4)
})

test_that("wariant ostry pokrywa sie z programem liniowym (3 x 2, 3 x 3)", {
  skip_if_not_installed("lpSolve")
  withr::local_seed(22)
  porownaj_z_lp(3, 2, 3)
  porownaj_z_lp(3, 3, 2)
})

test_that("granice PNS dla dwoch poziomow zabiegu sa ostre", {
  skip_if_not_installed("lpSolve")
  withr::local_seed(23)
  for (n in 2:3) {
    tab <- losowe_tablice(2, n)
    dane <- przygotuj_rozklady(tab$obserwacje, tab$interwencje)
    for (j in 1:2) {
      p <- 3 - j
      for (i in seq_len(n)) {
        for (k in seq_len(n)) {
          w <- granice_pns(dane, wynik_po = i, wynik_bez = k, zabieg = j)
          lp <- lp_granice(dane$obserwacje, dane$interwencje,
                           function(x, t) t[j] == i && t[p] == k)
          expect_equal(c(w$przedzialy$dolna, w$przedzialy$gorna), lp,
                       tolerance = 1e-7)
        }
      }
    }
  }
})

test_that("program liniowy potwierdza kontrprzyklad do twierdzenia 4", {
  skip_if_not_installed("lpSolve")
  poziomy <- list(c("x1", "x2"), c("y1", "y2"))
  o <- matrix(c(0.3, 0.1, 0.2, 0.4), 2, dimnames = poziomy)
  a <- matrix(c(0.8, 0.2, 0.2, 0.8), 2, dimnames = poziomy)
  lp <- lp_granice(o, a, function(x, t) t[1] == 1 && t[x] == 1)
  expect_equal(lp, c(0.4, 0.4), tolerance = 1e-9)
})
