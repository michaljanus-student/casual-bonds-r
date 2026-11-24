# Niezalezna kontrola granic: program liniowy po typach odpowiedzi.
#
# Zmienna q(x, t) jest masa jednostek, ktore otrzymaly zabieg x i maja typ
# odpowiedzi t = (Y_x1, ..., Y_xm). Ograniczenia: o[x, y] = suma q(x, t)
# po t z t_x = y (zgodnosc Y = Y_X) oraz a[j, y] = suma q(x, t) po x i t
# z t_j = y. Najmniejsza i najwieksza masa zdarzenia przy tych
# ograniczeniach sa granicami ostrymi.

typy_odpowiedzi <- function(m, n) {
  as.matrix(expand.grid(rep(list(seq_len(n)), m)))
}

lp_granice <- function(o, a, zdarzenie) {
  m <- nrow(o)
  n <- ncol(o)
  typy <- typy_odpowiedzi(m, n)
  liczba_typow <- nrow(typy)
  ograniczenia <- matrix(0, 2 * m * n, m * liczba_typow)
  cel <- numeric(m * liczba_typow)
  for (x in seq_len(m)) {
    for (t in seq_len(liczba_typow)) {
      kolumna <- (x - 1) * liczba_typow + t
      ograniczenia[(x - 1) * n + typy[t, x], kolumna] <- 1
      for (j in seq_len(m)) {
        ograniczenia[m * n + (j - 1) * n + typy[t, j], kolumna] <- 1
      }
      cel[kolumna] <- as.numeric(zdarzenie(x, typy[t, ]))
    }
  }
  prawe <- c(t(o), t(a))
  kierunki <- rep("=", length(prawe))
  dol <- lpSolve::lp("min", cel, ograniczenia, kierunki, prawe)
  gora <- lpSolve::lp("max", cel, ograniczenia, kierunki, prawe)
  stopifnot(dol$status == 0, gora$status == 0)
  c(dol$objval, gora$objval)
}

# Zdarzenia w kolejnosci wierszy granice_wszystkie()
lp_wszystkie <- function(o, a) {
  m <- nrow(o)
  n <- ncol(o)
  wyniki <- list()
  dodaj <- function(f) wyniki[[length(wyniki) + 1L]] <<- lp_granice(o, a, f)
  for (j in seq_len(m)) {
    for (i in seq_len(n)) {
      dodaj(function(x, t) t[j] == i && t[x] == i)
      for (k in setdiff(seq_len(n), i)) {
        dodaj(function(x, t) t[j] == i && t[x] == k)
      }
      for (p in setdiff(seq_len(m), j)) {
        dodaj(function(x, t) t[j] == i && x == p)
        for (k in seq_len(n)) {
          dodaj(function(x, t) t[j] == i && x == p && t[p] == k)
        }
      }
    }
  }
  do.call(rbind, wyniki)
}

# Losowe tablice spelniajace warunek zgodnosci o <= a <= o + 1 - u:
# o z rozkladu Dirichleta na komorkach, a_j = o_j + (1 - u_j) * Dirichlet.
losowe_tablice <- function(m, n, alfa = 0.4) {
  dirichlet <- function(d) {
    g <- stats::rgamma(d, alfa)
    g / sum(g)
  }
  poziomy <- list(paste0("x", seq_len(m)), paste0("y", seq_len(n)))
  o <- matrix(dirichlet(m * n), m, n, dimnames = poziomy)
  u <- rowSums(o)
  a <- o + t(vapply(seq_len(m), function(j) (1 - u[j]) * dirichlet(n),
                    numeric(n)))
  dimnames(a) <- poziomy
  list(obserwacje = o, interwencje = a)
}
