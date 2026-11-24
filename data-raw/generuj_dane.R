# Zbior przykladowy pakietu.
#
# Skrypt nie wchodzi w sklad zbudowanego pakietu; zapisuje zbior do katalogu
# data/. Uruchomienie z katalogu glownego pakietu:
#     Rscript -e "devtools::load_all(); source('data-raw/generuj_dane.R')"
#
# Tablice kontrolne pochodza z przykladu liczbowego opisu tematu: dwa poziomy
# zabiegu (x1, x2), dwa poziomy wyniku (y1 porazka, y2 sukces). Granice
# znane z rachunku recznego: PN [0,2; 0,3] wspolnie i [0,5; 0,75] warunkowo,
# PS [0,2; 0,3] i [2/3; 1], PNS [0,4; 0,6].

poziomy <- list(c("x1", "x2"), c("y1", "y2"))
tablice_kontrolne <- list(
  obserwacje = matrix(c(0.3, 0.1, 0.2, 0.4), nrow = 2, dimnames = poziomy),
  interwencje = matrix(c(0.6, 0.2, 0.4, 0.8), nrow = 2, dimnames = poziomy)
)

stopifnot(
  isTRUE(all.equal(sum(tablice_kontrolne$obserwacje), 1)),
  isTRUE(all.equal(unname(rowSums(tablice_kontrolne$interwencje)), c(1, 1))),
  all(tablice_kontrolne$obserwacje <= tablice_kontrolne$interwencje)
)

dir.create("data", showWarnings = FALSE)
save(tablice_kontrolne, file = "data/tablice_kontrolne.rda",
     compress = "xz", version = 3)

# TODO: osobny skrypt studium przypadku na zbiorze datasets::ChickWeight:
#   diety 1 i 2, wynik weight > 200 w dniu 21, liczba zwierzat bez pomiaru
#   koncowego, dwa scenariusze brakujacego wyniku (wszystkie braki jako
#   porazki albo jako sukcesy), tablice obserwacyjna i interwencyjna
#   z oszacowanych czestosci oraz opis zalozenia losowego przydzialu.
#   Granice z czestosci probki nie maja poziomu ufnosci.
