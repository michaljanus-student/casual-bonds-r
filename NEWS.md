# CausalBoundsR 0.0.1

* Przygotowanie tablic obserwacyjnej i interwencyjnej z dopasowaniem
  poziomów po nazwach i diagnostyką warunku zgodności
  (`przygotuj_rozklady()`).
* Granice czterech rodzajów zdarzeń z jednym wynikiem hipotetycznym, wspólne
  i warunkowe, w wariancie Li i Pearla (2024) oraz w wariancie ostrym Shu,
  Wanga i Li (2026) (`granice_zdarzenia()`, `granice_wszystkie()`).
* Granice prawdopodobieństwa konieczności i wystarczalności dla dwóch
  poziomów zabiegu (`granice_pns()`).
* Procedura generowania modeli przyczynowych o znanej wartości prawdziwej
  (`generuj_model()`) i zbiór `tablice_kontrolne`.
* Metody `print()`, `summary()` i `plot()` klasy `granice_przyczynowe`.
