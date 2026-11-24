#' CausalBoundsR: granice prawdopodobienstw przyczynowosci
#'
#' Pakiet wyznacza granice prawdopodobienstw przyczynowosci z jednym
#' zdarzeniem hipotetycznym dla zabiegu \eqn{X} o poziomach
#' \eqn{x_1, \ldots, x_m} i wyniku \eqn{Y} o poziomach
#' \eqn{y_1, \ldots, y_n}. Dane wejsciowe to rozklad obserwacyjny
#' \eqn{o_{ji} = P(X = x_j, Y = y_i)} oraz rozklady interwencyjne
#' \eqn{a_{ji} = P(Y_{x_j} = y_i)}.
#'
#' Pakiet udostepnia dwa warianty wzorow. Wariant \code{"artykul"} odtwarza
#' jawne granice z twierdzen 4--8 artykulu Li i Pearla (2024). Wariant
#' \code{"ostre"} stosuje granice w postaci zamknietej Shu, Wanga i Li (2026),
#' ktorych ostrosc wykazali Shu i Li (2026). Oba warianty daja te same
#' granice gorne; roznia sie dolnymi granicami zachowania i zastapienia
#' wyniku.
#'
#' Sciezka analizy:
#' \itemize{
#'   \item \code{\link{przygotuj_rozklady}} sprawdza i opisuje tablice,
#'   \item \code{\link{granice_zdarzenia}} i \code{\link{granice_wszystkie}}
#'     wyznaczaja granice zdarzen z jednym wynikiem hipotetycznym,
#'   \item \code{\link{granice_pns}} wyznacza granice prawdopodobienstwa
#'     koniecznosci i wystarczalnosci dla dwoch poziomow zabiegu,
#'   \item \code{\link{generuj_model}} tworzy model o znanej wartosci
#'     prawdziwej kazdego zdarzenia.
#' }
#'
#' @references
#' Li, A., Pearl, J. (2024). Probabilities of causation with nonbinary
#' treatment and effect. Proceedings of the AAAI Conference on Artificial
#' Intelligence, 38(18), 20465--20472. \doi{10.1609/aaai.v38i18.30030}
#'
#' Shu, X., Wang, S., Li, A. (2026). Identification of probabilities of
#' causation: from recursive to closed-form bounds. Proceedings of the
#' Thirty-Fifth International Joint Conference on Artificial Intelligence,
#' 4063--4070. \doi{10.24963/ijcai.2026/452}
#'
#' Shu, X., Li, A. (2026). Unified sharp bounds on probabilities of
#' causation. arXiv:2610.27398.
#'
#' @keywords internal
"_PACKAGE"
