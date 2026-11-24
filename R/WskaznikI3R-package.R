#' WskaznikI3R: nieparametryczny wskaznik oddzialywania I3*
#'
#' Pakiet wyznacza wskaznik Integrated Impact Indicator w wariancie I3*
#' (Leydesdorff, Bornmann i Adams, 2019). Kazda publikacja zostaje przypisana
#' do klas percentylowych ustalonego zbioru odniesienia, a wskaznik jest suma
#' wag klas po publikacjach jednostki. Pakiet zwraca wynik surowy, ktory
#' rosnie z wielkoscia dorobku, oraz wynik na jedna publikacje.
#'
#' Publikacje o jednakowej liczbie cytowan tworza w zbiorze odniesienia blok
#' remisowy. Wariant domyslny dzieli blok miedzy klasy ulamkowo (Waltman
#' i Schreiber, 2013); wariant dyskretny stosuje progi cytowan wyznaczone
#' z uporzadkowanego zbioru odniesienia.
#'
#' Sciezka analizy:
#' \itemize{
#'   \item \code{\link{przygotuj_odniesienie}} buduje bloki remisowe zbioru
#'     odniesienia,
#'   \item \code{\link{przypisz_klasy}} wyznacza udzialy publikacji w klasach,
#'   \item \code{\link{oblicz_i3}} sumuje wazone udzialy wedlug jednostek,
#'   \item \code{\link{generuj_cytowania}} tworzy dane o znanej strukturze.
#' }
#'
#' @references Leydesdorff, L., Bornmann, L., Adams, J. (2019). The
#'   Integrated Impact Indicator revisited (I3*): a non-parametric alternative
#'   to the journal impact factor. Scientometrics, 119, 1669--1694.
#'   \doi{10.1007/s11192-019-03099-8}
#'
#'   Waltman, L., Schreiber, M. (2013). On the calculation of percentile-based
#'   bibliometric indicators. Journal of the American Society for Information
#'   Science and Technology, 64(2), 372--379. \doi{10.1002/asi.22775}
#'
#' @keywords internal
"_PACKAGE"
