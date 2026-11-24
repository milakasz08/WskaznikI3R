#' Przykladowy zbior odniesienia
#'
#' Liczby cytowan 5000 publikacji wygenerowane funkcja
#' \code{\link{generuj_cytowania}} w skrypcie \code{data-raw/generuj_dane.R}.
#' Rozklad jest mieszanina zer strukturalnych i rozkladu Poissona o
#' intensywnosci log-normalnej.
#'
#' @format Ramka danych o 5000 wierszach i 2 zmiennych:
#' \describe{
#'   \item{id}{identyfikator publikacji}
#'   \item{cytowania}{liczba cytowan, liczba calkowita nieujemna}
#' }
#'
#' @source Dane symulowane, \code{data-raw/generuj_dane.R}.
#'
#' @examples
#' summary(odniesienie_przyklad$cytowania)
#' mean(odniesienie_przyklad$cytowania == 0)
"odniesienie_przyklad"


#' Przykladowe publikacje trzech jednostek
#'
#' Publikacje jednostek A (100), B (1000) i C (100) wygenerowane funkcja
#' \code{\link{generuj_cytowania}} w skrypcie \code{data-raw/generuj_dane.R}.
#' Jednostki A i B pochodza z rozkladu zbioru odniesienia; w jednostce C
#' piec publikacji ma intensywnosc cytowan pomnozona przez 50.
#'
#' @format Ramka danych o 1200 wierszach i 4 zmiennych:
#' \describe{
#'   \item{id}{identyfikator publikacji}
#'   \item{jednostka}{oznaczenie jednostki: A, B albo C}
#'   \item{cytowania}{liczba cytowan, liczba calkowita nieujemna}
#'   \item{wzmocniona}{\code{TRUE} dla publikacji o intensywnosci
#'     pomnozonej przez 50}
#' }
#'
#' @source Dane symulowane, \code{data-raw/generuj_dane.R}.
#'
#' @examples
#' tapply(jednostki_przyklad$cytowania, jednostki_przyklad$jednostka, mean)
"jednostki_przyklad"
