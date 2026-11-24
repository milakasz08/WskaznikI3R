#' Wskaznik I3* jednostek
#'
#' Funkcja sumuje wazone udzialy publikacji w klasach percentylowych
#' wedlug jednostek (czasopism, zespolow, instytucji). Dla jednostki
#' o licznosciach klas \eqn{n_1, \ldots, n_J} i wagach
#' \eqn{w_1, \ldots, w_J}
#' \deqn{I3^* = \sum_{j=1}^{J} w_j n_j, \qquad
#'       I3^* / N = \sum_{j=1}^{J} w_j \frac{n_j}{N},}
#' gdzie \eqn{N = \sum_j n_j} jest liczba publikacji jednostki. Licznosci
#' klas moga byc ulamkowe. Wynik surowy rosnie z wielkoscia dorobku; wynik
#' na publikacje opisuje sredni wklad jednej publikacji.
#'
#' Schemat z wagami \code{c(0, 1)} i granicami \code{c(0, 90, 100)} daje
#' liczbe publikacji w gornych 10\%, a wynik na publikacje jest ich udzialem.
#'
#' @param udzialy Obiekt klasy \code{udzialy_i3} zwrocony przez
#'   \code{\link{przypisz_klasy}} albo macierz udzialow (wiersze to
#'   publikacje, kolumny to klasy).
#' @param grupy Wektor przypisujacy publikacje do jednostek, o dlugosci
#'   rownej liczbie publikacji. Czynnik z poziomami bez publikacji daje
#'   jednostki puste. Wartosc \code{NULL} oznacza jedna jednostke.
#' @param wagi Wagi klas, po jednej dla kazdej klasy.
#'
#' @return Obiekt klasy \code{wynik_i3}, lista ze skladnikami:
#'   \describe{
#'     \item{wyniki}{ramka danych z kolumnami \code{jednostka}, \code{N},
#'       \code{I3}, \code{I3_na_publikacje}, \code{status}}
#'     \item{licznosci_klas}{macierz licznosci klas, wiersze to jednostki}
#'     \item{udzialy}{macierz udzialow publikacji}
#'     \item{odniesienie}{liczba publikacji zbioru odniesienia w kazdym
#'       przekroju oraz regula remisow}
#'     \item{parametry}{lista z wagami i granicami klas}
#'   }
#'
#' @examples
#' odn <- przygotuj_odniesienie(odniesienie_przyklad)
#' u <- przypisz_klasy(jednostki_przyklad, odn)
#' wynik <- oblicz_i3(u, jednostki_przyklad$jednostka)
#' wynik
#'
#' # Liczba i udzial publikacji w gornych 10\%
#' odn10 <- przygotuj_odniesienie(odniesienie_przyklad, progi = c(0, 90, 100))
#' u10 <- przypisz_klasy(jednostki_przyklad, odn10)
#' oblicz_i3(u10, jednostki_przyklad$jednostka, wagi = c(0, 1))
#'
#' @export
oblicz_i3 <- function(udzialy, grupy = NULL, wagi = c(1, 2, 10, 100)) {
  # TODO: kontrola wejscia zgodna z kontraktem bledow (SPEC.md):
  #   - macierz udzialow z elementami w [0, 1] i suma wiersza 1,
  #   - liczba wag rowna liczbie klas; komunikat
  #     "Liczba wag nie odpowiada liczbie klas",
  #   - wagi skonczone, nieujemne, przynajmniej jedna dodatnia,
  #   - `grupy` o dlugosci rownej liczbie publikacji, bez NA.
  #   Testy: tests/testthat/test-kontrakt-bledow.R (obecnie pomijane).
  if (inherits(udzialy, "udzialy_i3")) {
    obiekt <- udzialy
    macierz <- udzialy$udzialy
  } else {
    obiekt <- NULL
    macierz <- as.matrix(udzialy)
  }
  if (is.null(grupy)) {
    grupy <- rep("calosc", nrow(macierz))
  }
  if (!is.factor(grupy)) {
    grupy <- factor(grupy)
  }

  licznosci <- matrix(0, nrow = nlevels(grupy), ncol = ncol(macierz),
                      dimnames = list(levels(grupy), colnames(macierz)))
  if (nrow(macierz) > 0) {
    zsumowane <- rowsum(macierz, grupy, reorder = TRUE)
    licznosci[rownames(zsumowane), ] <- zsumowane
  }
  n <- rowSums(licznosci)
  i3 <- as.numeric(licznosci %*% wagi)
  pusta <- n == 0

  wyniki <- data.frame(
    jednostka = levels(grupy),
    N = as.numeric(n),
    I3 = i3,
    I3_na_publikacje = ifelse(pusta, NA_real_, i3 / n),
    status = ifelse(pusta, "pusta_jednostka", "zdefiniowany"),
    stringsAsFactors = FALSE
  )
  rownames(wyniki) <- NULL

  structure(
    list(
      wyniki = wyniki,
      licznosci_klas = licznosci,
      udzialy = macierz,
      odniesienie = list(
        liczebnosci = if (is.null(obiekt)) NULL else obiekt$liczebnosci_odniesienia,
        remisy = if (is.null(obiekt)) NA_character_ else obiekt$remisy
      ),
      parametry = list(
        wagi = wagi,
        progi = if (is.null(obiekt)) NULL else obiekt$progi
      )
    ),
    class = "wynik_i3"
  )
}
