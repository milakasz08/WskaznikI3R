#' Przygotowanie zbioru odniesienia
#'
#' Funkcja porzadkuje zbior odniesienia i dzieli go na bloki publikacji
#' o jednakowej liczbie cytowan. Blok \eqn{g} publikacji, ponizej ktorego
#' w zbiorze o \eqn{M} publikacjach lezy \eqn{b} publikacji o mniejszej
#' liczbie cytowan, zajmuje przedzial percentylowy
#' \eqn{[100 b / M, 100 (b + g) / M]}. Przedzialy blokow pokrywaja odcinek
#' od 0 do 100 bez przerw i bez nakladania sie.
#'
#' Granice klas \code{progi} dziela odcinek od 0 do 100 na rozlaczne klasy.
#' Domyslny schemat I3* stosuje klasy dolnych 50\%, kolejnych 40\%, kolejnych
#' 9\% i gornego 1\%. Funkcja zapisuje takze progi cytowan wariantu
#' dyskretnego: dla granicy \eqn{\pi_j} prog jest liczba cytowan publikacji
#' na pozycji \eqn{\lceil (100 - \pi_j) M / 100 \rceil} w porzadku malejacym.
#'
#' Zbior odniesienia moze sie skladac z kilku przekrojow (np. dziedzina
#' i rocznik), wskazanych kolumna \code{przekroj}. Bloki i progi sa wtedy
#' wyznaczane osobno w kazdym przekroju.
#'
#' @param publikacje Ramka danych z kolumna \code{cytowania} i opcjonalnie
#'   \code{id} oraz \code{przekroj}.
#' @param progi Rosnacy wektor granic klas percentylowych od 0 do 100.
#'
#' @return Obiekt klasy \code{odniesienie_i3}, lista ze skladnikami:
#'   \describe{
#'     \item{bloki}{ramka danych z kolumnami \code{przekroj},
#'       \code{cytowania}, \code{liczba} (wielkosc bloku \eqn{g}),
#'       \code{ponizej} (\eqn{b}), \code{M}, \code{dolny}, \code{gorny}
#'       (granice przedzialu percentylowego bloku)}
#'     \item{cytowania}{lista posortowanych wektorow cytowan, po jednym na
#'       przekroj}
#'     \item{progi_dyskretne}{macierz progow cytowan wariantu dyskretnego,
#'       wiersze to przekroje, kolumny to dolne granice klas}
#'     \item{progi}{granice klas}
#'     \item{liczebnosci}{liczba publikacji \eqn{M} w kazdym przekroju}
#'   }
#'
#' @examples
#' odn <- przygotuj_odniesienie(data.frame(cytowania = c(0, 0, 1, 3, 3, 3, 8,
#'                                                         12, 20, 41)))
#' odn
#' odn$bloki
#'
#' @export
przygotuj_odniesienie <- function(publikacje, progi = c(0, 50, 90, 99, 100)) {
  # TODO: kontrola wejscia zgodna z kontraktem bledow (SPEC.md):
  #   - `publikacje` jako ramka danych z kolumna `cytowania`,
  #   - cytowania skonczone, calkowite, nieujemne, bez NA; komunikat
  #     "Cytowania musza byc nieujemnymi liczbami calkowitymi",
  #   - identyfikatory `id` (jesli podane) unikatowe,
  #   - `progi` scisle rosnace od 0 do 100; komunikat
  #     "Progi musza rosnac od 0 do 100",
  #   - kazdy przekroj niepusty; komunikat
  #     "Zbior odniesienia nie moze byc pusty".
  #   Testy: tests/testthat/test-kontrakt-bledow.R (obecnie pomijane).
  if (!is.data.frame(publikacje) || !"cytowania" %in% names(publikacje)) {
    stop("Argument `publikacje` musi byc ramka danych z kolumna `cytowania`.",
         call. = FALSE)
  }
  przekroj <- odczytaj_przekroj(publikacje)
  cytowania <- split(as.numeric(publikacje$cytowania), przekroj)
  cytowania <- lapply(cytowania, sort)

  bloki <- lapply(names(cytowania), function(p) {
    x <- cytowania[[p]]
    m <- length(x)
    dlugosci <- rle(x)
    ponizej <- c(0, cumsum(dlugosci$lengths))[seq_along(dlugosci$lengths)]
    data.frame(
      przekroj = p,
      cytowania = dlugosci$values,
      liczba = dlugosci$lengths,
      ponizej = ponizej,
      M = m,
      dolny = 100 * ponizej / m,
      gorny = 100 * (ponizej + dlugosci$lengths) / m,
      stringsAsFactors = FALSE
    )
  })
  bloki <- do.call(rbind, bloki)
  rownames(bloki) <- NULL

  granice <- progi[-c(1, length(progi))]
  progi_dyskretne <- t(vapply(cytowania, function(x) {
    malejaco <- sort(x, decreasing = TRUE)
    pozycje <- pmax(1, ceiling((100 - granice) / 100 * length(x)))
    malejaco[pozycje]
  }, numeric(length(granice))))
  if (length(granice) == 1L) {
    progi_dyskretne <- t(progi_dyskretne)
  }
  dimnames(progi_dyskretne) <- list(names(cytowania), format(granice))

  structure(
    list(
      bloki = bloki,
      cytowania = cytowania,
      progi_dyskretne = progi_dyskretne,
      progi = progi,
      liczebnosci = lengths(cytowania)
    ),
    class = "odniesienie_i3"
  )
}


# Przekroj odniesienia: kolumna `przekroj` albo jeden przekroj "calosc".
odczytaj_przekroj <- function(publikacje) {
  if ("przekroj" %in% names(publikacje)) {
    as.character(publikacje$przekroj)
  } else {
    rep("calosc", nrow(publikacje))
  }
}


# Etykiety klas w postaci przedzialow, np. "[50,90)"; ostatnia klasa
# domknieta z prawej strony.
etykiety_klas <- function(progi) {
  j <- length(progi) - 1L
  prawy <- c(rep(")", j - 1L), "]")
  paste0("[", format(progi[-length(progi)], trim = TRUE), ",",
         format(progi[-1], trim = TRUE), prawy)
}
