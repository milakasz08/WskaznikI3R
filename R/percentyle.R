#' Przypisanie publikacji do klas percentylowych
#'
#' Funkcja wyznacza udzial kazdej publikacji w kazdej klasie percentylowej
#' zbioru odniesienia. Wiersz macierzy udzialow sumuje sie do jednosci.
#'
#' \strong{Wariant ulamkowy} (domyslny, wedlug Waltmana i Schreibera, 2013).
#' Publikacja z liczba cytowan \eqn{c} wystepujaca w zbiorze odniesienia
#' nalezy do bloku remisowego o przedziale
#' \eqn{[P_d, P_g] = [100 b / M, 100 (b + g) / M]}, gdzie \eqn{b} jest
#' liczba publikacji odniesienia o mniejszej liczbie cytowan, a \eqn{g}
#' wielkoscia bloku. Udzial w klasie \eqn{[\pi_{j-1}, \pi_j]} jest dlugoscia
#' przeciecia przedzialu bloku z klasa podzielona przez dlugosc bloku.
#' Publikacja z liczba cytowan nieobecna w odniesieniu otrzymuje pozycje
#' \eqn{P = 100 b / M} i caly udzial w klasie zawierajacej \eqn{P}; klasy sa
#' domkniete z lewej strony, a ostatnia takze z prawej.
#'
#' \strong{Wariant dyskretny} (regula progowa). Dla dolnej granicy klasy
#' \eqn{\pi_j} prog \eqn{t_j} jest liczba cytowan publikacji na pozycji
#' \eqn{\lceil (100 - \pi_j) M / 100 \rceil} w porzadku malejacym.
#' Publikacja trafia do najwyzszej klasy \eqn{j}, dla ktorej
#' \eqn{c \ge t_j}. Cala grupa remisowa na progu przechodzi do klasy
#' wyzszej, wiec liczebnosci klas moga odbiegac od nominalnych udzialow.
#'
#' @param publikacje Ramka danych z kolumnami \code{id} i \code{cytowania}
#'   oraz opcjonalnie \code{przekroj}, zgodnym z przekrojami odniesienia.
#' @param odniesienie Obiekt klasy \code{odniesienie_i3} zwrocony przez
#'   \code{\link{przygotuj_odniesienie}}.
#' @param remisy Regula przypisania: \code{"ulamkowe"} albo
#'   \code{"dyskretne"}.
#'
#' @return Obiekt klasy \code{udzialy_i3}, lista ze skladnikami:
#'   \describe{
#'     \item{udzialy}{macierz udzialow, wiersze to publikacje, kolumny to
#'       klasy}
#'     \item{slad}{ramka danych z kolumnami \code{id}, \code{przekroj},
#'       \code{cytowania}, \code{ponizej}, \code{rowne}, \code{M},
#'       \code{dolny}, \code{gorny}: slad dopasowania publikacji do
#'       odniesienia}
#'     \item{progi}{granice klas}
#'     \item{remisy}{zastosowana regula}
#'     \item{liczebnosci_odniesienia}{liczba publikacji \eqn{M} w kazdym
#'       przekroju odniesienia}
#'   }
#'
#' @examples
#' odn <- przygotuj_odniesienie(odniesienie_przyklad)
#' cele <- jednostki_przyklad[1:5, ]
#' u <- przypisz_klasy(cele, odn)
#' u$udzialy
#' przypisz_klasy(cele, odn, remisy = "dyskretne")$udzialy
#'
#' @export
przypisz_klasy <- function(publikacje, odniesienie,
                           remisy = c("ulamkowe", "dyskretne")) {
  remisy <- match.arg(remisy)
  # TODO: kontrola wejscia zgodna z kontraktem bledow (SPEC.md):
  #   - `odniesienie` klasy `odniesienie_i3`,
  #   - kolumny `id` (unikatowe) i `cytowania` (nieujemne, calkowite, bez
  #     NA); komunikat "Cytowania musza byc nieujemnymi liczbami
  #     calkowitymi",
  #   - kazdy przekroj publikacji obecny w odniesieniu; komunikat
  #     "Brak zbioru odniesienia dla publikacji".
  #   Testy: tests/testthat/test-kontrakt-bledow.R (obecnie pomijane).
  if (!is.data.frame(publikacje) || !"cytowania" %in% names(publikacje)) {
    stop("Argument `publikacje` musi byc ramka danych z kolumna `cytowania`.",
         call. = FALSE)
  }
  progi <- odniesienie$progi
  liczba_klas <- length(progi) - 1L
  c_cel <- as.numeric(publikacje$cytowania)
  przekroj <- odczytaj_przekroj(publikacje)
  id <- if ("id" %in% names(publikacje)) {
    as.character(publikacje$id)
  } else {
    as.character(seq_len(nrow(publikacje)))
  }

  ponizej <- rowne_lub_ponizej <- m <- numeric(length(c_cel))
  for (p in unique(przekroj)) {
    w <- przekroj == p
    x <- odniesienie$cytowania[[p]]
    ponizej[w] <- findInterval(c_cel[w], x, left.open = TRUE)
    rowne_lub_ponizej[w] <- findInterval(c_cel[w], x)
    m[w] <- length(x)
  }
  rowne <- rowne_lub_ponizej - ponizej
  dolny <- 100 * ponizej / m
  gorny <- 100 * rowne_lub_ponizej / m

  if (remisy == "ulamkowe") {
    udzialy <- udzialy_ulamkowe(dolny, gorny, progi)
  } else {
    udzialy <- udzialy_dyskretne(c_cel, przekroj, odniesienie)
  }
  dimnames(udzialy) <- list(id, etykiety_klas(progi))

  slad <- data.frame(
    id = id, przekroj = przekroj, cytowania = c_cel,
    ponizej = ponizej, rowne = rowne, M = m,
    dolny = dolny, gorny = gorny,
    stringsAsFactors = FALSE
  )

  structure(
    list(
      udzialy = udzialy,
      slad = slad,
      progi = progi,
      remisy = remisy,
      liczebnosci_odniesienia = odniesienie$liczebnosci
    ),
    class = "udzialy_i3"
  )
}


# Udzialy ulamkowe: dlugosc przeciecia przedzialu bloku [dolny, gorny]
# z kazda klasa, podzielona przez dlugosc bloku. Dla wartosci nieobecnej
# w odniesieniu (dolny == gorny) cala jednostka trafia do klasy zawierajacej
# pozycje, z klasami domknietymi z lewej i ostatnia domknieta z prawej.
udzialy_ulamkowe <- function(dolny, gorny, progi) {
  liczba_klas <- length(progi) - 1L
  lewe <- progi[-length(progi)]
  prawe <- progi[-1]
  dlugosc <- gorny - dolny
  przeciecie <- pmax(outer(gorny, prawe, pmin) - outer(dolny, lewe, pmax), 0)
  udzialy <- przeciecie / dlugosc

  punkt <- dlugosc == 0
  if (any(punkt)) {
    klasa <- findInterval(dolny[punkt], progi, rightmost.closed = TRUE)
    udzialy[punkt, ] <- 0
    udzialy[cbind(which(punkt), klasa)] <- 1
  }
  udzialy
}


# Udzialy dyskretne: publikacja trafia do najwyzszej klasy, ktorej prog
# cytowan osiaga. Prog klasy dolnej jest rowny minus nieskonczonosci.
udzialy_dyskretne <- function(c_cel, przekroj, odniesienie) {
  liczba_klas <- length(odniesienie$progi) - 1L
  klasa <- integer(length(c_cel))
  for (p in unique(przekroj)) {
    w <- przekroj == p
    progi_c <- c(-Inf, odniesienie$progi_dyskretne[p, ])
    klasa[w] <- vapply(c_cel[w], function(x) max(which(x >= progi_c)),
                       integer(1))
  }
  udzialy <- matrix(0, nrow = length(c_cel), ncol = liczba_klas)
  udzialy[cbind(seq_along(c_cel), klasa)] <- 1
  udzialy
}
