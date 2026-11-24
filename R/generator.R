#' Generowanie liczb cytowan o znanym rozkladzie
#'
#' Procedura tworzy zbior odniesienia i trzy jednostki o znanym
#' mechanizmie powstawania cytowan. Liczba cytowan publikacji jest mieszanina
#' zer strukturalnych i rozkladu Poissona o losowej intensywnosci:
#' \deqn{Z \sim \mathrm{Bernoulli}(0{,}3), \quad
#'       \log \lambda \sim N(1, 1{,}5^2), \quad
#'       C = 0 \textrm{ dla } Z = 1, \quad
#'       C \mid \lambda \sim \mathrm{Poisson}(\lambda) \textrm{ dla } Z = 0.}
#' Rozklad Poissona przy malej intensywnosci dodaje wlasne zera, wiec udzial
#' publikacji niecytowanych w zbiorze odniesienia wynosi okolo 45\%.
#'
#' Jednostki A i B pochodza z tego samego rozkladu co odniesienie i roznia
#' sie tylko liczba publikacji. Jednostka C ma tyle publikacji co A;
#' pierwszych 5\% publikacji jednostki C (identyfikatory od \code{C001})
#' ma intensywnosc pomnozona przez 50 i nie podlega zerom strukturalnym.
#' Pozostale publikacje jednostki C pochodza z rozkladu odniesienia.
#'
#' Prawda konstrukcyjna dotyczy udzialow w klasach i sum wazonych przy
#' ustalonym odniesieniu; procedura nie wyznacza jednego poprawnego
#' uporzadkowania jednostek, poniewaz wynik surowy i wynik na publikacje
#' odpowiadaja na rozne pytania.
#'
#' Kolejnosc losowania jest ustalona: odniesienie, jednostka A, B, C;
#' w kazdej grupie najpierw wskazniki zer strukturalnych, potem logarytmy
#' intensywnosci, na koncu liczby cytowan.
#'
#' @param n Liczba publikacji jednostki B; jednostki A i C licza po
#'   \code{n / 10} publikacji.
#' @param ziarno Ziarno generatora liczb losowych. Stan generatora w sesji
#'   uzytkownika zostaje przywrocony po zakonczeniu losowania.
#' @param n_odniesienia Liczba publikacji zbioru odniesienia.
#' @param wielokrotnosc Liczby cytowan sa zaokraglane do wielokrotnosci tej
#'   wartosci; wartosc 5 zwieksza liczbe remisow.
#'
#' @return Lista ze skladnikami:
#'   \describe{
#'     \item{odniesienie}{ramka danych z kolumnami \code{id},
#'       \code{cytowania}}
#'     \item{publikacje}{ramka danych z kolumnami \code{id},
#'       \code{jednostka}, \code{cytowania}, \code{wzmocniona}}
#'     \item{parametry}{lista argumentow i parametrow rozkladu}
#'   }
#'
#' @examples
#' dane <- generuj_cytowania(n = 200, n_odniesienia = 1000)
#' mean(dane$odniesienie$cytowania == 0)
#' table(dane$publikacje$jednostka)
#'
#' @export
generuj_cytowania <- function(n = 1000, ziarno = 202709, n_odniesienia = 5000,
                              wielokrotnosc = 1) {
  # TODO: kontrola argumentow (dodatnie liczby calkowite, `n` podzielne
  #   przez 10, `wielokrotnosc` dodatnia liczba calkowita).
  udzial_zer <- 0.3
  srednia_log <- 1
  odchylenie_log <- 1.5
  mnoznik <- 50
  n_male <- as.integer(round(n / 10))
  n_wzmocnione <- as.integer(round(0.05 * n_male))

  losuj <- function(liczba, wzmocnione = 0L) {
    zero <- stats::runif(liczba) < udzial_zer
    lambda <- exp(stats::rnorm(liczba, srednia_log, odchylenie_log))
    if (wzmocnione > 0L) {
      indeks <- seq_len(wzmocnione)
      zero[indeks] <- FALSE
      lambda[indeks] <- lambda[indeks] * mnoznik
    }
    ifelse(zero, 0L, stats::rpois(liczba, lambda))
  }

  wylosowane <- withr::with_seed(ziarno, list(
    odniesienie = losuj(n_odniesienia),
    A = losuj(n_male),
    B = losuj(n),
    C = losuj(n_male, n_wzmocnione)
  ))
  zaokraglij <- function(x) as.integer(wielokrotnosc * round(x / wielokrotnosc))
  wylosowane <- lapply(wylosowane, zaokraglij)

  odniesienie <- data.frame(
    id = sprintf("R%05d", seq_len(n_odniesienia)),
    cytowania = wylosowane$odniesienie,
    stringsAsFactors = FALSE
  )
  publikacje <- data.frame(
    id = c(sprintf("A%03d", seq_len(n_male)), sprintf("B%04d", seq_len(n)),
           sprintf("C%03d", seq_len(n_male))),
    jednostka = rep(c("A", "B", "C"), times = c(n_male, n, n_male)),
    cytowania = c(wylosowane$A, wylosowane$B, wylosowane$C),
    wzmocniona = rep(c(FALSE, FALSE, TRUE, FALSE),
                     times = c(n_male, n, n_wzmocnione, n_male - n_wzmocnione)),
    stringsAsFactors = FALSE
  )

  list(
    odniesienie = odniesienie,
    publikacje = publikacje,
    parametry = list(n = n, n_odniesienia = n_odniesienia, ziarno = ziarno,
                     wielokrotnosc = wielokrotnosc, udzial_zer = udzial_zer,
                     srednia_log = srednia_log,
                     odchylenie_log = odchylenie_log, mnoznik = mnoznik,
                     n_wzmocnione = n_wzmocnione)
  )
}
