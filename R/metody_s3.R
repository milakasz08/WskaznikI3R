#' Metody dla wyniku I3*
#'
#' Wypisanie, podsumowanie i wykres obiektu klasy \code{wynik_i3}.
#' Podsumowanie zestawia wyniki jednostek z licznosciami klas i wkladami
#' klas do wyniku surowego. Wykres pokazuje wklad kazdej klasy do wyniku
#' jednostki, w skali surowej albo na jedna publikacje.
#'
#' @param x,object Obiekt klasy \code{wynik_i3} albo
#'   \code{summary.wynik_i3}.
#' @param cyfry Liczba cyfr znaczacych w wydruku.
#' @param skala Skala wykresu: \code{"surowa"} (wklad klasy do \eqn{I3^*})
#'   albo \code{"na_publikacje"} (wklad klasy do \eqn{I3^* / N}).
#' @param ... Argumenty przekazywane dalej; w \code{plot()} do
#'   \code{\link[graphics]{barplot}}.
#'
#' @return \code{print()} niewidocznie zwraca \code{x}. \code{summary()}
#'   zwraca obiekt klasy \code{summary.wynik_i3} z wynikami, licznosciami
#'   klas i wkladami klas. \code{plot()} niewidocznie zwraca macierz wkladow
#'   przedstawiona na wykresie.
#'
#' @examples
#' odn <- przygotuj_odniesienie(odniesienie_przyklad)
#' u <- przypisz_klasy(jednostki_przyklad, odn)
#' wynik <- oblicz_i3(u, jednostki_przyklad$jednostka)
#' print(wynik)
#' summary(wynik)
#' plot(wynik, skala = "na_publikacje")
#'
#' @name metody_wynik_i3
NULL

#' @rdname metody_wynik_i3
#' @export
print.wynik_i3 <- function(x, cyfry = 4, ...) {
  cat("Wskaznik I3*\n")
  cat(sprintf("  wagi klas: %s\n", paste(format(x$parametry$wagi, trim = TRUE),
                                         collapse = ", ")))
  if (!is.na(x$odniesienie$remisy)) {
    cat(sprintf("  regula remisow: %s\n", x$odniesienie$remisy))
  }
  cat("\n")
  wyniki <- x$wyniki
  wyniki$N <- signif(wyniki$N, cyfry + 2)
  wyniki$I3 <- signif(wyniki$I3, cyfry + 2)
  wyniki$I3_na_publikacje <- signif(wyniki$I3_na_publikacje, cyfry)
  print(wyniki, row.names = FALSE)
  invisible(x)
}

#' @rdname metody_wynik_i3
#' @export
summary.wynik_i3 <- function(object, ...) {
  wklady <- sweep(object$licznosci_klas, 2L, object$parametry$wagi, "*")
  structure(
    list(
      wyniki = object$wyniki,
      licznosci_klas = object$licznosci_klas,
      wklady_klas = wklady,
      parametry = object$parametry,
      odniesienie = object$odniesienie
    ),
    class = "summary.wynik_i3"
  )
}

#' @rdname metody_wynik_i3
#' @export
print.summary.wynik_i3 <- function(x, cyfry = 4, ...) {
  cat("Wskaznik I3*: wyniki jednostek\n\n")
  wyniki <- x$wyniki
  wyniki$I3_na_publikacje <- signif(wyniki$I3_na_publikacje, cyfry)
  print(wyniki, row.names = FALSE)
  cat("\nLicznosci klas (ulamkowe przy regule ulamkowej)\n")
  print(round(x$licznosci_klas, 3))
  cat("\nWklady klas do wyniku surowego (licznosc razy waga)\n")
  print(round(x$wklady_klas, 3))
  if (!is.null(x$odniesienie$liczebnosci)) {
    cat(sprintf("\nZbior odniesienia: %s publikacji\n",
                paste(sprintf("%s = %d", names(x$odniesienie$liczebnosci),
                              x$odniesienie$liczebnosci), collapse = ", ")))
  }
  invisible(x)
}

#' @rdname metody_wynik_i3
#' @export
plot.wynik_i3 <- function(x, skala = c("surowa", "na_publikacje"), ...) {
  skala <- match.arg(skala)
  wklady <- sweep(x$licznosci_klas, 2L, x$parametry$wagi, "*")
  if (skala == "na_publikacje") {
    wklady <- wklady / pmax(rowSums(x$licznosci_klas), .Machine$double.eps)
  }
  kolory <- grDevices::hcl.colors(ncol(wklady), "Blues 3", rev = TRUE)
  stara <- graphics::par(mar = c(4.5, 4.5, 3, 8), xpd = NA)
  on.exit(graphics::par(stara), add = TRUE)
  graphics::barplot(
    t(wklady), col = kolory, border = NA, las = 1,
    xlab = "Jednostka",
    ylab = if (skala == "surowa") "I3*" else "I3* na publikacje",
    main = "Wklad klas percentylowych do wskaznika", ...
  )
  uzytkownika <- graphics::par("usr")
  graphics::legend(uzytkownika[2], uzytkownika[4], legend = colnames(wklady),
                   fill = kolory, border = NA, bty = "n", title = "Klasa")
  invisible(wklady)
}


#' Wypisanie zbioru odniesienia i udzialow
#'
#' @param x Obiekt klasy \code{odniesienie_i3} albo \code{udzialy_i3}.
#' @param ... Argumenty przekazywane dalej, obecnie nieuzywane.
#'
#' @return Niewidocznie zwraca \code{x}.
#'
#' @examples
#' odn <- przygotuj_odniesienie(odniesienie_przyklad)
#' odn
#' przypisz_klasy(jednostki_przyklad[1:3, ], odn)
#'
#' @name wypisanie_i3
NULL

#' @rdname wypisanie_i3
#' @export
print.odniesienie_i3 <- function(x, ...) {
  cat("Zbior odniesienia I3*\n")
  cat(sprintf("  granice klas: %s\n", paste(x$progi, collapse = ", ")))
  for (p in names(x$liczebnosci)) {
    bloki <- x$bloki[x$bloki$przekroj == p, ]
    cat(sprintf("  przekroj %s: %d publikacji, %d blokow remisowych, ",
                p, x$liczebnosci[[p]], nrow(bloki)))
    cat(sprintf("najwiekszy blok %d (cytowania = %s)\n", max(bloki$liczba),
                format(bloki$cytowania[which.max(bloki$liczba)])))
  }
  invisible(x)
}

#' @rdname wypisanie_i3
#' @export
print.udzialy_i3 <- function(x, ...) {
  cat(sprintf("Udzialy w klasach percentylowych (regula: %s)\n", x$remisy))
  cat(sprintf("  publikacje: %d, klasy: %s\n", nrow(x$udzialy),
              paste(colnames(x$udzialy), collapse = " ")))
  cat("  suma udzialow w klasach: ")
  cat(format(round(colSums(x$udzialy), 3)), "\n")
  invisible(x)
}
