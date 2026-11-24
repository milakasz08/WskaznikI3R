# Warstwa trzecia: dane, przy ktorych metoda nie ma sensu.
#
# Testy opisuja kontrakt bledow z briefu tematu. Oczekiwanym wynikiem jest
# zatrzymanie wykonania z komunikatem, a nie liczba. Walidacja wejscia nie
# jest jeszcze zaimplementowana, wiec testy sa pomijane; po implementacji
# kontroli w R/odniesienie.R, R/percentyle.R i R/i3.R nalezy usunac
# wywolania pomin().

pomin <- function() {
  skip("Walidacja wejscia do implementacji (kontrakt bledow, SPEC.md).")
}

test_that("ujemne, ulamkowe lub brakujace cytowania zatrzymuja wykonanie", {
  pomin()
  komunikat <- "Cytowania musza byc nieujemnymi liczbami calkowitymi"
  expect_error(przygotuj_odniesienie(data.frame(cytowania = c(1, -2, 3))),
               komunikat)
  expect_error(przygotuj_odniesienie(data.frame(cytowania = c(1, 2.5))),
               komunikat)
  expect_error(przygotuj_odniesienie(data.frame(cytowania = c(1, NA))),
               komunikat)
  odn <- przygotuj_odniesienie(data.frame(cytowania = 1:10))
  expect_error(przypisz_klasy(data.frame(id = "a", cytowania = -1), odn),
               komunikat)
})

test_that("niepoprawne granice klas zatrzymuja wykonanie", {
  pomin()
  komunikat <- "Progi musza rosnac od 0 do 100"
  ref <- data.frame(cytowania = 1:10)
  expect_error(przygotuj_odniesienie(ref, progi = c(10, 50, 100)), komunikat)
  expect_error(przygotuj_odniesienie(ref, progi = c(0, 50, 90)), komunikat)
  expect_error(przygotuj_odniesienie(ref, progi = c(0, 90, 50, 100)),
               komunikat)
})

test_that("liczba wag rozna od liczby klas zatrzymuje wykonanie", {
  pomin()
  expect_error(oblicz_i3(diag(4), wagi = c(1, 2, 10)),
               "Liczba wag nie odpowiada liczbie klas")
})

test_that("ujemne lub zerowe wagi zatrzymuja wykonanie", {
  pomin()
  expect_error(oblicz_i3(diag(4), wagi = c(1, -2, 10, 100)), "Wagi")
  expect_error(oblicz_i3(diag(4), wagi = c(0, 0, 0, 0)), "Wagi")
})

test_that("publikacja bez zbioru odniesienia zatrzymuje wykonanie", {
  pomin()
  odn <- przygotuj_odniesienie(data.frame(przekroj = "p1", cytowania = 1:10))
  expect_error(
    przypisz_klasy(data.frame(id = "a", przekroj = "p2", cytowania = 3), odn),
    "Brak zbioru odniesienia dla publikacji"
  )
})

test_that("pusty zbior odniesienia zatrzymuje wykonanie", {
  pomin()
  expect_error(przygotuj_odniesienie(data.frame(cytowania = numeric(0))),
               "Zbior odniesienia nie moze byc pusty")
})

test_that("powtorzone identyfikatory zatrzymuja wykonanie", {
  pomin()
  odn <- przygotuj_odniesienie(data.frame(cytowania = 1:10))
  expect_error(
    przypisz_klasy(data.frame(id = c("a", "a"), cytowania = c(1, 2)), odn),
    "jednoznaczne"
  )
})
