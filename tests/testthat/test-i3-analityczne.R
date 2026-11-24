# Warstwa pierwsza: przypadki o wyniku policzonym recznie ze wzorow
# I3* = sum_j w_j n_j oraz I3*/N, z udzialami ulamkowymi bloku remisowego
# s_j = |[P_d, P_g] n [pi_{j-1}, pi_j]| / (P_g - P_d).

test_that("po jednej publikacji w kazdej klasie", {
  # n = (1, 1, 1, 1), w = (1, 2, 10, 100)
  # I3 = 1 + 2 + 10 + 100 = 113, N = 4, I3/N = 113 / 4 = 28,25
  udzialy <- diag(4)
  w <- oblicz_i3(udzialy)
  expect_equal(w$wyniki$I3, 113)
  expect_equal(w$wyniki$N, 4)
  expect_equal(w$wyniki$I3_na_publikacje, 28.25)
})

test_that("schemat gornych 10% daje liczbe i udzial publikacji", {
  # Odniesienie: cytowania 1..100, klasy [0, 90) i [90, 100], wagi (0, 1).
  # Publikacje z cytowaniami 91, 95, 100 tworza bloki [90,91], [94,95] i
  # [99,100], w calosci w klasie 2;
  # siedem publikacji z cytowaniami 1..7 -> klasa 1.
  # I3 = 0 * 7 + 1 * 3 = 3, N = 10, udzial = 3 / 10 = 0,3
  odn <- przygotuj_odniesienie(data.frame(cytowania = 1:100),
                               progi = c(0, 90, 100))
  cele <- data.frame(id = letters[1:10], cytowania = c(1:7, 91, 95, 100))
  w <- oblicz_i3(przypisz_klasy(cele, odn), wagi = c(0, 1))
  expect_equal(w$wyniki$I3, 3)
  expect_equal(w$wyniki$I3_na_publikacje, 0.3)
})

test_that("blok remisowy na percentylach 80-100", {
  # Odniesienie M = 50: 40 roznych wartosci 0..39 oraz 10 remisow c = 100.
  # Blok: b = 40, g = 10, przedzial [80, 100], dlugosc 20.
  # Przeciecia: [50,90) -> 10, [90,99) -> 9, [99,100] -> 1
  # Udzialy: 10/20 = 0,5; 9/20 = 0,45; 1/20 = 0,05
  # Wklad: 0,5 * 2 + 0,45 * 10 + 0,05 * 100 = 1 + 4,5 + 5 = 10,5
  # Blok dziesieciu publikacji: 105
  odn <- przygotuj_odniesienie(data.frame(cytowania = c(0:39, rep(100, 10))))
  cele <- data.frame(id = paste0("t", 1:10), cytowania = rep(100, 10))
  u <- przypisz_klasy(cele, odn)
  expect_equal(unname(u$udzialy[1, ]), c(0, 0.5, 0.45, 0.05))
  w <- oblicz_i3(u)
  expect_equal(w$wyniki$I3, 105)
  expect_equal(w$wyniki$I3_na_publikacje, 10.5)
})

test_that("wariant dyskretny przenosi caly blok na progu do klasy wyzszej", {
  # Ten sam zbior odniesienia: prog gornego 1% jest liczba cytowan
  # publikacji na pozycji ceiling(0,01 * 50) = 1 w porzadku malejacym,
  # czyli 100. Kazda z dziesieciu publikacji c = 100 trafia do klasy
  # [99, 100] z waga 100; blok daje 1000 zamiast 105.
  odn <- przygotuj_odniesienie(data.frame(cytowania = c(0:39, rep(100, 10))))
  expect_equal(unname(odn$progi_dyskretne[1, ]), c(25, 100, 100))
  cele <- data.frame(id = paste0("t", 1:10), cytowania = rep(100, 10))
  w <- oblicz_i3(przypisz_klasy(cele, odn, remisy = "dyskretne"))
  expect_equal(w$wyniki$I3, 1000)
})

test_that("cytowania nieobecne w odniesieniu otrzymuja pozycje punktowa", {
  # Odniesienie jak wyzej. c = 39,5: b = 40, P = 80 -> klasa [50, 90).
  # c = 101: b = 50, P = 100 -> ostatnia klasa, domknieta z prawej.
  # c = 50: b = 40, P = 80 -> klasa [50, 90).
  odn <- przygotuj_odniesienie(data.frame(cytowania = c(0:39, rep(100, 10))))
  cele <- data.frame(id = c("a", "b", "c"), cytowania = c(39.5, 101, 50))
  u <- przypisz_klasy(cele, odn)$udzialy
  expect_equal(unname(u[1, ]), c(0, 1, 0, 0))
  expect_equal(unname(u[2, ]), c(0, 0, 0, 1))
  expect_equal(unname(u[3, ]), c(0, 1, 0, 0))
})

test_that("blok publikacji niecytowanych dzieli sie miedzy klasy", {
  # Odniesienie: 60 zer i 40 roznych wartosci 1..40, M = 100.
  # Blok zer zajmuje [0, 60]: udzialy 50/60 w [0,50) i 10/60 w [50,90).
  odn <- przygotuj_odniesienie(data.frame(cytowania = c(rep(0, 60), 1:40)))
  u <- przypisz_klasy(data.frame(id = "z", cytowania = 0), odn)$udzialy
  expect_equal(unname(u[1, ]), c(50 / 60, 10 / 60, 0, 0))
})

test_that("jednostka bez publikacji ma wynik zero i niezdefiniowana srednia", {
  odn <- przygotuj_odniesienie(data.frame(cytowania = 1:20))
  cele <- data.frame(id = c("a", "b"), cytowania = c(3, 18))
  grupy <- factor(c("J1", "J1"), levels = c("J1", "J2"))
  w <- oblicz_i3(przypisz_klasy(cele, odn), grupy)
  pusta <- w$wyniki[w$wyniki$jednostka == "J2", ]
  expect_equal(pusta$N, 0)
  expect_equal(pusta$I3, 0)
  expect_true(is.na(pusta$I3_na_publikacje))
  expect_identical(pusta$status, "pusta_jednostka")
})
