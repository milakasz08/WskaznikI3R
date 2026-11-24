# Warstwa druga: wlasnosci, ktore musza zachodzic niezaleznie od danych.

# Niezalezna, najprostsza wersja udzialow ulamkowych dla jednej publikacji:
# petla po klasach, bez wektoryzacji.
udzialy_reczne <- function(c, ref, progi) {
  m <- length(ref)
  b <- sum(ref < c)
  g <- sum(ref == c)
  liczba_klas <- length(progi) - 1
  s <- numeric(liczba_klas)
  if (g == 0) {
    p <- 100 * b / m
    for (j in seq_len(liczba_klas)) {
      ostatnia <- j == liczba_klas
      if (p >= progi[j] && (p < progi[j + 1] || (ostatnia && p <= progi[j + 1]))) {
        s[j] <- 1
      }
    }
    return(s)
  }
  dolny <- 100 * b / m
  gorny <- 100 * (b + g) / m
  for (j in seq_len(liczba_klas)) {
    s[j] <- max(0, min(gorny, progi[j + 1]) - max(dolny, progi[j])) /
      (gorny - dolny)
  }
  s
}

losowe_cytowania <- function(n) {
  ifelse(stats::runif(n) < 0.3, 0L, stats::rpois(n, exp(stats::rnorm(n, 1, 1.5))))
}

test_that("udzialy zgadzaja sie z niezaleznym obliczeniem", {
  withr::local_seed(1)
  progi <- c(0, 50, 90, 99, 100)
  ref <- losowe_cytowania(300)
  cele <- data.frame(id = paste0("c", 1:60),
                     cytowania = c(sample(ref, 40), sample(0:200, 20)))
  u <- przypisz_klasy(cele, przygotuj_odniesienie(data.frame(cytowania = ref)))
  oczekiwane <- t(vapply(cele$cytowania, udzialy_reczne, numeric(4),
                         ref = ref, progi = progi))
  expect_equal(unname(u$udzialy), oczekiwane, tolerance = 1e-12)
})

test_that("odniesienie przypisane do siebie odtwarza nominalne klasy", {
  # Przy regule ulamkowej bloki pokrywaja odcinek [0, 100] dokladnie raz,
  # wiec licznosci klas wynosza 50%, 40%, 9% i 1% M, a I3/M = 0,5 * 1 +
  # 0,4 * 2 + 0,09 * 10 + 0,01 * 100 = 3,2 dla kazdego zbioru odniesienia.
  withr::local_seed(2)
  for (m in c(37, 100, 1000, 2501)) {
    ref <- data.frame(id = seq_len(m), cytowania = losowe_cytowania(m))
    w <- oblicz_i3(przypisz_klasy(ref, przygotuj_odniesienie(ref)))
    expect_equal(unname(w$licznosci_klas[1, ]), m * c(0.5, 0.4, 0.09, 0.01),
                 tolerance = 1e-10)
    expect_equal(w$wyniki$I3_na_publikacje, 3.2, tolerance = 1e-12)
  }
})

test_that("wiersze udzialow sumuja sie do jednosci", {
  withr::local_seed(3)
  ref <- losowe_cytowania(500)
  odn <- przygotuj_odniesienie(data.frame(cytowania = ref))
  cele <- data.frame(id = 1:200, cytowania = sample(0:150, 200, replace = TRUE))
  for (remisy in c("ulamkowe", "dyskretne")) {
    u <- przypisz_klasy(cele, odn, remisy = remisy)$udzialy
    expect_equal(unname(rowSums(u)), rep(1, 200), tolerance = 1e-12)
    expect_true(all(u >= 0 & u <= 1))
  }
})

test_that("wynik miesci sie miedzy N min(w) a N max(w)", {
  withr::local_seed(4)
  ref <- losowe_cytowania(400)
  odn <- przygotuj_odniesienie(data.frame(cytowania = ref))
  cele <- data.frame(id = 1:90, cytowania = losowe_cytowania(90))
  grupy <- rep(c("a", "b", "c"), each = 30)
  wagi <- c(1, 2, 10, 100)
  w <- oblicz_i3(przypisz_klasy(cele, odn), grupy, wagi)
  expect_true(all(w$wyniki$I3 >= w$wyniki$N * min(wagi) - 1e-9))
  expect_true(all(w$wyniki$I3 <= w$wyniki$N * max(wagi) + 1e-9))
})

test_that("wynik surowy jest addytywny dla rozlacznych jednostek", {
  withr::local_seed(5)
  ref <- losowe_cytowania(400)
  odn <- przygotuj_odniesienie(data.frame(cytowania = ref))
  cele <- data.frame(id = 1:80, cytowania = losowe_cytowania(80))
  u <- przypisz_klasy(cele, odn)
  osobno <- oblicz_i3(u, rep(c("a", "b"), each = 40))
  razem <- oblicz_i3(u, rep("ab", 80))
  expect_equal(sum(osobno$wyniki$I3), razem$wyniki$I3)
  expect_equal(sum(osobno$wyniki$N), razem$wyniki$N)
})

test_that("wynik nie zalezy od kolejnosci publikacji", {
  withr::local_seed(6)
  ref <- data.frame(cytowania = losowe_cytowania(300))
  cele <- data.frame(id = 1:120, jednostka = rep(c("x", "y", "z"), 40),
                     cytowania = losowe_cytowania(120))
  a <- oblicz_i3(przypisz_klasy(cele, przygotuj_odniesienie(ref)),
                 cele$jednostka)
  tasowanie <- sample(nrow(cele))
  cele2 <- cele[tasowanie, ]
  ref2 <- ref[sample(nrow(ref)), , drop = FALSE]
  b <- oblicz_i3(przypisz_klasy(cele2, przygotuj_odniesienie(ref2)),
                 cele2$jednostka)
  expect_equal(a$wyniki, b$wyniki)
})

test_that("przekroje odniesienia sa liczone osobno", {
  ref <- data.frame(przekroj = rep(c("p1", "p2"), each = 10),
                    cytowania = c(1:10, 11:20))
  odn <- przygotuj_odniesienie(ref)
  expect_equal(as.numeric(odn$liczebnosci), c(10, 10))
  # c = 10 w przekroju p1 to blok [90, 100]; w przekroju p2 pozycja 0
  cele <- data.frame(id = c("a", "b"), przekroj = c("p1", "p2"),
                     cytowania = c(10, 10))
  u <- przypisz_klasy(cele, odn)$udzialy
  expect_equal(unname(u[1, ]), c(0, 0, 0.9, 0.1))
  expect_equal(unname(u[2, ]), c(1, 0, 0, 0))
})
