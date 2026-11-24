# Procedura generowania danych o znanej strukturze.

test_that("generator jest powtarzalny i nie zmienia stanu generatora", {
  set.seed(1)
  przed <- .Random.seed
  a <- generuj_cytowania(n = 200, n_odniesienia = 500)
  expect_identical(.Random.seed, przed)
  b <- generuj_cytowania(n = 200, n_odniesienia = 500)
  expect_identical(a, b)
})

test_that("struktura jednostek odpowiada parametrom", {
  d <- generuj_cytowania(n = 1000, n_odniesienia = 5000)
  expect_equal(nrow(d$odniesienie), 5000)
  expect_equal(as.numeric(table(d$publikacje$jednostka)), c(100, 1000, 100))
  wzmocnione <- d$publikacje[d$publikacje$wzmocniona, ]
  expect_equal(nrow(wzmocnione), 5)
  expect_true(all(wzmocnione$jednostka == "C"))
  expect_false(anyDuplicated(c(d$odniesienie$id, d$publikacje$id)) > 0)
})

test_that("udzial publikacji niecytowanych odpowiada rozkladowi", {
  # P(C = 0) = 0,3 + 0,7 * E[exp(-lambda)], log lambda ~ N(1, 1,5^2);
  # calka numeryczna daje okolo 0,448. Blad standardowy udzialu przy
  # M = 5000 wynosi okolo 0,007.
  teoria <- 0.3 + 0.7 * stats::integrate(
    function(x) exp(-exp(x)) * stats::dnorm(x, 1, 1.5), -Inf, Inf
  )$value
  d <- generuj_cytowania()
  expect_equal(mean(d$odniesienie$cytowania == 0), teoria, tolerance = 0.03)
})

test_that("zaokraglenie do wielokrotnosci 5 zwieksza remisy", {
  d1 <- generuj_cytowania(n = 200, n_odniesienia = 2000)
  d5 <- generuj_cytowania(n = 200, n_odniesienia = 2000, wielokrotnosc = 5)
  expect_true(all(d5$odniesienie$cytowania %% 5 == 0))
  expect_lt(nrow(przygotuj_odniesienie(d5$odniesienie)$bloki),
            nrow(przygotuj_odniesienie(d1$odniesienie)$bloki))
})

test_that("odniesienie z generatora odtwarza nominalne klasy", {
  d <- generuj_cytowania()
  odn <- przygotuj_odniesienie(d$odniesienie)
  w <- oblicz_i3(przypisz_klasy(d$odniesienie, odn))
  expect_equal(unname(w$licznosci_klas[1, ]), c(2500, 2000, 450, 50),
               tolerance = 1e-10)
  expect_equal(w$wyniki$I3_na_publikacje, 3.2)
})

test_that("publikacje wzmocnione podnosza wynik na publikacje jednostki C", {
  # Prawda konstrukcyjna dotyczy udzialow: piec publikacji jednostki C
  # o intensywnosci pomnozonej przez 50 lezy przewaznie w gornych klasach.
  d <- generuj_cytowania()
  odn <- przygotuj_odniesienie(d$odniesienie)
  u <- przypisz_klasy(d$publikacje, odn)
  wzmocnione <- u$udzialy[d$publikacje$wzmocniona, , drop = FALSE]
  expect_gt(sum(wzmocnione[, 3:4]), 2.5)
})
