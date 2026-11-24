test_that("metody wyniku I3* dzialaja i zwracaja obiekty", {
  odn <- przygotuj_odniesienie(odniesienie_przyklad)
  expect_output(print(odn), "5000 publikacji")
  u <- przypisz_klasy(jednostki_przyklad, odn)
  expect_output(print(u), "ulamkowe")
  w <- oblicz_i3(u, jednostki_przyklad$jednostka)
  expect_output(print(w), "I3_na_publikacje")
  s <- summary(w)
  expect_s3_class(s, "summary.wynik_i3")
  expect_equal(rowSums(s$wklady_klas), stats::setNames(w$wyniki$I3,
                                                       w$wyniki$jednostka))
  expect_output(print(s), "Wklady klas")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_equal(dim(plot(w)), c(3L, 4L))
  expect_equal(rowSums(plot(w, skala = "na_publikacje")),
               stats::setNames(w$wyniki$I3_na_publikacje, w$wyniki$jednostka))
})
