# Zbiory przykladowe pakietu.
#
# Skrypt nie wchodzi w sklad zbudowanego pakietu; zapisuje zbiory do katalogu
# data/. Uruchomienie z katalogu glownego pakietu po devtools::load_all():
#     Rscript -e "devtools::load_all(); source('data-raw/generuj_dane.R')"
#
# Zbiory powstaja procedura generuj_cytowania() z parametrami domyslnymi:
# zbior odniesienia 5000 publikacji oraz jednostki A (100), B (1000) i C (100).

dane <- generuj_cytowania(n = 1000, ziarno = 202709, n_odniesienia = 5000)

odniesienie_przyklad <- dane$odniesienie
jednostki_przyklad <- dane$publikacje

stopifnot(
  nrow(odniesienie_przyklad) == 5000,
  nrow(jednostki_przyklad) == 1200,
  all(odniesienie_przyklad$cytowania >= 0),
  sum(jednostki_przyklad$wzmocniona) == 5,
  !anyDuplicated(c(odniesienie_przyklad$id, jednostki_przyklad$id))
)

dir.create("data", showWarnings = FALSE)
save(odniesienie_przyklad, file = "data/odniesienie_przyklad.rda",
     compress = "xz", version = 3)
save(jednostki_przyklad, file = "data/jednostki_przyklad.rda",
     compress = "xz", version = 3)

# TODO: osobny skrypt pobrania danych OpenAlex (jeden podobszar, rocznik 2018,
#   caly dostepny zbior odniesienia przed wyborem pieciu czasopism, cytowania
#   do konca 2023 r. z jednego zamrozonego pobrania), z zapisem daty
#   i parametrow pobrania oraz raportem kompletnosci; braki nie sa zamieniane
#   na zera. Pobranie poza funkcjami pakietu i testami.
