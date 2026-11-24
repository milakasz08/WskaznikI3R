# Eksport aplikacji do postaci dzialajacej w przegladarce.
#
# Wynik trafia do katalogu docs/app, obok strony budowanej przez pkgdown.
# Aplikacja dziala wtedy bez serwera R: obliczenia wykonuja sie w przegladarce.
#
# Uruchomienie z katalogu glownego pakietu:
#     Rscript tools/eksport_shinylive.R
#
# Eksport wazy okolo stu megabajtow, bo zawiera cale srodowisko R skompilowane
# do postaci uruchamialnej w przegladarce. Katalog docs nie trafia do
# repozytorium: strone buduje i publikuje przeplyw pracy
# .github/workflows/strona.yaml.
#
# Aplikacja w przegladarce nie ma zainstalowanego pakietu, dlatego skrypt
# kopiuje obok pliku app.R wszystkie pliki z katalogu R oraz zbiory danych
# zapisane w formacie RDS. Aplikacja wczytuje je poleceniem source() i readRDS()
# (wzor: szablon SzablonR z materialow seminarium, inst/shiny-app/app.R).

if (!requireNamespace("shinylive", quietly = TRUE)) {
  stop("Zainstaluj pakiet shinylive: install.packages(\"shinylive\")",
       call. = FALSE)
}

KATALOG_APLIKACJI <- "inst/shiny-app"
KATALOG_DOCELOWY  <- file.path("docs", "app")

if (!file.exists(file.path(KATALOG_APLIKACJI, "app.R"))) {
  stop("Brak pliku ", file.path(KATALOG_APLIKACJI, "app.R"), call. = FALSE)
}

tymczasowy <- tempfile("aplikacja-")
dir.create(tymczasowy, recursive = TRUE)
on.exit(unlink(tymczasowy, recursive = TRUE, force = TRUE), add = TRUE)

file.copy(list.files(KATALOG_APLIKACJI, full.names = TRUE), tymczasowy,
          recursive = TRUE)

pliki_r <- list.files("R", pattern = "[.]R$", full.names = TRUE)
file.copy(pliki_r, tymczasowy)

for (plik in list.files("data", pattern = "[.]rda$", full.names = TRUE)) {
  srodowisko <- new.env()
  nazwy <- load(plik, envir = srodowisko)
  for (nazwa in nazwy) {
    saveRDS(get(nazwa, envir = srodowisko),
            file.path(tymczasowy, paste0(nazwa, ".rds")))
  }
}

if (dir.exists(KATALOG_DOCELOWY)) {
  unlink(KATALOG_DOCELOWY, recursive = TRUE, force = TRUE)
}
dir.create(KATALOG_DOCELOWY, recursive = TRUE, showWarnings = FALSE)

message("Eksport aplikacji do ", KATALOG_DOCELOWY, " ...")
shinylive::export(appdir = tymczasowy, destdir = KATALOG_DOCELOWY)

rozmiar <- sum(file.size(list.files(KATALOG_DOCELOWY, recursive = TRUE,
                                    full.names = TRUE)), na.rm = TRUE)
message(sprintf("Gotowe. Rozmiar eksportu: %.0f MB.", rozmiar / 1024^2))
message("Podglad lokalny: httpuv::runStaticServer(\"", KATALOG_DOCELOWY, "\")")
