# WAE
Projekt z przedmiotu Wstęp do Algorytmów Ewolucyjnych.

Użycie algorytmu ewolucyjnego do szukania maximum wiarogodności na dyskretnej przestrzeni permutacji $p$ elementowych.

Problem interpretuje się jako analizowanie korelacji celem znalezienia zależności między zmiennymi w danych tabelarycznych.

Algorytm ewolucyjny porównano z powszechnie używanym do tego celu algorytmem Metropolisa-Hastingsa (MH), zaimplementowanym pod `gips::MH()` w pakiecie `gips` dostępnym na stronie [GitHub](https://github.com/PrzeChoj/gips). Głównym celem projektu było znalezienie algorytmu radzącego sobie lepiej niż MH.

## Co zostało zrobione:
1. Zaimplementowano do porównania algorytmy losowy (Monte Carlo, MC) oraz największego wzrostu (Best Growth, BG).
2. Zaimplementowano algorytm ewolucyjny (Ewolutionary Optimization, EO) z adaptacją wielkości mutacji.
3. Przeprowadzono szereg eksperymentów numerycznych mających na celu dopasowanie parametrów algorytmu do rozważanej rodziny problemów.
4. Porównano wyniki działania algorytmu ze sobą oraz z algorytmami referencyjnymi MC, BG i MH.
5. Przeprowadzono testy statystyczne i wywnioskowano, że najlepsza znaleziona konfiguracja EO jest lepsza od algorytmów referencyjnych.

## Podsumowanie i wnioski
Projekt zakończył się pełnym sukcesem. Znaleziono konfigurację EO dającą znaczące lepsze rezultaty niż MH. Więcej informacji o szczegółach można przeczytać w raporcie TODO().

## Reprodukcja wyników i wykresów
Wszystkie wyniki są oparte o reprodukowalny skrypt generujący dane, znajdujący się w pliku `R/parameters_tuning_generate_data.R`. Skrypt ten generuje dane, które są następnie zapisywane w folderze `data`. Dane te są wykorzystywane przez skrypt `R/parameters_tuning_plots.R` do tworzenia wykresów i do przeprowadzania testów statystycznych. Wykresy są zapisywane w folderze `plots`.

## Szczegóły techniczne
Projekt wykonany jest w języku programowania `R`. Wykorzystano pakiet `gips` dostępnym na stronie [GitHub](https://github.com/PrzeChoj/gips). Skrypty są reprodukowalne na commicie pakietu `gips` o id `91ce43e068f`. Zainstalować więc można pakiet `gips` za pomocą zaklęcia: `devtools::install_github("PrzeChoj/gips", ref = "91ce43e068f")`.

Algorytm BG wytworzony w ramach tego projektu został później dołączony jako część pakietu `gips`, a algorytm MC będzie dołączony w przyszłości.

## Możliwości dalszego rozwoju
W pracy zidentyfikowano kilka potencjalnych ścieżek rozwoju:
1. Analiza przeprowadzona na większej ilości reprezentantów analizowanej rodziny funkcji celu, np o większej liczbie wymiarów $p$.
2. Porównanie EO z algorytmem symulowanego wyżarzania.
3. Rozszerzenie EO o mechanizm krzyżowania.
4. Analiza przykładowego praktycznego przypadku użycia, np. do klasyfikacji modelem QDA.
5. Zastanowić się nad innym rodzajem mutacji, np. w oparciu o cykle, a nie dowolne permutacje.




