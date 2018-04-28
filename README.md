# Fort-Proj-1

Użyłem kompilatora gfortran.

Program przyjmuje dwa argumenty:
* liczba podziałów
* krok

Zaczynając od wartości krok program oblicza w pętli średnie błędy rozwiązania dla trzech wartości kind, iterując o wartość krok do wartości liczba podziałów. Wyniki na bieżąco wypisuje na ekran.

Moduły umieściłem w folderze modules.
* gau\_jor\_elim.f90 -> zawiera subrutynę (trzy schowane pod interfejsem) rozwiązującą układ równań zadany macierzami
* coefficients\_gen.f90 -> zawiera subrutynę (trzy pod interfejsem) generującą współczynniki macierzy kwadratowej zadane przez warunki
* measure\_error.f90 -> wylicza średni błąd rozwiązania opierając się na znanym wzorze przy użyciu trzech subrutyn przykrytych interfejsem

Wyniki dla trzech atrybutów kind jako wykresy znajdują się w folderze graphs.

W folderze głównym są: main.f90 - główny program, Makefile i ten plik.
