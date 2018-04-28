# Fort-Proj-1

Użyłem kompilatora gfortran.

Program przyjmuje dwa argumenty:
* liczba podziałów
* krok
Zaczynając od wartości krok oblicza w pętli średnie błędy rozwiązania dla trzech wartości kind iterując o wartość krok do wartości liczba podziałów. Wyniki na bieżąco wypisuje na ekran.

Moduły umieściłem w folderze modules.
* gau\_jor\_elim.f90 -> zawiera subrutynę (trzy schowane pod interfejsem) rozwiązującą układ równań zadany macierzą
* coefficients\_gen.f90 -> zawiera subrutynę (trzy pod interfejsem) generującą układ równań zadany przez warunki
* measure\_error.f90 -> wylicza średni błąd rozwiązania opierając się na znanym wzorze przy użyciu trzech subrutyn przykrytych interfejsem

Wyniki dla trzech atrybutów kind jako wykresy znajdują się w folderze graphs.

W folderze głównym są main.f90 - program, Makefile i ten plik.
