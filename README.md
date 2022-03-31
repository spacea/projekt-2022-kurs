# Konwerter jednostek (wersja BETA)
**Autorzy: 
Adrian Kurs, 
Wojciech Żmidziński**

# Rola

Skrypt ten pozwala na konwertowanie interesującej nas w danej chwilii jednostki na inne jednostki, które są opisane poniżej.

Wybranie jednej z funkcji powoduje, że pojawia się ramka (data frame) przedstawiająca: 
- pełną nazwę jednostki, 
- wartość, 
- jednostkę (skrót).

Skrypt obsługuje takie jednostki jak:
- długość: (milimetry, centymetry, decymetry, metry, kilometry, cale, stopy, jardy, mile, mile morskie, rok świetlny);
- powierzchnia: (milimetry kwadratowe, centymetry kwadratowe, metry kwadratowe, ary, hektary, kilometry kwadratowe, stopy kwadratowe, jardy kwadratowe, akry);
- masa: (miligramy, gramy, kilogramy, tony, grany, uncje, funty, tony amerykańskie, tony brytyjskie, karaty, masa atomowa);
- objętość: (mililitry, centymetry sześcienne, litry, metry sześcienne, cale sześcnienne, stopy sześcienne, jardy sześcnienne, galon płynny amerykański, galon suchy amerykański, galon brytyjski, uncje amerykańskie, uncje brytyjskie, kwarty amerykańskie, kwarty brytyjskie, pinty amerykańskie, pinty brytyjskie, baryłki);
- prędkość: (metry/sekundę, metry/minutę, kilometry/sekundę, kilometry/minutę, kilometry/godzinę, stopy/sekundę, mile/godzinę, mach, węzeł);
- temperatura: (Kelvin, stopnie Celcjusza, stopnie Fahrenheita, stopnie Rankine'a, stopnie Réaumura, skala Newtona);
- ciśnienie: (atmosferyczne, paskale, hektopaskale, kilopaskale, bary, milimetry słupa rtęci, cale słupa rtęci, milimetry słupa wody, cale słupa wody, kilogramy/centymetr kwadratowy, funty/cal kwadratowy)

# Użycie

### DŁUGOŚĆ ###



dlugosc. -> pozwala na wybranie jednostki z zagadnienia "długość"

- dlugosc.mm -> przeliczenie z milimetrów

- dlugosc.cm -> przeliczenie z centymetrów

- dlugosc.dm -> przeliczenie z decymetrów

- dlugosc.m -> przeliczenie z metrów

- dlugosc.km -> przeliczenie z kilometrów

- dlugosc.in -> przeliczenie z cali

- dlugosc.ft -> przeliczenie ze stóp

- dlugosc.yd -> przeliczenie z jardów

- dlugosc.mi -> przeliczenie z mil lądowych

- dlugosc.nmi -> przeliczenie z mil morskich

- dlugosc.ly -> przeliczenie z lat świetlnych


### POWIERZCHNIA ###

powierzchnia. -> pozwala na wybranie jednostki z zagadnienia "powierzchnia"

- powierzchnia.mm2 -> przeliczenie z milimetrów kwadratowych

- powierzchnia.cm2 -> przeliczenie z centymetrów kwadratowych

- powierzchnia.m2 -> przeliczenie z metrów kwadratowych

- powierzchnia.a -> przeliczenie z arów

- powierzchnia.ha -> przeliczenie z hektarów

- powierzchnia.km2 -> przeliczenie z kilometrów kwadratowych

- powierzchnia.ft2 -> przeliczenie ze stóp kwadratowych

- powierzchnia.yd2 -> przeliczenie z jardów kwadratowych

- powierzchnia.acre -> przeliczenie z akrów


### MASA ###

masa. -> pozwala na wybranie jednostki z zagadnienia "masa"

- masa.mg -> przeliczenie z miligramów

- masa.g -> przeliczenie z gramów

- masa.kg -> przeliczenie z kilogramów

- masa.t -> przeliczenie z tony

- masa.gr -> przeliczenie z granów

- masa.oz -> przeliczenie z uncji

- masa.lb -> przeliczenie z funtów

- masa.ton_us -> przeliczenie z ton amerykańskich

- masa.ton_uk -> przeliczenie z ton brytyjskich

- masa.ct -> przeliczenie z karatów

- masa.u -> przeliczenie z masy (jednostek) atomowych


### OBJĘTOŚĆ ###

objetosc. -> pozwala na wybranie jednostki z zagadnienia "objętość"

- objetosc.ml -> przeliczenie z mililitrów

- objetosc.cm3 -> przeliczenie z centymetrów sześciennych

- objetosc.L -> przeliczenie z litrów

- objetosc.m3 -> przeliczenie z metrów sześciennych

- objetosc.in3 -> przeliczenie z cali sześciennych

- objetosc.ft3 -> przeliczenie ze stóp sześciennych

- objetosc.yd3 -> przeliczenie z jardów sześciennych

- objetosc.galon_us -> przeliczenie z galonu płynnego amerykańskiego

- objetosc.galon_dry_us -> przeliczenie z galonu suchego amerykańskiego

- objetosc.galon_uk -> przeliczenie z galonu brytyjskiego

- objetosc.uncja_us -> przeliczenie z uncji amerykańskiej

- objetosc.uncja_uk -> przeliczenie z uncji brytyjskiej

- objetosc.kwarta_us -> przeliczenie z kwarty amerykańskiej

- objetosc.kwarta_uk -> przeliczenie z kwarty brytyjskiej

- objetosc.pinta_us -> przeliczenie z pinty amerykańskiej

- objetosc.pints_uk -> przeliczenie z pinty brytyjskiej

- objetosc.barylka -> przeliczenie z baryłek


### PRĘDKOŚĆ ###

predkosc. -> pozwala na wybranie jednostki z zagadnienia "prędkość"

- predkosc.ms -> przeliczenie z metrów na sekundę

- predkosc.mmin -> przeliczenie z metrów na minutę

- predkosc.kms -> przeliczenie z kilometrów na sekundę

- predkosc.kmin -> przeliczenie z kilometrów na minutę

- predkosc.kmh -> przeliczenie z kilometrów na godzinę

- predkosc.fts -> przeliczenie ze stóp na sekundę

- predkosc.mph -> przeliczenie z mil na godzinę

- predkosc.Mach -> przeliczenie z mach

- predkosc.kn -> przeliczenie z węzłów


### TEMPERATURA ###

temperatura. -> pozwala na wybranie jednostki z zagadnienia "temperatura"

- temperatura.C -> przeliczenie ze skali Celcjusza

- temperatura.K -> przeliczenie ze skali Kelvina

- temperatura.F -> przeliczenie ze skali Fahrenheita

- temperatura.R -> przeliczenie ze skali Rankinea

- temperatura.Re -> przeliczenie ze skali Réaumura

- temperatura.N -> przeliczenie ze skali Newtona


### CIŚNIENIE ###

cisnienie. -> pozwala na wybranie jednostki z zagadnienia "ciśnienie"

- cisnienie.atm -> przeliczenie z atmosfery

- cisnienie.Pa -> przeliczenie z paskali

- cisnienie.hPa -> przeliczenie z hektopaskali

- cisnienie.kPa -> przeliczenie z kilopaskali

- cisnienie.bar -> przeliczenie z barów

- cisnienie.mmHg -> przeliczenie z milimetrów słupa rtęci

- cisnienie.inHg -> przeliczenie z cali słupa rtęci

- cisnienie.mmWg -> przeliczenie z milimetrów słupa wody

- cisnienie.inWg -> przeliczenie z cali słupa wody

- cisnienie.kgcm2 -> przeliczenie z kilogramów na centymetr kwadratowy

- cisnienie.psi -> przeliczenie z funtów na cal kwadratowy

# Przykład

Załóżmy, że interesuje nas wartość ***Mach 3*** na inne wartości prędkości. Użyjmy więc tej funkcji: `predkosc.Mach(3)`
```
predkosc.Mach(3)
        nazwa_jednostki     wartość jednostka
1                 machy     3.00000      Mach
2      metry na sekundę  1020.87000       m/s
3       metry na minutę 61252.20000     m/min
4  kilometry na sekundę     1.02087      km/s
5   kilometry na minutę    61.25220    km/min
6  kilometry na godzinę  3675.13200      km/h
7      stopy na sekundę  3349.31102      ft/s
8       mile na godzinę  2283.62115       mph
9                 węzły  1984.41424        kn
> 
```
Jak można zauważyć, pojawiła się ramka (data frame) dzieląca się na 3 kolumny [nazwa jednostki, wartość i jednostkę (skrót)]. Podane zostały różne jednostki, uwzględnione w funkcji jak i ich skrótowce.

# Nasze uwagi i założenie projektu

Zaczniemy od założenia projektu - ostatecznie planujemy, aby ten skrypt zapakować do pakietu, lecz najpierw oczekujemy na jego wstępną recenzję.

Co do uwag - na chwilę obecną uważamy, że kod może być za długi, lecz nie ma pomysłu jak go skrócić. Następna rzecz to warunki funkcji: otóż po wprowadzeniu drugiego argumentu do funkcji wyświetla się error z nieużywanym argumentem: 
```
> predkosc.Mach(3, 5)
Error in predkosc.Mach(3, 5) : unused argument (5)
> 
```
Nie wiemy jak ułożyć warunek, aby zmienić komunikat błędu na język polski.

To wszystko co zauważyliśmy.

### THE END