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



dlugosc_ -> pozwala na wybranie jednostki z zagadnienia "długość"

- dlugosc_mm -> przeliczenie z milimetrów

- dlugosc_cm -> przeliczenie z centymetrów

- dlugosc_dm -> przeliczenie z decymetrów

- dlugosc_m -> przeliczenie z metrów

- dlugosc_km -> przeliczenie z kilometrów

- dlugosc_in -> przeliczenie z cali

- dlugosc_ft -> przeliczenie ze stóp

- dlugosc_yd -> przeliczenie z jardów

- dlugosc_mi -> przeliczenie z mil lądowych

- dlugosc_nmi -> przeliczenie z mil morskich

- dlugosc_ly -> przeliczenie z lat świetlnych


### POWIERZCHNIA ###

powierzchnia_ -> pozwala na wybranie jednostki z zagadnienia "powierzchnia"

- powierzchnia_mm2 -> przeliczenie z milimetrów kwadratowych

- powierzchnia_cm2 -> przeliczenie z centymetrów kwadratowych

- powierzchnia_m2 -> przeliczenie z metrów kwadratowych

- powierzchnia_a -> przeliczenie z arów

- powierzchnia_ha -> przeliczenie z hektarów

- powierzchnia_km2 -> przeliczenie z kilometrów kwadratowych

- powierzchnia_ft2 -> przeliczenie ze stóp kwadratowych

- powierzchnia_yd2 -> przeliczenie z jardów kwadratowych

- powierzchnia_acre -> przeliczenie z akrów


### MASA ###

masa_ -> pozwala na wybranie jednostki z zagadnienia "masa"

- masa_mg -> przeliczenie z miligramów

- masa_g -> przeliczenie z gramów

- masa_kg -> przeliczenie z kilogramów

- masa_t -> przeliczenie z tony

- masa_gr -> przeliczenie z granów

- masa_oz -> przeliczenie z uncji

- masa_lb -> przeliczenie z funtów

- masa_ton_us -> przeliczenie z ton amerykańskich

- masa_ton_uk -> przeliczenie z ton brytyjskich

- masa_ct -> przeliczenie z karatów

- masa_u -> przeliczenie z masy (jednostek) atomowych


### OBJĘTOŚĆ ###

objetosc_ -> pozwala na wybranie jednostki z zagadnienia "objętość"

- objetosc_ml -> przeliczenie z mililitrów

- objetosc_cm3 -> przeliczenie z centymetrów sześciennych

- objetosc_L -> przeliczenie z litrów

- objetosc_m3 -> przeliczenie z metrów sześciennych

- objetosc_in3 -> przeliczenie z cali sześciennych

- objetosc_ft3 -> przeliczenie ze stóp sześciennych

- objetosc_yd3 -> przeliczenie z jardów sześciennych

- objetosc_galon_us -> przeliczenie z galonu płynnego amerykańskiego

- objetosc_galon_dry_us -> przeliczenie z galonu suchego amerykańskiego

- objetosc_galon_uk -> przeliczenie z galonu brytyjskiego

- objetosc_uncja_us -> przeliczenie z uncji amerykańskiej

- objetosc_uncja_uk -> przeliczenie z uncji brytyjskiej

- objetosc_kwarta_us -> przeliczenie z kwarty amerykańskiej

- objetosc_kwarta_uk -> przeliczenie z kwarty brytyjskiej

- objetosc_pinta_us -> przeliczenie z pinty amerykańskiej

- objetosc_pints_uk -> przeliczenie z pinty brytyjskiej

- objetosc_barylka -> przeliczenie z baryłek


### PRĘDKOŚĆ ###

predkosc_ -> pozwala na wybranie jednostki z zagadnienia "prędkość"

- predkosc_ms -> przeliczenie z metrów na sekundę

- predkosc_mmin -> przeliczenie z metrów na minutę

- predkosc_kms -> przeliczenie z kilometrów na sekundę

- predkosc_kmin -> przeliczenie z kilometrów na minutę

- predkosc_kmh -> przeliczenie z kilometrów na godzinę

- predkosc_fts -> przeliczenie ze stóp na sekundę

- predkosc_mph -> przeliczenie z mil na godzinę

- predkosc_Mach -> przeliczenie z mach

- predkosc_kn -> przeliczenie z węzłów


### TEMPERATURA ###

temperatura_ -> pozwala na wybranie jednostki z zagadnienia "temperatura"

- temperatura_C -> przeliczenie ze skali Celcjusza

- temperatura_K -> przeliczenie ze skali Kelvina

- temperatura_F -> przeliczenie ze skali Fahrenheita

- temperatura_R -> przeliczenie ze skali Rankinea

- temperatura_Re -> przeliczenie ze skali Réaumura

- temperatura_N -> przeliczenie ze skali Newtona


### CIŚNIENIE ###

cisnienie_ -> pozwala na wybranie jednostki z zagadnienia "ciśnienie"

- cisnienie_atm -> przeliczenie z atmosfery

- cisnienie_Pa -> przeliczenie z paskali

- cisnienie_hPa -> przeliczenie z hektopaskali

- cisnienie_kPa -> przeliczenie z kilopaskali

- cisnienie_bar -> przeliczenie z barów

- cisnienie_mmHg -> przeliczenie z milimetrów słupa rtęci

- cisnienie_inHg -> przeliczenie z cali słupa rtęci

- cisnienie_mmWg -> przeliczenie z milimetrów słupa wody

- cisnienie_inWg -> przeliczenie z cali słupa wody

- cisnienie_kgcm2 -> przeliczenie z kilogramów na centymetr kwadratowy

- cisnienie_psi -> przeliczenie z funtów na cal kwadratowy

# Przykład

Załóżmy, że interesuje nas wartość ***Mach 3*** na inne wartości prędkości. Użyjmy więc tej funkcji: `predkosc_Mach(3)`
```
predkosc_Mach(3)
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
> predkosc_Mach(3, 5)
Error in predkosc_Mach(3, 5) : unused argument (5)
> 
```
Nie wiemy jak ułożyć warunek, aby zmienić komunikat błędu na język polski.

To wszystko co zauważyliśmy.

### THE END