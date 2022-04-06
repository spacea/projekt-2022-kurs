# Konwerter jednostek 
**Autorzy: 
Adrian Kurs, 
Wojciech Żmidziński**

# Rola

Pakiet "UnitConverter" zawiera funkcje pozwalające na konwersje jednostek prędkości, objętości, masy, ciśnienia, powierzchni, temperatury i długości.

Po wybraniu odpowiedniej funkji należy do nawiasu wpisać liczbę (lub liczby), następnie pojawi się ramka (data frame) przedstawiająca: 
- pełną nazwę jednostki, 
- wartość, 
- jednostkę (skrót).

# Użycie

Do instalcji pakietu potrzebny jest pakiet "devtools". Jeśli go nie mamy to można go zainstalować za pomocą skryptu:

install.packages("devtools")

library(devtools) 

Następnie wpisujemy install_github(".../..."), w miejsce ... wpisujemy autora i nazwę pakietu, po czym library(UnitConverter), i w ten sposób powinniśmy otrzymać dostęp do pakietu. Użycwanie pakietu jest bardzo proste, wpisujemy nazwę funkcji która nas interesuje np. masa_mg, po czym w nawiasie dopisujemy liczbę, tak że całość ma wyglądać tak: 

masa_mg(44) 

Jeśli chcemy wyliczyć więcej niż jedną liczbę funckja ma wyglądać w ten sposób 

masa_mg(c(44,84,13,21,37)) 

### THE END
