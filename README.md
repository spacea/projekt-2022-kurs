# UnitConverter 
**Autorzy: 
Adrian Kurs, 
Wojciech Żmidziński**

# Rola 
Konwerter jednostek

Pakiet "UnitConverter" zawiera funkcje pozwalające na konwersje jednostek prędkości, objętości, masy, ciśnienia, powierzchni, temperatury i długości.

Po wybraniu odpowiedniej funkcji należy do nawiasu wpisać liczbę (lub liczby), następnie pojawi się ramka (data frame) przedstawiająca: 
- pełną nazwę jednostki, 
- wartość, 
- jednostkę (skrót).

# Użycie

Do instalcji pakietu potrzebny jest pakiet `"devtools"`. Jeśli go nie mamy to można go zainstalować za pomocą skryptu:

`install.packages("devtools")`

`library(devtools)` 

Następnie wpisujemy `install_github("nazwa_uzytkownika_github/nazwa_pakietu")` [w tym wypadku wpisujemy `install_github("spacea/projekt-2022-kurs")`].  Po zainstalowaniu pakietu wpisujemy `library(UnitConverter)`, i w ten sposób powinniśmy otrzymać dostęp do pakietu. Używanie pakietu jest bardzo proste, wpisujemy nazwę funkcji która nas interesuje np. `masa_mg`, po czym w nawiasie dopisujemy liczbę, tak że całość ma wyglądać tak: 
```
> masa_mg(44)
[[1]]
   nazwa_jednostki      wartosc jednostka
1         miligram 4.400000e+01        mg
2             gram 4.400000e-02         g
3         kilogram 4.400000e-05        kg
4             tona 4.400000e-08         t
5             gran 6.790238e-01        gr
6            uncja 1.552054e-03        oz
7             funt 9.700340e-05        lb
8       tona (USA) 4.850170e-08    ton_us
9        tona (UK) 4.330509e-08    ton_uk
10           karat 2.200000e-01        ct
11    masa atomowa 2.649742e+22         u

>
```
Jeśli chcemy wyliczyć więcej niż jedną liczbę funckja ma wyglądać w ten sposób: 
```
> masa_mg(c(44,84,13,21,37))
[[1]]
   nazwa_jednostki      wartosc jednostka
1         miligram 4.400000e+01        mg
2             gram 4.400000e-02         g
3         kilogram 4.400000e-05        kg
4             tona 4.400000e-08         t
5             gran 6.790238e-01        gr
6            uncja 1.552054e-03        oz
7             funt 9.700340e-05        lb
8       tona (USA) 4.850170e-08    ton_us
9        tona (UK) 4.330509e-08    ton_uk
10           karat 2.200000e-01        ct
11    masa atomowa 2.649742e+22         u

[[2]]
   nazwa_jednostki      wartosc jednostka
1         miligram 8.400000e+01        mg
2             gram 8.400000e-02         g
3         kilogram 8.400000e-05        kg
4             tona 8.400000e-08         t
5             gran 1.296318e+00        gr
6            uncja 2.963013e-03        oz
7             funt 1.851883e-04        lb
8       tona (USA) 9.259415e-08    ton_us
9        tona (UK) 8.267335e-08    ton_uk
10           karat 4.200000e-01        ct
11    masa atomowa 5.058599e+22         u

[[3]]
   nazwa_jednostki      wartosc jednostka
1         miligram 1.300000e+01        mg
2             gram 1.300000e-02         g
3         kilogram 1.300000e-05        kg
4             tona 1.300000e-08         t
5             gran 2.006207e-01        gr
6            uncja 4.585615e-04        oz
7             funt 2.866009e-05        lb
8       tona (USA) 1.433005e-08    ton_us
9        tona (UK) 1.279468e-08    ton_uk
10           karat 6.500000e-02        ct
11    masa atomowa 7.828785e+21         u

[[4]]
   nazwa_jednostki      wartosc jednostka
1         miligram 2.100000e+01        mg
2             gram 2.100000e-02         g
3         kilogram 2.100000e-05        kg
4             tona 2.100000e-08         t
5             gran 3.240795e-01        gr
6            uncja 7.407532e-04        oz
7             funt 4.629708e-05        lb
8       tona (USA) 2.314854e-08    ton_us
9        tona (UK) 2.066834e-08    ton_uk
10           karat 1.050000e-01        ct
11    masa atomowa 1.264650e+22         u

[[5]]
   nazwa_jednostki      wartosc jednostka
1         miligram 3.700000e+01        mg
2             gram 3.700000e-02         g
3         kilogram 3.700000e-05        kg
4             tona 3.700000e-08         t
5             gran 5.709973e-01        gr
6            uncja 1.305137e-03        oz
7             funt 8.157104e-05        lb
8       tona (USA) 4.078552e-08    ton_us
9        tona (UK) 3.641564e-08    ton_uk
10           karat 1.850000e-01        ct
11    masa atomowa 2.228193e+22         u

>
```

**Opcjonalnie** można zamienić notację naukową na liczbę używając na samym początku: `options("scipen"= 999)`. Wtedy całość będzie wyglądać tak:
```
> masa_mg(44)
[[1]]
   nazwa_jednostki                                wartosc jednostka
1         miligram                      44.00000000000000        mg
2             gram                       0.04400000000000         g
3         kilogram                       0.00004400000000        kg
4             tona                       0.00000004400000         t
5             gran                       0.67902376752940        gr
6            uncja                       0.00155205432578        oz
7             funt                       0.00009700339536        lb
8       tona (USA)                       0.00000004850170    ton_us
9        tona (UK)                       0.00000004330509    ton_uk
10           karat                       0.22000000000000        ct
11    masa atomowa 26497424755999998214424.00000000000000         u

>
```
Jeśli chcemy z powrotem otrzymywać wartości w notacji naukowej wystarczy wpisać `options("scipen"= 0)`.
### THE END
