options("scipen"=999)

#======#
# MASA #
#======#

masa_mg = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(miligram = data.frame(nazwa_jednostki = c("miligram", "gram", "kilogram", "tona", "gran", "uncja", "funt", "tona (USA)", 
                                                   "tona (UK)", "karat", "masa atomowa"),
                               wartość = c(x, x * 0.001, x * 0.000001, x * 0.000000001, x * 0.015432358352941, 
                                           x * 3.527396194958e-5, x * 2.204622621849e-6, x * 1.102311310924e-9, 
                                           x * 9.84206527611e-10, x * 0.005, x * 6.02214199e+20),
                               jednostka = c("mg", "g", "kg", "t", "gr", "oz", "lb", "ton_us", "ton_uk", "ct", "u")))
  print.data.frame(miligram)
}

masa_g = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(gram = data.frame(nazwa_jednostki = c("gram", "miligram", "kilogram", "tona", "gran", "uncja", "funt", "tona (USA)", 
                                        "tona (UK)", "karat", "masa atomowa"),
                    wartość = c(x, x / 0.001, x * 0.001, x * 0.000001, x * 15.4323583529414, 
                                x * 0.03527396194958, x * 0.00220462262185, x * 1.102311310924e-6, 
                                x * 9.84206527611e-7, x * 5, x * 6.02214199e+23),
                    jednostka = c("g", "mg", "kg", "t", "gr", "oz", "lb", "ton_us", "ton_uk", "ct", "u")))
  print.data.frame(gram)
}

masa_kg = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(kilogram = data.frame(nazwa_jednostki = c("kilogram", "miligram", "gram", "tona", "gran", "uncja", "funt", "tona (USA)", 
                                            "tona (UK)", "karat", "masa atomowa"),
                        wartość = c(x, x * 1000000, x / 0.001, x * 0.001, x * 15432.3583529414, 
                                    x * 35.27396194958041, x * 2.20462262184878, x * 0.00110231131092, 
                                    x * 0.000984206527611, x * 5000, x * 6.02214199e+26),
                        jednostka = c("kg", "mg", "g", "t", "gr", "oz", "lb", "ton_us", "ton_uk", "ct", "u")))
  print.data.frame(kilogram)
}

masa_t = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(tona = data.frame(nazwa_jednostki = c("tona", "miligram", "gram", "kilogram", "tona", "uncja", "funt", "tona (USA)", 
                                        "tona (UK)", "karat", "masa atomowa"),
                    wartość = c(x, x * 1e+9, x / 0.000001, x * 1000, x * 15432358.3529414, 
                                x * 35273.9619495804, x * 2204.62262184878, x * 1.10231131092439, 
                                x * 0.984206527611061, x * 5000000, x * 6.02214199e+29),
                    jednostka = c("t", "mg", "g", "kg", "gr", "oz", "lb", "ton_us", "ton_uk", "ct", "u")))
  print.data.frame(tona)
}

masa_gr = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(gran = data.frame(nazwa_jednostki = c("gran", "miligram", "gram", "kilogram", "tona", "uncja", "funt", "tona (USA)", 
                                        "tona (UK)", "karat", "masa atomowa"),
                    wartość = c(x, x * 64.79891, x * 0.06479891, x * 6.479891e-5, x * 6.479891e-8, 
                                x * 0.00228571428571, x * 0.00014285714286, x * 7.142857142857e-8, 
                                x * 6.377551020408e-8, x * 0.32399455, x * 3.90228236817e+22),
                    jednostka = c("gr", "mg", "g", "kg", "t", "oz", "lb", "ton_us", "ton_uk", "ct", "u")))
  print.data.frame(gran)
}

masa_oz = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(uncja = data.frame(nazwa_jednostki = c("uncja", "miligram", "gram", "kilogram", "tona", "gran", "funt", "tona (USA)", 
                                         "tona (UK)", "karat", "masa atomowa"),
                     wartość = c(x, x * 28349.523125, x * 28.349523125, x * 0.028349523125, x * 2.8349523125e-5, 
                                 x * 437.5, x * 0.0625, x * 3.125e-5, 
                                 x * 2.790178571429e-5, x * 141.747615625, x * 1.70724853608e+25),
                     jednostka = c("oz", "mg", "g", "kg", "t", "gr", "lb", "ton_us", "ton_uk", "ct", "u")))
  print.data.frame(uncja)
}

masa_lb = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(funt = data.frame(nazwa_jednostki = c("funt", "miligram", "gram", "kilogram", "tona", "gran", "uncja", "tona (USA)", 
                                        "tona (UK)", "karat", "masa atomowa"),
                    wartość = c(x, x * 453592.37, x * 453.59237, x * 0.45359237, x * 0.00045359237, 
                                x * 700, x * 16, x * 0.0005, 
                                x * 0.00044642857143, x * 2267.96185, x * 2.73159765772e+26),
                    jednostka = c("lb", "mg", "g", "kg", "t", "gr", "oz", "ton_us", "ton_uk", "ct", "u")))
  print.data.frame(funt)
}

masa_ton_us = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(tona_USA = data.frame(nazwa_jednostki = c("tona (USA)", "miligram", "gram", "kilogram", "tona", "gran", "uncja", "funt", 
                                            "tona (UK)", "karat", "masa atomowa"),
                        wartość = c(x, x * 907184740, x * 907184.74, x * 907.18474, x * 0.90718474, 
                                    x * 1.4e+7, x * 32000, x * 2000, 
                                    x * 0.89285714285714, x * 4535923.7, x * 5.46319531544e+29),
                        jednostka = c("ton_us", "mg", "g", "kg", "t", "gr", "oz", "lb", "ton_uk", "ct", "u")))
  print.data.frame(tona_USA)
}

masa_ton_uk = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(tona_UK = data.frame(nazwa_jednostki = c("tona (UK)", "miligram", "gram", "kilogram", "tona", "gran", "uncja", "funt", 
                                           "tona (USA)", "karat", "masa atomowa"),
                       wartość = c(x, x * 101604908.8, x * 1016046.9088, x * 1016.0469088, x * 1.0160469088, 
                                   x * 15680000, x * 35840, x * 2240, 
                                   x * 1.12, x * 5080234.544, x * 6.11877875329e+29),
                       jednostka = c("ton_uk", "mg", "g", "kg", "t", "gr", "oz", "lb", "ton_us", "ct", "u")))
  print.data.frame(tona_UK)
}

masa_ct = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(karat = data.frame(nazwa_jednostki = c("karat", "miligram", "gram", "kilogram", "tona", "gran", "uncja", "funt", 
                                         "tona (USA)", "tona (UK)", "masa atomowa"),
                     wartość = c(x, x * 200, x * 0.2, x * 0.0002, x * 2e-7, 
                                 x * 3.08647167058829, x * 0.00705479238992, x * 0.00044092452437, 
                                 x * 2.204622621849e-7, x * 1.968413055222e-7, x * 1.204428398e+23),
                     jednostka = c("ct", "mg", "g", "kg", "t", "gr", "oz", "lb", "ton_us", "ton_uk", "u")))
  print.data.frame(karat)
}

masa_u = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(masa_atomowa = data.frame(nazwa_jednostki = c("masa atomowa", "miligram", "gram", "kilogram", "tona", "gran", "uncja", "funt", 
                                                "tona (USA)", "tona (UK)", "karat"),
                            wartość = c(x, x * 1.66053872801e-21, x * 1.66053872801e-24, x * 1.66053872801e-27, x * 1.66053872801e-30, 
                                        x * 2.56260287097e-23, x * 5.85737799078e-26, x * 3.66086124424e-27, 
                                        x * 1.83043062212e-30, x * 1.63431305546e-30, x * 8.30269364007e-24),
                            jednostka = c("u", "mg", "g", "kg", "t", "gr", "oz", "lb", "ton_us", "ton_uk", "ct")))
  print.data.frame(masa_atomowa)
}

#==========#
# OBJĘTOŚĆ #
#==========#

objetosc_ml = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(mililitr = data.frame(nazwa_jednostki = c("mililitr", "centymetr sześcienny", "litr", "metr sześcienny", "cal sześcienny", 
                                            "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", "galon (suchy, USA)", 
                                            "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)", 
                                            "pinta (USA)", "pinta (UK)", "baryłka"),
                        wartość = c(x, x, x * 0.001, x * 0.000001, x * 0.06102374409473, x * 3.531466672149e-5, 
                                    x * 1.307950619314e-6, x * 0.00026417205236, x * 0.00022702074607, 
                                    x * 0.0002199692483, x * 0.03381402270184, x * 0.03519507972785, 
                                    x * 0.001057082452431, x * 0.00087989441267, x * 0.00211416490486, 
                                    x * 0.001759788825341, x * 6.28981077154e-6),
                        jednostka = c("ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us", "gal_dry_us", 
                                      "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl")))
  print.data.frame(mililitr)
}

objetosc_cm3 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(cm3 = data.frame(nazwa_jednostki = c("centymetr sześcienny", "mililitr", "litr", "metr sześcienny", "cal sześcienny", 
                                       "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", "galon (suchy, USA)", 
                                       "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)", 
                                       "pinta (USA)", "pinta (UK)", "baryłka"),
                   wartość = c(x, x, x * 0.001, x * 0.000001, x * 0.06102374409473, x * 3.531466672149e-5, 
                               x * 1.307950619314e-6, x * 0.00026417205236, x * 0.00022702074607, 
                               x * 0.0002199692483, x * 0.03381402270184, x * 0.03519507972785, 
                               x * 0.001057082452431, x * 0.00087989441267, x * 0.00211416490486, 
                               x * 0.001759788825341, x * 6.28981077154e-6),
                   jednostka = c("cm^3", "ml", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us", "gal_dry_us", 
                                 "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl")))
  print.data.frame(cm3)
}

objetosc_L = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(litr = data.frame(nazwa_jednostki = c("litr", "mililitr", "centymetr sześcienny", "metr sześcienny", "cal sześcienny", 
                                        "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", "galon (suchy, USA)", 
                                        "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)", 
                                        "pinta (USA)", "pinta (UK)", "baryłka"),
                    wartość = c(x, x * 1000, x * 1000, x * 0.001, x * 61.0237440947323, x * 0.03531466672149, 
                                x * 0.00130795061931, x * 0.26417205235815, x * 0.22702074606721, 
                                x * 0.21996924829909, x * 33.814022701843, x * 35.195079727854, 
                                x * 1.05708245243129, x * 0.87989441267048, x * 2.11416490486258, 
                                x * 1.75978882534096, x * 0.00628981077154),
                    jednostka = c("L", "ml", "cm^3", "m^3", "in^3", "ft^3", "yd^3", "gal_us", "gal_dry_us", 
                                  "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl")))
  print.data.frame(litr)
}

objetosc_m3 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(m3 = data.frame(nazwa_jednostki = c("metr sześcienny", "mililitr", "centymetr sześcienny", "litr", "cal sześcienny", 
                                      "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", "galon (suchy, USA)", 
                                      "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)", 
                                      "pinta (USA)", "pinta (UK)", "baryłka"),
                  wartość = c(x, x * 1000000, x * 1000000, x * 1000, x * 61023.7440947323, x * 35.3146667214886, 
                              x * 1.30795061931439, x * 264.172052358148, x * 227.020746067214, 
                              x * 219.969248299088, x * 33814.022701843, x * 35195.0797278541, 
                              x * 1057.08245243129, x * 879.89441267048, x * 2114.16490486258, 
                              x * 1759.78882534096, x * 6.28981077153983),
                  jednostka = c("m^3", "ml", "cm^3", "L", "in^3", "ft^3", "yd^3", "gal_us", "gal_dry_us", 
                                "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl")))
  print.data.frame(m3)
}

objetosc_in3 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(in3 = data.frame(nazwa_jednostki = c("cal sześcienny", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
                                       "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", "galon (suchy, USA)", 
                                       "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)", 
                                       "pinta (USA)", "pinta (UK)", "baryłka"),
                   wartość = c(x, x * 16.387064, x * 16.387064, x * 0.016387064, x * 0.016387064e-5, x * 0.0005787037037, 
                               x * 2.143347050754e-5, x * 0.004329004329, x * 0.003720203495131, 
                               x * 0.00360465014991, x * 0.55411255411255, x * 0.57674402398545, 
                               x * 0.01732247780127, x * 0.01441888605367, x * 0.03464495560254, 
                               x * 0.02883777210735, x * 0.000103071531661),
                   jednostka = c("in^3", "ml", "cm^3", "L", "m^3", "ft^3", "yd^3", "gal_us", "gal_dry_us", 
                                 "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl")))
  print.data.frame(in3)
}

objetosc_ft3 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(ft3 = data.frame(nazwa_jednostki = c("stopa sześcienna", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
                                       "cal sześcienny", "jard sześcienny", "galon (płyn, USA)", "galon (suchy, USA)", 
                                       "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)", 
                                       "pinta (USA)", "pinta (UK)", "baryłka"),
                   wartość = c(x, x * 28316.846592, x * 28316.846592, x * 28.316846592, x * 0.028316846592, x * 0.1728, 
                               x * 0.03703703703704, x * 7.48051948051948, x * 6.42851163958669, 
                               x * 6.22883545904283, x * 957.506493506493, x * 996.6136734468521, 
                               x * 29.933241640592, x * 24.91583510074791, x * 59.8664832811839, 
                               x * 49.8316702014958, x * 0.17811076067104),
                   jednostka = c("ft^3", "ml", "cm^3", "L", "m^3", "in^3", "yd^3", "gal_us", "gal_dry_us", 
                                 "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl")))
  print.data.frame(ft3)
}

objetosc_yd3 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(yd3 = data.frame(nazwa_jednostki = c("jard sześcienny", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
                                       "cal sześcienny", "stopa sześcienna", "galon (płyn, USA)", "galon (suchy, USA)", 
                                       "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)", 
                                       "pinta (USA)", "pinta (UK)", "baryłka"),
                   wartość = c(x, x * 764554.857984, x * 764554.857984, x * 764.554857984, x * 0.764554857984, x * 46656, 
                               x * 27, x * 201.974025974026, x * 173.569814268841, 
                               x * 168.178557394156, x * 25852.6753246753, x * 26908.56918306501, 
                               x * 808.197524295983, x * 672.727547720194, x * 1616.39504859197, 
                               x * 1345.45509544039, x * 4.808905381180871),
                   jednostka = c("yd^3", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "gal_us", "gal_dry_us", 
                                 "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl")))
  print.data.frame(yd3)
}

objetosc_galon_us = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(galon_us = data.frame(nazwa_jednostki = c("galon (płyn, USA)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
                                            "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (suchy, USA)", 
                                            "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)", 
                                            "pinta (USA)", "pinta (UK)", "baryłka"),
                        wartość = c(x, x * 3785.411784, x * 3785.411784, x * 3.785411784, x * 0.003785411784, x * 231, 
                                    x * 0.13368055555556, x * 0.00495113168724, x * 0.8593670073753, 
                                    x * 0.83267418462899, x * 128, x * 133.227869540638, 
                                    x * 4.00149237209302, x * 3.33076267839859, x * 8.00298474418605, 
                                    x * 6.66152535679718, x * 0.02380952381372),
                        jednostka = c("gal_us", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_dry_us", 
                                      "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl")))
  print.data.frame(galon_us)
}

objetosc_galon_dry_us = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(galon_dry = data.frame(nazwa_jednostki = c("galon (suchy, USA)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
                                             "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", 
                                             "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)", 
                                             "pinta (USA)", "pinta (UK)", "baryłka"),
                         wartość = c(x, x * 4404.88377086, x * 4404.88377086, x * 4.40488377086, x * 0.00440488377086, 
                                     x * 268.8025, x * 0.15555700231481, x * 0.0057613704561, x * 1.16364718614719, 
                                     x * 0.96893897192093, x * 148.94683982684, x * 155.0302355073481, 
                                     x * 4.65632533917548, x * 3.87583261844259, x * 9.31265067835095, 
                                     x * 7.75166523688517, x * 0.02770588538934),
                         jednostka = c("gal_dry_us", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us", 
                                       "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl")))
  print.data.frame(galon_dry)
}

objetosc_galon_uk = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(galon_uk = data.frame(nazwa_jednostki = c("galon (UK)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
                                            "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", 
                                            "galon (suchy, USA)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)", 
                                            "pinta (USA)", "pinta (UK)", "baryłka"),
                        wartość = c(x, x * 4546.09, x * 4546.09, x * 4.54609, x * 0.00454609, 
                                    x * 277.419432791622, x * 0.16054365323589, x * 0.00594606123096, x * 1.20094992550486, 
                                    x * 1.032056743488701, x * 153.721590464621, x * 160, 
                                    x * 4.80559196617336, x * 4.00007919049714, x * 9.61118393234673, 
                                    x * 8.00015838099428, x * 0.02859404585039),
                        jednostka = c("gal_uk", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us", 
                                      "gal_dry_us", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl")))
  print.data.frame(galon_uk)
}

objetosc_uncja_us = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(uncja_us = data.frame(nazwa_jednostki = c("uncja (USA)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
                                            "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", 
                                            "galon (suchy, USA)", "galon (UK)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)", 
                                            "pinta (USA)", "pinta (UK)", "baryłka"),
                        wartość = c(x, x * 29.5735295625, x * 29.5735295625, x * 0.0295735295625, x * 2.95735295625e-5, 
                                    x * 1.8046875, x * 0.00104437934028, x * 3.868071630658e-5, x * 0.0078125, 
                                    x * 0.00671380474512, x * 0.00650526706741, x * 1.04084273078624, 
                                    x * 0.03126165915698, x * 0.02602158342499, x * 0.06252331831395, 
                                    x * 0.05204316684998, x * 0.00018601190479),
                        jednostka = c("oz_us", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us", 
                                      "gal_dry_us", "gal_uk", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl")))
  print.data.frame(uncja_us)
}

objetosc_uncja_uk = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(uncja_uk = data.frame(nazwa_jednostki = c("uncja (UK)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
                                            "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", 
                                            "galon (suchy, USA)", "galon (UK)", "uncja (USA)", "kwarta (USA)", "kwarta (UK)", 
                                            "pinta (USA)", "pinta (UK)", "baryłka"),
                        wartość = c(x, x * 28.4130625, x * 28.4130625, x * 0.0284130625, x * 2.84130625e-5, 
                                    x * 1.73387145494763, x * 0.00100339783272, x * 3.716288269349e-5, x * 0.00750593703441, 
                                    x * 0.0064503546468, x * 0.00625, x * 0.96075994040388, 
                                    x * 0.03003494978858, x * 0.02500049494061, x * 0.06006989957717, 
                                    x * 0.05000098988121, x * 0.00017871278656),
                        jednostka = c("oz_uk", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us", 
                                      "gal_dry_us", "gal_uk", "oz_us", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl")))
  print.data.frame(uncja_uk)
}

objetosc_kwarta_us = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(kwarta_us = data.frame(nazwa_jednostki = c("kwarta (USA)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
                                             "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", 
                                             "galon (suchy, USA)", "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (UK)", 
                                             "pinta (USA)", "pinta (UK)", "baryłka"),
                         wartość = c(x, x * 946, x * 946, x * 0.946, x * 0.000946, 
                                     x * 57.7284619136167, x * 0.03340767471853, x * 0.001237321285871, x * 0.24990676153081, 
                                     x * 0.21476162577958, x * 0.20809090889094, x * 31.9880654759435, 
                                     x * 33.2945454225499, x * 0.83238011438627, x * 2, 
                                     x * 1.66476022877255, x * 0.00595016098988),
                         jednostka = c("qt_us", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us", 
                                       "gal_dry_us", "gal_uk", "oz_us", "oz_uk", "qt_uk", "pt_us", "pt_uk", "bbl")))
  print.data.frame(kwarta_us)
}

objetosc_kwarta_uk = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(kwarta_uk = data.frame(nazwa_jednostki = c("kwarta (UK)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
                                             "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", 
                                             "galon (suchy, USA)", "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", 
                                             "pinta (USA)", "pinta (UK)", "baryłka"),
                         wartość = c(x, x * 1136.5, x * 1136.5, x * 1.1365, x * 0.0011365, 
                                     x * 69.3534851636633, x * 0.04013511872897, x * 0.001486485878851, x * 0.30023153750504, 
                                     x * 0.25800907790539, x * 0.24999505069191, x * 38.4296368006446, 
                                     x * 39.9992081107061, x * 1.201374207188161, x * 2.40274841437632, 
                                     x * 2, x * 0.00714836994186),
                         jednostka = c("qt_uk", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us", 
                                       "gal_dry_us", "gal_uk", "oz_us", "oz_uk", "qt_us", "pt_us", "pt_uk", "bbl")))
  print.data.frame(kwarta_uk)
}

objetosc_pinta_us = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(pinta_us = data.frame(nazwa_jednostki = c("pinta (USA)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
                                            "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", 
                                            "galon (suchy, USA)", "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", 
                                            "kwarta (UK)", "pinta (UK)", "baryłka"),
                        wartość = c(x, x * 473, x * 473, x * 0.473, x * 0.000473, 
                                    x * 28.8642309568084, x * 0.01670383735926, x * 0.00061866064294, x * 0.1249533807654, 
                                    x * 0.10738081288979, x * 0.10404545444547, x * 15.9940327379717, 
                                    x * 16.647272711275, x * 0.5, x * 0.41619005719314, 
                                    x * 0.83238011438627, x * 0.00297508049494),
                        jednostka = c("pt_us", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us", 
                                      "gal_dry_us", "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_uk", "bbl")))
  print.data.frame(pinta_us)
}

objetosc_pinta_uk = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(pinta_uk = data.frame(nazwa_jednostki = c("pinta (UK)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
                                            "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", 
                                            "galon (suchy, USA)", "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", 
                                            "kwarta (UK)", "pinta (USA)", "baryłka"),
                        wartość = c(x, x * 568.25, x * 568.25, x * 0.56825, x * 0.00056825, 
                                    x * 34.6767425818316, x * 0.02006755936449, x * 0.00074324293943, x * 0.15011576875252, 
                                    x * 0.12900453895269, x * 0.12499752534596, x * 19.2148184003223, 
                                    x * 19.9996040553531, x * 0.600687103594081, x * 0.5, 
                                    x * 1.201374207188161, x * 0.00357418497093),
                        jednostka = c("pt_uk", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us", 
                                      "gal_dry_us", "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "bbl")))
  print.data.frame(pinta_uk)
}

objetosc_barylka = function(x){
  barylka = data.frame(nazwa_jednostki = c("baryłka", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
                                           "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", 
                                           "galon (suchy, USA)", "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", 
                                           "kwarta (UK)", "pinta (USA)", "pinta (UK)"),
                       wartość = c(x, x * 158987.2949, x * 158987.2949, x * 158.9872949, x * 0.1589872949, 
                                   x * 9701.99999829134, x * 5.61458333234452, x * 0.20794753082757, x * 41.9999999926032, 
                                   x * 36.0934143034062, x * 34.9723157482584, x * 5375.99999905321, 
                                   x * 5595.57051972134, x * 168.062679598309, x * 139.892032468104, 
                                   x * 336.125359196617, x * 279.784064936208),
                       jednostka = c("bbl", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us", 
                                     "gal_dry_us", "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk"))
  print.data.frame(barylka)
}

#===========#
# CIŚNIENIE #
#===========#

cisnienie_atm = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(atm = data.frame(nazwa_jednostki = c("atmosfera", "paskal", "hektopaskal", "kilopaskal", "bar", 
                                       "milimetr słupa rtęci", "cal słupa rtęci", "milimetr słupa wody", 
                                       "cal słupa wody", "kilogramy na centymetr kwadratowy", 
                                       "funty na cal kwadratowy"),
                   wartość = c(x, x * 101325, x * 1013.25, x * 101.325, x * 1.01325, x * 760.002100178515, 
                               x * 29.9212612376373, x * 10339.28571428571, x * 406.785580941599, 
                               x * 1.03322745279989, x * 14.695950008681), 
                   jednostka = c("atm", "Pa", "hPa", "kPa", "bar", "mmHg", "inHg", "mmWg", "inWg", "kgf/cm^2", "psi")))
  print.data.frame(atm)
}

cisnienie_Pa = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(Pa = data.frame(nazwa_jednostki = c("paskal", "atmosfera", "hektopaskal", "kilopaskal", "bar", 
                                      "milimetr słupa rtęci", "cal słupa rtęci", "milimetr słupa wody", 
                                      "cal słupa wody", "kilogramy na centymetr kwadratowy", 
                                      "funty na cal kwadratowy"),
                  wartość = c(x, x * 9.86923266716e-6, x * 0.01, x * 0.001, x * 0.00001, x * 0.00750063755419, 
                              x * 0.00029529988885, x * 0.102040816326531, x * 0.00401466154396, 
                              x * 1.019716212978e-5, x * 0.000145037749901), 
                  jednostka = c("Pa", "atm", "hPa", "kPa", "bar", "mmHg", "inHg", "mmWg", "inWg", "kgf/cm^2", "psi")))
  print.data.frame(Pa)
}

cisnienie_hPa = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(hPa = data.frame(nazwa_jednostki = c("hektopaskal", "atmosfera", "paskal", "kilopaskal", "bar", 
                                       "milimetr słupa rtęci", "cal słupa rtęci", "milimetr słupa wody", 
                                       "cal słupa wody", "kilogramy na centymetr kwadratowy", 
                                       "funty na cal kwadratowy"),
                   wartość = c(x, x * 0.00098692326672, x * 100, x * 0.1, x * 0.001, x * 0.750063755419211, 
                               x * 0.02952998888491, x * 10.2040816326531, x * 0.40146615439585, 
                               x * 0.00101971621298, x * 0.01450377499006), 
                   jednostka = c("hPa", "atm", "Pa", "kPa", "bar", "mmHg", "inHg", "mmWg", "inWg", "kgf/cm^2", "psi")))
  print.data.frame(hPa)
}

cisnienie_kPa = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(kPa = data.frame(nazwa_jednostki = c("kilopaskal", "atmosfera", "paskal", "hektopaskal", "bar", 
                                       "milimetr słupa rtęci", "cal słupa rtęci", "milimetr słupa wody", 
                                       "cal słupa wody", "kilogramy na centymetr kwadratowy", 
                                       "funty na cal kwadratowy"),
                   wartość = c(x, x * 0.00986923266716, x * 1000, x * 10, x * 0.01, x * 7.50063755419211, 
                               x * 0.29529988884912, x * 102.040816326531, x * 4.01466154395854, 
                               x * 0.01019716212978, x * 0.14503774990063), 
                   jednostka = c("kPa", "atm", "Pa", "hPa", "bar", "mmHg", "inHg", "mmWg", "inWg", "kgf/cm^2", "psi")))
  print.data.frame(kPa)
}

cisnienie_bar = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(bar = data.frame(nazwa_jednostki = c("bar", "atmosfera", "paskal", "hektopaskal", "kilopaskal", 
                                       "milimetr słupa rtęci", "cal słupa rtęci", "milimetr słupa wody", 
                                       "cal słupa wody", "kilogramy na centymetr kwadratowy", 
                                       "funty na cal kwadratowy"),
                   wartość = c(x, x * 0.98692326671601, x * 100000, x * 1000, x * 100, x * 750.063755419211, 
                               x * 29.5299888849122, x * 10204.0816326531, x * 401.466154395854, 
                               x * 1.01971621297793, x * 14.50377499006271), 
                   jednostka = c("bar", "atm", "Pa", "hPa", "kPa", "mmHg", "inHg", "mmWg", "inWg", "kgf/cm^2", "psi")))
  print.data.frame(bar)
}

cisnienie_mmHg = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(mmHg = data.frame(nazwa_jednostki = c("milimetr słupa rtęci", "atmosfera", "paskal", "hektopaskal", "kilopaskal", 
                                        "bar", "cal słupa rtęci", "milimetr słupa wody", 
                                        "cal słupa wody", "kilogramy na centymetr kwadratowy", 
                                        "funty na cal kwadratowy"),
                    wartość = c(x, x * 0.001315785837651, x * 133.322, x * 1.33322, x * 0.133322, x * 0.00133322, 
                                x * 0.03936997178114, x * 13.60428571428571, x * 0.53524270636364, 
                                x * 0.00135950604947, x * 0.019336722892251), 
                    jednostka = c("mmHg", "atm", "Pa", "hPa", "kPa", "bar", "inHg", "mmWg", "inWg", "kgf/cm^2", "psi")))
  print.data.frame(mmHg)
}

cisnienie_inHg = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(inHg = data.frame(nazwa_jednostki = c("cal słupa rtęci", "atmosfera", "paskal", "hektopaskal", "kilopaskal", 
                                        "bar", "milimetr słupa rtęci", "milimetr słupa wody", 
                                        "cal słupa wody", "kilogramy na centymetr kwadratowy", 
                                        "funty na cal kwadratowy"),
                    wartość = c(x, x * 0.003342105107328, x * 3386.388, x * 33.86388, x * 3.386388, x * 0.03386388, 
                                x * 25.4000690058655, x * 345.549795918367, x * 13.5952016765227, 
                                x * 0.03453154747034, x * 0.49115409581048), 
                    jednostka = c("inHg", "atm", "Pa", "hPa", "kPa", "bar", "mmHg", "mmWg", "inWg", "kgf/cm^2", "psi")))
  print.data.frame(inHg)
}

cisnienie_mmWg = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(mmWg = data.frame(nazwa_jednostki = c("milimetr słupa wody", "atmosfera", "paskal", "hektopaskal", "kilopaskal", 
                                        "bar", "milimetr słupa rtęci", "cal słupa rtęci", 
                                        "cal słupa wody", "kilogramy na centymetr kwadratowy", 
                                        "funty na cal kwadratowy"),
                    wartość = c(x, x * 9.671848013817e-5, x * 9.8, x * 0.098, x * 0.0098, x * 0.000098, 
                                x * 0.07350624803108, x * 0.002893938910721, x * 0.03934368313079, 
                                x * 9.993218887184e-5, x * 0.00142136994903), 
                    jednostka = c("mmWg", "atm", "Pa", "hPa", "kPa", "bar", "mmHg", "inHg", "inWg", "kgf/cm^2", "psi")))
  print.data.frame(mmWg)
}

cisnienie_inWg = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(inWg = data.frame(nazwa_jednostki = c("cal słupa wody", "atmosfera", "paskal", "hektopaskal", "kilopaskal", 
                                        "bar", "milimetr słupa rtęci", "cal słupa rtęci", 
                                        "milimetr słupa wody", "kilogramy na centymetr kwadratowy", 
                                        "funty na cal kwadratowy"),
                    wartość = c(x, x * 0.002458299755736, x * 249.087, x * 2.49087, x * 0.249087, x * 0.00249087, 
                                x * 1.86831130646105, x * 0.073555363413761, x * 25.4170408163265, 
                                x * 0.00253998052342, x * 0.0361270180095), 
                    jednostka = c("inWg", "atm", "Pa", "hPa", "kPa", "bar", "mmHg", "inHg", "mmWg", "kgf/cm^2", "psi")))
  print.data.frame(inWg)
}

cisnienie_kgcm2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(kgcm2 = data.frame(nazwa_jednostki = c("kilogramy na centymetr kwadratowy", "atmosfera", "paskal", "hektopaskal", "kilopaskal", 
                                         "bar", "milimetr słupa rtęci", "cal słupa rtęci", 
                                         "milimetr słupa wody", "cal słupa wody", 
                                         "funty na cal kwadratowy"),
                     wartość = c(x, x * 0.96784110535406, x * 98066.5, x * 980.665, x * 98.0665, x * 0.980665, 
                                 x * 735.56127270818, x * 28.95902654982241, x * 10006.78571428571, 
                                 x * 393.70380630061, x * 14.2233445006299), 
                     jednostka = c("kgf/cm^2", "atm", "Pa", "hPa", "kPa", "bar", "mmHg", "inHg", "mmWg", "inWg", "psi")))
  print.data.frame(kgcm2)
}

cisnienie_psi = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(psi = data.frame(nazwa_jednostki = c("funty na cal kwadratowy", "atmosfera", "paskal", "hektopaskal", "kilopaskal", 
                                       "bar", "milimetr słupa rtęci", "cal słupa rtęci", 
                                       "milimetr słupa wody", "cal słupa wody", 
                                       "kilogramy na centymetr kwadratowy"),
                   wartość = c(x, x * 0.0680459582, x * 6894.756714615, x * 68.94756714615, x * 6.894756714615, 
                               x * 0.06894756714615, x * 51.7150711406595, x * 2.03602089146755, x * 703.546603532143, 
                               x * 27.6801146371147, x * 0.07030695206431), 
                   jednostka = c("psi", "atm", "Pa", "hPa", "kPa", "bar", "mmHg", "inHg", "mmWg", "inWg", "kgf/cm^2")))
  print.data.frame(psi)
}

#==============#
# POWIERZCHNIA #
#==============#
  
powierzchnia_mm2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    powierzchnia=data.frame(nazwa_jednostki = c("milimetr kwadrotowy", "centymetr kwadratowy", "metr kwadratowy",
                                                "kilometr kwadratowy", "ar", "stopa kwadratowa", " jard kwadratowy",
                                                "akr"),
                            wartość = c(x, x/100, x/1000000, x/1000000000000, x/100000000, x/92903, x/836100, x/4046856422.4),
                            jednostka = c("mm^2", "cm^2", "m^2", "km^2", "a", "ft^2", "yd^2", "acre"))
  print.data.frame(powierzchnia)
}

powierzchnia_cm2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    powierzchnia=data.frame(nazwa_jednostki = c( "centymetr kwadratowy", "milimetr kwadrotowy", "metr kwadratowy",
                                                 "kilometr kwadratowy", "ar", "stopa kwadratowa", " jard kwadratowy",
                                                 "akr"),
                            wartość = c(x, x*100, x*0.0001, x*1e-6, x*1e-10, x*0.001076391041671,
                                        x*0.00011959900463, x*2.471053814672e-8),
                            jednostka = c("cm^2", "mm^2", "m^2", "km^2", "a", "ft^2", "yd^2", "acre"))
  print.data.frame(powierzchnia)
}
powierzchnia_cm2(1)
powierzchnia_m2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    powierzchnia=data.frame(nazwa_jednostki = c("metr kwadratowy", "milimetr kwadrotowy", "centymetr kwadratowy", 
                                                "kilometr kwadratowy", "ar", "stopa kwadratowa", " jard kwadratowy",
                                                "akr"),
                            wartość = c(x, x/1000000, x/1000000*100, x/1000000*1000000000000, x/1000000*100000000,
                                        x/1000000*92903, x/1000000*836100, x/1000000*4046856422.4),
                            jednostka = c("m^2", "mm^2", "cm^2", "km^2", "a", "ft^2", "yd^2", "acre"))
  print.data.frame(powierzchnia)
}

powierzchnia_km2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    powierzchnia=data.frame(nazwa_jednostki = c("kilometr kwadratowy", "milimetr kwadrotowy", "centymetr kwadratowy",
                                                "metr kwadratowy", "ar", "stopa kwadratowa", " jard kwadratowy","akr"),
                            wartość = c(x, x*1000000000000, x*1000000000000*100, x*1000000000000*1000000,
                                        x*1000000000000*100000000, x*1000000000000*92903,
                                        x*1000000000000*836100, x*1000000000000*4046856422.4),
                            jednostka = c( "km^2", "mm^2", "cm^2", "m^2", "a", "ft^2", "yd^2", "acre"))
  print.data.frame(powierzchnia)
}

powierzchnia_a = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    powierzchnia=data.frame(nazwa_jednostki = c("ar", "milimetr kwadrotowy", "centymetr kwadratowy", "metr kwadratowy",
                                                "kilometr kwadratowy", "stopa kwadratowa", " jard kwadratowy",
                                                "akr"),
                            wartość = c(x, x*100000000, x*100000000*100, x*100000000*1000000, x*100000000*1000000000000,
                                        x*100000000*92903, x*100000000*836100, x*100000000*4046856422.4),
                            jednostka = c( "a", "mm^2", "cm^2", "m^2", "km^2", "ft^2", "yd^2", "acre"))
  print.data.frame(powierzchnia)
}

powierzchnia_ft2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    powierzchnia=data.frame(nazwa_jednostki = c("stopa kwadratowa", "milimetr kwadrotowy", "centymetr kwadratowy", 
                                                "metr kwadratowy", "kilometr kwadratowy", "ar", " jard kwadratowy", "akr"),
                            wartość = c(x, x/92903, x/92903*100, x/92903*1000000, x/92903*1000000000000,
                                        x/92903*100000000,  x/92903*836100,  x/92903*4046856422.4),
                            jednostka = c( "ft^2", "mm^2", "cm^2", "m^2", "km^2", "a", "yd^2", "acre"))
  print.data.frame(powierzchnia)
}

powierzchnia_yd2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    powierzchnia=data.frame(nazwa_jednostki = c("jard kwadratowy", "milimetr kwadrotowy", "centymetr kwadratowy",
                                                "metr kwadratowy", "kilometr kwadratowy", "ar", "stopa kwadratowa", "akr"),
                            wartość = c(x, x/836100, x/836100*100, x/836100*1000000, x/836100*1000000000000, x/836100*100000000,
                                        x/836100*92903, x/836100*4046856422.4),
                            jednostka = c( "yd^2", "mm^2", "cm^2", "m^2", "km^2", "a", "ft^2", "acre"))
  print.data.frame(powierzchnia)
}

powierzchnia_acre = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    powierzchnia=data.frame(nazwa_jednostki = c("akr","milimetr kwadrotowy", "centymetr kwadratowy", "metr kwadratowy",
                                                "kilometr kwadratowy", "ar", "stopa kwadratowa", " jard kwadratowy" ),
                            wartość = c(x, x*4046856422.4, x*4046856422.4*100, x*4046856422.4*1000000,
                                        x*4046856422.4*1000000000000, x*4046856422.4*100000000,
                                        x*4046856422.4*92903, x*4046856422.4*836100),
                            jednostka = c( "acre", "mm^2", "cm^2", "m^2", "km^2", "a", "ft^2", "yd^2"))
  print.data.frame(powierzchnia)
}


#=============#
# TEMPERATURA #
#=============#

temperatura_C= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    temperatury=data.frame(nazwa_jednostki = c("skala Celsjusza", "skala Kelvina",  "skala Fahrenheita",
                                               "skala Rankinea", "skala Reaumura", "skala Newtona"),
                           wartość = c(x, x+273.15, (x*1.8)+32, (x+ 273.15)*1.8, (x*4)/5, x/3),
                           jednostka = c("°C", "K", "°F", "°R", "°Ré", "°N"))
  print.data.frame(temperatury)
}

temperatura_K= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    temperatury=data.frame(nazwa_jednostki = c("skala Kelvina","skala Celsjusza",   "skala Fahrenheita",
                                               "skala Rankinea", "skala Reaumura", "skala Newtona"),
                           wartość = c(x, x-273.15, ((x-273.15)*1.8)+32, x*1.8, ((x-273.15)*4)/5, (x-273.15)/3),
                           jednostka = c( "K", "°C", "°F", "°R", "°Ré", "°N"))
  print.data.frame(temperatury)
}

temperatura_F= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    temperatury=data.frame(nazwa_jednostki = c("skala Fahrenheita", "skala Celsjusza", "skala Kelvina",  
                                               "skala Rankinea", "skala Reaumura", "skala Newtona"),
                           wartość = c(x, (x-32)/1.8, (x+459.67)/1.8, x+459.67, (x-32)*4/9, (x-32)*11/60),
                           jednostka = c("°F", "°C", "K",  "°R", "°Ré", "°N"))
  print.data.frame(temperatury)
}

temperatura_R= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    temperatury=data.frame(nazwa_jednostki = c("skala Rankinea", "skala Celsjusza", "skala Kelvina",  "skala Fahrenheita",
                                               "skala Reaumura", "skala Newtona"),
                           wartość = c(x, (x/1.8)-273.15, x/1.8, x-459.67, ((x/1.8)-273.15)*4/5, ((x/1.8)-273.15)/3),
                           jednostka = c("°R", "°C", "K", "°F", "°Ré", "°N"))
  print.data.frame(temperatury)
}

temperatura_Re= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    temperatury=data.frame(nazwa_jednostki = c("skala Reaumura", "skala Celsjusza", "skala Kelvina",  "skala Fahrenheita",
                                               "skala Rankinea",  "skala Newtona"),
                           wartość = c(x, x*1.25, x*1.25+273.15, x*2.25+32, (x*1.25+273.15)*1.8, x*1.25/3),
                           jednostka = c("°Ré", "°C", "K", "°F", "°R", "°N"))
  print.data.frame(temperatury)
}

temperatura_N= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    temperatury=data.frame(nazwa_jednostki = c("skala Newtona", "skala Celsjusza", "skala Kelvina",  "skala Fahrenheita",
                                               "skala Rankinea", "skala Reaumura"),
                           wartość = c(x, x*3, x*3+273.15, x*60/11-32, (x*3+273.15)*1.8, x*3*4/5),
                           jednostka = c( "°N", "°C", "K", "°F", "°R", "°Ré"))
  print.data.frame(temperatury)
}

#==========#
# PRĘDKOŚĆ #
#==========#

predkosc_ms= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    prędkość=data.frame(nazwa_jednostki = c("metry na sekundę", "metry na minutę"," kilometry na sekundę", 
                                            "kilometry na minutę","kilometry na godzinę", "stopy na sekundę",
                                            "mile na godzinę", " machy", "węzły"),
                        wartość = c(x, x*60, x*0.001, x*0.06, x*3.6, x*3.2808398950131, x*2.2369362920544,
                                    x*0.002938669957977, x*1.9438461717893),
                        jednostka = c("m/s", "m/min", "km/s", "km/min", "km/h", "ft/s", "mph", "Mach", "kn"))
    print.data.frame(prędkość)
}

predkosc_mmin= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    prędkość=data.frame(nazwa_jednostki = c("metry na minutę", "metry na sekundę", " kilometry na sekundę", 
                                            "kilometry na minutę","kilometry na godzinę", "stopy na sekundę",
                                            "mile na godzinę", " machy", "węzły"),
                        wartość = c(x, x/60, x/60*0.001, x*0.001, x*0.06, x*3.2808398950131/60, x*2.2369362920544/60, 
                                    x*0.002938669957977/60, x*1.9438461717893/60  ),
                        jednostka = c( "m/min", "m/s", "km/s", "km/min", "km/h", "ft/s", "mph", "Mach", "kn"))
    print.data.frame(prędkość)
}


predkosc_kms= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    prędkość=data.frame(nazwa_jednostki = c("kilometry na sekundę", "metry na sekundę", "metry na minutę",  
                                            "kilometry na minutę","kilometry na godzinę", "stopy na sekundę",
                                            "mile na godzinę", " machy", "węzły"),
                        wartość = c(x, x*1000, x*1000*60, x*1000*0.06, x*1000*3.6, x*1000*3.2808398950131, 
                                    x*2.2369362920544*1000, x*1000*0.002938669957977, x*1000*1.9438461717893 ),
                        jednostka = c("km/s", "m/s", "m/min",  "km/min", "km/h", "ft/s", "mph", "Mach", "kn"))
    print.data.frame(prędkość)
}

predkosc_kmmin= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    prędkość=data.frame(nazwa_jednostki = c("kilometry na minutę", "metry na sekundę", "metry na minutę"," kilometry na sekundę", 
                                            "kilometry na godzinę", "stopy na sekundę",
                                            "mile na godzinę", " machy", "węzły"),
                        wartość = c(x, x/0.06, x/0.06*60, x/0.06*0.001, x/0.06*3.6, x/0.06*3.2808398950131, x/0.06*2.2369362920544, 
                                    x/0.06*x*0.002938669957977, x*1.9438461717893/0.06 ),
                        jednostka = c("km/min", "m/s", "m/min", "km/s", "km/h", "ft/s", "mph", "Mach", "kn"))
    print.data.frame(prędkość)
    
}

predkosc_kmh= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    prędkość=data.frame(nazwa_jednostki = c("kilometry na godzinę", "metry na sekundę", "metry na minutę"," kilometry na sekundę", 
                                            "kilometry na minutę", "stopy na sekundę",
                                            "mile na godzinę", " machy", "węzły"),
                        wartość = c(x, x/3.6, x/3.6*60, x/3.6*0.001, x/3.6*0.06, x/3.6*3.2808398950131,
                                    x/3.6*2.2369362920544, x/3.6*0.002938669957977, x/3.6*1.9438461717893 ),
                        jednostka = c("km/h", "m/s", "m/min", "km/s", "km/min", "ft/s", "mph", "Mach", "kn"))
    print.data.frame(prędkość)
    
    
}

predkosc_fts= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    prędkość=data.frame(nazwa_jednostki = c("stopy na sekundę", "metry na sekundę", "metry na minutę"," kilometry na sekundę", 
                                            "kilometry na minutę","kilometry na godzinę",
                                            "mile na godzinę", " machy", "węzły"),
                        wartość = c(x, x/3.2808398950131, x/3.2808398950131*60, x/3.2808398950131*0.001, x/3.2808398950131*0.06, 
                                    x/3.2808398950131*3.6,   x/3.2808398950131*2.2369362920544, 
                                    x/3.2808398950131*0.002938669957977,  x/3.2808398950131*1.9438461717893),
                        jednostka = c("ft/s", "m/s", "m/min", "km/s", "km/min", "km/h", "mph", "Mach", "kn"))
    print.data.frame(prędkość)
    
    
}

predkosc_mph= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    prędkość=data.frame(nazwa_jednostki = c("mile na godzinę", "metry na sekundę", "metry na minutę"," kilometry na sekundę", 
                                            "kilometry na minutę","kilometry na godzinę", "stopy na sekundę",
                                            " machy", "węzły"),
                        wartość = c(x, x/2.2369362920544, x/2.2369362920544*60, x/2.2369362920544*0.001, 
                                    x/2.2369362920544*0.06, x/2.2369362920544*3.6, x/2.2369362920544*3.2808398950131, 
                                    x/2.2369362920544*0.002938669957977, x/2.2369362920544*1.9438461717893),
                        jednostka = c("mph", "m/s", "m/min", "km/s", "km/min", "km/h", "ft/s",  "Mach", "kn"))
    print.data.frame(prędkość)
    
    
}

predkosc_Mach= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    prędkość=data.frame(nazwa_jednostki = c(" machy", "metry na sekundę", "metry na minutę"," kilometry na sekundę", 
                                            "kilometry na minutę","kilometry na godzinę", "stopy na sekundę",
                                            "mile na godzinę", "węzły"),
                        wartość = c(x, x/0.002938669957977, x/0.002938669957977*60, x/0.002938669957977*0.001,
                                    x/0.002938669957977*0.06, x/0.002938669957977*3.6, x/0.002938669957977*3.2808398950131,
                                    x/0.002938669957977*2.2369362920544, x/0.002938669957977*1.9438461717893),
                        jednostka = c("Mach", "m/s", "m/min", "km/s", "km/min", "km/h", "ft/s", "mph", "kn"))
    print.data.frame(prędkość)
    
    
}

predkosc_kn= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    prędkość=data.frame(nazwa_jednostki = c("węzły", "metry na sekundę", "metry na minutę"," kilometry na sekundę", 
                                            "kilometry na minutę","kilometry na godzinę", "stopy na sekundę",
                                            "mile na godzinę", " machy"),
                        wartość = c(x, x/1.9438461717893, x/1.9438461717893*60, x/1.9438461717893*0.001, x/1.9438461717893*0.06,
                                    x*1.852, x/1.9438461717893*3.2808398950131, 
                                    x/1.9438461717893*2.2369362920544, x/1.9438461717893*0.002938669957977),
                        jednostka = c( "kn", "m/s", "m/min", "km/s", "km/min", "km/h", "ft/s", "mph", "Mach"))
    print.data.frame(prędkość)
    
    
}

#=========#
# DŁUGOŚĆ #
#=========#

dlugosc_mm= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c("milimetr", "centrymetr", "decymetr", "metr", "kilometr",
                                           "cal", "stopa", "jard", "mila", "mila morska", "lata swietlne"),
                       wartość = c(x, x/10, x/100, x/1000, x/1000000, x*0.03937007874016, x*0.00328083989501, x*0.00109361329834,
                                   x*6.213711922373e-7, x*5.399568034557e-7, x*1.05702341052e-19),
                       jednostka = c("mm", "cm", "dm", "m", "km", "in", "ft", "yd", "mi", "nmi", "ly"))
    print.data.frame(długość)
    
    
}

dlugosc_cm= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c("centrymetr", "milimetr","decymetr", "metr",
                                           "kilometr", "cal", "stopa", "jard", "mila", "mila morska", "lata swietlne"),
                       wartość = c(x, x*10, x*10/100, x*10/1000, x*10/1000000, x*0.39370078740157, x*0.032808398950131,
                                   x*0.0109361329838, x*6.213711922373e-6, x*5.399568034557e-6, x*1.05702341052e-18),
                       jednostka = c("cm", "mm",  "dm", "m", "km", "in", "ft", "yd", "mi", "nmi", "ly"))
    print.data.frame(długość)
    
    
}

dlugosc_dm= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c("decymetr", "milimetr", "centrymetr", "metr",
                                           "kilometr", "cal", "stopa", "jard", "mila", "mila morska", "lata swietlne"),
                       wartość = c(x, x*100, x*100/10, x*100/1000, x*100/1000000, x*3.93700787401575, x*0.32808398950131, x*0.10936132983371, 
                                   x*6.213711922373e-5, x*5.399568034557e-5, x*1.05702341052e-17),
                       jednostka = c( "dm", "mm", "cm", "m", "km", "in", "ft", "yd", "mi", "nmi", "ly"))
    print.data.frame(długość)
    
}

dlugosc_m= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c("metr", "milimetr", "centrymetr", "decymetr",
                                           "kilometr", "cal", "stopa", "jard", "mila", "mila morska", "lata swietlne"),
                       wartość = c(x, x*1000, x*1000/10, x*1000/100, x*1000/1000000, x*1000/25.4, x*1000/304.8, x*1000/914.4,
                                   x*0.00062137119223733, x*0.00053995680345572, x*1.05702341052e-16),
                       jednostka = c("m", "mm", "cm", "dm", "km", "in", "ft", "yd", "mi", "nmi", "ly"))
    print.data.frame(długość)
    
}

dlugosc_km= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c("kilometr", "milimetr", "centrymetr", "decymetr", "metr",
                                           "cal", "stopa", "jard", "mila", "mila morska", "lata swietlne"),
                       wartość = c(x, x*1000000, x*100000, x*10000, x*1000, x*1000000/25.4, 
                                   x*1000000/304.8, x*1000000/914.4, x*0.62137119223733, x*0.53995680345572, x*1.05702341052e-13),
                       jednostka = c( "km", "mm", "cm", "dm", "m", "in", "ft", "yd", "mi", "nmi", "ly"))
    print.data.frame(długość)
    
}

dlugosc_in= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c("cal", "milimetr", "centrymetr", "decymetr", "metr",
                                           "kilometr", "stopa", "jard", "mila", "mila morska", "lata swietlne"),
                       wartość = c(x, x*25.4, x*25.4/10, x*25.4/100, x*25.4/1000, x*25.4/1000000, x*0.0833333333333, x*0.02777777777778,
                                   x*1.57828282828283e-5, x*1.371490280778e-5, x*2.68483946273e-5),
                       jednostka = c( "in", "mm", "cm", "dm", "m", "km", "ft", "yd", "mi", "nmi", "ly"))
    print.data.frame(długość)
    
}

dlugosc_ft= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c( "stopa", "milimetr", "centrymetr", "decymetr", "metr",
                                            "kilometr", "cal", "jard", "mila", "mila morska", "lata swietlne"),
                       wartość = c(x, x*304.8, x*304.8/10, x*304.8/100, x*304.8/1000, x*304.8/1000000, x*12, x*0.3333333333333,
                                   x*0.00018939393939, x*0.00016457883369, x*3.22180735527e-17),
                       jednostka = c( "ft", "mm", "cm", "dm", "m", "km", "in", "yd", "mi", "nmi", "ly"))
    print.data.frame(długość)
    
}

dlugosc_yd= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c( "jard", "milimetr", "centrymetr", "decymetr", "metr",
                                            "kilometr", "cal", "stopa", "mila", "mila morska", "lata swietlne"),
                       wartość = c(x, x*914.4, x*914.4/10, x*914.4/100, x*914.4/1000, x*914.4/1000000, x*36, x*3, 
                                   x*0.00056818181818, x*0.00049373650108, x*9.66542206582e-17),
                       jednostka = c("yd", "mm", "cm", "dm", "m", "km", "in", "ft",  "mi", "nmi", "ly"))
    print.data.frame(długość)
    
}

dlugosc_mila= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c("mila", "milimetr", "centrymetr", "decymetr", "metr",
                                           "kilometr", "cal", "stopa", "jard", "mila morska", "lata swietlne"),
                       wartość = c(x, x*1609344, x*1609344/10, x*1609344/100, x*1609344/1000, x*1609344/1000000, 
                                   x*1609344/25.4, x*1609344/304.8, x*1609344/914.4,
                                   x*0.86897624190065, x*1609344/9460528405000020000),
                       jednostka = c( "mi", "mm", "cm", "dm", "m", "km", "in", "ft", "yd","nmi", "ly"))
    print.data.frame(długość)
    
}

dlugosc_nmi= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c( "mila morska", "milimetr", "centrymetr", "decymetr", "metr",
                                            "kilometr", "cal", "stopa", "jard", "mila", "lata swietlne"),
                       wartość = c(x, x * 1852000, x * 185200, x * 18520, x * 1852, x * 1.852, x * 72913.3858267717, 
                                   x * 6076.1154855643, x * 2025.37182852143, x * 1.15077944802354, x * 1.95760735629e-13),
                       jednostka = c( "nmi", "mm", "cm", "dm", "m", "km", "in", "ft", "yd", "mi", "ly"))
    print.data.frame(długość)
    
}

dlugosc_ly= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else(długość=data.frame(nazwa_jednostki = c( "lata swietlne", "milimetr", "centrymetr", "decymetr", "metr",
                                            "kilometr", "cal", "stopa", "jard", "mila", "mila morska"),
                       wartość = c(x, x*9460528405000020000, x*9460528405000020000/10, x*9460528405000020000/100, x*9460528405000020000/1000,
                                   x*9460528405000020000/1000000, x*9460528405000020000/25.4, x*9460528405000020000/304.8,
                                   x*9460528405000020000/914.4, x*5878499814210.01, x*5108276676565.88),
                       jednostka = c( "ly", "mm", "cm", "dm", "m", "km", "in", "ft", "yd", "mi", "nmi")))
    print.data.frame(długość)
    
}
