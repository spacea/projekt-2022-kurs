options("scipen"=999)

#======#
# MASA #
#======#

#' Konwersja masy (miligramy)
#'
#' @description Funkcja sluzaca do konwersji miligramow na pozostale jednostki masy
#'
#' @param x argument zawierajacy wartosc miligramow
#'
#' @return data frame z pozostalymi jednostkami masy i ich wartosci
#' @export
#'
#' @examples
#' masa_mg(20)
#' masa_mg(c(40, 95))
masa_mg = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    masa_mg = function(x){
      (miligram = data.frame(nazwa_jednostki = c("miligram", "gram", "kilogram", "tona", "gran", "uncja", "funt", "tona (USA)", "tona (UK)", "karat", "masa atomowa"),
                             wartosc = c(x, x * 0.001, x * 0.000001, x * 0.000000001, x * 0.015432358352941,
                                         x * 3.527396194958e-5, x * 2.204622621849e-6, x * 1.102311310924e-9,
                                         x * 9.84206527611e-10, x * 0.005, x * 6.02214199e+20),
                             jednostka = c("mg", "g", "kg", "t", "gr", "oz", "lb", "ton_us", "ton_uk", "ct", "u")))
    }
  lapply(x, masa_mg)
}

#' Konwersja masy (gramy)
#'
#' @description Funkcja sluzaca do konwersji gramow na pozostale jednostki masy
#'
#' @param x argument zawierajacy wartosc gramow
#'
#' @return data frame z pozostalymi jednostkami masy i ich wartosci
#' @export
#'
#' @examples
#' masa_g(20)
#' masa_g(c(40, 95))
masa_g = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    masa_g = function(x){
      (gram = data.frame(nazwa_jednostki = c("gram", "miligram", "kilogram", "tona", "gran", "uncja", "funt", "tona (USA)",
                                             "tona (UK)", "karat", "masa atomowa"),
                         wartosc = c(x, x / 0.001, x * 0.001, x * 0.000001, x * 15.4323583529414,
                                     x * 0.03527396194958, x * 0.00220462262185, x * 1.102311310924e-6,
                                     x * 9.84206527611e-7, x * 5, x * 6.02214199e+23),
                         jednostka = c("g", "mg", "kg", "t", "gr", "oz", "lb", "ton_us", "ton_uk", "ct", "u")))
    }
  lapply(x, masa_g)
}

#' Konwersja masy (kilogramy)
#'
#' @description Funkcja sluzaca do konwersji kilogramow na pozostale jednostki masy
#'
#' @param x argument zawierajacy wartosc kilogramow
#'
#' @return data frame z pozostalymi jednostkami masy i ich wartosci
#' @export
#'
#' @examples
#' masa_kg(20)
#' masa_kg(c(40, 95))
masa_kg = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    masa_kg = function(x){
      (kilogram = data.frame(nazwa_jednostki = c("kilogram", "miligram", "gram", "tona", "gran", "uncja", "funt", "tona (USA)",
                                                 "tona (UK)", "karat", "masa atomowa"),
                             wartosc = c(x, x * 1000000, x / 0.001, x * 0.001, x * 15432.3583529414,
                                         x * 35.27396194958041, x * 2.20462262184878, x * 0.00110231131092,
                                         x * 0.000984206527611, x * 5000, x * 6.02214199e+26),
                             jednostka = c("kg", "mg", "g", "t", "gr", "oz", "lb", "ton_us", "ton_uk", "ct", "u")))
    }
  lapply(x, masa_kg)
}
#' Konwersja masy (tony)
#'
#' @description Funkcja sluzaca do konwersji ton na pozostale jednostki masy
#'
#' @param x argument zawierajacy wartosc ton
#'
#' @return data frame z pozostalymi jednostkami masy i ich wartosci
#' @export
#'
#' @examples
#' masa_t(20)
#' masa_t(c(40, 95))
masa_t = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    masa_t = function(x){
      (tona = data.frame(nazwa_jednostki = c("tona", "miligram", "gram", "kilogram", "tona", "uncja", "funt", "tona (USA)",
                                             "tona (UK)", "karat", "masa atomowa"),
                         wartosc = c(x, x * 1e+9, x / 0.000001, x * 1000, x * 15432358.3529414,
                                     x * 35273.9619495804, x * 2204.62262184878, x * 1.10231131092439,
                                     x * 0.984206527611061, x * 5000000, x * 6.02214199e+29),
                         jednostka = c("t", "mg", "g", "kg", "gr", "oz", "lb", "ton_us", "ton_uk", "ct", "u")))
    }
  lapply(x, masa_t)
}

#' Konwersja masy (grany)
#'
#' @description Funkcja sluzaca do konwersji granow na pozostale jednostki masy
#'
#' @param x argument zawierajacy wartosc granow
#'
#' @return data frame z pozostalymi jednostkami masy i ich wartosci
#' @export
#'
#' @examples
#' masa_gr(20)
#' masa_gr(c(40, 95))
masa_gr = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    masa_gr = function(x){
      (gran = data.frame(nazwa_jednostki = c("gran", "miligram", "gram", "kilogram", "tona", "uncja", "funt", "tona (USA)",
                                             "tona (UK)", "karat", "masa atomowa"),
                         wartosc = c(x, x * 64.79891, x * 0.06479891, x * 6.479891e-5, x * 6.479891e-8,
                                     x * 0.00228571428571, x * 0.00014285714286, x * 7.142857142857e-8,
                                     x * 6.377551020408e-8, x * 0.32399455, x * 3.90228236817e+22),
                         jednostka = c("gr", "mg", "g", "kg", "t", "oz", "lb", "ton_us", "ton_uk", "ct", "u")))
    }
  lapply(x, masa_gr)
}

#' Konwersja masy (uncje)
#'
#' @description Funkcja sluzaca do konwersji uncji na pozostale jednostki masy
#'
#' @param x argument zawierajacy wartosc uncji
#'
#' @return data frame z pozostalymi jednostkami masy i ich wartosci
#' @export
#'
#' @examples
#' masa_oz(20)
#' masa_oz(c(40, 95))
masa_oz = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    masa_oz = function(x){
      (uncja = data.frame(nazwa_jednostki = c("uncja", "miligram", "gram", "kilogram", "tona", "gran", "funt", "tona (USA)",
                                              "tona (UK)", "karat", "masa atomowa"),
                          wartosc = c(x, x * 28349.523125, x * 28.349523125, x * 0.028349523125, x * 2.8349523125e-5,
                                      x * 437.5, x * 0.0625, x * 3.125e-5,
                                      x * 2.790178571429e-5, x * 141.747615625, x * 1.70724853608e+25),
                          jednostka = c("oz", "mg", "g", "kg", "t", "gr", "lb", "ton_us", "ton_uk", "ct", "u")))
    }
  lapply(x, masa_oz)
}

#' Konwersja masy (funty)
#'
#' @description Funkcja sluzaca do konwersji funtow na pozostale jednostki masy
#'
#' @param x argument zawierajacy wartosc funtow
#'
#' @return data frame z pozostalymi jednostkami masy i ich wartosci
#' @export
#'
#' @examples
#' masa_lb(20)
#' masa_lb(c(40, 95))
masa_lb = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    masa_lb = function(x){
      (funt = data.frame(nazwa_jednostki = c("funt", "miligram", "gram", "kilogram", "tona", "gran", "uncja", "tona (USA)",
                                             "tona (UK)", "karat", "masa atomowa"),
                         wartosc = c(x, x * 453592.37, x * 453.59237, x * 0.45359237, x * 0.00045359237,
                                     x * 700, x * 16, x * 0.0005,
                                     x * 0.00044642857143, x * 2267.96185, x * 2.73159765772e+26),
                         jednostka = c("lb", "mg", "g", "kg", "t", "gr", "oz", "ton_us", "ton_uk", "ct", "u")))
    }
  lapply(x, masa_lb)
}

#' Konwersja masy (tona amerykanska)
#'
#' @description Funkcja sluzaca do konwersji ton amerykanskich na pozostale jednostki masy
#'
#' @param x argument zawierajacy wartosc ton amerykanskich
#'
#' @return data frame z pozostalymi jednostkami masy i ich wartosci
#' @export
#'
#' @examples
#' masa_ton_us(20)
#' masa_ton_us(c(40, 95))
masa_ton_us = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    masa_ton_us = function(x){
      (tona_USA = data.frame(nazwa_jednostki = c("tona (USA)", "miligram", "gram", "kilogram", "tona", "gran", "uncja", "funt",
                                                 "tona (UK)", "karat", "masa atomowa"),
                             wartosc = c(x, x * 907184740, x * 907184.74, x * 907.18474, x * 0.90718474,
                                         x * 1.4e+7, x * 32000, x * 2000,
                                         x * 0.89285714285714, x * 4535923.7, x * 5.46319531544e+29),
                             jednostka = c("ton_us", "mg", "g", "kg", "t", "gr", "oz", "lb", "ton_uk", "ct", "u")))
    }
  lapply(x, masa_ton_us)
}

#' Konwersja masy (tony brytyjskie)
#'
#' @description Funkcja sluzaca do konwersji ton brytyjskich na pozostale jednostki masy
#'
#' @param x argument zawierajacy wartosc ton brytyjskich
#'
#' @return data frame z pozostalymi jednostkami masy i ich wartosci
#' @export
#'
#' @examples
#' masa_ton_uk(20)
#' masa_ton_uk(c(40, 95))
masa_ton_uk = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    masa_ton_uk = function(x){
      (tona_UK = data.frame(nazwa_jednostki = c("tona (UK)", "miligram", "gram", "kilogram", "tona", "gran", "uncja", "funt",
                                                "tona (USA)", "karat", "masa atomowa"),
                            wartosc = c(x, x * 101604908.8, x * 1016046.9088, x * 1016.0469088, x * 1.0160469088,
                                        x * 15680000, x * 35840, x * 2240,
                                        x * 1.12, x * 5080234.544, x * 6.11877875329e+29),
                            jednostka = c("ton_uk", "mg", "g", "kg", "t", "gr", "oz", "lb", "ton_us", "ct", "u")))
    }
  lapply(x, masa_ton_uk)
}

#' Konwersja masy (karaty)
#'
#' @description Funkcja sluzaca do konwersji karatow na pozostale jednostki masy
#'
#' @param x argument zawierajacy wartosc karatow
#'
#' @return data frame z pozostalymi jednostkami masy i ich wartosci
#' @export
#'
#' @examples
#' masa_ct(20)
#' masa_ct(c(40, 95))
masa_ct = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    masa_ct = function(x){
      (karat = data.frame(nazwa_jednostki = c("karat", "miligram", "gram", "kilogram", "tona", "gran", "uncja", "funt",
                                              "tona (USA)", "tona (UK)", "masa atomowa"),
                          wartosc = c(x, x * 200, x * 0.2, x * 0.0002, x * 2e-7,
                                      x * 3.08647167058829, x * 0.00705479238992, x * 0.00044092452437,
                                      x * 2.204622621849e-7, x * 1.968413055222e-7, x * 1.204428398e+23),
                          jednostka = c("ct", "mg", "g", "kg", "t", "gr", "oz", "lb", "ton_us", "ton_uk", "u")))
    }
  lapply(x, masa_ct)
}

#' Konwersja masy (masa atomowa)
#'
#' @description Funkcja sluzaca do konwersji masy atomowej na pozostale jednostki masy
#'
#' @param x argument zawierajacy wartosc masy atomowej
#'
#' @return data frame z pozostalymi jednostkami masy i ich wartosci
#' @export
#'
#' @examples
#' masa_u(20)
#' masa_u(c(40, 95))
masa_u = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    masa_u = function(x){
      (masa_atomowa = data.frame(nazwa_jednostki = c("masa atomowa", "miligram", "gram", "kilogram", "tona", "gran", "uncja", "funt",
                                                     "tona (USA)", "tona (UK)", "karat"),
                                 wartosc = c(x, x * 1.66053872801e-21, x * 1.66053872801e-24, x * 1.66053872801e-27, x * 1.66053872801e-30,
                                             x * 2.56260287097e-23, x * 5.85737799078e-26, x * 3.66086124424e-27,
                                             x * 1.83043062212e-30, x * 1.63431305546e-30, x * 8.30269364007e-24),
                                 jednostka = c("u", "mg", "g", "kg", "t", "gr", "oz", "lb", "ton_us", "ton_uk", "ct")))
    }
  lapply(x, masa_u)
}

#==========#
# OBJĘTOŚĆ #
#==========#

#' Konwersja objetosci (mililitry)
#'
#' @description Funkcja sluzaca do konwersji mililitrow na pozostale jednostki objetosci
#'
#' @param x argument zawierajacy wartosc mililitrow
#'
#' @return data frame z pozostalymi jednostkami objetosci i ich wartosci
#' @export
#'
#' @examples
#' objetosc_ml(20)
#' objetosc_ml(c(40, 95))
objetosc_ml = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_ml=function(x){
      mililitr = data.frame(nazwa_jednostki = c("mililitr", "centymetr sześcienny", "litr", "metr sześcienny", "cal sześcienny",
                                                "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", "galon (suchy, USA)",
                                                "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)",
                                                "pinta (USA)", "pinta (UK)", "baryłka"),
                            wartosc = c(x, x, x * 0.001, x * 0.000001, x * 0.06102374409473, x * 3.531466672149e-5,
                                        x * 1.307950619314e-6, x * 0.00026417205236, x * 0.00022702074607,
                                        x * 0.0002199692483, x * 0.03381402270184, x * 0.03519507972785,
                                        x * 0.001057082452431, x * 0.00087989441267, x * 0.00211416490486,
                                        x * 0.001759788825341, x * 6.28981077154e-6),
                            jednostka = c("ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us", "gal_dry_us",
                                          "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl"))
    }
  lapply(x,objetosc_ml)
}


#' Konwersja objetosci (centymetry szescienne)
#'
#' @description Funkcja sluzaca do konwersji centymetrow szesciennych na pozostale jednostki objetosci
#'
#' @param x argument zawierajacy wartosc centymetrow szesciennych
#'
#' @return data frame z pozostalymi jednostkami objetosci i ich wartosci
#' @export
#'
#' @examples
#' objetosc_cm3(20)
#' objetosc_cm3(c(40, 95))
objetosc_cm3 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_cm3=function(x){
      cm3 = data.frame(nazwa_jednostki = c("centymetr sześcienny", "mililitr", "litr", "metr sześcienny", "cal sześcienny",
                                           "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", "galon (suchy, USA)",
                                           "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)",
                                           "pinta (USA)", "pinta (UK)", "baryłka"),
                       wartosc = c(x, x, x * 0.001, x * 0.000001, x * 0.06102374409473, x * 3.531466672149e-5,
                                   x * 1.307950619314e-6, x * 0.00026417205236, x * 0.00022702074607,
                                   x * 0.0002199692483, x * 0.03381402270184, x * 0.03519507972785,
                                   x * 0.001057082452431, x * 0.00087989441267, x * 0.00211416490486,
                                   x * 0.001759788825341, x * 6.28981077154e-6),
                       jednostka = c("cm^3", "ml", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us", "gal_dry_us",
                                     "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl"))
    }
  lapply(x,objetosc_cm3)
}

#' Konwersja objetosci (litr)
#'
#' @description Funkcja sluzaca do konwersji litrow na pozostale jednostki objetosci
#'
#' @param x argument zawierajacy wartosc litrow
#'
#' @return data frame z pozostalymi jednostkami objetosci i ich wartosci
#' @export
#'
#' @examples
#' objetosc_L(20)
#' objetosc_L(c(40, 95))
objetosc_L = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_L=function(x){
      litr = data.frame(nazwa_jednostki = c("litr", "mililitr", "centymetr sześcienny", "metr sześcienny", "cal sześcienny",
                                            "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", "galon (suchy, USA)",
                                            "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)",
                                            "pinta (USA)", "pinta (UK)", "baryłka"),
                        wartosc = c(x, x * 1000, x * 1000, x * 0.001, x * 61.0237440947323, x * 0.03531466672149,
                                    x * 0.00130795061931, x * 0.26417205235815, x * 0.22702074606721,
                                    x * 0.21996924829909, x * 33.814022701843, x * 35.195079727854,
                                    x * 1.05708245243129, x * 0.87989441267048, x * 2.11416490486258,
                                    x * 1.75978882534096, x * 0.00628981077154),
                        jednostka = c("L", "ml", "cm^3", "m^3", "in^3", "ft^3", "yd^3", "gal_us", "gal_dry_us",
                                      "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl"))
    }
  lapply(x,objetosc_L)
}

#' Konwersja objetosci (metry szescienne)
#'
#' @description Funkcja sluzaca do konwersji metrow szesciennych na pozostale jednostki objetosci
#'
#' @param x argument zawierajacy wartosc metrow szesciennych
#'
#' @return data frame z pozostalymi jednostkami objetosci i ich wartosci
#' @export
#'
#' @examples
#' objetosc_m3(20)
#' objetosc_m3(c(40, 95))
objetosc_m3 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_m3=function(x){
      m3 = data.frame(nazwa_jednostki = c("metr sześcienny", "mililitr", "centymetr sześcienny", "litr", "cal sześcienny",
                                          "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", "galon (suchy, USA)",
                                          "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)",
                                          "pinta (USA)", "pinta (UK)", "baryłka"),
                      wartosc = c(x, x * 1000000, x * 1000000, x * 1000, x * 61023.7440947323, x * 35.3146667214886,
                                  x * 1.30795061931439, x * 264.172052358148, x * 227.020746067214,
                                  x * 219.969248299088, x * 33814.022701843, x * 35195.0797278541,
                                  x * 1057.08245243129, x * 879.89441267048, x * 2114.16490486258,
                                  x * 1759.78882534096, x * 6.28981077153983),
                      jednostka = c("m^3", "ml", "cm^3", "L", "in^3", "ft^3", "yd^3", "gal_us", "gal_dry_us",
                                    "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl"))
    }
  lapply(x,objetosc_m3)
}

#' Konwersja objetosci (cale szescienne)
#'
#' @description Funkcja sluzaca do konwersji cali szesciennych na pozostale jednostki objetosci
#'
#' @param x argument zawierajacy wartosc cali szesciennych
#'
#' @return data frame z pozostalymi jednostkami objetosci i ich wartosci
#' @export
#'
#' @examples
#' objetosc_in3(20)
#' objetosc_in3(c(40, 95))
objetosc_in3 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_in3=function(x){
      in3 = data.frame(nazwa_jednostki = c("cal sześcienny", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny",
                                           "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)", "galon (suchy, USA)",
                                           "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)",
                                           "pinta (USA)", "pinta (UK)", "baryłka"),
                       wartosc = c(x, x * 16.387064, x * 16.387064, x * 0.016387064, x * 0.016387064e-5, x * 0.0005787037037,
                                   x * 2.143347050754e-5, x * 0.004329004329, x * 0.003720203495131,
                                   x * 0.00360465014991, x * 0.55411255411255, x * 0.57674402398545,
                                   x * 0.01732247780127, x * 0.01441888605367, x * 0.03464495560254,
                                   x * 0.02883777210735, x * 0.000103071531661),
                       jednostka = c("in^3", "ml", "cm^3", "L", "m^3", "ft^3", "yd^3", "gal_us", "gal_dry_us",
                                     "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl"))
    }
  lapply(x,objetosc_in3)
}

#' Konwersja objetosci (stopy szescienne)
#'
#' @description Funkcja sluzaca do konwersji stop szesciennych na pozostale jednostki objetosci
#'
#' @param x argument zawierajacy wartosc stop szesciennych
#'
#' @return data frame z pozostalymi jednostkami objetosci i ich wartosci
#' @export
#'
#' @examples
#' objetosc_ft3(20)
#' objetosc_ft3(c(40, 95))
objetosc_ft3 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_ft3=function(x){
      ft3 = data.frame(nazwa_jednostki = c("stopa sześcienna", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny",
                                           "cal sześcienny", "jard sześcienny", "galon (płyn, USA)", "galon (suchy, USA)",
                                           "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)",
                                           "pinta (USA)", "pinta (UK)", "baryłka"),
                       wartosc = c(x, x * 28316.846592, x * 28316.846592, x * 28.316846592, x * 0.028316846592, x * 0.1728,
                                   x * 0.03703703703704, x * 7.48051948051948, x * 6.42851163958669,
                                   x * 6.22883545904283, x * 957.506493506493, x * 996.6136734468521,
                                   x * 29.933241640592, x * 24.91583510074791, x * 59.8664832811839,
                                   x * 49.8316702014958, x * 0.17811076067104),
                       jednostka = c("ft^3", "ml", "cm^3", "L", "m^3", "in^3", "yd^3", "gal_us", "gal_dry_us",
                                     "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl"))
    }
  lapply(x,objetosc_ft3)
}

#' Konwersja objetosci (jardy szescienne)
#'
#' @description Funkcja sluzaca do konwersji jardow szesciennych na pozostale jednostki objetosci
#'
#' @param x argument zawierajacy wartosc jardow szesciennych
#'
#' @return data frame z pozostalymi jednostkami objetosci i ich wartosci
#' @export
#'
#' @examples
#' objetosc_yd3(20)
#' objetosc_yd3(c(40, 95))
objetosc_yd3 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_yd3 = function(x){
      yd3 = data.frame(nazwa_jednostki = c("jard sześcienny", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny",
                                           "cal sześcienny", "stopa sześcienna", "galon (płyn, USA)", "galon (suchy, USA)",
                                           "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)",
                                           "pinta (USA)", "pinta (UK)", "baryłka"),
                       wartosc = c(x, x * 764554.857984, x * 764554.857984, x * 764.554857984, x * 0.764554857984, x * 46656,
                                   x * 27, x * 201.974025974026, x * 173.569814268841,
                                   x * 168.178557394156, x * 25852.6753246753, x * 26908.56918306501,
                                   x * 808.197524295983, x * 672.727547720194, x * 1616.39504859197,
                                   x * 1345.45509544039, x * 4.808905381180871),
                       jednostka = c("yd^3", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "gal_us", "gal_dry_us",
                                     "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl"))
    }
  lapply(x,objetosc_yd3)
}

#' Konwersja objetosci (galon plynny amerykanski)
#'
#' @description Funkcja sluzaca do konwersji galonu plynnego, amerykanskiego na pozostale jednostki objetosci
#'
#' @param x argument zawierajacy wartosc galonow plynnych, amerykanskich
#'
#' @return data frame z pozostalymi jednostkami objetosci i ich wartosci
#' @export
#'
#' @examples
#' objetosc_galon_us(20)
#' objetosc_galon_us(c(40, 95))
objetosc_galon_us = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_galon_us = function(x){
      galon_us = data.frame(nazwa_jednostki = c("galon (płyn, USA)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny",
                                                "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (suchy, USA)",
                                                "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)",
                                                "pinta (USA)", "pinta (UK)", "baryłka"),
                            wartosc = c(x, x * 3785.411784, x * 3785.411784, x * 3.785411784, x * 0.003785411784, x * 231,
                                        x * 0.13368055555556, x * 0.00495113168724, x * 0.8593670073753,
                                        x * 0.83267418462899, x * 128, x * 133.227869540638,
                                        x * 4.00149237209302, x * 3.33076267839859, x * 8.00298474418605,
                                        x * 6.66152535679718, x * 0.02380952381372),
                            jednostka = c("gal_us", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_dry_us",
                                          "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl"))
    }
  lapply(x,objetosc_galon_us)
}

#' Konwersja objetosci (galon suchy amerykanski)
#'
#' @description Funkcja sluzaca do konwersji galonu suchego, amerykanskiego na pozostale jednostki objetosci
#'
#' @param x argument zawierajacy wartosc galonow suchych, amerykanskich
#'
#' @return data frame z pozostalymi jednostkami objetosci i ich wartosci
#' @export
#'
#' @examples
#' objetosc_galon_dry_us(20)
#' objetosc_galon_dry_us(c(40, 95))
objetosc_galon_dry_us = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_galon_dry_us = function(x){
      galon_dry = data.frame(nazwa_jednostki = c("galon (suchy, USA)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny",
                                                 "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)",
                                                 "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)",
                                                 "pinta (USA)", "pinta (UK)", "baryłka"),
                             wartosc = c(x, x * 4404.88377086, x * 4404.88377086, x * 4.40488377086, x * 0.00440488377086,
                                         x * 268.8025, x * 0.15555700231481, x * 0.0057613704561, x * 1.16364718614719,
                                         x * 0.96893897192093, x * 148.94683982684, x * 155.0302355073481,
                                         x * 4.65632533917548, x * 3.87583261844259, x * 9.31265067835095,
                                         x * 7.75166523688517, x * 0.02770588538934),
                             jednostka = c("gal_dry_us", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us",
                                           "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl"))
    }
  lapply(x,objetosc_galon_dry_us)
}

#' Konwersja objetosci (galon brytyjski)
#'
#' @description Funkcja sluzaca do konwersji galonow brytyjskich na pozostale jednostki objetosci
#'
#' @param x argument zawierajacy wartosc galonow brytyjskich
#'
#' @return data frame z pozostalymi jednostkami objetosci i ich wartosci
#' @export
#'
#' @examples
#' objetosc_galon_uk(20)
#' objetosc_galon_uk(c(40, 95))
objetosc_galon_uk = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_galon_uk = function(x){
      galon_uk = data.frame(nazwa_jednostki = c("galon (UK)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny",
                                                "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)",
                                                "galon (suchy, USA)", "uncja (USA)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)",
                                                "pinta (USA)", "pinta (UK)", "baryłka"),
                            wartosc = c(x, x * 4546.09, x * 4546.09, x * 4.54609, x * 0.00454609,
                                        x * 277.419432791622, x * 0.16054365323589, x * 0.00594606123096, x * 1.20094992550486,
                                        x * 1.032056743488701, x * 153.721590464621, x * 160,
                                        x * 4.80559196617336, x * 4.00007919049714, x * 9.61118393234673,
                                        x * 8.00015838099428, x * 0.02859404585039),
                            jednostka = c("gal_uk", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us",
                                          "gal_dry_us", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl"))
    }
  lapply(x,objetosc_galon_uk)
}

#' Konwersja objetosci (uncje amerykanskie)
#'
#' @description Funkcja sluzaca do konwersji uncji amerykanskich na pozostale jednostki objetosci
#'
#' @param x argument zawierajacy wartosc uncji amerykanskich
#'
#' @return data frame z pozostalymi jednostkami objetosci i ich wartosci
#' @export
#'
#' @examples
#' objetosc_uncja_us(20)
#' objetosc_uncja_us(c(40, 95))
objetosc_uncja_us = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_uncja_us = function(x){
      uncja_us = data.frame(nazwa_jednostki = c("uncja (USA)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny",
                                                "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)",
                                                "galon (suchy, USA)", "galon (UK)", "uncja (UK)", "kwarta (USA)", "kwarta (UK)",
                                                "pinta (USA)", "pinta (UK)", "baryłka"),
                            wartosc = c(x, x * 29.5735295625, x * 29.5735295625, x * 0.0295735295625, x * 2.95735295625e-5,
                                        x * 1.8046875, x * 0.00104437934028, x * 3.868071630658e-5, x * 0.0078125,
                                        x * 0.00671380474512, x * 0.00650526706741, x * 1.04084273078624,
                                        x * 0.03126165915698, x * 0.02602158342499, x * 0.06252331831395,
                                        x * 0.05204316684998, x * 0.00018601190479),
                            jednostka = c("oz_us", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us",
                                          "gal_dry_us", "gal_uk", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl"))
    }
  lapply(x,objetosc_uncja_us)
}

#' Konwersja objetosci (uncja brytyjska)
#'
#' @description Funkcja sluzaca do konwersji uncji brytyjskich na pozostale jednostki objetosci
#'
#' @param x argument zawierajacy wartosc uncji brytyjskich
#'
#' @return data frame z pozostalymi jednostkami objetosci i ich wartosci
#' @export
#'
#' @examples
#' objetosc_uncja_uk(20)
#' objetosc_uncja_uk(c(40, 95))
objetosc_uncja_uk = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_uncja_uk = function(x){
      uncja_uk = data.frame(nazwa_jednostki = c("uncja (UK)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny",
                                                "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)",
                                                "galon (suchy, USA)", "galon (UK)", "uncja (USA)", "kwarta (USA)", "kwarta (UK)",
                                                "pinta (USA)", "pinta (UK)", "baryłka"),
                            wartosc = c(x, x * 28.4130625, x * 28.4130625, x * 0.0284130625, x * 2.84130625e-5,
                                        x * 1.73387145494763, x * 0.00100339783272, x * 3.716288269349e-5, x * 0.00750593703441,
                                        x * 0.0064503546468, x * 0.00625, x * 0.96075994040388,
                                        x * 0.03003494978858, x * 0.02500049494061, x * 0.06006989957717,
                                        x * 0.05000098988121, x * 0.00017871278656),
                            jednostka = c("oz_uk", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us",
                                          "gal_dry_us", "gal_uk", "oz_us", "qt_us", "qt_uk", "pt_us", "pt_uk", "bbl"))
    }
  lapply(x,objetosc_uncja_uk)
}

#' Konwersja objetosci (kwarta amerykanska)
#'
#' @description Funkcja sluzaca do konwersji kwarty amerykanskiej na pozostale jednostki objetosci
#'
#' @param x argument zawierajacy wartosc kwarty amerykanskiej
#'
#' @return data frame z pozostalymi jednostkami objetosci i ich wartosci
#' @export
#'
#' @examples
#' objetosc_kwarta_us(20)
#' objetosc_kwarta_us(c(40, 95))
objetosc_kwarta_us = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_kwarta_us = function(x){
      kwarta_us = data.frame(nazwa_jednostki = c("kwarta (USA)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny",
                                                 "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)",
                                                 "galon (suchy, USA)", "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (UK)",
                                                 "pinta (USA)", "pinta (UK)", "baryłka"),
                             wartosc = c(x, x * 946, x * 946, x * 0.946, x * 0.000946,
                                         x * 57.7284619136167, x * 0.03340767471853, x * 0.001237321285871, x * 0.24990676153081,
                                         x * 0.21476162577958, x * 0.20809090889094, x * 31.9880654759435,
                                         x * 33.2945454225499, x * 0.83238011438627, x * 2,
                                         x * 1.66476022877255, x * 0.00595016098988),
                             jednostka = c("qt_us", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us",
                                           "gal_dry_us", "gal_uk", "oz_us", "oz_uk", "qt_uk", "pt_us", "pt_uk", "bbl"))
    }
  lapply(x,objetosc_kwarta_us)
}

#' Konwersja objetosci (kwarta brytyjska)
#'
#' @description Funkcja sluzaca do konwersji kwarty brytyjskiej na pozostale jednostki objetosci
#'
#' @param x argument zawierajacy wartosc kwarty brytyjskiej
#'
#' @return data frame z pozostalymi jednostkami objetosci i ich wartosci
#' @export
#'
#' @examples
#' objetosc_kwarta_uk(20)
#' objetosc_kwarta_uk(c(40, 95))
objetosc_kwarta_uk = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_kwarta_uk = function(x){
      kwarta_uk = data.frame(nazwa_jednostki = c("kwarta (UK)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny",
                                                 "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)",
                                                 "galon (suchy, USA)", "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)",
                                                 "pinta (USA)", "pinta (UK)", "baryłka"),
                             wartosc = c(x, x * 1136.5, x * 1136.5, x * 1.1365, x * 0.0011365,
                                         x * 69.3534851636633, x * 0.04013511872897, x * 0.001486485878851, x * 0.30023153750504,
                                         x * 0.25800907790539, x * 0.24999505069191, x * 38.4296368006446,
                                         x * 39.9992081107061, x * 1.201374207188161, x * 2.40274841437632,
                                         x * 2, x * 0.00714836994186),
                             jednostka = c("qt_uk", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us",
                                           "gal_dry_us", "gal_uk", "oz_us", "oz_uk", "qt_us", "pt_us", "pt_uk", "bbl"))
    }
  lapply(x,objetosc_kwarta_uk)
}

#' Konwersja objetosci (pinta amerykanska)
#'
#' @description Funkcja sluzaca do konwersji pinty amerykanskiej na pozostale jednostki objetosci
#'
#' @param x argument zawierajacy wartosc pinty amerykanskiej
#'
#' @return data frame z pozostalymi jednostkami objetosci i ich wartosci
#' @export
#'
#' @examples
#' objetosc_pinta_us(20)
#' objetosc_pinta_us(c(40, 95))
objetosc_pinta_us = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_pinta_us = function(x){
      pinta_us = data.frame(nazwa_jednostki = c("pinta (USA)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny",
                                                "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)",
                                                "galon (suchy, USA)", "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)",
                                                "kwarta (UK)", "pinta (UK)", "baryłka"),
                            wartosc = c(x, x * 473, x * 473, x * 0.473, x * 0.000473,
                                        x * 28.8642309568084, x * 0.01670383735926, x * 0.00061866064294, x * 0.1249533807654,
                                        x * 0.10738081288979, x * 0.10404545444547, x * 15.9940327379717,
                                        x * 16.647272711275, x * 0.5, x * 0.41619005719314,
                                        x * 0.83238011438627, x * 0.00297508049494),
                            jednostka = c("pt_us", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us",
                                          "gal_dry_us", "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_uk", "bbl"))
    }
  lapply(x,objetosc_pinta_us)
}

#' Konwersja objetosci (pinta brytyjska)
#'
#' @description Funkcja sluzaca do konwersji pinty brytyjskiej na pozostale jednostki objetosci
#'
#' @param x argument zawierajacy wartosc pinty brytyjskiej
#'
#' @return data frame z pozostalymi jednostkami objetosci i ich wartosci
#' @export
#'
#' @examples
#' objetosc_pinta_uk(20)
#' objetosc_pinta_uk(c(40, 95))
objetosc_pinta_uk = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_pinta_uk = function(x){
      pinta_uk = data.frame(nazwa_jednostki = c("pinta (UK)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny",
                                                "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)",
                                                "galon (suchy, USA)", "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)",
                                                "kwarta (UK)", "pinta (USA)", "baryłka"),
                            wartosc = c(x, x * 568.25, x * 568.25, x * 0.56825, x * 0.00056825,
                                        x * 34.6767425818316, x * 0.02006755936449, x * 0.00074324293943, x * 0.15011576875252,
                                        x * 0.12900453895269, x * 0.12499752534596, x * 19.2148184003223,
                                        x * 19.9996040553531, x * 0.600687103594081, x * 0.5,
                                        x * 1.201374207188161, x * 0.00357418497093),
                            jednostka = c("pt_uk", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us",
                                          "gal_dry_us", "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "bbl"))
    }
  lapply(x,objetosc_pinta_uk)
}

#' Konwersja objetosci (barylki)
#'
#' @description Funkcja sluzaca do konwersji barylek na pozostale jednostki objetosci
#'
#' @param x argument zawierajacy wartosc barylek
#'
#' @return data frame z pozostalymi jednostkami objetosci i ich wartosci
#' @export
#'
#' @examples
#' objetosc_barylka(20)
#' objetosc_barylka(c(40, 95))
objetosc_barylka = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_barylka = function(x){
      barylka = data.frame(nazwa_jednostki = c("baryłka", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny",
                                               "cal sześcienny", "stopa sześcienna", "jard sześcienny", "galon (płyn, USA)",
                                               "galon (suchy, USA)", "galon (UK)", "uncja (USA)", "uncja (UK)", "kwarta (USA)",
                                               "kwarta (UK)", "pinta (USA)", "pinta (UK)"),
                           wartosc = c(x, x * 158987.2949, x * 158987.2949, x * 158.9872949, x * 0.1589872949,
                                       x * 9701.99999829134, x * 5.61458333234452, x * 0.20794753082757, x * 41.9999999926032,
                                       x * 36.0934143034062, x * 34.9723157482584, x * 5375.99999905321,
                                       x * 5595.57051972134, x * 168.062679598309, x * 139.892032468104,
                                       x * 336.125359196617, x * 279.784064936208),
                           jednostka = c("bbl", "ml", "cm^3", "L", "m^3", "in^3", "ft^3", "yd^3", "gal_us",
                                         "gal_dry_us", "gal_uk", "oz_us", "oz_uk", "qt_us", "qt_uk", "pt_us", "pt_uk"))
    }
  lapply(x,objetosc_barylka)
}

#===========#
# CIŚNIENIE #
#===========#

#' Konwersja cisnienia (cisnienie atmosferyczne)
#'
#' @description Funkcja sluzaca do konwersji cisnienia atmosferycznego na pozostale jednostki cisnienia
#'
#' @param x argument zawierajacy wartosc cisnienia atmosferycznego
#'
#' @return data frame z pozostalymi jednostkami cisnienia i ich wartosci
#' @export
#'
#' @examples
#' cisnienie_atm(20)
#' cisnienie_atm(c(40, 95))
cisnienie_atm = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    cisnienie_atm = function(x){
      (atm = data.frame(nazwa_jednostki = c("atmosfera", "paskal", "hektopaskal", "kilopaskal", "bar",
                                            "milimetr słupa rtęci", "cal słupa rtęci", "milimetr słupa wody",
                                            "cal słupa wody", "kilogramy na centymetr kwadratowy",
                                            "funty na cal kwadratowy"),
                        wartosc = c(x, x * 101325, x * 1013.25, x * 101.325, x * 1.01325, x * 760.002100178515,
                                    x * 29.9212612376373, x * 10339.28571428571, x * 406.785580941599,
                                    x * 1.03322745279989, x * 14.695950008681),
                        jednostka = c("atm", "Pa", "hPa", "kPa", "bar", "mmHg", "inHg", "mmWg", "inWg", "kgf/cm^2", "psi")))
    }
  lapply(x, cisnienie_atm)
}

#' Konwersja cisnienia (paskal)
#'
#' @description Funkcja sluzaca do konwersji paskali na pozostale jednostki cisnienia
#'
#' @param x argument zawierajacy wartosc paskali
#'
#' @return data frame z pozostalymi jednostkami cisnienia i ich wartosci
#' @export
#'
#' @examples
#' cisnienie_Pa(20)
#' cisnienie_Pa(c(40, 95))
cisnienie_Pa = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    cisnienie_Pa = function(x){
      (Pa = data.frame(nazwa_jednostki = c("paskal", "atmosfera", "hektopaskal", "kilopaskal", "bar",
                                           "milimetr słupa rtęci", "cal słupa rtęci", "milimetr słupa wody",
                                           "cal słupa wody", "kilogramy na centymetr kwadratowy",
                                           "funty na cal kwadratowy"),
                       wartosc = c(x, x * 9.86923266716e-6, x * 0.01, x * 0.001, x * 0.00001, x * 0.00750063755419,
                                   x * 0.00029529988885, x * 0.102040816326531, x * 0.00401466154396,
                                   x * 1.019716212978e-5, x * 0.000145037749901),
                       jednostka = c("Pa", "atm", "hPa", "kPa", "bar", "mmHg", "inHg", "mmWg", "inWg", "kgf/cm^2", "psi")))
    }
  lapply(x, cisnienie_Pa)
}

#' Konwersja cisnienia (hektopaskale)
#'
#' @description Funkcja sluzaca do konwersji hektopaskali na pozostale jednostki cisnienia
#'
#' @param x argument zawierajacy wartosc hektopaskali
#'
#' @return data frame z pozostalymi jednostkami cisnienia i ich wartosci
#' @export
#'
#' @examples
#' cisnienie_hPa(20)
#' cisnienie_hPa(c(40, 95))
cisnienie_hPa = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    cisnienie_hPa = function(x){
      (hPa = data.frame(nazwa_jednostki = c("hektopaskal", "atmosfera", "paskal", "kilopaskal", "bar",
                                            "milimetr słupa rtęci", "cal słupa rtęci", "milimetr słupa wody",
                                            "cal słupa wody", "kilogramy na centymetr kwadratowy",
                                            "funty na cal kwadratowy"),
                        wartosc = c(x, x * 0.00098692326672, x * 100, x * 0.1, x * 0.001, x * 0.750063755419211,
                                    x * 0.02952998888491, x * 10.2040816326531, x * 0.40146615439585,
                                    x * 0.00101971621298, x * 0.01450377499006),
                        jednostka = c("hPa", "atm", "Pa", "kPa", "bar", "mmHg", "inHg", "mmWg", "inWg", "kgf/cm^2", "psi")))
    }
  lapply(x, cisnienie_hPa)
}

#' Konwersja cisnienia (kilopaskale)
#'
#' @description Funkcja sluzaca do konwersji kilopaskali na pozostale jednostki cisnienia
#'
#' @param x argument zawierajacy wartosc kilopaskali
#'
#' @return data frame z pozostalymi jednostkami cisnienia i ich wartosci
#' @export
#'
#' @examples
#' cisnienie_kPa(20)
#' cisnienie_kPa(c(40, 95))
cisnienie_kPa = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    cisnienie_kPa = function(x){
      (kPa = data.frame(nazwa_jednostki = c("kilopaskal", "atmosfera", "paskal", "hektopaskal", "bar",
                                            "milimetr słupa rtęci", "cal słupa rtęci", "milimetr słupa wody",
                                            "cal słupa wody", "kilogramy na centymetr kwadratowy",
                                            "funty na cal kwadratowy"),
                        wartosc = c(x, x * 0.00986923266716, x * 1000, x * 10, x * 0.01, x * 7.50063755419211,
                                    x * 0.29529988884912, x * 102.040816326531, x * 4.01466154395854,
                                    x * 0.01019716212978, x * 0.14503774990063),
                        jednostka = c("kPa", "atm", "Pa", "hPa", "bar", "mmHg", "inHg", "mmWg", "inWg", "kgf/cm^2", "psi")))
    }
  lapply(x, cisnienie_kPa)
}

#' Konwersja cisnienia (bar)
#'
#' @description Funkcja sluzaca do konwersji barow na pozostale jednostki cisnienia
#'
#' @param x argument zawierajacy wartosc barow
#'
#' @return data frame z pozostalymi jednostkami cisnienia i ich wartosci
#' @export
#'
#' @examples
#' cisnienie_bar(20)
#' cisnienie_bar(c(40, 95))
cisnienie_bar = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    cisnienie_bar = function(x){
      (bar = data.frame(nazwa_jednostki = c("bar", "atmosfera", "paskal", "hektopaskal", "kilopaskal",
                                            "milimetr słupa rtęci", "cal słupa rtęci", "milimetr słupa wody",
                                            "cal słupa wody", "kilogramy na centymetr kwadratowy",
                                            "funty na cal kwadratowy"),
                        wartosc = c(x, x * 0.98692326671601, x * 100000, x * 1000, x * 100, x * 750.063755419211,
                                    x * 29.5299888849122, x * 10204.0816326531, x * 401.466154395854,
                                    x * 1.01971621297793, x * 14.50377499006271),
                        jednostka = c("bar", "atm", "Pa", "hPa", "kPa", "mmHg", "inHg", "mmWg", "inWg", "kgf/cm^2", "psi")))
    }
  lapply(x, cisnienie_bar)
}

#' Konwersja cisnienia (milimetr slupa rteci)
#'
#' @description Funkcja sluzaca do konwersji milimetrow slupa rteci na pozostale jednostki cisnienia
#'
#' @param x argument zawierajacy wartosc milimetrow slupa rteci
#'
#' @return data frame z pozostalymi jednostkami cisnienia i ich wartosci
#' @export
#'
#' @examples
#' cisnienie_mmHg(20)
#' cisnienie_mmHg(c(40, 95))
cisnienie_mmHg = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    cisnienie_mmHg = function(x){
      (mmHg = data.frame(nazwa_jednostki = c("milimetr słupa rtęci", "atmosfera", "paskal", "hektopaskal", "kilopaskal",
                                             "bar", "cal słupa rtęci", "milimetr słupa wody",
                                             "cal słupa wody", "kilogramy na centymetr kwadratowy",
                                             "funty na cal kwadratowy"),
                         wartosc = c(x, x * 0.001315785837651, x * 133.322, x * 1.33322, x * 0.133322, x * 0.00133322,
                                     x * 0.03936997178114, x * 13.60428571428571, x * 0.53524270636364,
                                     x * 0.00135950604947, x * 0.019336722892251),
                         jednostka = c("mmHg", "atm", "Pa", "hPa", "kPa", "bar", "inHg", "mmWg", "inWg", "kgf/cm^2", "psi")))
    }
  lapply(x, cisnienie_mmHg)
}

#' Konwersja cisnienia (cal slupa rteci)
#'
#' @description Funkcja sluzaca do konwersji cali slupa rteci na pozostale jednostki cisnienia
#'
#' @param x argument zawierajacy wartosc cali slupa rteci
#'
#' @return data frame z pozostalymi jednostkami cisnienia i ich wartosci
#' @export
#'
#' @examples
#' cisnienie_inHg(20)
#' cisnienie_inHg(c(40, 95))
cisnienie_inHg = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    cisnienie_inHg = function(x){
      (inHg = data.frame(nazwa_jednostki = c("cal słupa rtęci", "atmosfera", "paskal", "hektopaskal", "kilopaskal",
                                             "bar", "milimetr słupa rtęci", "milimetr słupa wody",
                                             "cal słupa wody", "kilogramy na centymetr kwadratowy",
                                             "funty na cal kwadratowy"),
                         wartosc = c(x, x * 0.003342105107328, x * 3386.388, x * 33.86388, x * 3.386388, x * 0.03386388,
                                     x * 25.4000690058655, x * 345.549795918367, x * 13.5952016765227,
                                     x * 0.03453154747034, x * 0.49115409581048),
                         jednostka = c("inHg", "atm", "Pa", "hPa", "kPa", "bar", "mmHg", "mmWg", "inWg", "kgf/cm^2", "psi")))
    }
  lapply(x, cisnienie_inHg)
}

#' Konwersja cisnienia (milimetr slupa wody)
#'
#' @description Funkcja sluzaca do konwersji milimetrow slupa wody na pozostale jednostki cisnienia
#'
#' @param x argument zawierajacy wartosc milimetrow slupa wody
#'
#' @return data frame z pozostalymi jednostkami cisnienia i ich wartosci
#' @export
#'
#' @examples
#' cisnienie_mmWg(20)
#' cisnienie_mmWg(c(40, 95))
cisnienie_mmWg = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    cisnienie_mmWg = function(x){
      (mmWg = data.frame(nazwa_jednostki = c("milimetr słupa wody", "atmosfera", "paskal", "hektopaskal", "kilopaskal",
                                             "bar", "milimetr słupa rtęci", "cal słupa rtęci",
                                             "cal słupa wody", "kilogramy na centymetr kwadratowy",
                                             "funty na cal kwadratowy"),
                         wartosc = c(x, x * 9.671848013817e-5, x * 9.8, x * 0.098, x * 0.0098, x * 0.000098,
                                     x * 0.07350624803108, x * 0.002893938910721, x * 0.03934368313079,
                                     x * 9.993218887184e-5, x * 0.00142136994903),
                         jednostka = c("mmWg", "atm", "Pa", "hPa", "kPa", "bar", "mmHg", "inHg", "inWg", "kgf/cm^2", "psi")))
    }
  lapply(x, cisnienie_mmWg)
}

#' Konwersja cisnienia (cal slupa wody)
#'
#' @description Funkcja sluzaca do konwersji cali slupa wody na pozostale jednostki cisnienia
#'
#' @param x argument zawierajacy wartosc cali slupa wody
#'
#' @return data frame z pozostalymi jednostkami cisnienia i ich wartosci
#' @export
#'
#' @examples
#' cisnienie_inWg(20)
#' cisnienie_inWg(c(40, 95))
cisnienie_inWg = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    cisnienie_inWg = function(x){
      (inWg = data.frame(nazwa_jednostki = c("cal słupa wody", "atmosfera", "paskal", "hektopaskal", "kilopaskal",
                                             "bar", "milimetr słupa rtęci", "cal słupa rtęci",
                                             "milimetr słupa wody", "kilogramy na centymetr kwadratowy",
                                             "funty na cal kwadratowy"),
                         wartosc = c(x, x * 0.002458299755736, x * 249.087, x * 2.49087, x * 0.249087, x * 0.00249087,
                                     x * 1.86831130646105, x * 0.073555363413761, x * 25.4170408163265,
                                     x * 0.00253998052342, x * 0.0361270180095),
                         jednostka = c("inWg", "atm", "Pa", "hPa", "kPa", "bar", "mmHg", "inHg", "mmWg", "kgf/cm^2", "psi")))
    }
  lapply(x, cisnienie_inWg)
}

#' Konwersja cisnienia (kilogramy na centymetr kwadratowy)
#'
#' @description Funkcja sluzaca do konwersji kilogramow na centymetr kwadratowy na pozostale jednostki cisnienia
#'
#' @param x argument zawierajacy wartosc kilogramow na centymetr kwadratowy
#'
#' @return data frame z pozostalymi jednostkami cisnienia i ich wartosci
#' @export
#'
#' @examples
#' cisnienie_kgcm2(20)
#' cisnienie_kgcm2(c(40, 95))
cisnienie_kgcm2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    cisnienie_kgcm2 = function(x){
      (kgcm2 = data.frame(nazwa_jednostki = c("kilogramy na centymetr kwadratowy", "atmosfera", "paskal", "hektopaskal", "kilopaskal",
                                              "bar", "milimetr słupa rtęci", "cal słupa rtęci",
                                              "milimetr słupa wody", "cal słupa wody",
                                              "funty na cal kwadratowy"),
                          wartosc = c(x, x * 0.96784110535406, x * 98066.5, x * 980.665, x * 98.0665, x * 0.980665,
                                      x * 735.56127270818, x * 28.95902654982241, x * 10006.78571428571,
                                      x * 393.70380630061, x * 14.2233445006299),
                          jednostka = c("kgf/cm^2", "atm", "Pa", "hPa", "kPa", "bar", "mmHg", "inHg", "mmWg", "inWg", "psi")))
    }
  lapply(x, cisnienie_kgcm2)
}

#' Konwersja cisnienia (funty na cal kwadratowy)
#'
#' @description Funkcja sluzaca do konwersji funtow na cal kwadratowy na pozostale jednostki cisnienia
#'
#' @param x argument zawierajacy wartosc funtow na cal kwadratowy
#'
#' @return data frame z pozostalymi jednostkami cisnienia i ich wartosci
#' @export
#'
#' @examples
#' cisnienie_psi(20)
#' cisnienie_psi(c(40, 95))
cisnienie_psi = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    cisnienie_psi = function(x){
      (psi = data.frame(nazwa_jednostki = c("funty na cal kwadratowy", "atmosfera", "paskal", "hektopaskal", "kilopaskal",
                                            "bar", "milimetr słupa rtęci", "cal słupa rtęci",
                                            "milimetr słupa wody", "cal słupa wody",
                                            "kilogramy na centymetr kwadratowy"),
                        wartosc = c(x, x * 0.0680459582, x * 6894.756714615, x * 68.94756714615, x * 6.894756714615,
                                    x * 0.06894756714615, x * 51.7150711406595, x * 2.03602089146755, x * 703.546603532143,
                                    x * 27.6801146371147, x * 0.07030695206431),
                        jednostka = c("psi", "atm", "Pa", "hPa", "kPa", "bar", "mmHg", "inHg", "mmWg", "inWg", "kgf/cm^2")))
    }
  lapply(x, cisnienie_psi)
}

#==============#
# POWIERZCHNIA #
#==============#

#' Konwersja powierzchni (milimetry kwadratowe)
#'
#' @description Funkcja sluzaca do konwersji milimetrow kwadratowych na pozostale jednostki powierzchni
#'
#' @param x argument zawierajacy wartosc milimetrow kwadratowych
#'
#' @return data frame z pozostalymi jednostkami powierzchni i ich wartosci
#' @export
#'
#' @examples
#' powierzchnia_mm2(20)
#' powierzchnia_mm2(c(40, 95))
powierzchnia_mm2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    powierzchnia_mm2 = function(x){
      powierzchnia=data.frame(nazwa_jednostki = c("milimetr kwadrotowy", "centymetr kwadratowy", "metr kwadratowy",
                                                  "kilometr kwadratowy", "ar", "hektar", "stopa kwadratowa", " jard kwadratowy",
                                                  "akr"),
                              wartosc = c(x, x/100, x/1000000, x/1000000000000, x/100000000, x*1e-10, x/92903, x/836100, x/4046856422.4),
                              jednostka = c("mm^2", "cm^2", "m^2", "km^2", "a", "ha", "ft^2", "yd^2", "acre"))
    }
  lapply(x, powierzchnia_mm2)
}

#' Konwersja powierzchni (centymetry kwadratowe)
#'
#' @description Funkcja sluzaca do konwersji centymetrow kwadratowych na pozostale jednostki powierzchni
#'
#' @param x argument zawierajacy wartosc centymetrow kwadratowych
#'
#' @return data frame z pozostalymi jednostkami powierzchni i ich wartosci
#' @export
#'
#' @examples
#' powierzchnia_cm2(20)
#' powierzchnia_cm2(c(40, 95))
powierzchnia_cm2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    powierzchnia_cm2 = function(x){
      powierzchnia=data.frame(nazwa_jednostki = c( "centymetr kwadratowy", "milimetr kwadrotowy", "metr kwadratowy",
                                                   "kilometr kwadratowy", "ar", "hektar", "stopa kwadratowa", " jard kwadratowy",
                                                   "akr"),
                              wartosc = c(x, x*100, x*0.0001, x*1e-6, x*1e-10, x*1e-8, x*0.001076391041671,
                                          x*0.00011959900463, x*2.471053814672e-8),
                              jednostka = c("cm^2", "mm^2", "m^2", "km^2", "a", "ha", "ft^2", "yd^2", "acre"))
    }
  lapply(x, powierzchnia_cm2)
}

#' Konwersja powierzchni (metry kwadratowe)
#'
#' @description Funkcja sluzaca do konwersji metrow kwadratowych na pozostale jednostki powierzchni
#'
#' @param x argument zawierajacy wartosc metrow kwadratowych
#'
#' @return data frame z pozostalymi jednostkami powierzchni i ich wartosci
#' @export
#'
#' @examples
#' powierzchnia_m2(20)
#' powierzchnia_m2(c(40, 95))
powierzchnia_m2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    powierzchnia_m2 = function(x){
      powierzchnia=data.frame(nazwa_jednostki = c("metr kwadratowy", "milimetr kwadrotowy", "centymetr kwadratowy",
                                                  "kilometr kwadratowy", "ar", "hektar", "stopa kwadratowa", " jard kwadratowy",
                                                  "akr"),
                              wartosc = c(x, x/1000000, x/1000000*100, x/1000000*1000000000000, x/1000000*100000000,
                                          x*0.0001, x/1000000*92903, x/1000000*836100, x/1000000*4046856422.4),
                              jednostka = c("m^2", "mm^2", "cm^2", "km^2", "a", "ha", "ft^2", "yd^2", "acre"))
    }
  lapply(x, powierzchnia_m2)
}

#' Konwersja powierzchni (kilometry kwadratowe)
#'
#' @description Funkcja sluzaca do konwersji kilometrow kwadratowych na pozostale jednostki powierzchni
#'
#' @param x argument zawierajacy wartosc kilometrow kwadratowych
#'
#' @return data frame z pozostalymi jednostkami powierzchni i ich wartosci
#' @export
#'
#' @examples
#' powierzchnia_km2(20)
#' powierzchnia_km2(c(40, 95))
powierzchnia_km2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    powierzchnia_km2 = function(x){
      powierzchnia=data.frame(nazwa_jednostki = c("kilometr kwadratowy", "milimetr kwadrotowy", "centymetr kwadratowy",
                                                  "metr kwadratowy", "ar", "hektar", "stopa kwadratowa", " jard kwadratowy","akr"),
                              wartosc = c(x, x*1000000000000, x*1000000000000*100, x*1000000000000*1000000,
                                          x*1000000000000*100000000, x*100, x*1000000000000*92903,
                                          x*1000000000000*836100, x*1000000000000*4046856422.4),
                              jednostka = c( "km^2", "mm^2", "cm^2", "m^2", "a", "ha", "ft^2", "yd^2", "acre"))
    }
  lapply(x, powierzchnia_km2)
}

#' Konwersja powierzchni (ary)
#'
#' @description Funkcja sluzaca do konwersji arow na pozostale jednostki powierzchni
#'
#' @param x argument zawierajacy wartosc arow
#'
#' @return data frame z pozostalymi jednostkami powierzchni i ich wartosci
#' @export
#'
#' @examples
#' powierzchnia_a(20)
#' powierzchnia_a(c(40, 95))
powierzchnia_a = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    powierzchnia_a = function(x){
      powierzchnia=data.frame(nazwa_jednostki = c("ar", "milimetr kwadrotowy", "centymetr kwadratowy", "metr kwadratowy",
                                                  "kilometr kwadratowy", "hektar", "stopa kwadratowa", " jard kwadratowy",
                                                  "akr"),
                              wartosc = c(x, x*100000000, x*100000000/100, x*100000000/1000000, x*100000000/1000000000000,
                                          x*0.01, x*100000000/92903, x*100000000/836100, x*100000000/4046856422.4),
                              jednostka = c( "a", "mm^2", "cm^2", "m^2", "km^2", "ha", "ft^2", "yd^2", "acre"))
    }
  lapply(x, powierzchnia_a)
}

#' Konwersja powierzchni (hektary)
#'
#' @description Funkcja sluzaca do konwersji hektarow na pozostale jednostki powierzchni
#'
#' @param x argument zawierajacy wartosc hektarow
#'
#' @return data frame z pozostalymi jednostkami powierzchni i ich wartosci
#' @export
#'
#' @examples
#' powierzchnia_ha(20)
#' powierzchnia_ha(c(40, 95))
powierzchnia_ha = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    powierzchnia_ha = function(x){
      powierzchnia=data.frame(nazwa_jednostki = c("hektar", "milimetr kwadrotowy", "centymetr kwadratowy", "metr kwadratowy",
                                                  "kilometr kwadratowy", "ar", "stopa kwadratowa", " jard kwadratowy",
                                                  "akr"),
                              wartosc = c(x, x*1e+10, x*1e+8, x*10000, x*0.01, x*100,
                                          x*107639.104167097, x*11959.9004630108, x*2.47105381467165),
                              jednostka = c( "ha", "mm^2", "cm^2", "m^2", "km^2", "a", "ft^2", "yd^2", "acre"))
    }
  lapply(x, powierzchnia_ha)
}

#' Konwersja powierzchni (stopy kwadratowe)
#'
#' @description Funkcja sluzaca do konwersji stop kwadratowych na pozostale jednostki powierzchni
#'
#' @param x argument zawierajacy wartosc stop kwadratowych
#'
#' @return data frame z pozostalymi jednostkami powierzchni i ich wartosci
#' @export
#'
#' @examples
#' powierzchnia_ft2(20)
#' powierzchnia_ft2(c(40, 95))
powierzchnia_ft2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    powierzchnia_ft2 = function(x){
      powierzchnia=data.frame(nazwa_jednostki = c("stopa kwadratowa", "milimetr kwadrotowy", "centymetr kwadratowy",
                                                  "metr kwadratowy", "kilometr kwadratowy", "ar", "hektar", " jard kwadratowy", "akr"),
                              wartosc = c(x, x*92903.04, x*929.0304, x*0.09290304, x*9.290304e-8,
                                          x*0.0009290304,  x*9.290304e-6,  x*0.1111111111111, 2.295684113866e-5),
                              jednostka = c( "ft^2", "mm^2", "cm^2", "m^2", "km^2", "a", "ha", "yd^2", "acre"))
    }
  lapply(x, powierzchnia_ft2)
}

#' Konwersja powierzchni (jardy kwadratowe)
#'
#' @description Funkcja sluzaca do konwersji jardow kwadratowych na pozostale jednostki powierzchni
#'
#' @param x argument zawierajacy wartosc jardow kwadratowych
#'
#' @return data frame z pozostalymi jednostkami powierzchni i ich wartosci
#' @export
#'
#' @examples
#' powierzchnia_yd2(20)
#' powierzchnia_yd2(c(40, 95))
powierzchnia_yd2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    powierzchnia_yd2 = function(x){
      powierzchnia=data.frame(nazwa_jednostki = c("jard kwadratowy", "milimetr kwadrotowy", "centymetr kwadratowy",
                                                  "metr kwadratowy", "kilometr kwadratowy", "ar", "hektar", "stopa kwadratowa", "akr"),
                              wartosc = c(x, x*836127.36, x*8361.2736, x*0.83612736, x*8.3612736e-7, x*0.0083612736,
                                          x*8.3612736e-5, x*9, x*0.00020661157025),
                              jednostka = c( "yd^2", "mm^2", "cm^2", "m^2", "km^2", "a", "ha", "ft^2", "acre"))
    }
  lapply(x, powierzchnia_yd2)
}

#' Konwersja powierzchni (akry)
#'
#' @description Funkcja sluzaca do konwersji akrow na pozostale jednostki powierzchni
#'
#' @param x argument zawierajacy wartosc akrow
#'
#' @return data frame z pozostalymi jednostkami powierzchni i ich wartosci
#' @export
#'
#' @examples
#' powierzchnia_acre(20)
#' powierzchnia_acre(c(40, 95))
powierzchnia_acre = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    powierzchnia_acre = function(x){
      powierzchnia=data.frame(nazwa_jednostki = c("akr","milimetr kwadrotowy", "centymetr kwadratowy", "metr kwadratowy",
                                                  "kilometr kwadratowy", "ar", "hekatr", "stopa kwadratowa", " jard kwadratowy" ),
                              wartosc = c(x, x*4046856422.4, x*40468564.224, x*4046.8564224,
                                          x*0.0040468564224, x*40.468564224, x*0.40468564224,
                                          x*43560, x*4840),
                              jednostka = c( "acre", "mm^2", "cm^2", "m^2", "km^2", "a", "ha", "ft^2", "yd^2"))
    }
  lapply(x, powierzchnia_acre)
}


#=============#
# TEMPERATURA #
#=============#

#' Konwersja temperatury (stopnie Celcjusza)
#'
#' @description Funkcja sluzaca do konwersji stopni Celcjusza na pozostale jednostki temperatury
#'
#' @param x argument zawierajacy wartosc stopni Celcjusza
#'
#' @return data frame z pozostalymi jednostkami temperatury i ich wartosci
#' @export
#'
#' @examples
#' temperatura_C(20)
#' temperatura_C(c(40, 95))
temperatura_C= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    temperatura_C= function(x){
      temperatury=data.frame(nazwa_jednostki = c("skala Celsjusza", "skala Kelvina",  "skala Fahrenheita",
                                                 "skala Rankinea", "skala Reaumura", "skala Newtona"),
                             wartosc = c(x, x+273.15, (x*1.8)+32, (x+ 273.15)*1.8, (x*4)/5, x/3),
                             jednostka = c("°C", "K", "°F", "°R", "°Ré", "°N"))
    }
  lapply(x,temperatura_C)
}

#' Konwersja temperatury (Kelviny)
#'
#' @description Funkcja sluzaca do konwersji Kelvinow na pozostale jednostki temperatury
#'
#' @param x argument zawierajacy wartosc Kelvinow
#'
#' @return data frame z pozostalymi jednostkami temperatury i ich wartosci
#' @export
#'
#' @examples
#' temperatura_K(20)
#' temperatura_K(c(40, 95))
temperatura_K= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    temperatura_K= function(x){
      temperatury=data.frame(nazwa_jednostki = c("skala Kelvina","skala Celsjusza",   "skala Fahrenheita",
                                                 "skala Rankinea", "skala Reaumura", "skala Newtona"),
                             wartosc = c(x, x-273.15, ((x-273.15)*1.8)+32, x*1.8, ((x-273.15)*4)/5, (x-273.15)/3),
                             jednostka = c( "K", "°C", "°F", "°R", "°Ré", "°N"))
    }
  lapply(x,temperatura_K)
}

#' Konwersja temperatury (stopnie Fahrenheita)
#'
#' @description Funkcja sluzaca do konwersji stopni Fahrenheita na pozostale jednostki temperatury
#'
#' @param x argument zawierajacy wartosc stopni Celcjusza
#'
#' @return data frame z pozostalymi jednostkami temperatury i ich wartosci
#' @export
#'
#' @examples
#' temperatura_F(20)
#' temperatura_F(c(40, 95))
temperatura_F= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    temperatura_F= function(x){
      temperatury=data.frame(nazwa_jednostki = c("skala Fahrenheita", "skala Celsjusza", "skala Kelvina",
                                                 "skala Rankinea", "skala Reaumura", "skala Newtona"),
                             wartosc = c(x, (x-32)/1.8, (x+459.67)/1.8, x+459.67, (x-32)*4/9, (x-32)*11/60),
                             jednostka = c("°F", "°C", "K",  "°R", "°Ré", "°N"))
    }
  lapply(x,temperatura_F)
}

#' Konwersja temperatury (stopnie Rankinea)
#'
#' @description Funkcja sluzaca do konwersji stopni Rankinea na pozostale jednostki temperatury
#'
#' @param x argument zawierajacy wartosc stopni Rankinea
#'
#' @return data frame z pozostalymi jednostkami temperatury i ich wartosci
#' @export
#'
#' @examples
#' temperatura_R(20)
#' temperatura_R(c(40, 95))
temperatura_R= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    temperatura_R= function(x){
      temperatury=data.frame(nazwa_jednostki = c("skala Rankinea", "skala Celsjusza", "skala Kelvina",  "skala Fahrenheita",
                                                 "skala Reaumura", "skala Newtona"),
                             wartosc = c(x, (x/1.8)-273.15, x/1.8, x-459.67, ((x/1.8)-273.15)*4/5, ((x/1.8)-273.15)/3),
                             jednostka = c("°R", "°C", "K", "°F", "°Ré", "°N"))
    }
  lapply(x,temperatura_R)
}

#' Konwersja temperatury (stopnie Reaumura)
#'
#' @description Funkcja sluzaca do konwersji stopni Reaumura na pozostale jednostki temperatury
#'
#' @param x argument zawierajacy wartosc stopni Reaumura
#'
#' @return data frame z pozostalymi jednostkami temperatury i ich wartosci
#' @export
#'
#' @examples
#' temperatura_Re(20)
#' temperatura_Re(c(40, 95))
temperatura_Re= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    temperatura_Re= function(x){
      temperatury=data.frame(nazwa_jednostki = c("skala Reaumura", "skala Celsjusza", "skala Kelvina",  "skala Fahrenheita",
                                                 "skala Rankinea",  "skala Newtona"),
                             wartosc = c(x, x*1.25, x*1.25+273.15, x*2.25+32, (x*1.25+273.15)*1.8, x*1.25/3),
                             jednostka = c("°Ré", "°C", "K", "°F", "°R", "°N"))
    }
  lapply(x,temperatura_Re)
}

#' Konwersja temperatury (stopnie Newtona)
#'
#' @description Funkcja sluzaca do konwersji stopni Newtona na pozostale jednostki temperatury
#'
#' @param x argument zawierajacy wartosc stopni Newtona
#'
#' @return data frame z pozostalymi jednostkami temperatury i ich wartosci
#' @export
#'
#' @examples
#' temperatura_N(20)
#' temperatura_N(c(40, 95))
temperatura_N= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    temperatura_N= function(x){
      temperatury=data.frame(nazwa_jednostki = c("skala Newtona", "skala Celsjusza", "skala Kelvina",  "skala Fahrenheita",
                                                 "skala Rankinea", "skala Reaumura"),
                             wartosc = c(x, x*3, x*3+273.15, x*60/11-32, (x*3+273.15)*1.8, x*3*4/5),
                             jednostka = c( "°N", "°C", "K", "°F", "°R", "°Ré"))
    }
  lapply(x,temperatura_N)
}

#==========#
# PRĘDKOŚĆ #
#==========#

#' Konwersja predkosci (metry na sekunde)
#'
#' @description Funkcja sluzaca do konwersji metrow na sekunde na pozostale jednostki predkosci
#'
#' @param x argument zawierajacy wartosc metrow na sekunde
#'
#' @return data frame z pozostalymi jednostkami predkosci i ich wartosci
#' @export
#'
#' @examples
#' predkosc_ms(20)
#' predkosc_ms(c(40, 95))
predkosc_ms= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    predkosc_ms = function(x){
      predkosc = data.frame(nazwa_jednostki = c("metry na sekundę", "metry na minutę"," kilometry na sekundę",
                                                "kilometry na minutę","kilometry na godzinę", "stopy na sekundę",
                                                "mile na godzinę", " machy", "węzły"),
                            wartosc = c(x, x*60, x*0.001, x*0.06, x*3.6, x*3.2808398950131, x*2.2369362920544,
                                        x*0.002938669957977, x*1.9438461717893),
                            jednostka = c("m/s", "m/min", "km/s", "km/min", "km/h", "ft/s", "mph", "Mach", "kn"))
    }
  lapply(x, predkosc_ms)
}

#' Konwersja predkosci (metry na minute)
#'
#' @description Funkcja sluzaca do konwersji metrow na minute na pozostale jednostki predkosci
#'
#' @param x argument zawierajacy wartosc metrow na minute
#'
#' @return data frame z pozostalymi jednostkami predkosci i ich wartosci
#' @export
#'
#' @examples
#' predkosc_mmin(20)
#' predkosc_mmin(c(40, 95))
predkosc_mmin= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    predkosc_mmin = function(x){
      predkosc = data.frame(nazwa_jednostki = c("metry na minutę", "metry na sekundę", " kilometry na sekundę",
                                                "kilometry na minutę","kilometry na godzinę", "stopy na sekundę",
                                                "mile na godzinę", " machy", "węzły"),
                            wartosc = c(x, x/60, x/60*0.001, x*0.001, x*0.06, x*3.2808398950131/60, x*2.2369362920544/60,
                                        x*0.002938669957977/60, x*1.9438461717893/60  ),
                            jednostka = c( "m/min", "m/s", "km/s", "km/min", "km/h", "ft/s", "mph", "Mach", "kn"))
    }
  lapply(x, predkosc_mmin)
}


#' Konwersja predkosci (kkilometry na sekunde)
#'
#' @description Funkcja sluzaca do konwersji kilometrow na sekunde na pozostale jednostki predkosci
#'
#' @param x argument zawierajacy wartosc kilometrow na sekunde
#'
#' @return data frame z pozostalymi jednostkami predkosci i ich wartosci
#' @export
#'
#' @examples
#' predkosc_kms(20)
#' predkosc_kms(c(40, 95))
predkosc_kms= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    predkosc_kms = function(x){
      predkosc = data.frame(nazwa_jednostki = c("kilometry na sekundę", "metry na sekundę", "metry na minutę",
                                                "kilometry na minutę","kilometry na godzinę", "stopy na sekundę",
                                                "mile na godzinę", " machy", "węzły"),
                            wartosc = c(x, x*1000, x*1000*60, x*1000*0.06, x*1000*3.6, x*1000*3.2808398950131,
                                        x*2.2369362920544*1000, x*1000*0.002938669957977, x*1000*1.9438461717893 ),
                            jednostka = c("km/s", "m/s", "m/min",  "km/min", "km/h", "ft/s", "mph", "Mach", "kn"))
    }
  lapply(x, predkosc_kms)
}

#' Konwersja predkosci (kilometry na minute)
#'
#' @description Funkcja sluzaca do konwersji kilometrow na minute na pozostale jednostki predkosci
#'
#' @param x argument zawierajacy wartosc kilometrow na minute
#'
#' @return data frame z pozostalymi jednostkami predkosci i ich wartosci
#' @export
#'
#' @examples
#' predkosc_kmmin(20)
#' predkosc_kmmin(c(40, 95))
predkosc_kmmin= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    predkosc_kmmin = function(x){
      predkosc = data.frame(nazwa_jednostki = c("kilometry na minutę", "metry na sekundę", "metry na minutę"," kilometry na sekundę",
                                                "kilometry na godzinę", "stopy na sekundę",
                                                "mile na godzinę", " machy", "węzły"),
                            wartosc = c(x, x/0.06, x/0.06*60, x/0.06*0.001, x/0.06*3.6, x/0.06*3.2808398950131, x/0.06*2.2369362920544,
                                        x/0.06*x*0.002938669957977, x*1.9438461717893/0.06 ),
                            jednostka = c("km/min", "m/s", "m/min", "km/s", "km/h", "ft/s", "mph", "Mach", "kn"))
    }
  lapply(x, predkosc_kmmin)
}

#' Konwersja predkosci (kilometry na godzine)
#'
#' @description Funkcja sluzaca do konwersji kilometrow na godzine na pozostale jednostki predkosci
#'
#' @param x argument zawierajacy wartosc kilometrow na godzine
#'
#' @return data frame z pozostalymi jednostkami predkosci i ich wartosci
#' @export
#'
#' @examples
#' predkosc_kmh(20)
#' predkosc_kmh(c(40, 95))
predkosc_kmh= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    predkosc_kmh = function(x){
      predkosc = data.frame(nazwa_jednostki = c("kilometry na godzinę", "metry na sekundę", "metry na minutę"," kilometry na sekundę",
                                                "kilometry na minutę", "stopy na sekundę",
                                                "mile na godzinę", " machy", "węzły"),
                            wartosc = c(x, x/3.6, x/3.6*60, x/3.6*0.001, x/3.6*0.06, x/3.6*3.2808398950131,
                                        x/3.6*2.2369362920544, x/3.6*0.002938669957977, x/3.6*1.9438461717893 ),
                            jednostka = c("km/h", "m/s", "m/min", "km/s", "km/min", "ft/s", "mph", "Mach", "kn"))
    }
  lapply(x, predkosc_kmh)
}

#' Konwersja predkosci (stopy na sekunde)
#'
#' @description Funkcja sluzaca do konwersji stop na sekunde na pozostale jednostki predkosci
#'
#' @param x argument zawierajacy wartosc stop na sekunde
#'
#' @return data frame z pozostalymi jednostkami predkosci i ich wartosci
#' @export
#'
#' @examples
#' predkosc_fts(20)
#' predkosc_fts(c(40, 95))
predkosc_fts= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    predkosc_fts = function(x){
      predkosc = data.frame(nazwa_jednostki = c("stopy na sekundę", "metry na sekundę", "metry na minutę"," kilometry na sekundę",
                                                "kilometry na minutę","kilometry na godzinę",
                                                "mile na godzinę", " machy", "węzły"),
                            wartosc = c(x, x/3.2808398950131, x/3.2808398950131*60, x/3.2808398950131*0.001, x/3.2808398950131*0.06,
                                        x/3.2808398950131*3.6,   x/3.2808398950131*2.2369362920544,
                                        x/3.2808398950131*0.002938669957977,  x/3.2808398950131*1.9438461717893),
                            jednostka = c("ft/s", "m/s", "m/min", "km/s", "km/min", "km/h", "mph", "Mach", "kn"))
    }
  lapply(x, predkosc_fts)
}

#' Konwersja predkosci (mile na godzine)
#'
#' @description Funkcja sluzaca do konwersji mil na godzine na pozostale jednostki predkosci
#'
#' @param x argument zawierajacy wartosc mil na godzine
#'
#' @return data frame z pozostalymi jednostkami predkosci i ich wartosci
#' @export
#'
#' @examples
#' predkosc_mph(20)
#' predkosc_mph(c(40, 95))
predkosc_mph= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    predkosc_mph = function(x){
      predkosc = data.frame(nazwa_jednostki = c("mile na godzinę", "metry na sekundę", "metry na minutę"," kilometry na sekundę",
                                                "kilometry na minutę","kilometry na godzinę", "stopy na sekundę",
                                                " machy", "węzły"),
                            wartosc = c(x, x/2.2369362920544, x/2.2369362920544*60, x/2.2369362920544*0.001,
                                        x/2.2369362920544*0.06, x/2.2369362920544*3.6, x/2.2369362920544*3.2808398950131,
                                        x/2.2369362920544*0.002938669957977, x/2.2369362920544*1.9438461717893),
                            jednostka = c("mph", "m/s", "m/min", "km/s", "km/min", "km/h", "ft/s",  "Mach", "kn"))
    }
  lapply(x, predkosc_mph)
}

#' Konwersja predkosci (machy)
#'
#' @description Funkcja sluzaca do konwersji Mach na pozostale jednostki predkosci
#'
#' @param x argument zawierajacy wartosc Mach
#'
#' @return data frame z pozostalymi jednostkami predkosci i ich wartosci
#' @export
#'
#' @examples
#' predkosc_Mach(20)
#' predkosc_Mach(c(40, 95))
predkosc_Mach= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    predkosc_Mach = function(x){
      predkosc = data.frame(nazwa_jednostki = c(" machy", "metry na sekundę", "metry na minutę"," kilometry na sekundę",
                                                "kilometry na minutę","kilometry na godzinę", "stopy na sekundę",
                                                "mile na godzinę", "węzły"),
                            wartosc = c(x, x/0.002938669957977, x/0.002938669957977*60, x/0.002938669957977*0.001,
                                        x/0.002938669957977*0.06, x/0.002938669957977*3.6, x/0.002938669957977*3.2808398950131,
                                        x/0.002938669957977*2.2369362920544, x/0.002938669957977*1.9438461717893),
                            jednostka = c("Mach", "m/s", "m/min", "km/s", "km/min", "km/h", "ft/s", "mph", "kn"))
    }
  lapply(x, predkosc_Mach)
}

#' Konwersja predkosci (wezly)
#'
#' @description Funkcja sluzaca do konwersji wezlow na pozostale jednostki predkosci
#'
#' @param x argument zawierajacy wartosc wezlow
#'
#' @return data frame z pozostalymi jednostkami predkosci i ich wartosci
#' @export
#'
#' @examples
#' predkosc_kn(20)
#' predkosc_kn(c(40, 95))
predkosc_kn= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    predkosc_kn = function(x){
      predkosc = data.frame(nazwa_jednostki = c("węzły", "metry na sekundę", "metry na minutę"," kilometry na sekundę",
                                                "kilometry na minutę","kilometry na godzinę", "stopy na sekundę",
                                                "mile na godzinę", " machy"),
                            wartosc = c(x, x/1.9438461717893, x/1.9438461717893*60, x/1.9438461717893*0.001, x/1.9438461717893*0.06,
                                        x*1.852, x/1.9438461717893*3.2808398950131,
                                        x/1.9438461717893*2.2369362920544, x/1.9438461717893*0.002938669957977),
                            jednostka = c( "kn", "m/s", "m/min", "km/s", "km/min", "km/h", "ft/s", "mph", "Mach"))
    }
  lapply(x, predkosc_kn)
}

#=========#
# DŁUGOŚĆ #
#=========#

#' Konwersja dlugosci (milimetry)
#'
#' @description Funkcja sluzaca do konwersji milimetrow na pozostale jednostki dlugosci
#'
#' @param x argument zawierajacy wartosc milimetrow
#'
#' @return data frame z pozostalymi jednostkami dlugosci i ich wartosci
#' @export
#'
#' @examples
#' dlugosc_mm(20)
#' dlugosc_mm(c(40, 95))
dlugosc_mm = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    dlugosc_mm = function(x){
      dlugosc = data.frame(nazwa_jednostki = c("milimetr", "centrymetr", "decymetr", "metr", "kilometr",
                                               "cal", "stopa", "jard", "mila", "mila morska", "lata swietlne"),
                           wartosc = c(x, x/10, x/100, x/1000, x/1000000, x*0.03937007874016, x*0.00328083989501, x*0.00109361329834,
                                       x*6.213711922373e-7, x*5.399568034557e-7, x*1.05702341052e-19),
                           jednostka = c("mm", "cm", "dm", "m", "km", "in", "ft", "yd", "mi", "nmi", "ly"))
    }
  lapply(x, dlugosc_mm)
}

#' Konwersja dlugosci (centymetry)
#'
#' @description Funkcja sluzaca do konwersji centymetrow na pozostale jednostki dlugosci
#'
#' @param x argument zawierajacy wartosc centymetrow
#'
#' @return data frame z pozostalymi jednostkami dlugosci i ich wartosci
#' @export
#'
#' @examples
#' dlugosc_cm(20)
#' dlugosc_cm(c(40, 95))
dlugosc_cm= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    dlugosc_cm = function(x){
      dlugosc = data.frame(nazwa_jednostki = c("centrymetr", "milimetr","decymetr", "metr",
                                               "kilometr", "cal", "stopa", "jard", "mila", "mila morska", "lata swietlne"),
                           wartosc = c(x, x*10, x*10/100, x*10/1000, x*10/1000000, x*0.39370078740157, x*0.032808398950131,
                                       x*0.0109361329838, x*6.213711922373e-6, x*5.399568034557e-6, x*1.05702341052e-18),
                           jednostka = c("cm", "mm",  "dm", "m", "km", "in", "ft", "yd", "mi", "nmi", "ly"))
    }
  lapply(x, dlugosc_cm)
}

#' Konwersja dlugosci (decymetry)
#'
#' @description Funkcja sluzaca do konwersji decymetrow na pozostale jednostki dlugosci
#'
#' @param x argument zawierajacy wartosc decymetrow
#'
#' @return data frame z pozostalymi jednostkami dlugosci i ich wartosci
#' @export
#'
#' @examples
#' dlugosc_dm(20)
#' dlugosc_dm(c(40, 95))
dlugosc_dm= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    dlugosc_dm = function(x){
      dlugosc = data.frame(nazwa_jednostki = c("decymetr", "milimetr", "centrymetr", "metr",
                                               "kilometr", "cal", "stopa", "jard", "mila", "mila morska", "lata swietlne"),
                           wartosc = c(x, x*100, x*100/10, x*100/1000, x*100/1000000, x*3.93700787401575, x*0.32808398950131, x*0.10936132983371,
                                       x*6.213711922373e-5, x*5.399568034557e-5, x*1.05702341052e-17),
                           jednostka = c( "dm", "mm", "cm", "m", "km", "in", "ft", "yd", "mi", "nmi", "ly"))
    }
  lapply(x, dlugosc_dm)
}

#' Konwersja dlugosci (metry)
#'
#' @description Funkcja sluzaca do konwersji metrow na pozostale jednostki dlugosci
#'
#' @param x argument zawierajacy wartosc metrow
#'
#' @return data frame z pozostalymi jednostkami dlugosci i ich wartosci
#' @export
#'
#' @examples
#' dlugosc_m(20)
#' dlugosc_m(c(40, 95))
dlugosc_m= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    dlugosc_m = function(x){
      dlugosc = data.frame(nazwa_jednostki = c("metr", "milimetr", "centrymetr", "decymetr",
                                               "kilometr", "cal", "stopa", "jard", "mila", "mila morska", "lata swietlne"),
                           wartosc = c(x, x*1000, x*1000/10, x*1000/100, x*1000/1000000, x*1000/25.4, x*1000/304.8, x*1000/914.4,
                                       x*0.00062137119223733, x*0.00053995680345572, x*1.05702341052e-16),
                           jednostka = c("m", "mm", "cm", "dm", "km", "in", "ft", "yd", "mi", "nmi", "ly"))
    }
  lapply(x, dlugosc_m)
}

#' Konwersja dlugosci (kilometry)
#'
#' @description Funkcja sluzaca do konwersji kilometrow na pozostale jednostki dlugosci
#'
#' @param x argument zawierajacy wartosc kilometrow
#'
#' @return data frame z pozostalymi jednostkami dlugosci i ich wartosci
#' @export
#'
#' @examples
#' dlugosc_km(20)
#' dlugosc_km(c(40, 95))
dlugosc_km = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    dlugosc_km = function(x){
      dlugosc = data.frame(nazwa_jednostki = c("kilometr", "milimetr", "centrymetr", "decymetr", "metr",
                                               "cal", "stopa", "jard", "mila", "mila morska", "lata swietlne"),
                           wartosc = c(x, x*1000000, x*100000, x*10000, x*1000, x*1000000/25.4,
                                       x*1000000/304.8, x*1000000/914.4, x*0.62137119223733, x*0.53995680345572, x*1.05702341052e-13),
                           jednostka = c( "km", "mm", "cm", "dm", "m", "in", "ft", "yd", "mi", "nmi", "ly"))
    }
  lapply(x, dlugosc_km)
}

#' Konwersja dlugosci (cale)
#'
#' @description Funkcja sluzaca do konwersji cali na pozostale jednostki dlugosci
#'
#' @param x argument zawierajacy wartosc cali
#'
#' @return data frame z pozostalymi jednostkami dlugosci i ich wartosci
#' @export
#'
#' @examples
#' dlugosc_in(20)
#' dlugosc_in(c(40, 95))
dlugosc_in= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    dlugosc_in = function(x){
      dlugosc = data.frame(nazwa_jednostki = c("cal", "milimetr", "centrymetr", "decymetr", "metr",
                                               "kilometr", "stopa", "jard", "mila", "mila morska", "lata swietlne"),
                           wartosc = c(x, x*25.4, x*25.4/10, x*25.4/100, x*25.4/1000, x*25.4/1000000, x*0.0833333333333, x*0.02777777777778,
                                       x*1.57828282828283e-5, x*1.371490280778e-5, x*2.68483946273e-5),
                           jednostka = c( "in", "mm", "cm", "dm", "m", "km", "ft", "yd", "mi", "nmi", "ly"))
    }
  lapply(x, dlugosc_in)
}

#' Konwersja dlugosci (stopy)
#'
#' @description Funkcja sluzaca do konwersji stop na pozostale jednostki dlugosci
#'
#' @param x argument zawierajacy wartosc stop
#'
#' @return data frame z pozostalymi jednostkami dlugosci i ich wartosci
#' @export
#'
#' @examples
#' dlugosc_ft(20)
#' dlugosc_ft(c(40, 95))
dlugosc_ft= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    dlugosc_ft = function(x){
      dlugosc = data.frame(nazwa_jednostki = c( "stopa", "milimetr", "centrymetr", "decymetr", "metr",
                                                "kilometr", "cal", "jard", "mila", "mila morska", "lata swietlne"),
                           wartosc = c(x, x*304.8, x*304.8/10, x*304.8/100, x*304.8/1000, x*304.8/1000000, x*12, x*0.3333333333333,
                                       x*0.00018939393939, x*0.00016457883369, x*3.22180735527e-17),
                           jednostka = c( "ft", "mm", "cm", "dm", "m", "km", "in", "yd", "mi", "nmi", "ly"))
    }
  lapply(x, dlugosc_ft)
}

#' Konwersja dlugosci (jardy)
#'
#' @description Funkcja sluzaca do konwersji jardow na pozostale jednostki dlugosci
#'
#' @param x argument zawierajacy wartosc jardow
#'
#' @return data frame z pozostalymi jednostkami dlugosci i ich wartosci
#' @export
#'
#' @examples
#' dlugosc_yd(20)
#' dlugosc_yd(c(40, 95))
dlugosc_yd= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    dlugosc_yd = function(x){
      dlugosc = data.frame(nazwa_jednostki = c( "jard", "milimetr", "centrymetr", "decymetr", "metr",
                                                "kilometr", "cal", "stopa", "mila", "mila morska", "lata swietlne"),
                           wartosc = c(x, x*914.4, x*914.4/10, x*914.4/100, x*914.4/1000, x*914.4/1000000, x*36, x*3,
                                       x*0.00056818181818, x*0.00049373650108, x*9.66542206582e-17),
                           jednostka = c("yd", "mm", "cm", "dm", "m", "km", "in", "ft",  "mi", "nmi", "ly"))
    }
  lapply(x, dlugosc_yd)
}

#' Konwersja dlugosci (mila)
#'
#' @description Funkcja sluzaca do konwersji mil na pozostale jednostki dlugosci
#'
#' @param x argument zawierajacy wartosc mil
#'
#' @return data frame z pozostalymi jednostkami dlugosci i ich wartosci
#' @export
#'
#' @examples
#' dlugosc_mila(20)
#' dlugosc_mila(c(40, 95))
dlugosc_mila= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    dlugosc_mila = function(x){
      dlugosc = data.frame(nazwa_jednostki = c("mila", "milimetr", "centrymetr", "decymetr", "metr",
                                               "kilometr", "cal", "stopa", "jard", "mila morska", "lata swietlne"),
                           wartosc = c(x, x*1609344, x*1609344/10, x*1609344/100, x*1609344/1000, x*1609344/1000000,
                                       x*1609344/25.4, x*1609344/304.8, x*1609344/914.4,
                                       x*0.86897624190065, x*1609344/9460528405000020000),
                           jednostka = c( "mi", "mm", "cm", "dm", "m", "km", "in", "ft", "yd","nmi", "ly"))
    }
  lapply(x, dlugosc_mila)
}

#' Konwersja dlugosci (mila morska)
#'
#' @description Funkcja sluzaca do konwersji mil morskich na pozostale jednostki dlugosci
#'
#' @param x argument zawierajacy wartosc mil morskich
#'
#' @return data frame z pozostalymi jednostkami dlugosci i ich wartosci
#' @export
#'
#' @examples
#' dlugosc_nmi(20)
#' dlugosc_nmi(c(40, 95))
dlugosc_nmi= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    dlugosc_nmi = function(x){
      dlugosc = data.frame(nazwa_jednostki = c( "mila morska", "milimetr", "centrymetr", "decymetr", "metr",
                                                "kilometr", "cal", "stopa", "jard", "mila", "lata swietlne"),
                           wartosc = c(x, x * 1852000, x * 185200, x * 18520, x * 1852, x * 1.852, x * 72913.3858267717,
                                       x * 6076.1154855643, x * 2025.37182852143, x * 1.15077944802354, x * 1.95760735629e-13),
                           jednostka = c( "nmi", "mm", "cm", "dm", "m", "km", "in", "ft", "yd", "mi", "ly"))
    }
  lapply(x, dlugosc_nmi)
}

#' Konwersja dlugosci (lata swietlne)
#'
#' @description Funkcja sluzaca do konwersji lat swietlnych na pozostale jednostki dlugosci
#'
#' @param x argument zawierajacy wartosc lat swietlnych
#'
#' @return data frame z pozostalymi jednostkami dlugosci i ich wartosci
#' @export
#'
#' @examples
#' dlugosc_ly(20)
#' dlugosc_ly(c(40, 95))
dlugosc_ly= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    dlugosc_ly = function(x){
      dlugosc = data.frame(nazwa_jednostki = c( "lata swietlne", "milimetr", "centrymetr", "decymetr", "metr",
                                                "kilometr", "cal", "stopa", "jard", "mila", "mila morska"),
                           wartosc = c(x, x*9460528405000020000, x*9460528405000020000/10, x*9460528405000020000/100, x*9460528405000020000/1000,
                                       x*9460528405000020000/1000000, x*9460528405000020000/25.4, x*9460528405000020000/304.8,
                                       x*9460528405000020000/914.4, x*5878499814210.01, x*5108276676565.88),
                           jednostka = c( "ly", "mm", "cm", "dm", "m", "km", "in", "ft", "yd", "mi", "nmi"))
    }
  lapply(x, dlugosc_ly)
}
