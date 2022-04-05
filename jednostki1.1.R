#objetosc

objetosc_ml = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_ml=function(x){
      (mililitr = data.frame(nazwa_jednostki = c("mililitr", "centymetr sześcienny", "litr", "metr sześcienny", "cal sześcienny", 
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
    }
  lapply(x,objetosc_ml)
}


objetosc_cm3 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else 
    objetosc_cm3=function(x){
      (cm3 = data.frame(nazwa_jednostki = c("centymetr sześcienny", "mililitr", "litr", "metr sześcienny", "cal sześcienny", 
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
    }
  lapply(x,objetosc_cm3)
}

objetosc_L = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else 
    objetosc_L=function(x){
      (litr = data.frame(nazwa_jednostki = c("litr", "mililitr", "centymetr sześcienny", "metr sześcienny", "cal sześcienny", 
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
    }
  lapply(x,objetosc_L)
}

objetosc_m3 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_m3=function(x){
      (m3 = data.frame(nazwa_jednostki = c("metr sześcienny", "mililitr", "centymetr sześcienny", "litr", "cal sześcienny", 
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
    }
  lapply(x,objetosc_m3)
}

objetosc_in3 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_in3=function(x){
      (in3 = data.frame(nazwa_jednostki = c("cal sześcienny", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
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
    }
  lapply(x,objetosc_in3)
}

objetosc_ft3 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_ft3=function(x){
      (ft3 = data.frame(nazwa_jednostki = c("stopa sześcienna", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
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
    }
  lapply(x,objetosc_ft3)
}

objetosc_yd3 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_yd3 = function(x){
      (yd3 = data.frame(nazwa_jednostki = c("jard sześcienny", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
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
    }
  lapply(x,objetosc_yd3)
}

objetosc_galon_us = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_galon_us = function(x){
      (galon_us = data.frame(nazwa_jednostki = c("galon (płyn, USA)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
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
    }
  lapply(x,objetosc_galon_us)
}

objetosc_galon_dry_us = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_galon_dry_us = function(x){
      (galon_dry = data.frame(nazwa_jednostki = c("galon (suchy, USA)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
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
    }
  lapply(x,objetosc_galon_dry_us)
}

objetosc_galon_uk = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_galon_uk = function(x){
      (galon_uk = data.frame(nazwa_jednostki = c("galon (UK)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
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
    }
  lapply(x,objetosc_galon_uk)
}

objetosc_uncja_us = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_uncja_us = function(x){ 
      (uncja_us = data.frame(nazwa_jednostki = c("uncja (USA)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
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
    }
  lapply(x,objetosc_uncja_us)
}

objetosc_uncja_uk = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_uncja_uk = function(x){
      (uncja_uk = data.frame(nazwa_jednostki = c("uncja (UK)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
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
    }
  lapply(x,objetosc_uncja_uk)
}

objetosc_kwarta_us = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_kwarta_us = function(x){
      (kwarta_us = data.frame(nazwa_jednostki = c("kwarta (USA)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
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
    }
  lapply(x,objetosc_kwarta_us)
}

objetosc_kwarta_uk = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_kwarta_uk = function(x){
      (kwarta_uk = data.frame(nazwa_jednostki = c("kwarta (UK)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
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
    }
  lapply(x,objetosc_kwarta_uk)
}

objetosc_pinta_us = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_pinta_us = function(x){
      (pinta_us = data.frame(nazwa_jednostki = c("pinta (USA)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
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
    }
  lapply(x,objetosc_pinta_us)
}

objetosc_pinta_uk = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    objetosc_pinta_uk = function(x){
      (pinta_uk = data.frame(nazwa_jednostki = c("pinta (UK)", "mililitr", "centymetr sześcienny", "litr", "metr sześcienny", 
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
    }
  lapply(x,objetosc_pinta_uk)
}

objetosc_barylka = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
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
    }
  lapply(x,objetosc_barylka)
}



#temperatura

temperatura_C= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    temperatura_C= function(x){
      temperatury=data.frame(nazwa_jednostki = c("skala Celsjusza", "skala Kelvina",  "skala Fahrenheita",
                                                 "skala Rankinea", "skala Reaumura", "skala Newtona"),
                             wartość = c(x, x+273.15, (x*1.8)+32, (x+ 273.15)*1.8, (x*4)/5, x/3),
                             jednostka = c("°C", "K", "°F", "°R", "°Ré", "°N"))
    }
  lapply(x,temperatura_C)
}

temperatura_K= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    temperatura_K= function(x){
      temperatury=data.frame(nazwa_jednostki = c("skala Kelvina","skala Celsjusza",   "skala Fahrenheita",
                                                 "skala Rankinea", "skala Reaumura", "skala Newtona"),
                             wartość = c(x, x-273.15, ((x-273.15)*1.8)+32, x*1.8, ((x-273.15)*4)/5, (x-273.15)/3),
                             jednostka = c( "K", "°C", "°F", "°R", "°Ré", "°N"))
    }
  lapply(x,temperatura_K)
}

temperatura_F= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    temperatura_F= function(x){
      temperatury=data.frame(nazwa_jednostki = c("skala Fahrenheita", "skala Celsjusza", "skala Kelvina",  
                                                 "skala Rankinea", "skala Reaumura", "skala Newtona"),
                             wartość = c(x, (x-32)/1.8, (x+459.67)/1.8, x+459.67, (x-32)*4/9, (x-32)*11/60),
                             jednostka = c("°F", "°C", "K",  "°R", "°Ré", "°N"))
    }
  lapply(x,temperatura_F)
}

temperatura_R= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    temperatura_R= function(x){
      temperatury=data.frame(nazwa_jednostki = c("skala Rankinea", "skala Celsjusza", "skala Kelvina",  "skala Fahrenheita",
                                                 "skala Reaumura", "skala Newtona"),
                             wartość = c(x, (x/1.8)-273.15, x/1.8, x-459.67, ((x/1.8)-273.15)*4/5, ((x/1.8)-273.15)/3),
                             jednostka = c("°R", "°C", "K", "°F", "°Ré", "°N"))
    }
  lapply(x,temperatura_R)
}

temperatura_Re= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    temperatura_Re= function(x){
      temperatury=data.frame(nazwa_jednostki = c("skala Reaumura", "skala Celsjusza", "skala Kelvina",  "skala Fahrenheita",
                                                 "skala Rankinea",  "skala Newtona"),
                             wartość = c(x, x*1.25, x*1.25+273.15, x*2.25+32, (x*1.25+273.15)*1.8, x*1.25/3),
                             jednostka = c("°Ré", "°C", "K", "°F", "°R", "°N"))
    }
  lapply(x,temperatura_Re)
}

temperatura_N= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else
    temperatura_N= function(x){
      temperatury=data.frame(nazwa_jednostki = c("skala Newtona", "skala Celsjusza", "skala Kelvina",  "skala Fahrenheita",
                                                 "skala Rankinea", "skala Reaumura"),
                             wartość = c(x, x*3, x*3+273.15, x*60/11-32, (x*3+273.15)*1.8, x*3*4/5),
                             jednostka = c( "°N", "°C", "K", "°F", "°R", "°Ré"))
    }
  lapply(x,temperatura_N)
}