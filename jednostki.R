#POWIERZCHNIA

powierzchnia.mm2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    powierzchnia=data.frame(nazwa_jednostki = c("milimetr_kwadrotowy", "centymetr_kwadratowy", "metr_kwadratowy",
                                                "kilometr_kwadratowy", "ar", "stopa_kwadratowa", " jard_kwadratowy",
                                                "akr"),
                            wartość = c(x, x*100, x*1000000, x*1000000000000, x*100000000, x*92903, x*836100, x*4046856422.4),
                            jednostka = c("mm^2", "cm^2", "m^2", "km^2", "a", "ft^2", "yd^2", "acre"))
  print.data.frame(powierzchnia)
}

powierzchnia.cm2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    powierzchnia=data.frame(nazwa_jednostki = c( "centymetr_kwadratowy", "milimetr_kwadrotowy", "metr_kwadratowy",
                                                 "kilometr_kwadratowy", "ar", "stopa_kwadratowa", " jard_kwadratowy",
                                                 "akr"),
                            wartość = c(x, x/100, x/100*1000000, x/100*1000000000000, x/100*100000000, x/100*92903,
                                        x/100*836100, x/100*4046856422.4),
                            jednostka = c("cm^2", "mm^2", "m^2", "km^2", "a", "ft^2", "yd^2", "acre"))
  print.data.frame(powierzchnia)
}

powierzchnia.m2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    powierzchnia=data.frame(nazwa_jednostki = c("metr_kwadratowy", "milimetr_kwadrotowy", "centymetr_kwadratowy", 
                                                "kilometr_kwadratowy", "ar", "stopa_kwadratowa", " jard_kwadratowy",
                                                "akr"),
                            wartość = c(x, x/1000000, x/1000000*100, x/1000000*1000000000000, x/1000000*100000000,
                                        x/1000000*92903, x/1000000*836100, x/1000000*4046856422.4),
                            jednostka = c("m^2", "mm^2", "cm^2", "km^2", "a", "ft^2", "yd^2", "acre"))
  print.data.frame(powierzchnia)
}

powierzchnia.km2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    powierzchnia=data.frame(nazwa_jednostki = c("kilometr_kwadratowy", "milimetr_kwadrotowy", "centymetr_kwadratowy",
                                                "metr_kwadratowy", "ar", "stopa_kwadratowa", " jard_kwadratowy","akr"),
                            wartość = c(x, x*1000000000000, x*1000000000000*100, x*1000000000000*1000000,
                                        x*1000000000000*100000000, x*1000000000000*92903,
                                        x*1000000000000*836100, x*1000000000000*4046856422.4),
                            jednostka = c( "km^2", "mm^2", "cm^2", "m^2", "a", "ft^2", "yd^2", "acre"))
  print.data.frame(powierzchnia)
}

powierzchnia.a = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    powierzchnia=data.frame(nazwa_jednostki = c("ar", "milimetr_kwadrotowy", "centymetr_kwadratowy", "metr_kwadratowy",
                                                "kilometr_kwadratowy", "stopa_kwadratowa", " jard_kwadratowy",
                                                "akr"),
                            wartość = c(x, x*100000000, x*100000000*100, x*100000000*1000000, x*100000000*1000000000000,
                                        x*100000000*92903, x*100000000*836100, x*100000000*4046856422.4),
                            jednostka = c( "a", "mm^2", "cm^2", "m^2", "km^2", "ft^2", "yd^2", "acre"))
  print.data.frame(powierzchnia)
}

powierzchnia.ft2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    powierzchnia=data.frame(nazwa_jednostki = c("stopa_kwadratowa", "milimetr_kwadrotowy", "centymetr_kwadratowy", 
                                                "metr_kwadratowy", "kilometr_kwadratowy", "ar", " jard_kwadratowy", "akr"),
                            wartość = c(x, x/92903, x/92903*100, x/92903*1000000, x/92903*1000000000000,
                                        x/92903*100000000,  x/92903*836100,  x/92903*4046856422.4),
                            jednostka = c( "ft^2", "mm^2", "cm^2", "m^2", "km^2", "a", "yd^2", "acre"))
  print.data.frame(powierzchnia)
}

powierzchnia.yd2 = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    powierzchnia=data.frame(nazwa_jednostki = c("jard_kwadratowy", "milimetr_kwadrotowy", "centymetr_kwadratowy",
                                                "metr_kwadratowy", "kilometr_kwadratowy", "ar", "stopa_kwadratowa", "akr"),
                            wartość = c(x, x/836100, x/836100*100, x/836100*1000000, x/836100*1000000000000, x/836100*100000000,
                                        x/836100*92903, x/836100*4046856422.4),
                            jednostka = c( "yd^2", "mm^2", "cm^2", "m^2", "km^2", "a", "ft^2", "acre"))
  print.data.frame(powierzchnia)
}

powierzchnia.acre = function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    powierzchnia=data.frame(nazwa_jednostki = c("akr","milimetr_kwadrotowy", "centymetr_kwadratowy", "metr_kwadratowy",
                                                "kilometr_kwadratowy", "ar", "stopa_kwadratowa", " jard_kwadratowy" ),
                            wartość = c(x, x*4046856422.4, x*4046856422.4*100, x*4046856422.4*1000000,
                                        x*4046856422.4*1000000000000, x*4046856422.4*100000000,
                                        x*4046856422.4*92903, x*4046856422.4*836100),
                            jednostka = c( "acre", "mm^2", "cm^2", "m^2", "km^2", "a", "ft^2", "yd^2"))
  print.data.frame(powierzchnia)
}



#TEMPERATURA


temperatura.C= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    temperatury=data.frame(nazwa_jednostki = c("skala_Celsjusza", "skala_Kelvina",  "skala_Fahrenheita",
                                               "skala_Rankinea", "skala_Reaumura", "skala_Newtona"),
                           wartość = c(x, x+273.15, (x*1.8)+32, (x+ 273.15)*1.8, (x*4)/5, x/3),
                           jednostki = c("^C", "K", "^F", "^R", "^Ré", "^N"))
  print.data.frame(temperatury)
}

temperatura.K= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    temperatury=data.frame(nazwa_jednostki = c("skala_Kelvina","skala_Celsjusza",   "skala_Fahrenheita",
                                               "skala_Rankinea", "skala_Reaumura", "skala_Newtona"),
                           wartość = c(x, x-273.15, ((x-273.15)*1.8)+32, x*1.8, ((x-273.15)*4)/5, (x-273.15)/3),
                           jednostki = c( "K", "^C", "^F", "^R", "^Ré", "^N"))
  print.data.frame(temperatury)
}

temperatura.F= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    temperatury=data.frame(nazwa_jednostki = c("skala_Fahrenheita", "skala_Celsjusza", "skala_Kelvina",  
                                               "skala_Rankinea", "skala_Reaumura", "skala_Newtona"),
                           wartość = c(x, (x-32)/1.8, (x+459.67)/1.8, x+459.67, (x-32)*4/9, (x-32)*11/60),
                           jednostki = c("^F", "^C", "K",  "^R", "^Ré", "^N"))
  print.data.frame(temperatury)
}

temperatura.R= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    temperatury=data.frame(nazwa_jednostki = c("skala_Rankinea", "skala_Celsjusza", "skala_Kelvina",  "skala_Fahrenheita",
                                               "skala_Reaumura", "skala_Newtona"),
                           wartość = c(x, (x/1.8)-273.15, x/1.8, x-459.67, ((x/1.8)-273.15)*4/5, ((x/1.8)-273.15)/3),
                           jednostki = c("^R", "^C", "K", "^F", "^Ré", "^N"))
  print.data.frame(temperatury)
}

temperatura.Re= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    temperatury=data.frame(nazwa_jednostki = c("skala_Reaumura", "skala_Celsjusza", "skala_Kelvina",  "skala_Fahrenheita",
                                               "skala_Rankinea",  "skala_Newtona"),
                           wartość = c(x, x*1.25, x*1.25+273.15, x*2.25+32, (x*1.25+273.15)*1.8, x*1.25/3),
                           jednostki = c("^Ré", "^C", "K", "^F", "^R", "^N"))
  print.data.frame(temperatury)
}

temperatura.K= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    temperatury=data.frame(nazwa_jednostki = c("skala_Newtona", "skala_Celsjusza", "skala_Kelvina",  "skala_Fahrenheita",
                                               "skala_Rankinea", "skala_Reaumura"),
                           wartość = c(x, x*3, x*3+273.15, x*60/11-32, (x*3+273.15)*1.8, x*3*4/5),
                           jednostki = c( "^N", "^C", "K", "^F", "^R", "^Ré"))
  print.data.frame(temperatury)
}


#PRĘDKOŚĆ

predkosc_ms= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    prędkość=data.frame(nazwa_jednostki = c("metry_na_sekundę", "metry_na_minutę"," kilometry_na_sekundę", 
                                            "kilometry_na_minutę","kilometry_na_godzinę", "stopy_na_sekundę",
                                            "mile_na_godzinę", " machy", "węzły"),
                        wartość = c(x, x*60, x*0.001, x*0.06, x*3.6, x*3.2808398950131, x*2.2369362920544,
                                    x*0.002938669957977, x*1.9438461717893),
                        jednostki = c("m/s", "m/min", "km/s", "km/min", "km/h", "ft/s", "mph", "Mach", "kn"))
    print.data.frame(prędkość)
}

predkosc_mmin= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    prędkość=data.frame(nazwa_jednostki = c("metry_na_minutę", "metry_na_sekundę", " kilometry_na_sekundę", 
                                            "kilometry_na_minutę","kilometry_na_godzinę", "stopy_na_sekundę",
                                            "mile_na_godzinę", " machy", "węzły"),
                        wartość = c(x, x/60, x/60*0.001, x*0.001, x*0.06, x*3.2808398950131/60, x*2.2369362920544/60, 
                                    x*0.002938669957977/60, x*1.9438461717893/60  ),
                        jednostki = c( "m/min", "m/s", "km/s", "km/min", "km/h", "ft/s", "mph", "Mach", "kn"))
    print.data.frame(prędkość)
}


predkosc_kms= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    prędkość=data.frame(nazwa_jednostki = c("kilometry_na_sekundę", "metry_na_sekundę", "metry_na_minutę",  
                                            "kilometry_na_minutę","kilometry_na_godzinę", "stopy_na_sekundę",
                                            "mile_na_godzinę", " machy", "węzły"),
                        wartość = c(x, x*1000, x*1000*60, x*1000*0.06, x*1000*3.6, x*1000*3.2808398950131, 
                                    x*2.2369362920544*1000, x*1000*0.002938669957977, x*1000*1.9438461717893 ),
                        jednostki = c("km/s", "m/s", "m/min",  "km/min", "km/h", "ft/s", "mph", "Mach", "kn"))
    print.data.frame(prędkość)
}

predkosc_kmmin= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    prędkość=data.frame(nazwa_jednostki = c("kilometry_na_minutę", "metry_na_sekundę", "metry_na_minutę"," kilometry_na_sekundę", 
                                            "kilometry_na_godzinę", "stopy_na_sekundę",
                                            "mile_na_godzinę", " machy", "węzły"),
                        wartość = c(x, x/0.06, x/0.06*60, x/0.06*0.001, x/0.06*3.6, x/0.06*3.2808398950131, x/0.06*2.2369362920544, 
                                    x/0.06*x*0.002938669957977, x*1.9438461717893/0.06 ),
                        jednostki = c("km/min", "m/s", "m/min", "km/s", "km/h", "ft/s", "mph", "Mach", "kn"))
    print.data.frame(prędkość)
    
}

predkosc_kmh= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    prędkość=data.frame(nazwa_jednostki = c("kilometry_na_godzinę", "metry_na_sekundę", "metry_na_minutę"," kilometry_na_sekundę", 
                                            "kilometry_na_minutę", "stopy_na_sekundę",
                                            "mile_na_godzinę", " machy", "węzły"),
                        wartość = c(x, x/3.6, x/3.6*60, x/3.6*0.001, x/3.6*0.06, x/3.6*3.2808398950131,
                                    x/3.6*2.2369362920544, x/3.6*0.002938669957977, x/3.6*1.9438461717893 ),
                        jednostki = c("km/h", "m/s", "m/min", "km/s", "km/min", "ft/s", "mph", "Mach", "kn"))
    print.data.frame(prędkość)
    
    
}

predkosc_fts= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    prędkość=data.frame(nazwa_jednostki = c("stopy_na_sekundę", "metry_na_sekundę", "metry_na_minutę"," kilometry_na_sekundę", 
                                            "kilometry_na_minutę","kilometry_na_godzinę",
                                            "mile_na_godzinę", " machy", "węzły"),
                        wartość = c(x, x/3.2808398950131, x/3.2808398950131*60, x/3.2808398950131*0.001, x/3.2808398950131*0.06, 
                                    x/3.2808398950131*3.6,   x/3.2808398950131*2.2369362920544, 
                                    x/3.2808398950131*0.002938669957977,  x/3.2808398950131*1.9438461717893),
                        jednostki = c("ft/s", "m/s", "m/min", "km/s", "km/min", "km/h", "mph", "Mach", "kn"))
    print.data.frame(prędkość)
    
    
}

predkosc_mph= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    prędkość=data.frame(nazwa_jednostki = c("mile_na_godzinę", "metry_na_sekundę", "metry_na_minutę"," kilometry_na_sekundę", 
                                            "kilometry_na_minutę","kilometry_na_godzinę", "stopy_na_sekundę",
                                            " machy", "węzły"),
                        wartość = c(x, x/2.2369362920544, x/2.2369362920544*60, x/2.2369362920544*0.001, 
                                    x/2.2369362920544*0.06, x/2.2369362920544*3.6, x/2.2369362920544*3.2808398950131, 
                                    x/2.2369362920544*0.002938669957977, x/2.2369362920544*1.9438461717893),
                        jednostki = c("mph", "m/s", "m/min", "km/s", "km/min", "km/h", "ft/s",  "Mach", "kn"))
    print.data.frame(prędkość)
    
    
}

predkosc_Mach= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    prędkość=data.frame(nazwa_jednostki = c(" machy", "metry_na_sekundę", "metry_na_minutę"," kilometry_na_sekundę", 
                                            "kilometry_na_minutę","kilometry_na_godzinę", "stopy_na_sekundę",
                                            "mile_na_godzinę", "węzły"),
                        wartość = c(x, x/0.002938669957977, x/0.002938669957977*60, x/0.002938669957977*0.001,
                                    x/0.002938669957977*0.06, x/0.002938669957977*3.6, x/0.002938669957977*3.2808398950131,
                                    x/0.002938669957977*2.2369362920544, x/0.002938669957977*1.9438461717893),
                        jednostki = c("Mach", "m/s", "m/min", "km/s", "km/min", "km/h", "ft/s", "mph", "kn"))
    print.data.frame(prędkość)
    
    
}

predkosc_kn= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    prędkość=data.frame(nazwa_jednostki = c("węzły", "metry_na_sekundę", "metry_na_minutę"," kilometry_na_sekundę", 
                                            "kilometry_na_minutę","kilometry_na_godzinę", "stopy_na_sekundę",
                                            "mile_na_godzinę", " machy"),
                        wartość = c(x, x/1.9438461717893, x/1.9438461717893*60, x/1.9438461717893*0.001, x/1.9438461717893*0.06,
                                    x/1.9438461717893*3.6, x/1.9438461717893*3.2808398950131, 
                                    x/1.9438461717893*2.2369362920544, x/1.9438461717893*0.002938669957977),
                        jednostki = c( "kn", "m/s", "m/min", "km/s", "km/min", "km/h", "ft/s", "mph", "Mach"))
    print.data.frame(prędkość)
    
    
}


#DŁUGOŚĆ


dlugosc_mm= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c("milimetr", "centrymetr", "decymetr", "metr", "kilometr",
                                           "cal", "stopa", "jard", "mila", "mila_morska", "lata_swietlne"),
                       wartość = c(x, x*10, x*100, x*1000, x*1000000, x*25.4, x*304.8, x*914.4,
                                   x*1609344, x*1853184, x*9460528405000020000),
                       jednostki = c("mm", "cm", "dm", "m", "km", "in", "ft", "yd", "mila", "Mm", "ly"))
    print.data.frame(długość)
    
    
}

dlugosc_cm= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c("centrymetr", "milimetr","decymetr", "metr",
                                           "kilometr", "cal", "stopa", "jard", "mila", "mila_morska", "lata_swietlne"),
                       wartość = c(x, x/10, x/10*100, x/10*1000, x/10*1000000, x/10*25.4, x/10*304.8,
                                   x/10*914.4, x/10*1609344, x/10, x/10*9460528405000020000),
                       jednostki = c("cm", "mm",  "dm", "m", "km", "in", "ft", "yd", "mila", "Mm", "ly"))
    print.data.frame(długość)
    
    
}

dlugosc_dm= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c("decymetr", "milimetr", "centrymetr", "metr",
                                           "kilometr", "cal", "stopa", "jard", "mila", "mila_morska", "lata_swietlne"),
                       wartość = c(x, x/100, x/100*10, x/100*1000, x/100*1000000, x/100*25.4, x/100*304.8, x/100*914.4, 
                                   x/100*1609344, x/100*1853184, x/100*9460528405000020000),
                       jednostki = c( "dm", "mm", "cm", "m", "km", "in", "ft", "yd", "mila", "Mm", "ly"))
    print.data.frame(długość)
    
}

dlugosc_m= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c("metr", "milimetr", "centrymetr", "decymetr",
                                           "kilometr", "cal", "stopa", "jard", "mila", "mila_morska", "lata_swietlne"),
                       wartość = c(x, x/1000, x/1000*10, x/1000*100, x/1000*1000000, x/1000*25.4, x/1000*304.8, x/1000*914.4,
                                   x/1000*1609344, x/1000*1853184, x/1000*9460528405000020000),
                       jednostki = c("m", "mm", "cm", "dm", "km", "in", "ft", "yd", "mila", "Mm", "ly"))
    print.data.frame(długość)
    
}

dlugosc_km= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c("kilometr", "milimetr", "centrymetr", "decymetr", "metr",
                                           "cal", "stopa", "jard", "mila", "mila_morska", "lata_swietlne"),
                       wartość = c(x, x/1000000, x/1000000*10, x/1000000*100, x/1000000*1000, x/1000000*25.4, 
                                   x/1000000*304.8, x/1000000*914.4, x/1000000*1609344, x/1000000*1853184, x/1000000*9460528405000020000),
                       jednostki = c( "km", "mm", "cm", "dm", "m", "in", "ft", "yd", "mila", "Mm", "ly"))
    print.data.frame(długość)
    
}

dlugosc_in= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c("cal", "milimetr", "centrymetr", "decymetr", "metr",
                                           "kilometr", "stopa", "jard", "mila", "mila_morska", "lata_swietlne"),
                       wartość = c(x, x/25.4, x/25.4*10, x/25.4*100, x/25.4*1000, x/25.4*1000000, x/25.4*304.8, x/25.4*914.4,
                                   x/25.4*1609344, x/25.4*1853184, x/25.4*9460528405000020000),
                       jednostki = c( "in", "mm", "cm", "dm", "m", "km", "ft", "yd", "mila", "Mm", "ly"))
    print.data.frame(długość)
    
}

dlugosc_ft= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c( "stopa", "milimetr", "centrymetr", "decymetr", "metr",
                                            "kilometr", "cal", "jard", "mila", "mila_morska", "lata_swietlne"),
                       wartość = c(x, x/304.8, x/304.8*10, x/304.8*100, x/304.8*1000, x/304.8*1000000, x/304.8*25.4, x/304.8*914.4,
                                   x/304.8*1609344, x/304.8*1853184, x/304.8*946052840500002000),
                       jednostki = c( "ft", "mm", "cm", "dm", "m", "km", "in", "yd", "mila", "Mm", "ly"))
    print.data.frame(długość)
    
}

dlugosc_yd= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c( "jard", "milimetr", "centrymetr", "decymetr", "metr",
                                            "kilometr", "cal", "stopa", "mila", "mila_morska", "lata_swietlne"),
                       wartość = c(x, x/914.4, x/914.4*10, x/914.4*100, x/914.4*1000, x/914.4*1000000, x/914.4*25.4, x/914.4*304.8, 
                                   x/914.4*1609344, x/914.4*1853184, x/914.4*9460528405000020000),
                       jednostki = c("yd", "mm", "cm", "dm", "m", "km", "in", "ft",  "mila", "Mm", "ly"))
    print.data.frame(długość)
    
}

dlugosc_mila= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c("mila", "milimetr", "centrymetr", "decymetr", "metr",
                                           "kilometr", "cal", "stopa", "jard", "mila_morska", "lata_swietlne"),
                       wartość = c(x, x/1609344, x/1609344*10, x/1609344*100, x/1609344*1000, x/1609344*1000000, 
                                   x/1609344*25.4, x/1609344*304.8, x/1609344*914.4,
                                   x/1609344*1853184, x/1609344*9460528405000020000),
                       jednostki = c( "mila", "mm", "cm", "dm", "m", "km", "in", "ft", "yd","Mm", "ly"))
    print.data.frame(długość)
    
}

dlugosc_Mm= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c( "mila_morska", "milimetr", "centrymetr", "decymetr", "metr",
                                            "kilometr", "cal", "stopa", "jard", "mila", "lata_swietlne"),
                       wartość = c(x, x/1853184, x/1853184*10, x/1853184*100, x/1853184*1000, x/1853184*1000000, x/1853184*25.4, 
                                   x/1853184*304.8, x/1853184*914.4, x/1853184, x/1853184*9460528405000020000),
                       jednostki = c( "Mm", "mm", "cm", "dm", "m", "km", "in", "ft", "yd", "mila", "ly"))
    print.data.frame(długość)
    
}

dlugosc_ly= function(x){
  if(!(is.numeric(x))){
    stop("Argument 'x' musi być typu numerycznego, np. 10, 420, 69420")
  } else if(!(all(c(length(x) == 1)))){
    stop("Każdy z argumentów może przyjmować tylko jedną wartość.")
  } else
    długość=data.frame(nazwa_jednostki = c( "lata_swietlne", "milimetr", "centrymetr", "decymetr", "metr",
                                            "kilometr", "cal", "stopa", "jard", "mila", "mila_morska"),
                       wartość = c(x, x/9460528405000020000, x/9460528405000020000*10, x/9460528405000020000*100, x/9460528405000020000*1000,
                                   x/9460528405000020000*1000000, x/9460528405000020000*25.4, x/9460528405000020000*304.8,
                                   x/9460528405000020000*914.4, x/9460528405000020000*1609344, x/9460528405000020000*1853184),
                       jednostki = c( "ly", "mm", "cm", "dm", "m", "km", "in", "ft", "yd", "mila", "Mm"))
    print.data.frame(długość)
    
}
