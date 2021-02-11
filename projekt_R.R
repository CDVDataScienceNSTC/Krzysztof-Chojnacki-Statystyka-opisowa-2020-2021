#Zadanie:
#Zmiana cen detalicznych wybranych towarów
#i usług konsumpcyjnych w Polsce w latach 2006-2019
#miary średnie i miary dyspersji
#gdzie i kiedy w zadanych ramach czasowych   
#życie w Polsce było najtańsze a gdzie i kiedy było 
#najdroższe w odniesieniu do pozostałych regionów kraju

#import biblioteki tidverse
library (tidyverse)
#pobór danych z pliku CSV
dane = read.csv("CENY_2917_CTAB_20210207171649.csv", sep = ";", dec = ",", encoding = "UTF-8")

#funkcja do wyznaczania dominanty
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#dane - główna tabela z danymi z pliku csv (dane GUS o pięciu produktach, 
#dla 16 województw, w zakresie lat 2006-2019
dane = as_tibble(dane)

#pętla do wylistowania województw
n = 1
lista_wojewodztw <- list ()
for (i in dane$Nazwa){
  lista_wojewodztw = append(lista_wojewodztw, lista_wojewodztw[[n]] <- i)
  n = n + 1
}

#stworzenie bardziej ogólnej tabeli do zobrazownia średnich cen
#dla produktów w latach 2006-2019
kategorie_towarow = dane
kategorie_towarow = kategorie_towarow[ , apply(kategorie_towarow, 2, function(x) !any(is.na(x)))]

#wyodrębnienie 5 tabel dla 5 produktów na podstawie słóW kluczowych
piwo = dane %>% select (1:2, contains("piwo"))
zimna_woda = dane %>% select (1:2, contains("zimna"))
CO = dane %>% select (1:2, contains("ogrzewanie"))
ciepla_woda =dane %>% select (1:2, contains("ciepła"))
wizyta_u_lekarza = dane %>% select (1:2, contains("lekarza"))

#w pętli grupuję wartości w ramach średnich cen po latach 2006-2019, każdy rok oddzielnie
#1Piwo
for (i in 6:19){
  if (i < 10){
    rok_1 = paste("200",as.character(i), sep = "")
    srednia_kolumn = piwo$mean <- round(rowMeans(subset(piwo %>% select (contains(rok_1)), na.rm = TRUE)),2)
    colnames(piwo)[length(piwo)] = c(paste("śr.cena.",rok_1, sep = ""))
  }
  else{
    rok_2 = paste("20",as.character(i), sep = "")
    srednia_kolumn = piwo$mean <- round(rowMeans(subset(piwo %>% select (contains(rok_2)), na.rm = TRUE)),2)
    colnames(piwo)[length(piwo)] = c(paste("śr.cena.",rok_2, sep = ""))
  }
}
#usuwam niepotrzebne kolumny dla poszczegolnych miesięcy
piwo = piwo [-c(1,3:170)]

#ta wartosc posluzy mi do wyznaczenia granicy dzialań na kolumnach
#w zakresie poszczególnych wierszy
dlugosc = length(piwo)

#przy pomocy apply wyznaczam odchylenie standardowe (sd) oraz mediane (md)
#dla kazdego wiersza
std_dev_row = round(apply(piwo[,2:dlugosc],1,sd),3)
piwo$odch.std. <- std_dev_row
med_row = round(apply(piwo[,2:dlugosc],1,median),2)
piwo$mediana <- med_row

#przy pomocy tej pętli wpisuję wartości mody do wierszy
for (i in 1:16){
  moda = getmode(piwo[i,2:dlugosc])
  piwo[i,18]<- as.character(moda)
}
colnames(piwo)[length(piwo)] = c("Dominanta")

#wyznaczam średnią z wszystkich lat dla danego wyrobu
srednia_wiersze = round(apply (piwo[,2:dlugosc],1,mean, na.rm = TRUE),2)
piwo$"sr.cena 06-19" <- srednia_wiersze

#określam, w którym wojewódzwie były max i min warotści
#średnich cen dla danego wyrobu oraz odchylenia standardowego
#aby móc stwiedzić, gdzie było najtaniej/najdrożej
#oraz gdzie ceny były najbardziej/najmniej stabilne
max_piwo = rownames(piwo)[which.max(piwo$`sr.cena 06-19`)]
wojewodztwo_max_piwo = lista_wojewodztw[as.numeric(max_piwo)]
min_piwo = rownames(piwo)[which.min(piwo$`sr.cena 06-19`)]
wojewodztwo_min_piwo = lista_wojewodztw[as.numeric(min_piwo)]
max_dev_piwo = rownames(piwo)[which.max(piwo$odch.std.)]
wojewodztwo_max_dev_piwo = lista_wojewodztw[as.numeric(max_dev_piwo)]
min_dev_piwo = rownames(piwo)[which.min(piwo$odch.std.)]
wojewodztwo_min_dev_piwo = lista_wojewodztw[as.numeric(min_dev_piwo)]

#dla kolejnych produktów nie będę opisywał - analogiczne działanie
#2 ZImna woda
for (i in 6:19){
  if (i < 10){
    rok_1 = paste("200",as.character(i), sep = "")
    srednia_kolumn = zimna_woda$mean <- round(rowMeans(subset(zimna_woda %>% select (contains(rok_1)), na.rm = TRUE)),2)
    colnames(zimna_woda)[length(zimna_woda)] = c(paste("śr.cena.",rok_1, sep = ""))
  }
  else{
    rok_2 = paste("20",as.character(i), sep = "")
    srednia_kolumn = zimna_woda$mean <- round(rowMeans(subset(zimna_woda %>% select (contains(rok_2)), na.rm = TRUE)),2)
    colnames(zimna_woda)[length(zimna_woda)] = c(paste("śr.cena.",rok_2, sep = ""))
  }
}
zimna_woda = zimna_woda [-c(1,3:170)]
std_dev_row = round(apply(zimna_woda[,2:dlugosc],1,sd),3)
zimna_woda$odch.std. <- std_dev_row
med_row = round(apply(zimna_woda[,2:dlugosc],1,median),2)
zimna_woda$mediana <- med_row
for (i in 1:16){
  moda = getmode(zimna_woda[i,2:dlugosc])
  zimna_woda[i,18]<- as.character(moda)
}
colnames(zimna_woda)[length(zimna_woda)] = c("Dominanta")
srednia_wiersze = round(apply (zimna_woda[,2:dlugosc],1,mean, na.rm = TRUE),2)
zimna_woda$"sr.cena 06-19" <- srednia_wiersze
max_zimna_woda = rownames(zimna_woda)[which.max(zimna_woda$`sr.cena 06-19`)]
wojewodztwo_max_zimna_woda = lista_wojewodztw[as.numeric(max_zimna_woda)]
min_zimna_woda = rownames(zimna_woda)[which.min(zimna_woda$`sr.cena 06-19`)]
wojewodztwo_min_zimna_woda = lista_wojewodztw[as.numeric(min_zimna_woda)]

max_dev_zwoda = rownames(zimna_woda)[which.max(piwo$odch.std.)]
wojewodztwo_max_dev_zwoda = lista_wojewodztw[as.numeric(max_dev_zwoda)]
min_dev_zwoda = rownames(zimna_woda)[which.min(piwo$odch.std.)]
wojewodztwo_min_dev_zwoda = lista_wojewodztw[as.numeric(min_dev_zwoda)]

#3 Centralne ogrzewanie
for (i in 6:19){
  if (i < 10){
    rok_1 = paste("200",as.character(i), sep = "")
    srednia_kolumn = CO$mean <- round(rowMeans(subset(CO %>% select (contains(rok_1)), na.rm = T)),2)
    colnames(CO)[length(CO)] = c(paste("śr.cena.",rok_1, sep = ""))
  }
  else{
    rok_2 = paste("20",as.character(i), sep = "")
    srednia_kolumn = CO$mean <- round(rowMeans(subset(CO %>% select (contains(rok_2)), na.rm = T)),2)
    colnames(CO)[length(CO)] = c(paste("śr.cena.",rok_2, sep = ""))
  }
}
CO = CO [-c(1,3:170)]
#przy pomocy tej funkcji usuwam kolumny bez wartości liczbowych
#tak aby móc liczyć statystyki
#reszta taka sama jak poprzednio - różni się tylko ilość
#kolumn brana do kalkulacji
CO = CO[ , apply(CO, 2, function(x) !any(is.na(x)))]
dlugosc_CO = length(CO)
std_dev_row = round(apply(CO[,2:dlugosc_CO],1,sd, na.rm = TRUE),2)
CO$odch.std. <- std_dev_row
med_row = round(apply(CO[,2:dlugosc_CO],1,median, na.rm = TRUE),2)
CO$mediana <- med_row
for (i in 1:16){
  moda = getmode(CO[i,2:dlugosc_CO])
  CO[i,11]<- as.character(moda)
}
colnames(CO)[length(CO)] = c("Dominanta")
srednia_wiersze = round(apply (CO[,2:dlugosc_CO],1,mean, na.rm = TRUE),2)
CO$"sr.cena 06-19" <- srednia_wiersze
max_CO = rownames(CO)[which.max(CO$`sr.cena 06-19`)]
wojewodztwo_max_CO = lista_wojewodztw[as.numeric(max_CO)]
min_CO = rownames(CO)[which.min(CO$`sr.cena 06-19`)]
wojewodztwo_min_CO = lista_wojewodztw[as.numeric(min_CO)]

max_dev_CO = rownames(CO)[which.max(CO$odch.std.)]
wojewodztwo_max_dev_CO = lista_wojewodztw[as.numeric(max_dev_CO)]
min_dev_CO = rownames(CO)[which.min(CO$odch.std.)]
wojewodztwo_min_dev_CO = lista_wojewodztw[as.numeric(min_dev_CO)]

#4 Ciepła woda
for (i in 6:19){
  if (i < 10){
    rok_1 = paste("200",as.character(i), sep = "")
    srednia_kolumn = ciepla_woda$mean <- round(rowMeans(subset(ciepla_woda %>% select (contains(rok_1)), na.rm = TRUE)),2)
    colnames(ciepla_woda)[length(ciepla_woda)] = c(paste("śr.cena.",rok_1, sep = ""))
  }
  else{
    rok_2 = paste("20",as.character(i), sep = "")
    srednia_kolumn = ciepla_woda$mean <- round(rowMeans(subset(ciepla_woda %>% select (contains(rok_2)), na.rm = TRUE)),2)
    colnames(ciepla_woda)[length(ciepla_woda)] = c(paste("śr.cena.",rok_2, sep = ""))
  }
}
ciepla_woda = ciepla_woda [-c(1,3:170)]
std_dev_row = round(apply(ciepla_woda[,2:dlugosc],1,sd),2)
ciepla_woda$odch.std. <- std_dev_row
med_row = round(apply(ciepla_woda[,2:dlugosc],1,median),2)
ciepla_woda$mediana <- med_row
for (i in 1:16){
  moda = getmode(ciepla_woda[i,2:dlugosc])
  ciepla_woda[i,18]<- as.character(moda)
}
colnames(ciepla_woda)[length(ciepla_woda)] = c("Dominanta")
srednia_wiersze = round(apply (ciepla_woda[,2:dlugosc],1,mean, na.rm = TRUE),2)
ciepla_woda$"sr.cena 06-19" <- srednia_wiersze
max_ciepla_woda = rownames(ciepla_woda)[which.max(ciepla_woda$`sr.cena 06-19`)]
wojewodztwo_max_ciepla_woda = lista_wojewodztw[as.numeric(max_ciepla_woda)]
min_ciepla_woda = rownames(ciepla_woda)[which.min(ciepla_woda$`sr.cena 06-19`)]
wojewodztwo_min_ciepla_woda = lista_wojewodztw[as.numeric(min_ciepla_woda)]

max_dev_cwoda = rownames(ciepla_woda)[which.max(ciepla_woda$odch.std.)]
wojewodztwo_max_dev_cwoda = lista_wojewodztw[as.numeric(max_dev_cwoda)]
min_dev_cwoda = rownames(ciepla_woda)[which.min(ciepla_woda$odch.std.)]
wojewodztwo_min_dev_cwoda = lista_wojewodztw[as.numeric(min_dev_cwoda)]

#5 Wizyta u lekarza
for (i in 6:19){
  if (i < 10){
    rok_1 = paste("200",as.character(i), sep = "")
    srednia_kolumn = wizyta_u_lekarza$mean <- round(rowMeans(subset(wizyta_u_lekarza %>% select (contains(rok_1)), na.rm = TRUE)),2)
    colnames(wizyta_u_lekarza)[length(wizyta_u_lekarza)] = c(paste("śr.cena.",rok_1, sep = ""))
  }
  else{
    rok_2 = paste("20",as.character(i), sep = "")
    srednia_kolumn = wizyta_u_lekarza$mean <- round(rowMeans(subset(wizyta_u_lekarza %>% select (contains(rok_2)), na.rm = TRUE)),2)
    colnames(wizyta_u_lekarza)[length(wizyta_u_lekarza)] = c(paste("śr.cena.",rok_2, sep = ""))
  }
}
wizyta_u_lekarza = wizyta_u_lekarza [-c(1,3:170)]
std_dev_row = round(apply(wizyta_u_lekarza[,2:dlugosc],1,sd),2)
wizyta_u_lekarza$odch.std. <- std_dev_row
med_row = round(apply(wizyta_u_lekarza[,2:dlugosc],1,median),2)
wizyta_u_lekarza$mediana <- med_row
for (i in 1:16){
  moda = getmode(wizyta_u_lekarza[i,2:dlugosc])
  wizyta_u_lekarza[i,18]<- as.character(moda)
}
colnames(wizyta_u_lekarza)[length(wizyta_u_lekarza)] = c("Dominanta")
srednia_wiersze = round(apply (wizyta_u_lekarza[,2:dlugosc],1,mean, na.rm = TRUE),2)
wizyta_u_lekarza$"sr.cena 06-19" <- srednia_wiersze
max_wizyta_u_lekarza = rownames(wizyta_u_lekarza)[which.max(wizyta_u_lekarza$`sr.cena 06-19`)]
wojewodztwo_max_wizyta_u_lekarza = lista_wojewodztw[as.numeric(max_wizyta_u_lekarza)]
min_wizyta_u_lekarza = rownames(wizyta_u_lekarza)[which.min(wizyta_u_lekarza$`sr.cena 06-19`)]
wojewodztwo_min_wizyta_u_lekarza = lista_wojewodztw[as.numeric(min_wizyta_u_lekarza)]

max_dev_lekarz = rownames(wizyta_u_lekarza)[which.max(wizyta_u_lekarza$odch.std.)]
wojewodztwo_max_dev_lekarz = lista_wojewodztw[as.numeric(max_dev_lekarz)]
min_dev_lekarz = rownames(wizyta_u_lekarza)[which.min(wizyta_u_lekarza$odch.std.)]
wojewodztwo_min_dev_lekarz = lista_wojewodztw[as.numeric(min_dev_lekarz)]

#tutaj właśnie ta mniejsza, zbiorcza tabela do prezentajci
#średnich cen bez podziału na lata
dlugosc_kategorie = length(kategorie_towarow)
srednia_kategoria = 
  kategorie_towarow$"piwo 6-19" <- round(rowMeans(subset(kategorie_towarow %>% select (contains("piwo")), na.rm = TRUE)),2) 
srednia_kategoria = 
  kategorie_towarow$"z.woda 6-19" <- round(rowMeans(subset(kategorie_towarow %>% select (contains("zimna")), na.rm = TRUE)),2) 
srednia_kategoria = 
  kategorie_towarow$"c.woda 6-19" <- round(rowMeans(subset(kategorie_towarow %>% select (contains("ciepła")), na.rm = TRUE)),2) 
srednia_kategoria = 
  kategorie_towarow$"C.O. 6-19" <- round(rowMeans(subset(kategorie_towarow %>% select (contains("ogrzewanie")), na.rm = TRUE)),2) 
srednia_kategoria = 
  kategorie_towarow$"lekarz 6-19" <- round(rowMeans(subset(kategorie_towarow %>% select (contains("lekarza")), na.rm = TRUE)),2) 
kategorie_towarow = kategorie_towarow [-c(1,3:dlugosc_kategorie)]

#opis statystyczny - dość ubogi...
print (paste("Piwo w latach 2006-2019 było najdroższe w województwie", wojewodztwo_max_piwo,":", "średnia cena w złotych" ,max(piwo$`sr.cena 06-19`)))
print (paste("Piwo w latach 2006-2019 było najtańsze w województwie", wojewodztwo_min_piwo, ":", "średnia cena w złotych", min(piwo$`sr.cena 06-19`)))
print (paste("Najczęściej powtarzająca się cena z województwie", wojewodztwo_max_piwo, "to:", "cena w złotych" ,max(piwo$Dominanta)))
print (paste("Najczęściej powtarzająca się cena z województwie", wojewodztwo_min_piwo, "to:", "cena w złotych" ,min(piwo$Dominanta)))
print (paste("Wartość drugiego kwartyla dla województwa", wojewodztwo_max_piwo, "to:", "cena w złotych" ,max(piwo$mediana)))
print (paste("Wartość drugiego kwartyla dla województwa", wojewodztwo_min_piwo, "to:", "cena w złotych" ,min(piwo$mediana)))
print ("*******************************************")

print (paste("Zimna woda w latach 2006-2019 była najdroższa w województwie", wojewodztwo_max_zimna_woda, ":", "średnia cena w złotych",max(zimna_woda$`sr.cena 06-19`) ))
print (paste("Zimna woda w latach 2006-2019 była najtańsza w województwie", wojewodztwo_min_zimna_woda, ":", "średnia cena w złotych", min(zimna_woda$`sr.cena 06-19`)))
print (paste("Najczęściej powtarzająca się cena z województwie", wojewodztwo_max_zimna_woda, "to:", "cena w złotych" ,max(zimna_woda$Dominanta)))
print (paste("Najczęściej powtarzająca się cena z województwie", wojewodztwo_min_zimna_woda, "to:", "cena w złotych" ,min(zimna_woda$Dominanta)))
print (paste("Wartość drugiego kwartyla dla województwa", wojewodztwo_max_zimna_woda, "to:", "cena w złotych" ,max(zimna_woda$mediana)))
print (paste("Wartość drugiego kwartyla dla województwa", wojewodztwo_min_zimna_woda, "to:", "cena w złotych" ,min(zimna_woda$mediana)))
print ("*******************************************")

print (paste("Ciepła woda w latach 2006-2019 była najdroższa w województwie", wojewodztwo_max_ciepla_woda, ":", "średnia cena w złotych",max(ciepla_woda$`sr.cena 06-19`)))
print (paste("Ciepła woda w latach 2006-2019 była najtańsza w województwie", wojewodztwo_min_ciepla_woda, ":", "średnia cena w złotych",min(ciepla_woda$`sr.cena 06-19`)))
print (paste("Najczęściej powtarzająca się cena z województwie", wojewodztwo_max_ciepla_woda, "to:", "cena w złotych" ,max(ciepla_woda$Dominanta)))
print (paste("Najczęściej powtarzająca się cena z województwie", wojewodztwo_min_ciepla_woda, "to:", "cena w złotych" ,min(ciepla_woda$Dominanta)))
print (paste("Wartość drugiego kwartyla dla województwa", wojewodztwo_max_ciepla_woda, "to:", "cena w złotych" ,max(ciepla_woda$mediana)))
print (paste("Wartość drugiego kwartyla dla województwa", wojewodztwo_min_ciepla_woda, "to:", "cena w złotych" ,min(ciepla_woda$mediana)))
print ("*******************************************")

print (paste("Centralne ogrzewanie w latach 2006-2019 było najdroższe w województwie", wojewodztwo_max_CO, ":", "średnia cena w złotych", max(CO$`sr.cena 06-19`)))
print (paste("Centralne ogrzewanie w latach 2006-2019 było najtańsze w województwie", wojewodztwo_min_CO, ":", "średnia cena w złotych",min(CO$`sr.cena 06-19`)))
print (paste("Najczęściej powtarzająca się cena z województwie", wojewodztwo_max_CO, "to:", "cena w złotych" ,max(CO$Dominanta)))
print (paste("Najczęściej powtarzająca się cena z województwie", wojewodztwo_min_CO, "to:", "cena w złotych" ,min(CO$Dominanta)))
print (paste("Wartość drugiego kwartyla dla województwa", wojewodztwo_max_CO, "to:", "cena w złotych" ,max(CO$mediana)))
print (paste("Wartość drugiego kwartyla dla województwa", wojewodztwo_min_CO, "to:", "cena w złotych" ,min(CO$mediana)))
print ("*******************************************")

print (paste("Wizyta u lekarza w latach 2006-2019 była najdroższa w województwie", wojewodztwo_max_wizyta_u_lekarza, ":", "średnia cena w złotych",max(wizyta_u_lekarza$`sr.cena 06-19`)))
print (paste("Wizyta u lekarza w latach 2006-2019 była najtańsza w województwie", wojewodztwo_min_wizyta_u_lekarza, ":", "średnia cena w złotych",min(wizyta_u_lekarza$`sr.cena 06-19`)))
print (paste("Najczęściej powtarzająca się cena z województwie", wojewodztwo_max_wizyta_u_lekarza, "to:", "cena w złotych" ,max(wizyta_u_lekarza$Dominanta)))
print (paste("Najczęściej powtarzająca się cena z województwie", wojewodztwo_min_wizyta_u_lekarza, "to:", "cena w złotych" ,min(wizyta_u_lekarza$Dominanta)))
print (paste("Wartość drugiego kwartyla dla województwa", wojewodztwo_max_wizyta_u_lekarza, "to:", "cena w złotych" ,max(wizyta_u_lekarza$mediana)))
print (paste("Wartość drugiego kwartyla dla województwa", wojewodztwo_min_wizyta_u_lekarza, "to:", "cena w złotych" ,min(wizyta_u_lekarza$mediana)))
print ("*******************************************")

print (paste("Ceny dla piwa były najmniej stabilne w województwie:", wojewodztwo_max_dev_piwo))
print (paste("Ceny dla piwa były najbardziej stabilne w województwie:", wojewodztwo_min_dev_piwo))
print ("*******************************************")

print (paste("Ceny za zimną wodę były najmniej stabilne w województwie:", wojewodztwo_max_dev_zwoda))
print (paste("Ceny za zimną wodę były najbardziej stabilne w województwie:", wojewodztwo_min_dev_zwoda))
print ("*******************************************")

print (paste("Ceny za ciepłą wodę były najmniej stabilne w województwie:", wojewodztwo_max_dev_cwoda))
print (paste("Ceny za ciepłą wodę były najbardziej stabilne w województwie:", wojewodztwo_min_dev_cwoda))
print ("*******************************************")

print (paste("Ceny za CO były najmniej stabilne w województwie:", wojewodztwo_max_dev_CO))
print (paste("Ceny za CO były najbardziej stabilne w województwie:", wojewodztwo_min_dev_CO))
print ("*******************************************")

print (paste("Ceny za wizytę u lekarza były najmniej stabilne w województwie:", wojewodztwo_max_dev_lekarz))
print (paste("Ceny za wizytę u lekarza były najbardziej stabilne w województwie:", wojewodztwo_min_dev_lekarz))
print ("*******************************************")
