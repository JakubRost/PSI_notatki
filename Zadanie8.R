typ_gospodarstwa = function(dochod, wydatki, dzieci, miasto) {
  if (wydatki > dochod) {
    kategoria = "Trudna sytuacja"
  } else if (wydatki < dochod && dzieci >= 2) {
    kategoria = "Przeciętna sytuacja"
  } else if (wydatki <= 0.8*dochod && miasto == "male") {
    kategoria = "Stabilna sytacja"
  }
  return(kategoria)
}
typ_gospodarstwa(4000, 4500, 1, "duze")  
typ_gospodarstwa(5000, 4800, 2, "duze")  
typ_gospodarstwa(5000, 3500, 0, "male")  
