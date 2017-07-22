# Pruebas de obtencion de Datos Online de la FED
# Con tres librerias diferentes

library(quantmod)    # No requiere nada
library(Quandl)      # Requiere API Key de Quandl
library(FredR)       # A! Hay que instalarla con devtools y requiere API Key FED


# TESTEOS con QUANDL
# setear la API key de Quandl (sacada de la pagina)
Quandl.api_key("tsZbpzJvdyqB5zx9LN5k")
# Busco Datos para Chile Precios, y M1
datos <- Quandl(c("FRED/CHLCPIALLMINMEI","FRED/MANMM101CLM189S", "CCUSSP02CLM650N"), type = "xts")

# TESTEOS con QUANTMOD con datos de Chile
P <- getSymbols('CHLCPIALLMINMEI',src='FRED' , auto.assign = FALSE)   # Precios
M <- getSymbols('MANMM101CLM189S',src='FRED' , auto.assign = FALSE)   # M1
TCN <- getSymbols('CCUSSP02CLM650N',src='FRED' , auto.assign = FALSE) # TCN

datos <- merge(P,M,TCN , all = TRUE)                       # juntar todo
colnames(datos) <- c("P","M","TCN")
datos.scaled <- sweep(100*datos, 2, datos["1986:1"], "/")  # escalar todo 1986:1 = 100
cor(datos.scaled["1986/2016"])                             # matriz de correlaciones entre las fechas 86/16
plot(datos.scaled["1986:2016","M"])
lines(datos.scaled["1986:2016","P"])
lines(datos.scaled["1986:2016","TCN"])

# TESTEOS con QUANTMOD con datos de Brasil
P <- getSymbols('BRACPIALLMINMEI',src='FRED' , auto.assign = FALSE)   # Precios
M <- getSymbols('MYAGM2BRM189N',src='FRED' , auto.assign = FALSE)   # M2
TCN <- getSymbols('EXBZUS',src='FRED' , auto.assign = FALSE) # TCN

datos <- merge(P,M,TCN , all = TRUE)                       # juntar todo
colnames(datos) <- c("P","M","TCN")
datos.scaled <- sweep(100*datos, 2, datos["1995:1"], "/")  # escalar todo 1986:1 = 100
cor(datos.scaled["1995/2016"])                             # matriz de correlaciones entre las fechas 86/16
plot(datos.scaled["1995/2016","M"])
lines(datos.scaled["1995/2016","P"])
lines(datos.scaled["1995/2016","TCN"])

# TESTEOS con QUANTMOD con datos de Argentina
P <- getSymbols('DDOE01ARA086NWDB',src='FRED' , auto.assign = FALSE)   # Precios
M <- getSymbols('MYAGM2ARM189N',src='FRED' , auto.assign = FALSE)   # M2
TCN <- getSymbols('FXRATEARA618NUPN',src='FRED' , auto.assign = FALSE) # TCN

datos <- merge(P,M,TCN , all = TRUE)                       # juntar todo
colnames(datos) <- c("P","M","TCN")
datos.scaled <- sweep(100*datos, 2, datos["1998:1"], "/")  # escalar todo 1986:1 = 100
cor(datos.scaled["1998/2008"])                             # matriz de correlaciones entre las fechas 86/16
plot(datos.scaled["1998/2008","M"])
lines(datos.scaled["1998/2008","P"])
lines(datos.scaled["1998/2008","TCN"])




# TESTEOS FREDR
api.key = 'd33e156daab3ff06dd18183d1c172213'          # USO MI API KEY A FRED (sacada de la pagina)
fred <- FredR(api.key)
Chile.series <- fred$series.search("Chile")         # Buscar Argentina
M <- fred$series.observations(series_id = 'MANMM101CLM189S')  # Bajar M1

library(pipeR)
library(dplyr)
M %>>% select(date, value) %>>% 
  mutate(date = as.Date(date),value = as.numeric(value)) ->dt
library(ggplot2)
qplot(data = dt, x = date, y = value, geom = 'line')



