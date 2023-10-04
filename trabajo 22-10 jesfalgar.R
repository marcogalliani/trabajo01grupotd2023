### Problema: enunciado y solución hecho por todos los criterios de incertidumbre
### Jesús Manuel Falcón García, grupo 5 TD

## ENUNCIADO:

##Juan y Pedro quieren comprarse un coche.
##El coche lo pueden comprar en cuatro concesionarios distintos pero cada 
##uno ofrece diferentes precios y términos por el mismo coche:
  
##Concesionario RedWheel: les ofrece como mejor oferta por el coche una fianza de 10.000€ 
##y un precio final de 30.000€
##Concesionario ToyotaPower: les ofrece una fianza de 5.000€ y como precio final 40.000€
##Concesionario FreeAutos: su mejor oferta es pagar una entrada de 10.000€ y como 
##precio final 20.000€
##Concesionario Route100: les ofrece no pagar fianza a cambio de un precio final de 50.000€

##Pero esto no es todo, ya que la decisión no es tan fácil porque dependiendo del 
##concesionario la calidad del coche será muy diferente, ya que los del concesionario 
##les pueden dar una de dos, o un coche de segunda mano que parece nuevo o un coche 
##nuevo, así que Juan y Pedro han estimado los siguientes gastos que tendrían con el coche:
  
##Si coche fraude:
  
##RedWheel: 500€ al año durante 15 años
##ToyotaPower: 100€ al año durante 20 años
##FreeAutos: 3000€ al año por 25 años
##Route100: 250€ al año por 10 años

##Si coche nuevo de verdad:
  
##RedWheel: 50€ al año durante 5 años
##ToyotaPower: 10€ al año durante 4 años
##FreeAutos: 30€ al año por 2 años
##Route100: 25€ al año por 3 años

##Teniendo en cuenta, que Juan piensa que no los van a engañar con el coche y Pedro 
##quiere elegir la opción que les permita arrepentirse menos, ¿qué decidirá cada uno?
   
## SOLUCIÓN:

#los estados de la naturaleza son (ei):
#e1 -> les venden coche de segunda mano
#e2 -> les venden coche nuevo y bueno

#las alternativas son (di):
#d1 -> concesionario RedWheel
#d2 -> concesionario ToyotaPower
#d3 -> concesionario FreeAutos
#d4 -> concesionario Route100

#matriz de costos (en este caso tenemos en cuenta que los datos de la matriz
#contiene datos que queremos minimizar):

#(d1, e1) -> 10.000+30.000 + 500*15
#(d1, e2) -> 10.000+30.000 + 50*5
#(d2, e1) -> 5.000+40.000 + 100*20
#(d2, e2) -> 5.000+40.000 + 10*4
#(d3, e1) -> 10.000+20.000 + 3000*25
#(d3, e2) -> 10.000+20.000 + 30*2
#(d4, e1) -> 50.000 + 250*10
#(d4, e2) -> 50.000 + 25*3

source("teoriadecision_funciones_incertidumbre.R")
#para tener acceso a las funciones de ese archivo y poder aplicar los criterios

matriz.costos = crea.tablaX(c(40000+500*15, 40000+50*5,
                              45000+100*20, 45000+10*4,
                              30000+3000*25, 30000+30*2,
                              50000+250*10, 50000+25*3), numalternativas = 4,
                            numestados = 2)
matriz.costos

rownames(matriz.costos) = c("RedWheel", "ToyotaPower", "FreeAutos", "Route100")

decision.Juan = criterio.Optimista(matriz.costos, F) #la F es para indicar que no es
#una matriz de datos favorables sino de costes, para la decisión de Juan hay que 
#aplicar este criterio porque se corresponde con la actitud optimista de Juan
decision.Juan

#vemos que Juan optará por comprar el coche en el concesionario FreeAutos,
#alcanzado un valor óptimo de 30.060€

#estudiamos también el caso opuesto, es decir, si fuera pesimista:

decision.Juan2 = criterio.Wald(matriz.costos, F)
decision.Juan2
#en este caso Juan escogería el concesionario ToyotaPower y el valor óptimo 
#que obtendría sería de 47.000€

decision.Pedro = criterio.Savage(matriz.costos, F) #aplicamos este criterio para 
#Pedro ya que quiere la opción que le haga arrepentirse menos

decision.Pedro
#vemos que Pedro se decantará por el concesionario RedWheel, teniendo valor óptimo
# de 10.190€

decision.total = criterio.Todos(matriz.costos, 0.5, F)
decision.total
#aquí vemos que en general la mejor opción con alfa 
#de 0.5 es la del concesionario FreeAutos 
#seguido del Route100

decision.total = criterio.Todos(matriz.costos, 0.1, F)
decision.total
#aquí vemos que en general la mejor opción cambiando el valor de alfa 
#a 0.5 es la del concesionario RedWheel

decision.total = criterio.Todos(matriz.costos, 0.9, F)
decision.total
#y por último con otro valor de alfa, esta vez 0.9, vemos que
#prácticamente hay un empate entre FreeAutos y RedWheel


##Como conclusiones finales podemos decir que en general el 
#concesionario Route100 es el peor mientras que RedWheel y FreeAutos están
#prácticamente empatados como la mejor opción



