#Inspeccionar una tabla

#para inspeccionar una tabla o data frame vamos primero a invocarlo

iris #o
print(iris)

plot(iris) # la representa gráficamente
summary(iris) # resumen estadístico de las columnas
str(iris) # "representación textual" del objeto
----------------------------------------------
#Ejercicio 2.1.1 Trata de interpretar el gráfico generado con plot(iris). 
#¿Qué nos dice, p.e., de la relación entre la anchura y la longitud de los pétalos?

# hay una relación ascedente y directa entre la anchura y la longitud

#Ejercicio 2.1.2 Trata de intepretar la salida del resto de las anteriores líneas de código.
#¿Qué nos dicen del conjunto de datos?

#salvo por especies, todas nuestras variables son numericas. 5 variables y 150 observaciones.
#el número de datos por especie es el mismo, pero según cada variable hay distintas observaciones.
----------------------------------------------
head(iris) # primeras seis filas
tail(iris) # últimas seis filas

#Consulta la ayuda de la función head y averigua cómo mostrar
#las diez primeras filas de iris en lugar de las seis que aparecen por defecto.
?head
head(iris, n= 10L)
----------------------------------------------
dim(iris) # filas x columnas (viene de "dimensions of an object")
nrow(iris) # número de filas
ncol(iris) # número de columnas
colnames(iris) #nombres de las columnas
---------------------------------------------
#Ejercicio 2.1.4 Consulta el tamaño, número de filas y el número y nombre de las columnas del conjunto
#de datos airquality; muestra también las primeras 13 filas de esa tabla.
airquality
nrow(airquality)
head(airquality, n=13L)
#Ejercicio 2.1.5 Examina el conjunto de datos attenu. Consulta su ayuda (?attenu) para averiguar qué
#tipo de información contiene. Finalmente, usa summary para ver si contiene algún nulo en alguna columna.
summary (attenu) #16nas en station
-------------------------------------------------------------------
  
iris[1:10,] # diez primeras filas. OJO SIEMPRE CON LA COMA
iris[, 3:4] # columnas 3 y 4
iris[1:10, 3:4]
#iris[, "Species"] o iris$Species para ver los datos de esa columna. se prefiere la segunda forma.
iris[iris$Species == "setosa",] #selección especifica de la variable de una columna.
---------------------------------------------------------
#Ejercicio 2.2.1 Selecciona las filas de iris cuya longitud del pétalo sea mayor que 4.
  iris[iris$Petal.Length >= 4,]
#Ejercicio 2.2.2 Selecciona las filas donde cyl sea menor que 6 y gear igual a 5 en mtcars.
#Nota: el operador AND en R es &.
mtcars[mtcars$cyl > 6 & mtcars$gear == 5,]
---------------------------------------------------------
  mi.iris <- iris # mi.iris es una copia de iris
#Los nombres de objetos siguen las reglas habituales en otros lenguajes de programación: son secuencias
#de letras y números (aunque no pueden comenzar por un número) y se admiten los separadores _ y .

ls() # lista de objetos en memoria
rm(mi.iris) # borra el objeto mi.iris

#Añadir una columna a una tabla es como crear una nueva variable dentro de ella.
#Una manera de eliminarlas es asignarle el valor NULL.
mi.iris <- iris
mi.iris$Petal.Area <- mi.iris$Petal.Length * mi.iris$Petal.Width
mi.iris$Petal.Area <- NULL
---------------------------------------------------------
#Ejercicio 2.3.1 Crea una copia del conjunto de datos airquality. 
#Comprueba con ls que está efectivamente creado y luego añádele una columna nueva llamada temperatura
#que contenga una copia de Temp.
#Comprueba que efectivamente está allí y luego, elimínala. Finalmente, borra la tabla.
copia_air <- airquality
copia_air$temperatura <- airquality$Temp
ls(copia_air)
rm(copia_air)
---------------------------------------------------------
#R no dispone de ninguna función de serie para ordenar por una columna (o varias)7. En R, ordenar es
#seleccionar ordenadamente.
  
mi.iris <- iris[order(iris$Petal.Length),]

#La función order aplicada a un vector devuelve otro vector de la misma longitud que tiene el valor 1 en
#el primer elemento del vector, 2 en el segundo, etc.Ten en cuenta que en R se puede ordenar 
#por dos o más columnas porque order admite dos o más argumentos.

iris[order(iris$Petal.Length, iris$Sepal.Length),]

