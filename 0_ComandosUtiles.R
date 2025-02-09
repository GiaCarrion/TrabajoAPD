windows() # Abrir gráfico en otra ventana

#Opciones para manejar los ceros
#Crear una variable binaria:
#Puedes crear una columna adicional TieneBalance que
#indique si el balance es mayor a cero o no. Esto puede
#ser útil como variable explicativa adicional.

data1$TieneBalance <- ifelse(data1$Balance == 0, 0, 1)