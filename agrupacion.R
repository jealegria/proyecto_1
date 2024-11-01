#Separa fecha y hora y deja solo columna Fecha

datos.a <- datos.a %>%
  mutate(Fecha = as.Date(sub(" .*", "", `Fecha de ingreso`), format = "%d/%m/%Y")) %>%
  select(-`Fecha de ingreso`)  # Eliminar la columna original si no la necesitas

View(datos.a)

#Asigna semana epidemiologica a la columna Fecha
datos.a <- datos.a %>%
  mutate(semana_epidemiologica = week(Fecha))

conteo_tipo_egreso <- datos.a %>%
  group_by(`Tipo de egreso`) %>%
  summarise(cantidad = n()) %>%
  arrange(desc(cantidad))  # Opcional: ordenar por cantidad

print(conteo_tipo_egreso)
