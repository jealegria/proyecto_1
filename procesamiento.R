#Cambiar fecha de ingreso a formato fecha en ambas bases

# Para datos.a
datos.a$Fecha <- as.Date(sub(" .*", "", datos.a$`Fecha de ingreso`), format="%d/%m/%Y")

# Para datos.p
datos.p$Fecha <- as.Date(sub(" .*", "", datos.p$`Fecha de ingreso`), format="%d/%m/%Y")

#eliminar la columna original "Fecha de ingreso"
datos.a$`Fecha de ingreso` <- NULL
datos.p$`Fecha de ingreso` <- NULL

#Poner formato fecha en la tabla relacional

tabla.calendario$Fecha <- as.Date(tabla.calendario$Fecha)

####-----------------------------parte 2--------------

#Asingarle SE a las tablas

datos.a_joined <- left_join(datos.a, tabla.calendario[,c(1,3)], by = "Fecha")
datos.p_joined <- left_join(datos.p, tabla.calendario[,c(1,3)], by = "Fecha")

# Conteo de cada variable en la columna "Tipo de egreso" de datos.a
conteo_a <- table(datos.a$`Tipo de egreso`)
#print(conteo_a)

# Conteo de cada variable en la columna "Tipo de egreso" de datos.p
conteo_p <- table(datos.p$`Tipo de egreso`)
#print(conteo_p)


# Convertir a dataframe
conteo_a_df <- as.data.frame(conteo_a)
conteo_p_df <- as.data.frame(conteo_p)

# Cambiar nombres de las columnas
colnames(conteo_a_df) <- c("Tipo de egreso", "Cantidad")
colnames(conteo_p_df) <- c("Tipo de egreso", "Cantidad")

# Mostrar los dataframes
#print(conteo_a_df)
#print(conteo_p_df)


# Combinar los dataframes y agregar columna "Servicio" donde diga a que servicio pertenece el dato
datos_combined <- bind_rows(
  datos.a_joined %>% mutate(Servicio = "A"),
  datos.p_joined %>% mutate(Servicio = "P")
)



tabla_egresos <- datos_combined %>%
  group_by(Semana, `Tipo de egreso`, Servicio) %>%
  summarise(Cantidad = n(), .groups = 'drop')

# Filtrar solo las primeras cinco semanas
tabla_egresos <- datos_combined %>%
  group_by(Semana, `Tipo de egreso`, Servicio) %>%
  summarise(Cantidad = n(), .groups = 'drop') %>%
  filter(Semana %in% 1:5)  # Filtrar para mantener solo semanas 1 a 5

tabla_ancha <- tabla_egresos %>%
  pivot_wider(names_from = c(Semana, Servicio), values_from = Cantidad, values_fill = 0)


#################################################################################
# Crear la tabla gt y personalizar nombres y tab_spanner
# Renombrar las columnas para evitar problemas
colnames(tabla_ancha) <- gsub("^(\\d+)_(A|P)$", "Semana\\1_\\2", colnames(tabla_ancha))

tabla_gt <- tabla_ancha %>%
  gt() %>%
  cols_label(`Tipo de egreso` = "TIPO DE EGRESO") %>%
  tab_header(title = "Egresos por Semana") %>%
  tab_spanner(label = "Semana 1", columns = contains("Semana1")) %>%
  tab_spanner(label = "Semana 2", columns = contains("Semana2")) %>%
  tab_spanner(label = "Semana 3", columns = contains("Semana3")) %>%
  tab_spanner(label = "Semana 4", columns = contains("Semana4")) %>%
  tab_spanner(label = "Semana 5", columns = contains("Semana5")) %>%
  cols_label(
    Semana1_A = "A",
    Semana1_P = "P",
    Semana2_A = "A",
    Semana2_P = "P",
    Semana3_A = "A",
    Semana3_P = "P",
    Semana4_A = "A",
    Semana4_P = "P",
    Semana5_A = "A",
    Semana5_P = "P"
  )

# Mostrar la tabla
print(tabla_gt)



###################################################################

# Filtrar datos para mantener solo tipos de egreso específicos
datos_a_filtrados <- datos.a_joined %>%
  filter(`Tipo de egreso` %in% c("Alta médica", "Defunción", "Derivación", "Internación"))

datos_p_filtrados <- datos.p_joined %>%
  filter(`Tipo de egreso` %in% c("Alta médica", "Defunción", "Derivación", "Internación"))

# Gráfico para datos.a_joined
ggplot(datos_a_filtrados, aes(x = factor(Semana), fill = `Tipo de egreso`)) +
  geom_bar(aes(y = ..count..), position = "dodge") +
  labs(title = "Egresos por Semana - Servicio A",
       x = "Semana",
       y = "Cantidad") +
  theme_minimal()

# Gráfico para datos.p_joined
ggplot(datos_p_filtrados, aes(x = factor(Semana), fill = `Tipo de egreso`)) +
  geom_bar(aes(y = ..count..), position = "dodge") +
  labs(title = "Egresos por Semana - Servicio P",
       x = "Semana",
       y = "Cantidad") +
  theme_minimal()



