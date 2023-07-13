

# Recibe una columna con datos respuestas separadas por ";" y devuelve un listado con todos los valores separados
separar_datos <- function(df, var) {
  # La pregunta de perfiles es de repsuesta múltiple, por ende hay que separar las respuestas
  many_columns <- df %>% 
    select({{var}}) %>% 
    filter(!is.na({{var}})) %>% 
    mutate(n = str_count({{var}}, ";")) %>% 
    separate(col = {{var}},into = paste0("var", 1:max(.$n, na.rm = T)), sep = ";") %>% 
    select(-n) 
  
  # Se genera un lista con todos los perfiles  
  a_remover <-  c("don't know", "?", "")
  lista <- c()
  n <- 1
  for (row in 1:nrow(many_columns) ) {
    for (col in 1:ncol(many_columns)) {
      cell <- many_columns[row, col]
      if (!is.na(cell) & !cell %in% a_remover) {
        lista[n] <- cell        
      }
      n <- n + 1   
    }
  }
  lista <- unlist(lista)
  return(lista)
}
