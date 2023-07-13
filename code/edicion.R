library(tidyverse)
library(readxl)
library(janitor)
library(feather)

ones <- read_excel("data/Implementación de técnicas de machine learning para la producción estadística(1-21).xlsx")
source("code/helpers.R", encoding = "utf-8")


###########################
# Limpiar y editar nombres #
###########################
ones_edit <- ones %>% 
  clean_names() %>% 
  rename(
    n_personas = aproximadamente_cuantas_personas_trabajan_en_su_institucion,
    existe_unidad = en_su_institucion_existe_alguna_unidad_dedicada_exclusivamente_a_la_implementacion_de_metodologias_que_utilicen_machine_learning_inteligencia_artificial_u_otras_herramientas_de_la_ciencia_de_datos,
    organigrama = describa_donde_se_encuentra_esta_unidad_en_el_organigrama_institucional,
    participacion_ml = ha_participado_en_proyectos_que_impliquen_el_uso_machine_learning_o_herramientas_provenientes_de_la_ciencia_de_datos,
    anios_institucion = cuantos_anos_lleva_trabajando_en_la_institucion,
    pais = en_que_pais_esta_ubicada_su_institucion,
    cargo_institucion = cual_es_su_cargo_dentro_de_la_institucion,
    nombre_unidad = cual_es_nombre_de_la_unidad_en_la_que_trabaja,
    nombre_institucion = nombre_de_la_institucion,
    nombre_unidad_ds = cual_es_el_nombre_de_la_unidad,
    cliente_productores = la_unidad_desarrolla_proyectos_o_trabajos_para_equipos_productores_de_estadisticas,
    relacion_cliente = describa_como_se_relaciona_esta_unidad_con_las_unidades_productoras_de_estadisticas_considere_cosas_como_1_origen_del_requerimiento_2_tiempo_de_desarrollo_3_profesionales_involucrados_en_am,
    mantenimiento_desarrollo = una_vez_que_finaliza_un_proyecto_realizado_con_unidades_productoras_de_estadistica_que_equipo_esta_encargado_de_darle_mantenimiento_y_continuidad,
    adopcion_ml = segun_usted_cual_es_el_nivel_de_adopcion_de_herramientas_y_estrategias_de_trabajo_basadas_en_machine_learning_en_su_institucion,
    conocimiento_ml_clientes = tienen_un_alto_nivel_de_conocimiento_sobre_machine_learning,
    capacidad_implementacion_ml_clientes = son_capaces_de_implementar_estrategias_basadas_en_machine_learning,
    continuidad_ml_cliente = son_capaces_de_dar_continuidad_y_mantenimiento_a_modelos_creados_por_especialistas_en_la_materia,
    existe_experiencia_ml = existe_al_menos_una_experiencia_de_utilizacion_de_machine_learning_en_la_institucion_considere_tanto_las_iniciativas_en_fase_piloto_como_aquellas_que_ya_estan_en_produccion,
    n_experiencias = cuantas_experiencias_de_utilizacion_de_machine_learning_existen_en_su_institucion_considere_tanto_las_iniciativas_en_fase_piloto_como_aquellas_que_ya_estan_en_produccion,
    n_iniciativas_produccion = cuantas_iniciativas_ya_estan_en_produccion,
    
    
    n_iniciativas_otras = cuantas_iniciativas_abordan_otras_areas_no_consideradas_en_las_preguntas_anteriores,
    n_iniciativas_anonimizacion = cuantas_iniciativas_abordan_tareas_de_anonimizacion_de_datos,
    n_iniciativas_imagenes = cuantas_iniciativas_abordan_tareas_de_clasificacion_de_imagenes,
    n_iniciativas_texto = cuantas_iniciativas_abordan_tareas_de_clasificacion_de_texto,
    n_iniciativas_edicion = cuantas_iniciativas_abordan_tareas_de_imputacion_o_edicion_de_datos,
    n_iniciativas_rutas = cuantas_iniciativas_abordan_tareas_de_optimizacion_de_rutas,
    
    titulo_proyecto1 = cual_es_el_titulo_del_proyecto,
    titulo_proyecto2 = cual_es_el_titulo_del_proyecto_2,
    titulo_proyecto3 = cual_es_el_titulo_del_proyecto_3,
    
    tipo_perfiles = que_tipo_de_perfiles_trabajan_en_esta_unidad,
         etapas_proyecto1 = en_cual_de_las_siguientes_etapas_de_la_produccion_estadistica_se_situa_el_proyecto_puede_marcar_mas_de_una_alternativa,
         etapas_proyecto2 = en_cual_de_las_siguientes_etapas_de_la_produccion_estadistica_se_situa_el_proyecto_puede_marcar_mas_de_una_alternativa2,
         etapas_proyecto3 = en_cual_de_las_siguientes_etapas_de_la_produccion_estadistica_se_situa_el_proyecto_puede_marcar_mas_de_una_alternativa3,
         algoritmo_proyecto1 = que_algoritmo_se_utiliza_puede_marcar_mas_de_una_alternativa,
         algoritmo_proyecto2 = que_algoritmo_se_utiliza_puede_marcar_mas_de_una_alternativa2,
         algoritmo_proyecto3 = que_algoritmo_se_utiliza_puede_marcar_mas_de_una_alternativa3,
         lenguaje_proyecto1 = que_lenguaje_de_programacion_se_utiliza_o_utilizo_en_el_proyecto,
         lenguaje_proyecto2 = que_lenguaje_de_programacion_se_utiliza_o_utilizo_en_el_proyecto_2,
         lenguaje_proyecto3 = que_lenguaje_de_programacion_se_utiliza_o_utilizo_en_el_proyecto_3,
         area_tematica_proyecto1 = cual_es_el_area_tematica_que_mas_se_acerca_al_proyecto_que_esta_describiendo,
         area_tematica_proyecto2 = cual_es_el_area_tematica_que_mas_se_acerca_al_proyecto_que_esta_describiendo_2,
         area_tematica_proyecto3 = cual_es_el_area_tematica_que_mas_se_acerca_al_proyecto_que_esta_describiendo_3,
         descripcion_proyecto1 = describa_en_no_mas_de_500_palabras_en_que_consiste_el_proyecto_considere_en_la_descripcion_elementos_como_objetivo_actores_involucrados_tarea_concreta_a_resolver_entre_otras,
         descripcion_proyecto2 = describa_en_no_mas_de_500_palabras_en_que_consiste_el_proyecto_considere_en_la_descripcion_elementos_como_objetivo_actores_involucrados_tarea_concreta_a_resolver_entre_otras_2,
         descripcion_proyecto3 = describa_en_no_mas_de_500_palabras_en_que_consiste_el_proyecto_considere_en_la_descripcion_elementos_como_objetivo_actores_involucrados_tarea_concreta_a_resolver_entre_otras_3,
    tipo_tarea_proyecto1 = que_tipo_de_tarea_resuelve_el_algoritmo,
    tipo_tarea_proyecto2 = que_tipo_de_tarea_resuelve_el_algoritmo_2,
    tipo_tarea_proyecto3 = que_tipo_de_tarea_resuelve_el_algoritmo_3,
    alcance_proyecto1 = cual_es_el_alcance_del_proyecto,
    alcance_proyecto2 = cual_es_el_alcance_del_proyecto_2,
    alcance_proyecto3 = cual_es_el_alcance_del_proyecto_3
         
         )

# Arreglar el nombre de méxico y trabajr con el reporte agregado de iniciativas
ones_edit <- ones_edit %>% 
  mutate(pais = if_else(pais == "MÉXICO", "México", pais)) %>% 
  mutate_at(vars(n_iniciativas_texto , n_iniciativas_anonimizacion , n_iniciativas_imagenes , n_iniciativas_edicion , n_iniciativas_rutas , n_iniciativas_otras),
            as.numeric
  ) %>% 
  mutate_at(vars(n_iniciativas_texto , n_iniciativas_anonimizacion , n_iniciativas_imagenes , n_iniciativas_edicion , n_iniciativas_rutas , n_iniciativas_otras),
            ~if_else(is.na(.), 0, .)) %>% 
  mutate(suma_iniciativas = n_iniciativas_texto + n_iniciativas_anonimizacion + n_iniciativas_imagenes + n_iniciativas_edicion + n_iniciativas_rutas + n_iniciativas_otras) %>% 
  mutate(pais2 = if_else(nombre_institucion == "OECD", nombre_institucion, pais )) 

  
write_feather(ones_edit, "data/editada.feather")


########################################
# Separar datos por que vienen pegados #
########################################

# Perfiles profesionales
lista_perfiles <- separar_datos(ones_edit, tipo_perfiles)
df_perfiles <- data.frame(perfil = lista_perfiles)
write_feather(df_perfiles, "data/df_perfiles.feather")



# Preguntas específicas dentro de cada proyecto respecto a las etapas en el GSBPM
# Debido a que se hace la misma pregunta tres veces, es necesario repetir el procedimiento para cada uno de los proyectos detallados
lista_etapas1 <- separar_datos(ones_edit, etapas_proyecto1)
lista_etapas2 <- separar_datos(ones_edit, etapas_proyecto2)
lista_etapas3 <- separar_datos(ones_edit, etapas_proyecto3)

full_etapas <- c(lista_etapas1, lista_etapas2, lista_etapas3 ) 
df_etapas <- data.frame(etapas = full_etapas)

write_feather(df_etapas, "data/etapas.feather")

# Preguntas sobre algoritmos usados en cada proyecto
algoritmo1 <- separar_datos(ones_edit, algoritmo_proyecto1)
algoritmo2 <- separar_datos(ones_edit, algoritmo_proyecto2)
algoritmo3 <- separar_datos(ones_edit, algoritmo_proyecto3)

full_algoritmo <- c(algoritmo1, algoritmo2, algoritmo3)
df_algoritmo <- data.frame(algoritmo = full_algoritmo)

write_feather(df_algoritmo, "data/algoritmo.feather")

# Preguntas sobre lenguaje
lenguaje1 <- separar_datos(ones_edit, lenguaje_proyecto1)
lenguaje2 <- separar_datos(ones_edit, lenguaje_proyecto2)
lenguaje3 <- separar_datos(ones_edit, lenguaje_proyecto3)

full_lenguaje <- c(lenguaje1, lenguaje2, lenguaje3)
df_lenguaje <- data.frame(lenguaje = full_lenguaje)

write_feather(df_lenguaje, "data/lenguaje.feather")

#######################################
# Apilar datos sin respuesta múltiple #
#######################################

# Se apilan las preguntas de los proyectos que no son de respuesta múltiple
proyectos_apilados <- ones_edit %>% 
  select(pais, titulo_proyecto1, descripcion_proyecto1, area_tematica_proyecto1, tipo_tarea_proyecto1, alcance_proyecto1) %>% 
  bind_rows(ones_edit %>% 
              select(pais, titulo_proyecto2, descripcion_proyecto2, area_tematica_proyecto2, tipo_tarea_proyecto2, alcance_proyecto2)) %>% 
  bind_rows(ones_edit %>% 
              select(pais, titulo_proyecto3, descripcion_proyecto3, area_tematica_proyecto3, tipo_tarea_proyecto3, alcance_proyecto3))

write_feather(proyectos_apilados, "data/proyectos_apilados.feather")


##############################################
# Crear tabla para iniciativas en produccion #
##############################################

iniciativas_long <- ones_edit %>% 
  select(pais = pais2, n_iniciativas_texto , n_iniciativas_anonimizacion , n_iniciativas_imagenes , n_iniciativas_edicion , n_iniciativas_rutas , n_iniciativas_otras) %>% 
  pivot_longer(cols = -pais, names_to = "iniciativa", values_to = "n") %>% 
  mutate(iniciativa = str_remove(iniciativa, "n_iniciativas_"))
  
write_feather(iniciativas_long, "data/iniciativas_long.feather")


