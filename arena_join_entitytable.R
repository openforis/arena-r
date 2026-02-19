


#########################################################
# This function function reads an Arena category table 'arena_join' and uses it to join
# selected attributes to selected entities. This method can be used in cases where
# survey hierarchy is incomplete, and some base unit level attributes have to read
# from another entity, that is at the same level as 'tree', 'sapling', etc.
# A typical use case is a rather small-area sample plot, which can be split into stands
# and the first stand in the table is regarded presenting the sampled point.

join_entity <- function( ) {  
  
  base_UUID_          <- paste0( arena.chainSummary$baseUnit, "_uuid")
  df_base_unit        <- get( arena.chainSummary$baseUnit )
  
  tMessage            <- "Entity join successful"
  source.entity       <- categories$arena_join$entity[1]
  source.attributes   <- categories$arena_join  %>% 
    filter( attribute_arena != "")              %>%
    select( attribute_arena)                    %>%
    pull()
  
  source.attributes = c( source.attributes, paste(source.attributes,"label",sep="_"), paste(source.attributes,"scientific_name",sep="_"))
  
  
  link_value          <- categories$arena_join$link_value[1]
  df_source           <- get( source.entity)                      %>% 
    select( all_of( base_UUID_), any_of( source.attributes))      %>%
    filter( eval( parse( text = source.attributes[1])) == link_value)
  
  df_source[is.na( df_source)] <- ""
  source.attribute             <- paste( 'df_source', source.attributes[1], sep=".")
  
  target.entities <- categories$arena_join %>% 
    filter( row_number() != 1)             %>%
    filter( !is.na(entity) & entity != "") %>%
    select( entity)                        %>% 
    pull()
  # print( target.entities)
  
  # join base unit data first:
  df_base_unit <- df_base_unit %>% left_join( df_source, by = base_UUID_)
  assign( arena.chainSummary$baseUnit, df_base_unit, env = .GlobalEnv)
  
  
  for (i in 1:length( target.entities)) {
    if ( exists( target.entities[i])) {
      df_target        <- get(target.entities[i])
      # create a temporary base unit ID, needed to avoid duplicate names in output joined table
      df_target$join_field_ <- df_target[[base_UUID_]]
      df_target[base_UUID_] <- NULL
      
      target.attribute <- categories$arena_join %>% 
        filter( entity == target.entities[i])   %>%
        select( attribute_arena)                %>% 
        pull()
  
      s_SQL  <- paste0('SELECT df_target.*, df_source.* FROM df_target LEFT JOIN df_source ON df_target.', 
                     target.attribute, ' = ', source.attribute, 
                     ' AND df_target.join_field_ = df_source.', base_UUID_)

      df_target             <- sqldf( s_SQL )
      df_target$join_field_ <- NULL
      assign( target.entities[i], df_target, env = .GlobalEnv)
      rm( df_target)
    } # if 
  } # for i
  return( tMessage)
} # join_entity


