
##########################################################################################################################################
# Lauri Vesa, FAO Forestry Division
# 3/2026
##########################################################################################################################################
# This function reads the category table 'arena_join' and uses it to join
# selected attributes to selected entities. This method can be used in cases where the point‑sampling theory is followed. 
# In this case, in an Arena survey, 'plot_section' or 'stand' is at the same level at the hierarchy as 'tree', 'sapling', etc.
# A typical case of use is a rather small-area sample plot, which can be split into stands
# and the stand_id '1' (or 'A') in the table is regarded presenting the sampled point.

# The rationale is well‑established in point‑sampling theory, design‑based inference, and standard NFI sampling practice, 
# all of which appear e.g. in published literature that Finland NFI by Luke relies on. 
# The basic principle is the following: Each plot represents a point, not an area. In large-area forest inventory, 
# the statistical estimation is design-based, meaning: 
#  1) The sample point is the observation unit, and 
#  2) the plot is only a device for measuring attributes around that point.

# Although a circular plot may overlap e.g. multiple land use/cover classes, the centre‑point rule ensures unbiased estimates 
# because plot centres are selected randomly or systematically, and the overlap with boundaries is random. 
# This approach maintains consistency, minimizes classification errors, and avoids artificial subdivision of the sampling unit.

join_entity <- function( ) {  
  
  base_UUID_          <- paste0( arena.chainSummary$baseUnit, "_uuid")
  df_base_unit        <- get( arena.chainSummary$baseUnit )
  
  tMessage            <- "Entity join successful"
  source.entity       <- categories$arena_join$entity[1]
  source.attributes   <- categories$arena_join  %>% 
    filter( attribute_arena != "")              %>%
    select( attribute_arena)                    %>%
    pull()
  
  source.attributes = c( source.attributes, paste(source.attributes,"label", sep="_"), paste(source.attributes, "scientific_name", sep="_"))
  
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
  
  # join base unit data first:
  df_base_unit <- df_base_unit %>% left_join( df_source, by = base_UUID_)
  assign( arena.chainSummary$baseUnit, df_base_unit, env = .GlobalEnv)
  
  
  for (i in 1:length( target.entities)) {
    if ( exists( target.entities[i])) {
      df_target        <- get(target.entities[i])
      # create a temporary base unit ID, needed to avoid duplicate names in output joined table
      df_target$join_field_ <- df_target[[base_UUID_]]
      df_target[base_UUID_] <- NULL
      
      s_SQL  <- paste0('SELECT df_target.*, df_source.* FROM df_target LEFT JOIN df_source ON', 
                       ' df_target.join_field_ = df_source.', base_UUID_)
      
      df_target             <- sqldf( s_SQL )
      df_target$join_field_ <- NULL
      assign( target.entities[i], df_target, env = .GlobalEnv)
      rm( df_target)
    } # if 
  } # for i
  return( tMessage)
} # join_entity
