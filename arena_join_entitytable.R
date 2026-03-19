
##########################################################################################################################################
# Lauri Vesa, FAO Forestry Division
# 3/2026
##########################################################################################################################################
# This function reads the category table 'arena_join' and uses it as rules to join a given source table (=entity) and its
# selected attributes to selected entities. 
# A typical case in a forest inventory where data is collected on field sample plots, which can be split into stands (i.e., plot sections).
# but 'plot_section' or 'stand' is in the Arena survey at the same level at the hierarchy as 'tree', 'sapling', etc.
# 
# There are two options to use this function, based on the argument 'joinByLinkValue': 
#   1) Follow the point‑sampling theory ( joinByLinkValue == TRUE )
#   2) Join the source entity by matching "attribute_arena" fields ( joinByLinkValue == FALSE ) 
#
# CASE 1: 
# A typical case of use is a nested concentric sample plot, which can be split into stands
# and the stand_id '1' (or 'A') in the table is presenting the sampled point in the estimation.
# The rationale is well‑established in point‑sampling theory, design‑based inference, and standard NFI sampling practice, 
# all of which appear e.g. in published literature that Finland NFI by Luke relies on. 
# The basic principle is the following: Each plot represents a point, not an area. In large-area forest inventory, 
# the statistical estimation is design-based, meaning: 
#  1) The sample point is the observation unit, and 
#  2) the plot is only a device for measuring attributes around that point.
# Although a circular plot may overlap e.g. multiple land use/cover classes, the centre‑point rule ensures unbiased estimates 
# because plot centres are selected randomly or systematically, and the overlap with boundaries is random. 
# This approach maintains consistency, minimizes classification errors, and avoids artificial subdivision of the sampling unit.
#
# CASE 2:
# A typical case of use is a fixed-area sample plot, which can be split into plot-sections. 
# For example, the old FAO NFMA method applied 20m x 250m rectangular plots, that could be split into plot sections. 
#   WARNING: the script can not execute the join in a case, where a 'tree' contains a 'tree_stand_no' value that 
#   does not have a match in the 'stand' table. For example, if tree data contains stand_no '4', but there is no hit 
#   in the 'stand' data, new attributes (to be joined) will get NAs.

join_entity <- function( joinByLinkValue ) {  
  
  base_UUID_          <- paste0( arena.chainSummary$baseUnit, "_uuid")
  df_base_unit        <- get( arena.chainSummary$baseUnit )
  
  tMessage            <- "Entity join successful"
  source.entity       <- categories$arena_join$entity[1]
  source.attributes   <- categories$arena_join  %>% 
    filter( attribute_arena != "")              %>%
    select( attribute_arena)                    %>%
    pull()
  
  source.attributes = c( source.attributes, paste(source.attributes,"label", sep="_"), paste(source.attributes, "scientific_name", sep="_"))
  
  link_value          <- as.character( categories$arena_join$link_value[1])
  df_source           <- get( source.entity)                      %>% 
    select( all_of( base_UUID_), any_of( source.attributes))
  
  if( joinByLinkValue) df_source <- df_source %>% 
    filter(  eval( parse( text = source.attributes[1])) == link_value)
  
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
      df_target             <- get(target.entities[i])
      # create a temporary base unit ID, used to avoid duplicate names in output joined table
      df_target$join_field_ <- df_target[[base_UUID_]]
      df_target[base_UUID_] <- NULL
      
      if( joinByLinkValue) {
        s_SQL  <- paste0('SELECT df_target.*, df_source.* FROM df_target LEFT JOIN df_source ON', 
                         ' df_target.join_field_ = df_source.', base_UUID_)
      } else {
        target.attribute <- categories$arena_join %>% 
          filter( entity == target.entities[i])   %>%
          select( attribute_arena)                %>% 
          pull()
        
        s_SQL  <- paste0('SELECT df_target.*, df_source.* FROM df_target LEFT JOIN df_source ON df_target.', 
                         target.attribute, ' = ', source.attribute, 
                         ' AND df_target.join_field_ = df_source.', base_UUID_)
      }
      print(s_SQL)
      
      df_target             <- sqldf( s_SQL )
      df_target$join_field_ <- NULL
      
      assign( target.entities[i], df_target, env = .GlobalEnv)
      rm( df_target)
    } # if 
  } # for i
  return( tMessage)
} # join_entity