# #######################################################################*
#
# The function 'arenaAnalytics_Widetable' is used to compute estimates for base units and clusters, 
# and a Wide Table for Arena Shiny tool
#
# This function can be called in the Arena's data processing chain
# 
# The script createsa list of data frames for statistical analysis called 'result_cat' 
# This object contains "totals" and area, at the base unit level for all area-based (active) variables, 
# across all categorical, taxonomic and boolean attributes, listed by entities.   
# Note: reported result entities' names we can get as follows: names(result_cat)
#
# Required R packages (with dependencies): dplyr, stringr, rlang
#
# Created by:   Lauri Vesa, FAO
#               Javier Garcia Perez, FAO
#               Anibal Cuchietti, FAO
#               Jimena Saucedo Miranda (FAO)
#
#######################################################################*

arenaReadJSON <- function() {
  
  # read JSON file 'chain_summary.json'
  chain_summary_json <-  paste(getwd(), 'chain_summary.json', sep = .Platform$file.sep)
  if ( file.exists( chain_summary_json ))  arena.chainSummary <- jsonlite::fromJSON( chain_summary_json )
  
  # Default values for missing data: 
  # a) no base unit -> no sampling design 
  if ( arena.chainSummary$baseUnit == "" )              arena.chainSummary$samplingDesign    <- FALSE
  # b) stratum attribute is missing
  if ( is.null( arena.chainSummary$stratumAttribute ))  arena.chainSummary$stratumAttribute  <- ""
  # nonresponse bias correction is missing
  if ( is.null( arena.chainSummary$analysis$nonResponseBiasCorrection )) arena.chainSummary$analysis$nonResponseBiasCorrection   <- FALSE
  
  arena.analyze   <- list()
  arena.analyze$stratification_area_exists <- FALSE
  
  # change comma to dot (if it is used as decimal separator in UI)
  arena.chainSummary$analysis$reportingArea <- stringr::str_replace( arena.chainSummary$analysis$reportingArea, ",", ".")
  arena.analyze$reportingArea               <- as.numeric( paste0( "0", trimws( arena.chainSummary$analysis$reportingArea ))) 
  
  return( list(arena.analyze, arena.chainSummary) )
} # arenaReadJSON

#########################################################################*

conversion_HierarchicalCodeAttributes <- function( df_data, arena.chainSummary ) {
  # This function is used for re-coding hierarchical categorical data, so that a child category item gets all its parents' codes, separated with asterisk (*)
  # for example: '1' Province_A: 'A' District_A, 'B' District_B  ==> Districts can be re-coded as: '1*A' District_A, '1*B' District_B   
  # New codes are valid only during the run time, and they are not returned back into the Arena database
  
  if ( is.null( df_data ))  return( df_data )
  if ( nrow( df_data ) == 0 | is.null( arena.chainSummary$categoryAttributeAncestors )) return( df_data )
  if ( length( arena.chainSummary$categoryAttributeAncestors$attribute ) == 0)          return( df_data )
  categoryNames = unique(arena.chainSummary$categoryAttributeAncestors$categoryName)
  
  for (j in 1: length(categoryNames)) {
    cat_table <- arena.chainSummary$categoryAttributeAncestors %>% filter(categoryName==categoryNames[j]) %>%
      arrange( categoryLevel)
    for ( i in 1 : length( cat_table$attribute )) {
      if ( cat_table$attribute[[i]] %in% names( df_data )) {
        varname <- cat_table$attribute[[i]]
        df_data <- df_data %>%
          unite( !!varname, any_of( c( cat_table$ancestors[[i]][i], varname )), sep = "*", remove=FALSE ) 
      }
    }}
  return( df_data )
} 


#########################################################################*

# MAIN PROGRAM

arenaAnalytics_LowAggData <- function( server_report_step ) {
  
  usePackage('pacman')
  # require v. 1.3.0 or newer
  pacman::p_install_version(
    'tidyr',  
    '1.3.0'   # the minimum version to be installed
  )
  
  usePackage('tidyr')
  
  if (!exists( "server_report_step" ))    server_report_step <- "last"
  if (is_missing( server_report_step ))   server_report_step <- "last"


  # B. Read json data -------------------------------------------------------

  arena_return        <- arenaReadJSON()
  if (!is.list(arena_return)) return( arena_return ) # Error or no data message is returned
  arena.analyze       <- arena_return[[1]]
  arena.chainSummary  <- arena_return[[2]]
  rm(arena_return)
  
  # 
  if ( !arena.chainSummary$samplingDesign ) return("No sampling design in this chain")
  if ( is.null(arena.chainSummary$samplingStrategy)) return( "Arena Analytics: No sampling strategy selected" )
  
  # SAMPLING DESIGN EXISTS. 
  # Compute expansion factors, sum of area-based variables & weights up to base unit level, and non-response bias corrections
   
# D. Create a folder for output data --------------------------------------

    if ( !exists('user_file_path')) user_file_path <- './user_output/'
    # create a folder for files to be exported
    if ( !dir.exists( user_file_path )) dir.create( user_file_path, showWarnings = FALSE )
    
    # get all files in the main output folder
    f <- list.files(user_file_path, full.names = TRUE, recursive = FALSE)
    f <- f[!file.info(f)$isdir]
    
    # remove the files in the main output folder
    if (length(f)) file.remove(f)
    rm(f)
    

# E. Read base unit data and parameters--------------------------------------------------

    # get base unit data into a data frame
    df_base_unit                   <- get( arena.chainSummary$baseUnit )
    df_base_unit$weight[ is.na( df_base_unit$weight)] <- 0
    # next function is used for re-coding hierarchical categorical attributes, so that a child category item gets all its parents' codes, separated with asterisk (*)
    df_base_unit                   <- conversion_HierarchicalCodeAttributes( df_base_unit, arena.chainSummary)
    
    # Key attribute names: base unit and clustering attributes
    base_UUID_                     <- paste0( arena.chainSummary$baseUnit, "_uuid")
    cluster_UUID_                  <- ifelse( arena.chainSummary$clusteringEntity != "", paste0( arena.chainSummary$clusteringEntity, "_uuid"), "")    
    # Stratification check: method, attribute and areas
    arena.analyze$stratification   <- ifelse(( arena.chainSummary$samplingStrategy == 3 | arena.chainSummary$samplingStrategy == 4  | arena.chainSummary$samplingStrategy == 5 ) & arena.chainSummary$stratumAttribute != "", TRUE, FALSE)
    arena.analyze$strat_attribute  <- ifelse( arena.analyze$stratification, arena.chainSummary$stratumAttribute, "")
    

    # G1. Read AOIs (areas of strata) -----------------------------------------

    aoi_df     <- NULL
    
    if ( arena.analyze$stratification ) {
      aoi_df        <- as.data.frame( categories[[ arena.chainSummary$stratumAttributeCategory ]]) %>%
        mutate(code = as.character(code), code_joint = as.character(code_joint) )
      

      # G2. Read PSU and SSU numbers --------------------------------------------
      # PSU = primary sampling unit, SSU = secondary sampling unit
      if ( arena.chainSummary$analysis$nonResponseBiasCorrection ) {
        if (  'design_psu' %in% names( aoi_df) & !'design_ssu' %in% names( aoi_df)) aoi_df$design_ssu <- 0
        if ( !'design_psu' %in% names( aoi_df) &  'design_ssu' %in% names( aoi_df)) aoi_df$design_psu <- 0
        if (  'design_psu' %in% names( aoi_df) &  'design_ssu' %in% names( aoi_df)) {
          aoi_df$design_psu <- as.numeric( aoi_df$design_psu)
          aoi_df$design_ssu <- as.numeric( aoi_df$design_ssu)
        } else {
          arena.chainSummary$analysis$nonResponseBiasCorrection <- FALSE
          aoi_df$design_psu <- 0
          aoi_df$design_ssu <- 0
        }
      } else {
        aoi_df$design_psu <- 0
        aoi_df$design_ssu <- 0
      }
      
      # G3. read AOI data from the correct level --------------------------------
      # if hierarchical table
      if (('level_1_code' %in% names( aoi_df )) & ('area_cumulative' %in% names( aoi_df ))) {
        
        # set missing area to 0
        if ( anyNA( aoi_df$area_cumulative)) aoi_df$area_cumulative[ is.na( aoi_df$area_cumulative) ] <- 0
        aoi_df$area_cumulative <- as.numeric( aoi_df$area_cumulative )
        if ( !all( aoi_df$area_cumulative == 0)) arena.analyze$stratification_area_exists <- TRUE
        
        aoi_df      <- aoi_df %>%
          filter( level == arena.chainSummary$stratumAttributeCategoryLevel) %>%
          select( code = code_joint, label, area=area_cumulative, design_psu, design_ssu)
        
      } else if (('level_1_code' %in% names( aoi_df)) & 'area' %in% names( aoi_df)) {    # flat lookup table with area
        if ( anyNA( aoi_df$area)) aoi_df$area[ is.na( aoi_df$area) ] <- 0
        aoi_df$area <- as.numeric( aoi_df$area )
        if ( !all( aoi_df$area == 0)) arena.analyze$stratification_area_exists <- TRUE
        
        if ('code_joint' %in% names(aoi_df )) {
          aoi_df      <- aoi_df %>%
            dplyr::filter(level == arena.chainSummary$stratumAttributeCategoryLevel) %>%
            dplyr::select( code = code_joint, label, area, design_psu, design_ssu)
        } else {
          aoi_df      <- aoi_df %>%
            dplyr::filter(level == arena.chainSummary$stratumAttributeCategoryLevel) %>%
            dplyr::select( code, label, area, design_psu, design_ssu)
        }
        
      } else if ( 'area' %in% names( aoi_df)) {    # flat lookup table with area, no "level"
        if ( anyNA( aoi_df$area)) aoi_df$area[ is.na( aoi_df$area) ] <- 0
        aoi_df$area <- as.numeric( aoi_df$area )
        if ( !all( aoi_df$area == 0)) arena.analyze$stratification_area_exists <- TRUE
        
        aoi_df <- aoi_df %>%
          select( code, label, area, design_psu, design_ssu)
        
      } else if ( arena.analyze$reportingArea > 0 ) {
        aoi_df$area <- 0.0
      } else {
        arena.analyze$stratification <- FALSE
      }
    } 
    

# H. Assign  cluster weights ---------------------------------------------
    
    # CLUSTER SAMPLING: Base units' weights are re-scaled so that the sum of weights in a cluster is equal to 1. 
    
    if (cluster_UUID_ != "" ) {
      
      df_base_unit <- df_base_unit                                              %>% 
        dplyr::summarize( sum_weight_ = sum( weight ), cluster_count_ = n(), .by = all_of(cluster_UUID_ ))   %>%
        dplyr::select( all_of( cluster_UUID_), sum_weight_, cluster_count_ )    %>%
        dplyr::right_join( df_base_unit, by = cluster_UUID_ )                
      
      if (!arena.chainSummary$analysis$clusteringVariances) df_base_unit$weight = df_base_unit$weight / df_base_unit$sum_weight_ 
      df_base_unit$weight[ is.na(df_base_unit$weight) ] <- 0
    } 
    

# I.Set default non-response correction values ---------------------------

    df_base_unit$arena_psu_correction       <- 1  # correction factor for PSUs
    df_base_unit$arena_ssu_correction       <- 1  # correction factor for SSUs
    

    # J1. Apply nonresponse bias correction for weights: SSUs ----------------------------

    # MISSING SECONDARY SAMPLING UNITS (SSUs): non-response bias correction, naive imputation method
    if ( arena.analyze$stratification & !is.null( aoi_df) & cluster_UUID_ != "" & arena.chainSummary$analysis$nonResponseBiasCorrection) {  # aoi exist
      aoi_df[ arena.analyze$strat_attribute ] <- aoi_df$code
      
      if ('design_ssu' %in% names( aoi_df)) {
        aoi_df$design_ssu[ is.na( aoi_df$design_ssu)] <- 0
        
        if ( sum( aoi_df$design_ssu ) > 0) {        
          df_base_unit$arena_ssu_correction <- NULL
          arena_cluster_statistics <- aoi_df                                      %>%
            dplyr::select( all_of( arena.analyze$strat_attribute), design_ssu )   %>%
            dplyr::right_join( df_base_unit                                       %>% 
                                 dplyr::filter( weight > 0 )                      %>%
                                 dplyr::summarize( cluster_count_ = n(), sum_weight_ = sum( weight ), .by = c(!!! syms( arena.analyze$strat_attribute), all_of( cluster_UUID_ ))), 
                               by = arena.analyze$strat_attribute)                %>%
            dplyr::mutate( arena_ssu_correction = ifelse( design_ssu > 0 & sum_weight_ > 0, ( design_ssu / cluster_count_) * sum_weight_, 1)) # this works if a full base unit weight is 1 !!
          
          
          # check whether some clusters are split over more than 1 stratum
          if ( nrow( arena_cluster_statistics) != nrow( unique( arena_cluster_statistics[cluster_UUID_] ))) {
            # list of clusters belonging to multiple strata
            analyze_overlaps <- arena_cluster_statistics                      %>%
              dplyr::summarize( c_count = n(), .by = all_of(cluster_UUID_ ))  %>%
              dplyr::filter( c_count > 1 )                                    %>%
              pull( cluster_UUID_ ) 
            
            # fix this later, overlaps get all 1:
            arena_cluster_statistics$arena_ssu_correction <- with( arena_cluster_statistics,
                                                                   ifelse( cluster_UUID_ %in% analyze_overlaps, 1, arena_ssu_correction)) 
            
            df_base_unit <- df_base_unit %>%
              dplyr::left_join( arena_cluster_statistics %>% 
                                  dplyr::select( !!! syms( arena.analyze$strat_attribute), all_of( cluster_UUID_), arena_ssu_correction), by = c( arena.analyze$strat_attribute, cluster_UUID_))
            
          } else {  # no clusters split across strata
            df_base_unit <- df_base_unit                        %>%
              dplyr::left_join( arena_cluster_statistics        %>% 
                                  dplyr::select( all_of( cluster_UUID_), arena_ssu_correction), by = cluster_UUID_)
          }
        }
      }
      
    } else if (cluster_UUID_ != "" & arena.chainSummary$analysis$nonResponseBiasCorrection & !arena.chainSummary$analysis$clusteringVariances) {
      # non-stratified cluster sampling with non-response bias correction (for missing samples in clusters)
      ssu_max = max( df_base_unit$cluster_count_ )
      df_base_unit$arena_ssu_correction <- ifelse( ssu_max > 0 & df_base_unit$sum_weight_ > 0, ( ssu_max / df_base_unit$cluster_count_ ) , 1 )
    }
    
    # 1. non-stratified sampling, compute expansion factor for the base unit
    # 2. stratified sampling,  bias correction for missing clusters/plots
    

    # J2. Apply nonresponse bias correction for weights: PSUs -----------------------------

    # STRATIFIED SAMPLING, MISSING PRIMARY SAMPLING UNITS (PSUs): non-response bias correction, naive imputation method
    if ( arena.analyze$stratification & !is.null( aoi_df ) & arena.chainSummary$analysis$nonResponseBiasCorrection ) {  
      aoi_df[ arena.analyze$strat_attribute ] <- aoi_df$code
      
      if ( 'design_psu' %in% names( aoi_df) ) {
        aoi_df$design_psu[ is.na( aoi_df$design_psu) ] <- 0
        
        if ( sum( aoi_df$design_psu) > 0) {
          df_base_unit$arena_psu_correction    <- NULL
          
          # clustered sampling
          if ( cluster_UUID_ != "" ) {
            arena_psu_statistics <- df_base_unit                                                   %>% 
              dplyr::filter( weight > 0 )                                                          %>%
              dplyr::distinct( !!! syms( arena.analyze$strat_attribute), !!! syms(cluster_UUID_ )) %>%
              dplyr::summarize( psu_count = n(), .by = all_of(arena.analyze$strat_attribute ))     %>%
              dplyr::left_join( aoi_df  %>% 
                                  dplyr::select( all_of( arena.analyze$strat_attribute), design_psu ), 
                                by = arena.analyze$strat_attribute)                                           %>%
              dplyr::mutate( arena_psu_correction = design_psu / psu_count )                       %>%
              dplyr::select( all_of( arena.analyze$strat_attribute), arena_psu_correction )        %>%
              as.data.frame()
          } else {
            # non clustered sampling
            arena_psu_statistics <- df_base_unit                                                    %>% 
              dplyr::filter( weight > 0 )                                                           %>%
              dplyr::summarize( baseunit_count = n(), .by = all_of(arena.analyze$strat_attribute )) %>%
              dplyr::left_join( aoi_df  %>% 
                                  dplyr::select( all_of( arena.analyze$strat_attribute), design_psu), 
                                by = arena.analyze$strat_attribute )                                           %>%
              dplyr::mutate( arena_psu_correction = design_psu / baseunit_count )                   %>%
              dplyr::select( all_of( arena.analyze$strat_attribute), arena_psu_correction )         %>%
              as.data.frame()
          } 
          
          # join PSU correction with base unit table 
          df_base_unit <- df_base_unit %>%
            dplyr::left_join( arena_psu_statistics, by = arena.analyze$strat_attribute)
        } # if (sum(aoi_df$design_psu > 0)
      }
    }

  # J3. Add SSU and PSU non-response bias corrections to weight -------------
    df_base_unit$weight <- df_base_unit$weight * df_base_unit$arena_ssu_correction * df_base_unit$arena_psu_correction
    

  # K. Calculate area expansion factors for base units ----------------------
    
    df_base_unit$exp_factor_ <- NULL
    
    # AOI table exists?
    if ( arena.analyze$stratification & !is.null( aoi_df)) {
      aoi_df[ arena.analyze$strat_attribute ] <- aoi_df$code
      
      arena.expansion_factor <- df_base_unit                         %>%             # Do: missing stratum in base unit?
        dplyr::filter( weight > 0 )                                  %>%
        dplyr::summarize( aoi_weight_ = sum( weight ), .by = all_of( arena.analyze$strat_attribute ) )  %>%   # https://www.tidyverse.org/blog/2023/02/dplyr-1-1-0-per-operation-grouping/
        dplyr::left_join( aoi_df                                     %>% 
                            dplyr::select( all_of( arena.analyze$strat_attribute), area), by = arena.analyze$strat_attribute ) %>% 
        data.frame()
      
      # test this
      if (arena.chainSummary$samplingStrategy == 5) {
        arena.analyze$reportingArea <- 0
        df_base_unit$exp_factor_    <- NULL
        arena.expansion_factor$join_var  <- arena.expansion_factor[[1]]  
        join_df                          <- twoPhaseSampling_results[[3]] %>% dplyr::select(join_var = code, area) 
        
        arena.expansion_factor <- arena.expansion_factor %>%
          dplyr::select( -area ) %>%
          left_join( join_df, by = 'join_var') %>%
          dplyr::select( -join_var)
      }
      # test ends
      
      if ( all( aoi_df$area == 0) & ( arena.analyze$reportingArea > 0 )) arena.expansion_factor$area <-  arena.analyze$reportingArea / sum( arena.expansion_factor$aoi_weight_) *  arena.expansion_factor$aoi_weight_ 
      
      arena.expansion_factor$exp_factor_ <- arena.expansion_factor$area / arena.expansion_factor$aoi_weight_ 
      
      df_base_unit <- df_base_unit                                   %>%
        dplyr::left_join( arena.expansion_factor                     %>%
                            dplyr::select( !!! syms( arena.analyze$strat_attribute), exp_factor_), by = arena.analyze$strat_attribute) %>%
        dplyr::mutate( exp_factor_ = exp_factor_ * weight)
      
    } else if ( arena.analyze$stratification) { 
      arena.expansion_factor <- df_base_unit                         %>%
        dplyr::filter( weight > 0 )                                  %>%
        dplyr::summarize( aoi_weight_ = sum( weight ), .by = all_of( arena.analyze$strat_attribute )) %>% 
        data.frame()
      
      if ( arena.analyze$reportingArea > 0 ) arena.expansion_factor$area <-  arena.analyze$reportingArea / sum( arena.expansion_factor$aoi_weight_) *  arena.expansion_factor$aoi_weight_ 
      arena.expansion_factor$exp_factor_ <- arena.expansion_factor$area / arena.expansion_factor$aoi_weight_ 
      
      df_base_unit <- df_base_unit                                   %>%
        dplyr::left_join( arena.expansion_factor                     %>%
                            dplyr::select( !!! syms( arena.analyze$strat_attribute), exp_factor_), by = arena.analyze$strat_attribute) %>%
        dplyr::mutate( exp_factor_ = exp_factor_ * weight)
      
    } else if ( arena.analyze$reportingArea > 0 ) {
      # area is read from the UI: arena.analyze$reportingArea
      df_base_unit$exp_factor_   <- arena.analyze$reportingArea / sum( df_base_unit$weight) * df_base_unit$weight
      
    } else {
      # no stratification, no AOI data 
      df_base_unit$exp_factor_   <- df_base_unit$weight 
    }
    
    df_base_unit$exp_factor_[ is.na( df_base_unit$exp_factor_) ] <- 0
    
    ##########################################################################################*
    

# L. Get list of active area-based result variables --------

    # active area-based result variables (of all entities)
    # parents of base units cannot have area-based variables!
    result_entities <- arena.chainSummary$resultVariables %>%
      dplyr::filter( areaBased == TRUE & active == TRUE & stringr::str_detect(entityPath, arena.chainSummary$baseUnit))  %>%
      #      filter(entity != arena.chainSummary$baseUnit) %>%  
      select( entity ) %>% 
      unique()         %>%
      pull()
    
    # next loop over result entities, collects a list of all categorical, taxonomic and boolean variables
    # get: entity, base_UUID_, exp_factor_, and combinations for all categorical variables in the data
    result_cat_attributes <- list()
    result_cat            <- list()
    base_unit.results     <- list()
    cluster.results       <- list()
    
    # ADD area estimates based on exp_factor
    

# M1. Loop across result entities [i] --------------------------------------

    # Quantitative result variables against categorical and taxonomic data
    for ( i in (1:length(result_entities))) {
      
      # PART 1.compute sums (per AOIs) and means (/ha) across all categorical variables 
      # drop out columns where all data is NA.  https://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na
      
      df_entitydata              <- get( result_entities[[i]])
      # next function is used for re-coding hierarchical categorical attributes, so that a child category item gets all its parents' codes, separated with asterisk (*)
      df_entitydata              <- conversion_HierarchicalCodeAttributes( df_entitydata, arena.chainSummary )
      

      # M2. List of all categorical, taxonomic and boolean attributes -----------
      
      # categorical, taxonomical
      result_cat_attributes[[i]] <- ( df_entitydata  %>% select( where( ~!all( is.na(.)))) %>% select( where( ~is.character(.))) )  %>%
        select(ends_with("_label") | ends_with("_scientific_name")) %>% 
        names()

      # booleans
      boolean_list <-  ( df_entitydata  %>% select( where( ~!all( is.na(.)))) %>% select( where( ~is.logical(.))) )  %>%
        names()
      
      if (length( boolean_list) > 0) result_cat_attributes[[i]] <- unique( c(result_cat_attributes[[i]], boolean_list))
      # rm( boolean_list)
      
      # Remove: _label, _scientific_name 
      result_cat_attributes[[i]] <- ifelse( stringr::str_sub(result_cat_attributes[[i]], -6, -1)  == "_label",           stringr::str_sub(result_cat_attributes[[i]], 0, -7),  result_cat_attributes[[i]] )  
      result_cat_attributes[[i]] <- ifelse( stringr::str_sub(result_cat_attributes[[i]], -16, -1) == "_scientific_name", stringr::str_sub(result_cat_attributes[[i]], 0, -17), result_cat_attributes[[i]] )  
      
      # join parents' categorical result attributes with entity data
      # add categorical result variables
      # 1) search parents' names
      parent_names               <- df_entitydata %>% 
        dplyr::select(ends_with("_uuid"), -ends_with("_file_uuid"), -record_uuid, -record_owner_uuid, -paste0(result_entities[[i]], "_uuid")) %>% 
        names %>%
        stringr::str_sub(., 0,-6)
      
      
      # 2) join parents' cat. data
      if (length(parent_names) > 0) {
        res_cat                    <- arena.chainSummary$resultVariables %>% 
          dplyr::filter( type == "C" & active == TRUE & entity %in% parent_names)   %>% 
          dplyr::select( entity, name) %>%
          dplyr::filter( !name %in% names(df_entitydata))
        
        if ( nrow( res_cat) > 0) {
          for (j in (1:nrow(res_cat) )) {
            df_join       <- get( res_cat$entity[j]) %>% select( paste0( res_cat$entity[j], "_uuid"), res_cat$name[j] ) 
            df_entitydata <- df_entitydata %>%
              dplyr::left_join( df_join, by = paste0( res_cat$entity[j], "_uuid")) 
            
            rm( df_join )
          }
        }
      } 
      
      # 3) list of names: add base unit id, and categorical result variables (of parents & selected entity)
      res_cat         <- arena.chainSummary$resultVariables %>% 
        dplyr::filter( type=="C" & active==TRUE & entity %in% c(parent_names, result_entities[[i]]) ) %>% 
        pull( name)
      
      result_cat_attributes[[i]] <- unique( c(base_UUID_, result_cat_attributes[[i]], res_cat) )
      
      resultVariables <- arena.chainSummary$resultVariables %>%
        dplyr::filter( areaBased == TRUE & active == TRUE & entity == result_entities[[i]]) %>%
        dplyr::mutate( name = paste0( name, "_ha")) %>%
        pull( name) 
      
      
      if (result_entities[[i]] != arena.chainSummary$baseUnit) { # result entity is not same as base unit entity

        # M3. List of base unit IDs that are not in entity data -------------------
        # (e.g. treeless plots)
        
        missing_ids     <- setdiff( unique( unlist( df_base_unit %>% filter( weight > 0) %>% select( all_of( base_UUID_)) )), unique( unlist( df_entitydata[ base_UUID_] )))
        
        if ( length( missing_ids) > 0) {
          # get list of attributes that exist in base unit data
          names_in_data   <- intersect( names( df_entitydata), names( df_base_unit)) 
          
          df_base_unit2   <- subset( df_base_unit,  eval( parse( text = base_UUID_)) %in% missing_ids)
          result_cat[[i]] <- bind_rows( df_entitydata, df_base_unit2 %>% select( all_of( names_in_data)) )
          result_cat[[i]] <- result_cat[[i]]  %>% 
            dplyr::mutate( across( where( is.numeric), ~tidyr::replace_na(., 0)))
          rm( names_in_data); rm( df_base_unit2)
        } else {
          result_cat[[i]] <- df_entitydata
        }
        
        rm( missing_ids )

        # M4. Total (and mean) for each category & boolean & taxon combination at base unit level --------

        result_cat[[i]]$entity_count_                          <- 0
        result_cat[[i]]$entity_count_[1 : nrow(df_entitydata)] <- 1
        
        result_cat[[i]] <- result_cat[[i]]                             %>%
          dplyr::right_join( df_base_unit %>% select( all_of( base_UUID_), exp_factor_), by = base_UUID_ ) %>% # join expansion factor
          dplyr::group_by(  across( result_cat_attributes[[i]] ))      %>%
          dplyr::summarize( across(.cols= all_of(resultVariables), 
                                   list( Total = ~sum(exp_factor_ * .x, na.rm = TRUE), Mean = ~sum(.x, na.rm = TRUE) ),  
                                   .names = "{.col}.{.fn}"), 
                            entity_count_ = sum(entity_count_) )       %>%
          data.frame() 
        
      } else { # entity is the base unit
        result_cat[[i]] <- df_entitydata                               %>%
          dplyr::left_join( df_base_unit %>% select( all_of(base_UUID_), exp_factor_ ), by = base_UUID_) %>%
          dplyr::group_by(  across( result_cat_attributes[[i]] ))      %>%
          dplyr::summarize( across( .cols= all_of( resultVariables), 
                                    list( Total = ~sum(exp_factor_ * .x, na.rm = TRUE), Mean = ~sum( .x, na.rm = TRUE) ),  
                                    .names = "{.col}.{.fn}"), 
                            entity_count_ = n() )                      %>%
          data.frame()
      }
      
      # M5. Finalize OLAP table -------------------------------------------------

      # add weight, exp_factor_; AND IF EXISTS: cluster_UUID_, arena.analyze$strat_attribute (This is actually already in dataframe because it is categorical!) 
      temp_list_variables <- c(base_UUID_, "weight", "exp_factor_")
      if ( cluster_UUID_ != ""    &  !( cluster_UUID_ %in% names(result_cat[[i]])) )        temp_list_variables <- c( temp_list_variables, cluster_UUID_)
      if ( arena.analyze$stratification  &  !( arena.analyze$strat_attribute %in% names(result_cat[[i]])) ) temp_list_variables <- c( temp_list_variables, arena.analyze$strat_attribute)
      
      result_cat[[i]] <- result_cat[[i]] %>%
        dplyr::left_join( df_base_unit %>% select( all_of( temp_list_variables)), by = base_UUID_)
      
      rm( temp_list_variables )
      

      # M6. OLAP: Compute per hectare results at base unit level for res. variables  --------
      
      # Note: above-computed (M4) Means are not used!
      ## get results at the base unit level for each result variable for OLAP
      if (result_entities[[i]] != arena.chainSummary$baseUnit) {
        out_path  <- "OLAP/"
        dir.create( paste0( user_file_path, out_path ), showWarnings = FALSE )
        
        keys_to_add <- which( !(arena.chainSummary$baseUnitEntityKeys %in% names(result_cat[[i]])))
        if (length(keys_to_add) > 0) {
          join_col <- df_base_unit %>% select(all_of(base_UUID_), all_of(arena.chainSummary$baseUnitEntityKeys[keys_to_add])) %>%
            dplyr::mutate( across( where( is.numeric), ~as.character(.))) %>% distinct()
          
          out_file_olap_data <- result_cat[[i]] %>% left_join(join_col, by = base_UUID_) 
        } else {
          out_file_olap_data <- result_cat[[i]]
        }
        
        # Keep only TOTALS
        out_file_olap_data        <- out_file_olap_data %>% select( -ends_with(".Mean"))
        data_names                <- names( out_file_olap_data)
        names(out_file_olap_data) <- gsub( "_ha.Total", "", data_names) 

      }      

      # M8. Per hectare results at base unit level (out) --------
      
      ## Per hectare results at the base unit level for each result variable (out)
      base_unit.results[[i]] <- df_entitydata %>%
        # Add expansion factor for all result entities
        dplyr::right_join(( df_base_unit %>% select( all_of( base_UUID_), weight, exp_factor_)), by = base_UUID_) %>%
        dplyr::group_by( across( all_of(base_UUID_ ))) %>%
        dplyr::summarize( across( .cols= all_of( resultVariables),
                                  list( Total = ~sum( exp_factor_ * .x, na.rm = TRUE), Mean = ~sum( .x, na.rm = TRUE) ),
                                  .names = "{.col}.{.fn}"),
                          item_count = n() ) %>%
        dplyr::mutate( item_count = ifelse( if_all(paste0( resultVariables, ".Total") , ~ .x == 0), 0, item_count))
      
      
      # join results with the clone of base unit
      df_base_unit <- df_base_unit %>%
        dplyr::left_join( base_unit.results[[i]] %>% select(-item_count), by = base_UUID_)
      
      # M7b. Add plot totals into the OLAP table -------------------------------------------
      
      if (exists('out_file_olap_data')) {
        olap_file_names           <- names( out_file_olap_data)
        
        base_unit_attribute_names <- olap_file_names[ result_cat_attributes[[i]] %in% names(df_base_unit)]
        olap_base_unit_totals     <- df_base_unit %>% select( any_of(base_unit_attribute_names)) %>% 
          left_join(base_unit.results[[i]] %>% select(all_of(base_UUID_), any_of(ends_with(".Total")), entity_count_ = item_count) , by = base_UUID_ )
        
        data_names <- names(olap_base_unit_totals)
        names( olap_base_unit_totals) <- gsub( "_ha.Total", "", data_names) 
        out_file_olap_data$OLAP_baseunit_total    <- FALSE
        olap_base_unit_totals$OLAP_baseunit_total <- TRUE
        out_file_olap_data <- dplyr::bind_rows( out_file_olap_data, olap_base_unit_totals)
        out_file_olap_data[ is.na(out_file_olap_data )] <- ""
        
        rm( base_unit_attribute_names); rm( olap_file_names) 
        rm( data_names); rm (olap_base_unit_totals)
        
        # M7. Write OLAP table into CSV -------------------------------------------
        
        out_file_name <- paste0(user_file_path, "OLAP/OLAP_", result_entities[i], ".csv")
        tryCatch({if (exists('user_file_path'))  write.csv(out_file_olap_data, out_file_name,  row.names = F)},
                 warning = function( w ) { cat("No output - OLAP data") },
                 error   = function( e ) { cat("No output - OLAP data")
                 })
        rm( keys_to_add ); rm( out_file_olap_data ); rm( out_file_name )
      }
      
      
      
      # M9. Per hectare results at cluster level (out)  -------------------------
      
      ## compute sum of per hectare results at the cluster level for each result variable
      
      if ( cluster_UUID_ != "" ) {
        
          clusterVariables= str_replace(resultVariables, "_ha.Total", "")
          cluster.results[[i]] <- df_base_unit %>% 
            dplyr::group_by( across( all_of( cluster_UUID_ )))  %>%
            dplyr::summarize( across( .cols= all_of( ends_with(".Total")) & starts_with(clusterVariables), ~sum( .x)), 
                              weight = sum(weight), exp_factor = sum(exp_factor_)) %>%
            dplyr::mutate( across(ends_with( "Total" ),
                                  ~ .x/exp_factor))
          
          n_names = names(cluster.results[[i]])
          n_names = str_replace(n_names, "_ha.Total", "")
          names(cluster.results[[i]]) = n_names
          rm(n_names)
      }
        
      rm(resultVariables)
    } # end of i-loop
    
    names(result_cat) <- result_entities 
    print( names(result_cat))
    
    # create OLAP zip file for ARENA Shiny Reporter
    # The new Shiny application will be launced 2026
    if ( dir.exists( './user_output/OLAP')) {
      # with categories, taxonomies, chainSummary, SchemaSummary
      write.csv( arena.schemaSummary, "./user_output/OLAP/SchemaSummary.csv", row.names = F)
      if ( exists( 'categories')) saveRDS( categories, "./user_output/OLAP/categories.rds")
      if ( exists( 'taxonomies')) saveRDS( taxonomies, "./user_output/OLAP/taxonomies.rds")
      files_to_zip                             <- list.files("./user_output/OLAP", full.names = TRUE)
      files_to_zip[ length( files_to_zip) + 1] <- "./chain_summary.json"
      
      f_name <- paste0('./user_output/OLAP_Shiny_(', arena.chainSummary$surveyName, ').zip')
      zip::zipr( f_name, files_to_zip)
    }   
  
# ******************************************************************* -----

  
  processMessage = ""  

  # if ( arena.chainSummary$analysis$nonResponseBiasCorrection) {
  #   if ( arena.analyze$stratification ) {
  #     nonResponse_out1 <- df_base_unit %>% select( STRATUM = all_of( arena.analyze$strat_attribute), correction_factor = arena_psu_correction ) %>% unique() %>% arrange( STRATUM)  
  #     tryCatch({if (exists('user_file_path') & exists("nonResponse_out1")) write.csv( nonResponse_out1, out_file[[5]], row.names = F)},
  #              warning = function( w ) { cat("No output - nonResponse_out1") },
  #              error   = function( e ) { cat("No output - nonResponse_out1")
  #              })
  #   }
  # }
  
  # get results by sampling units out
  out_path <- paste0(user_file_path, "sampling unit results", "/")
  # create a folder for files to be exported
  if ( !dir.exists( out_path )) dir.create( out_path, showWarnings = FALSE )
  
  for ( i in 1:length( result_entities )) {
    outfile7              <- paste0( out_path, result_entities[[i]], "_base_unit_results.csv")
    base_unit.results_out <- base_unit.results[i] %>% as.data.frame() %>% 
      select(-ends_with(".Total")) %>% 
      setNames( stringr::str_replace( names(.), ".Mean", "")) 
    
    dimension_names <- arena.chainSummary$baseUnitEntityKeys
    if ( arena.analyze$stratification )      dimension_names <- unique( c( dimension_names, arena.analyze$strat_attribute))
    
    # use original label for hier. categorical attribute data, on levels 2-.. 
    if (length( arena.chainSummary$categoryAttributeAncestors$attribute ) > 0) {
      category_attribute_ancestors <- intersect( dimension_names, arena.chainSummary$categoryAttributeAncestors$attribute)
      if ( length( category_attribute_ancestors ) > 0 ) {
        dimension_names <- setdiff( dimension_names, category_attribute_ancestors)
        dimension_names <- c( dimension_names, paste0( category_attribute_ancestors, "_label" ))
      }
    }
    
    base_unit.results_out <- df_base_unit %>% select( all_of( base_UUID_), all_of( dimension_names ), weight, exp_factor=exp_factor_) %>%
      dplyr::left_join( base_unit.results_out, by = base_UUID_) %>%
      dplyr::select( -all_of( base_UUID_)) %>%
      mutate(item_count = ifelse(weight == 0, 0, item_count))
    
    tryCatch({if (exists('user_file_path')) write.csv( base_unit.results_out, outfile7, row.names = F)},
             warning = function( w ) { cat("No output - base unit results") },
             error   = function( e ) { cat("No output - base unit results")
             })
    
    if ( cluster_UUID_ !="" ) {
      outfile8            <- paste0( out_path, result_entities[[i]], "_cluster_results.csv")
      cluster.results_out <- cluster.results[i] %>% as.data.frame() %>% select(-ends_with(".Total"))
      cluster.results_out <- df_base_unit %>% dplyr::select( all_of( cluster_UUID_), all_of( arena.chainSummary$clusteringEntityKeys )) %>%
        unique() %>%
        dplyr::left_join( cluster.results_out, by = cluster_UUID_) %>%
        dplyr::select( -all_of( cluster_UUID_ ))
      
      cluster.results_out[ is.na( cluster.results_out)] <- 0
      
      tryCatch({if (exists('user_file_path')) write.csv( cluster.results_out, outfile8, row.names = F)},
               warning = function( w ) { cat("No output - cluster results") },
               error   = function( e ) { cat("No output - cluster results")
               })
    }
    
  }
  if ( Sys.getenv("RSTUDIO_PROGRAM_MODE") == "server" & exists('user_file_path')  & server_report_step == "last") { 
    # zip all files
    export_filename  <- paste0( user_file_path, 'arena_results_(', arena.chainSummary$surveyName, ').zip')
    files2zip        <- dir( user_file_path, full.names = TRUE )
    if ( length(files2zip) > 0 ) {
      zip(zipfile = export_filename, files = files2zip, mode = "cherry-pick")
      browseURL( export_filename )
    }
  }
  
  if ( Sys.getenv("RSTUDIO_PROGRAM_MODE") == "desktop" & exists('user_file_path') ) { 
    if ( Sys.info()['sysname']=="Windows" ) {
      processMessage = paste0(" Result files in ", getwd(), "/user_output/")
      utils::browseURL( user_file_path )
    }
  }
  
  processMessage = paste0("Arena Analytics: Process completed. ", processMessage )
  return( processMessage )
  
}

# END -------------------------------------------------------------
###################################################################*
