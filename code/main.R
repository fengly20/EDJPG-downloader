# testrun 

library( 'httr' )

# ---
# 0.5 basic set-ups 
tango_url <- 'https://lance.modaps.eosdis.nasa.gov/cgi-bin/imagery/gallery.cgi' 
user_start_date <- '2016-01-28'
user_end_date <- '2017-12-03' 

keyword <- 'hurricane'

# ---
# 1. make the post request 
keyword_req <- list( keyword = keyword,
                     submit = 'Search by keyword' )

res <- POST( tango_url, body = keyword_req )

if ( res$status_code != 200 ) { 
  stop( 'the page fails to load...' )
  }

res_content <- content( res, 'text' ) 

html_longlist <- unlist( strsplit( res_content, '\n' ) )

# ---
# 2. separate the page source 
tango_1st_line_ind <- grep( '^<table', html_longlist ) 
tango_last_line_ind <- grep( '^</table>', html_longlist )

all_td_start_line <- tango_1st_line_ind + 5 
all_td_end_line <- tango_last_line_ind - 2 

tango_td_lines <- html_longlist[ all_td_start_line : all_td_end_line ]

sub_td_start_inds <- grep( '^    <td', tango_td_lines )
sub_td_end_inds <- grep( '^    </td>', tango_td_lines )

if ( !identical( length( sub_td_start_inds ), length( sub_td_end_inds ) ) )  { 
  stop( 'something\'s wrong...' )
}

td_info_list <- lapply( 1 : length( sub_td_start_inds ), function( i ) { 
  html_block <- tango_td_lines[ sub_td_start_inds[ i ] : sub_td_end_inds[ i ] ]
  
  td_year <- as.integer( substr( html_block[ 2 ], 29, 32 ) ) 
  td_jday <- as.integer( substr( html_block[ 2 ], 34, 36 ) )
  td_date <- substr( html_block[ 2 ], 40, 44 )
  td_month <- as.integer( unlist( strsplit( td_date, '/' ) )[ 1 ] )  
  td_monthday <- as.integer( unlist( strsplit( td_date, '/' ) )[ 2 ] )
  td_sat <- unlist( strsplit( grep( '^<br /><br />', html_block, value = T ), ': ' ) )[ 2 ] 
  td_info_line <- html_block[ grep( '^<br /><br />', html_block ) - 1 ]
  td_utc <- paste0( gsub( ' ', '', unlist( strsplit( td_info_line, 'UTC<br />' ) ) [ 1 ] ), ' UTC' ) 
  td_info <- unlist( strsplit( td_info_line, 'UTC<br />' ) ) [ 2 ]
  
  td_link_lines_ind <- grep( '^        <a href=', html_block ) 
  
  piclink_storage <- c( )
  resolution_storage <- c( )
  for ( ind in td_link_lines_ind ) { 
    temp_line <- html_block[ ind ] 
    temp_link <- paste0( 'https://lance.modaps.eosdis.nasa.gov', unlist( strsplit( temp_line, '\"', fixed = T ) ) [ 2 ] ) 
    temp_resolution <- unlist( strsplit( unlist( strsplit( temp_line, '">', fixed = T ) )[ 2 ], '</a>', fixed = T ) ) [ 1 ]
    piclink_storage <- c( piclink_storage, temp_link ) 
    resolution_storage <- c( resolution_storage, temp_resolution )
  }
  td_link_df <- data.frame( resolution = resolution_storage, 
                            link = piclink_storage, 
                            stringsAsFactors = F )
  out_list <- list( year = td_year, 
                    jday = td_jday,
                    date = td_date,
                    month = td_month,
                    monthday = td_monthday, 
                    sat = td_sat, 
                    utc = td_utc,  
                    info = td_info,  
                    linkdf = td_link_df )
  return( out_list )
} )

# --- 
# 3. trim the selection 
user_start_date_p <- as.POSIXlt( user_start_date, format = "%Y-%m-%d", tz = 'GMT' )
user_end_date_p <- as.POSIXlt( user_end_date, format = "%Y-%m-%d", tz = 'GMT' )
user_start_year <- as.integer( substr( user_start_date, 1, 4 ) )
user_end_year <- as.integer( substr( user_end_date, 1, 4 ) )

trim_year_list <- user_start_year : user_end_year 

trim_jdaystart_list <- c( )
trim_jdayend_list <- c( )
for ( year in trim_year_list ) { 
  if ( year != user_start_year ) { temp_jday <- 1 } else { temp_jday <- user_start_date_p$yday + 1 }
  trim_jdaystart_list <- c( trim_jdaystart_list, temp_jday )
  if ( year != user_end_year ) { temp_jday <- 365 } else { temp_jday <- user_end_date_p$yday + 1 }
  trim_jdayend_list <- c( trim_jdayend_list, temp_jday )
  }

# select list items only fall into the date range 
selected_td_info_list <- lapply( td_info_list, function( td_info ) { 
  if ( td_info$year %in% trim_year_list ) {
    td_info_year <- td_info$year 
    year_index <- which( trim_year_list %in% td_info_year )
    jday_start <- trim_jdaystart_list[ year_index ]
    jday_end <- trim_jdayend_list[ year_index ]
    if ( td_info$jday <= jday_end & td_info$jday >= jday_start ) { 
      selected_td_info <- td_info 
      } else { selected_td_info <- NA }
    } else { selected_td_info <- NA } 
  } )
selected_td_info_list <- selected_td_info_list[ !is.na( selected_td_info_list ) ]

# diagnostic - all links for all resolution 
info_df_list <- lapply( selected_td_info_list, function( td_info ) { 
  link_df <- td_info$linkdf
  out_df <- link_df 
  out_df$year <- td_info$year 
  out_df$jday <- td_info$jday 
  out_df$date <- paste0( td_info$year, '/', td_info$date )
  out_df$sat <- td_info$sat 
  out_df$tz <- td_info$utc 
  out_df$info <- td_info$info 
  out_df <- out_df[ , c( 'year', 'jday', 'date', 'tz', 'sat', 'info', 'resolution', 'link' ) ]
  return( out_df )
  } )
info_df <- do.call( 'rbind', info_df_list )
write.csv( info_df, './diag-out/all_links.csv', row.names = F )

# trim by resolution 
res_info_df_list <- lapply( selected_td_info_list, function( td_info ) { 
  link_df <- td_info$linkdf
  res_list <- link_df$resolution 
  if ( '1km' %in% res_list ) { 
    link_df <- link_df[ link_df$resolution == '1km', ]
  } else { 
      if ( '750m' %in% res_list ) { 
        link_df <- link_df[ link_df$resolution == '750m', ] 
      } else { 
        if ( '500m' %in% res_list ) { 
          link_df <- link_df[ link_df$resolution == '500m', ] 
        } else { 
          if ( '250m' %in% res_list ) { 
            link_df <- link_df[ link_df$resolution == '250m', ] 
          } else { 
            link_df <- data.frame( resolution = 'NA', link = 'NA', stringsAsFactors = F )    
            }
        }
      }  
  }
  out_df <- link_df 
  out_df$fn <- unlist( strsplit( out_df$link, '?image=', fixed = T ) ) [ 2 ]
  out_df$year <- td_info$year 
  out_df$jday <- td_info$jday 
  out_df$date <- paste0( td_info$year, '/', td_info$date )
  out_df$sat <- td_info$sat 
  out_df$tz <- td_info$utc 
  out_df$info <- td_info$info 
  out_df <- out_df[ , c( 'year', 'jday', 'date', 'tz', 'sat', 'info', 'resolution', 'link', 'fn' ) ]
  return( out_df )
} )
res_info_df <- do.call( 'rbind', res_info_df_list )
write.csv( res_info_df, './diag-out/selected_links.csv', row.names = F )

for ( i in 1 : nrow( res_info_df ) )  { 
  fn_w_path <- paste0( './int-out/', res_info_df[ i, 'fn' ] )
  download.file( url, fn_w_path ) 
}
