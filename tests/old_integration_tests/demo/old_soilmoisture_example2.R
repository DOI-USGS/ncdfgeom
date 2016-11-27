# Old example from write_timeseries_dsg

#'@examples
#'\dontrun{
#'library(soilmoisturetools)
#'ok = ok_data()
#'ok_meta = ok_sites_metadata(ok$station)[, c('station', 'latitude', 
#'                                            'longitude', 'elevation')]
#'all_data = merge(ok, ok_meta)
#'
#'names(all_data) = c('station_name', 'time', 'soil_moisture', 
#'                    'soil_moisture_depth', 'lat', 'lon', 'alt')
#'write_ragged_timeseries_dsg('test.nc', 
#'                             all_data,list(soil_moisture='%', 
#'                             soil_moisture_depth='inches'))
#'
#'}