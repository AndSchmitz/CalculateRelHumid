CalculateRelHumid <- function(
  CurrentTemperature_DegC,
  DewPoint_DegC
) {
  #Following https://bmcnoldy.rsmas.miami.edu/Humidity.html
  
  #August–Roche–Magnus formula for saturation water vapour pressure
  #https://en.wikipedia.org/wiki/Clausius%E2%80%93Clapeyron_relation#Meteorology_and_climatology
  CalculateSaturationWaterVapourPressure <- function(Temperature_DegC) {
    SaturationWaterVapourPressure_hPa = 6.1094 * exp( (17.625 * Temperature_DegC) / (Temperature_DegC + 243.04) )
    return(SaturationWaterVapourPressure_hPa)
  }
  
  #Current absolute water content of the air as partial pressure
  AbsVapPressure_hPa <- CalculateSaturationWaterVapourPressure(DewPoint_DegC)
  #Maximum absolute water content of the air at current temperature as partial pressure
  MaxVapPressure_hPa <- CalculateSaturationWaterVapourPressure(CurrentTemperature_DegC)
  
  #Relative humidity
  RelHumid_percent <- round(AbsVapPressure_hPa / MaxVapPressure_hPa * 100,2)
  
  return(RelHumid_percent)
  
}
