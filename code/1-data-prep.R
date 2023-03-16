# ==============================================================================
# DATA PREPARATION
# ==============================================================================

# load EPBM data
data_orig <- read_csv('../../../../2-data/seattle/SR99.csv')
# store units
col_units <- data_orig[1, ]

# ------------------------------------------------------------------------------
# CLEAN ORIGINAL DATA
# ------------------------------------------------------------------------------

# remove unused rows and cols
data <- data_orig %>% 
  # remove rows of units, except the first row
  # .[which(.[, 1] != 'ring'), ] %>% rbind(col_units, .) %>%
  # remove rows with unit <= 0
  # .[which(.[, 1] > 0), ] %>% 
  # remove columns with 'NA' unit
  .[ , which(!is.na(.[1,]))] %>%
  # remove columns with categorical data
  .[ , which(.[1,] != 'on/off')] %>%
  .[ , which(.[1,] != 'off/on')] %>%
  select(!contains('status')) %>%
  # remove unused columns, i.e. contains (Not Used) & [DEL]
  select(!contains('(Not Used)')) %>%
  select(!contains('[DEL]')) %>%
  # remove columns contain information on cumulative, capacity, target
  .[ , which(.[1,] != 'cnt')] %>%
  .[ , which(.[1,] != 'Cnt')] %>%
  select(!contains('counter')) %>%
  select(!contains('cumulative')) %>%
  select(!contains('target')) %>%
  select(!contains('tgt')) %>%
  select(!contains('limit')) %>%
  select(!contains('capacity')) %>%
  select(!contains('max')) %>%
  select(!contains('range')) %>%
  select(!contains('level')) %>%
  select(!contains('setting')) 

# ------------------------------------------------------------------------------
# INDEX
# ------------------------------------------------------------------------------

ring <- data %>%
  select(contains('ring no')) %>%
  set_df(.) %>%
  .[ ,1] %>% # remove redundant column  
  set_colnames(., 'Ring')

chainage <- data %>%
  select(contains('chainage')) %>%
  .[, which(.[1,] == 'ft')] %>%
  set_df(.) %>%
  set_colnames(., c('ChainageHead', 
                    'ChainageArt', 
                    'ChainageTail')) 

# ------------------------------------------------------------------------------
# CUTTER
# ------------------------------------------------------------------------------

cutter_force <- data %>%
  select(contains('cutter')) %>%
  select(contains('force')) %>%
  select(contains('total')) %>%
  select(!contains('articulation')) %>%
  .[ , which(.[1,] == 'kN')] %>%
  set_df(.) %>%
  set_colnames(., 'CutterHeadForce')

cutter_torque <- data %>%
  select(contains('cutter')) %>%
  select(contains('torque')) %>% 
  .[ , which(.[1,] == 'kN-m')] %>%
  set_df(.) %>%
  set_colnames(., 'CutterTorque')

cutter_rot <- data %>%
  select(contains('cutter')) %>%
  select(contains('speed')) %>%
  # .[ , which(.[1,] == 'rpm')] %>%
  set_df(.) %>%
  set_colnames(., 'CutterRotSpeed')  

df_cutter <- cbind(cutter_torque, cutter_rot, cutter_force) %>%
  replace_zero_df(.)

# ------------------------------------------------------------------------------
# ADVANCE THRUST
# ------------------------------------------------------------------------------

advance_speed <- data %>%
  select(contains('speed')) %>% 
  select(contains('average')) %>%
  .[ , which(.[1,] == 'mm/min')] %>%
  set_df(.) %>%
  set_colnames(., 'AdvanceSpeed')

thrust_force <- data %>%
  select(contains('thrust')) %>%
  select(contains('force')) %>%
  select(!contains('articulation')) %>%
  .[ , which(.[1,] == 'kN')] %>%
  set_df(.) %>%
  set_colnames(., 'ThrustForce')

thrust_stroke <- data %>%
  select(contains('net stroke')) %>%
  .[ , which(.[1,] == 'mm')] %>%
  .[ ,1] %>% # remove redundant column  
  set_df(.) %>%
  set_colnames(., 'ThrustStroke')

# penet_rate <- round(advance_speed / cutter_rot, 2) %>%
#   set_colnames(., 'PenetrationRate') # mm/rot

df_thrust <- cbind(advance_speed, thrust_force, thrust_stroke) %>%
  replace_zero_df(.)

# ------------------------------------------------------------------------------
# SHIELD ATTITUDE
# ------------------------------------------------------------------------------

attitude_pitch <- data %>%
  select(contains('pitch')) %>% 
  .[ , which(.[1,] == 'deg')] %>%
  .[, 1:2] %>%
  set_df(.) %>%
  set_colnames(., c('PitchFront', 'PitchRear'))

attitude_roll <- data %>%
  select(contains('roll')) %>% 
  .[ , which(.[1,] == 'deg')] %>%
  .[, 1:2] %>%
  set_df(.) %>%
  set_colnames(., c('RollFront', 'RollRear'))

attitude_yaw <- data %>%
  select(contains('bear')) %>% 
  .[ , which(.[1,] == 'deg')] %>%
  .[, 3:4] %>%
  set_df(.) %>%
  set_colnames(., c('YawFront', 'YawRear'))

df_attitude <- cbind(attitude_roll)#,attitude_pitch, attitude_yaw)

# ------------------------------------------------------------------------------
# CHAMBER PRESSURE
# ------------------------------------------------------------------------------

earth_pres <- data %>%
  select(contains('earth')) %>%
  # select(contains('soil')) %>% 
  select(contains('pres')) %>% 
  select(contains('average')) %>%
  .[ , which(.[1,] == 'bar')] %>%
  .[ ,1:3] %>% # remove redundant column  
  set_df(.) %>%
  set_colnames(., c('EPUpper', 
                    'EPMiddle', 
                    'EPLower'))

earth_pres['ChamberBulkDensity'] <- (earth_pres['EPLower'] - earth_pres['EPUpper']) * 100 / 17

df_chamber <- earth_pres['ChamberBulkDensity']

# earth_pres_ave <- data %>%
#   # select(contains('earth')) %>%
#   select(contains('e-pres')) %>%
#   .[ , which(.[1,] == 'MPa')] %>%
#   set_df(.) %>%
#   rowMeans(., na.rm = F) %>% as_tibble(.) %>%
#   set_colnames(., 'EarthPresAve')

# ------------------------------------------------------------------------------
# SCREW 
# ------------------------------------------------------------------------------
  
screw_pres <- data %>%
  select(contains('screw')) %>%
  select(contains('pres')) %>%
  select(!contains('casing')) %>%
  select(!contains('injection')) %>%
  .[ , which(.[1,] == 'bar')] %>%
  set_df(.) %>%
  set_colnames(., c('ScrewPres1A',
                    'ScrewPres1B',
                    'ScrewPres2A',
                    'ScrewPres2B'))

# screw_pres['ScrewPres'] <- rowSums(screw_pres, na.rm = T)

screw_rot <- data %>%
  select(contains('screw')) %>% 
  select(contains('rev')) %>%
  # .[ , which(.[1,] == 'rpm')] %>%
  set_df(.) %>%
  set_colnames(., c('ScrewRotSpeed1', 'ScrewRotSpeed2'))

# screw_rot['ScrewRot'] <- rowSums(screw_rot, na.rm = T)

screw_torque <- data %>%
  select(contains('screw')) %>%
  select(contains('torque')) %>%
  .[ , which(.[1,] == 'kN-m')] %>%
  set_df(.) %>%
  set_colnames(., 'ScrewTorque')

screw_muck_vol <- data %>%
  select(contains('screw')) %>%
  select(contains('muck vol')) %>%
  select(contains('ring')) %>%
  .[ , which(.[1,] == 'm3')] %>%
  set_df(.) %>%
  set_colnames(., 'ScrewMuckVol')

screw_muck_flow <- data %>%
  select(contains('screw')) %>%
  select(contains('muck flow')) %>%
  .[ , which(.[1,] == 'm3/min')] %>%
  set_df(.) %>%
  set_colnames(., 'ScrewMuckFlow')

df_screw <- cbind(screw_pres, screw_torque, screw_rot,
                  screw_muck_vol)#, screw_muck_flow)

# ------------------------------------------------------------------------------
# CONVEYOR BELT MUCK
# ------------------------------------------------------------------------------

belt_muck_weight <- data %>%
    select(contains('belt')) %>%
    # select(contains('muck')) %>%
    select(contains('weight')) %>%
    select(contains('ring')) %>%
    .[ , which(.[1,] == 'ton')] %>%
    .[, 1:2] %>%
    set_df(.) %>%
    set_colnames(., c('BeltMuckWeightFront', 'BeltMuckWeightRear'))

belt_muck_vol <- data %>%
  select(contains('belt')) %>%
  # select(contains('muck')) %>%
  select(contains('vol')) %>%
  select(contains('ring')) %>%
  .[ , which(.[1,] == 'm3')] %>%
  .[ , 1:2] %>%
  set_df(.) %>%
  set_colnames(., c('BeltMuckVolFront', 'BeltMuckVolRear'))

df_muck <- cbind(belt_muck_weight, belt_muck_vol) 

# ------------------------------------------------------------------------------
# TAIL GROUTING 
# ------------------------------------------------------------------------------

grout_pres <- data %>%
  select(contains('grout')) %>%
  select(contains('press')) %>%
  .[ , which(.[1,] == 'Bar')] %>%
  set_df(.) %>%
  set_colnames(., 'GroutPres')

grout_vol <- data %>%
  select(contains('grout')) %>%
  select(contains('vol')) %>%
  select(contains('ring')) %>%
  .[ , which(.[1,] == 'm3')] %>%
  set_df(.) %>%
  set_colnames(., 'GroutVol')

grout_flow <- data %>%
  select(contains('grout')) %>% 
  select(contains('flow')) %>%
  select(!contains( c('primary', 'secondary'))) %>%
  .[ , which(.[1,] == 'L/min')] %>%
  set_df(.) %>%
  set_colnames(., 'GroutFlow')
  
grout_fill_rate <- data %>%
  select(contains('grout')) %>%
  .[ , which(.[1,] == '%')] %>%
  set_df(.) %>%
  set_colnames(., 'GroutFillRate')

df_grout <- cbind(grout_vol, grout_pres) #, grout_flow, grout_fill_rate)

# ------------------------------------------------------------------------------
# FOAM
# ------------------------------------------------------------------------------

# note: seems there are 32 foam injection valves

foam_pres <- data %>%
  select(contains('foam')) %>%
  select(contains('injection')) %>%
  select(contains('pres')) %>%
  .[ , which(.[1,] == 'bar')] %>%
  set_df(.) %>%
  rowSums(., na.rm = T) %>% as_tibble(.) %>%
  set_colnames(., 'FoamPres')

foam_vol <- data %>%
  select(contains('foam vol')) %>%
  select(contains('ring')) %>%
  .[ , which(.[1,] == 'm3')] %>%
  set_df(.) %>%
  set_colnames(., 'FoamVol')

foam_flow <- data %>%
  select(contains('foam total flow')) %>%
  select(!contains(c('liquid', 'agent'))) %>%
  .[ , which(.[1,] == 'L/min')] %>%
  .[ , 1] %>%
  set_df(.) %>%
  set_colnames(., 'FoamFlow')

foam_fill_rate <- data %>%
  select(contains('foam')) %>%
  select(contains('fill rate')) %>%
  .[ , which(.[1,] == '%')] %>%
  set_df(.) %>%
  set_colnames(., 'FoamFillRate')

# ------------------------------------------------------------------------------

foam_liquid_pres <- data %>%
  select(contains('foam liquid')) %>%
  select(contains('press')) %>%
  .[ , which(.[1,] == 'bar')] %>%
  set_df(.) %>%
  rowSums(., na.rm = T) %>% as_tibble(.) %>%
  set_colnames(., 'FoamLiquidPres')

foam_liquid_vol <- data %>%
  select(contains('total foam liquid vol')) %>%
  .[ , which(.[1,] == 'm3')] %>%
  set_df(.) %>%
  set_colnames(., 'FoamLiquidVol')

foam_liquid_flow <- data %>%
  select(contains('foam liquid flow')) %>%
  .[ , which(.[1,] == 'L/min')] %>%
  set_df(.) %>%
  rowSums(., na.rm = T) %>% as_tibble(.) %>%
  set_colnames(., 'FoamLiquidFlow')

# ------------------------------------------------------------------------------

foam_agent_vol <- data %>%
  select(contains('foam agent')) %>%
  select(contains('vol')) %>%
  select(!contains('flow')) %>%
  # seems equivalent to select(contains('flow')) %>%
  .[ , which(.[1,] == 'm3')] %>%
  set_df(.) %>%
  rowSums(., na.rm = T) %>% as_tibble(.) %>%
  set_colnames(., 'FoamAgentVol')

foam_agent_flow <- data %>%
  select(contains('foam agent')) %>% 
  select(contains('flow')) %>%
  .[ , which(.[1,] == 'L/min')] %>%
  set_df(.) %>%
  rowSums(., na.rm = T) %>% as_tibble(.) %>%
  set_colnames(., 'FoamAgentFlow')

# ------------------------------------------------------------------------------

air_pres <- data %>%
  select(contains('air')) %>%
  select(contains('pres')) %>%
  .[ , which(.[1,] == 'bar')] %>%
  set_df(.) %>%
  rowSums(., na.rm = T) %>% as_tibble(.) %>%
  set_colnames(., 'AirPres')

air_vol <- data %>%
  select(contains('air total')) %>%
  select(contains('vol')) %>%
  .[ , which(.[1,] == 'm3')] %>%
  set_df(.) %>%
  set_colnames(., 'AirVol')

air_flow <- data %>%
  select(contains('air')) %>% 
  select(contains('flow total')) %>%
  .[ , which(.[1,] == 'L/min')] %>%
  set_df(.) %>%
  set_colnames(., 'AirFlow')

df_foam <- cbind(foam_pres, foam_vol, foam_flow, #foam_fill_rate,
                 foam_liquid_pres, foam_liquid_vol, foam_liquid_flow,
                 foam_agent_vol, foam_agent_flow) #,
                 # air_pres, air_vol, air_flow)

# ------------------------------------------------------------------------------
# POLYMER
# ------------------------------------------------------------------------------

polymer_pres <- data %>%
  select(contains('polymer injection pres')) %>%
  .[ , which(.[1,] == 'bar')] %>%
  set_df(.) %>%
  rowSums(., na.rm = T) %>% as_tibble(.) %>%
  set_colnames(., 'PolymerPres')  
  
polymer_vol <- data %>%
  select(contains('total polymer vol')) %>%
  select(contains('ring')) %>%
  .[ , which(.[1,] == 'm3')] %>%
  set_df(.) %>%
  set_colnames(., 'PolymerVol')

polymer_flow <- data %>%
  select(contains('total polymer')) %>% 
  select(contains('flow')) %>%
  .[ , which(.[1,] == 'L/min')] %>%
  set_df(.) %>%
  set_colnames(., 'PolymerFlow')

# ------------------------------------------------------------------------------
polymer_agent_vol <- data %>%
  select(contains('polymer agent')) %>%
  select(contains('vol')) %>%
  select(!contains('flow')) %>%
  .[ , which(.[1,] == 'm3')] %>%
  set_df(.) %>%
  rowSums(., na.rm = T) %>% as_tibble(.) %>%
  set_colnames(., 'PolymerAgentVol')

polymer_agent_flow <- data %>%
  select(contains('polymer agent')) %>% 
  select(contains('flow')) %>%
  .[ , which(.[1,] == 'L/min')] %>%
  set_df(.) %>%
  rowSums(., na.rm = T) %>% as_tibble(.) %>%
  set_colnames(., 'PolymerAgentFlow')

df_polymer <- cbind(polymer_pres, polymer_vol, polymer_flow,
                    polymer_agent_vol, polymer_agent_flow) 

# ------------------------------------------------------------------------------

# additive_vol <- data %>%
#   select(contains('additive')) %>%
#   select(contains('vol')) %>%
#   select(contains('ring')) %>%
#   select(!contains('no.')) %>%
#   .[ , which(.[1,] == 'm3')] %>%
#   set_df(.) %>%
#   set_colnames(., 'AdditiveVol')
# 
# additive_fill_rate <- data %>%
#   select(contains('additive')) %>%
#   .[ , which(.[1,] == '%')] %>%
#   set_df(.) %>%
#   set_colnames(., 'AdditiveFillRate')

# ------------------------------------------------------------------------------
# PREPARE DATA FRAME
# ------------------------------------------------------------------------------

# merge df
df <- cbind(df_cutter,
            df_thrust,
            df_attitude,
            df_chamber,
            df_foam,
            df_polymer,
            df_screw, 
            df_muck,
            df_grout) 

# normalize volume to net stroke (m3/m)
colnames <- df %>% select(contains('vol')) %>% names(.)
for (colname in colnames) {
  df[colname] <- df[colname] / (df['ThrustStroke'] * 0.001)
}

# clean df
# df <- df %>% replace_outliers_df(.) #%>%


# add ring and chainage
df <- cbind(df, 
            Chainage = chainage$ChainageHead,
            Ring = ring$Ring) 

# remove rows with duplicated chainage
n_occur <- data.frame(table(df$Chainage))
duplic <- n_occur[n_occur$Freq > 1,]$Var1
df <- df %>% filter(Chainage %!in% duplic)

# ------------------------------------------------------------------------------
# SET LABEL
# ------------------------------------------------------------------------------

# geologic segment
geo_label <- read_csv('../../../../2-data/seattle/seattle-geo.csv')

# soil label
for (i in 1:nrow(df)) {
  for (j in 1:nrow(geo_label)) {
    if (df$Chainage[i] > geo_label$ChainageStart[j] &
        df$Chainage[i] <= geo_label$ChainageEnd[j]) {
      df$SoilLabel[i] <- geo_label$SoilLabel[j]
    }
  }
  if (is.null(df$SoilLabel[i])) {
    df$SoilLabel[i] <- 'Unlabeled'
  }
}

# geologic segment
for (i in 1:nrow(df)) {
  for (j in 1:nrow(geo_label)) {
    if (df$Chainage[i] > geo_label$ChainageStart[j] &
        df$Chainage[i] <= geo_label$ChainageEnd[j]) {
      df$GeologicSegment[i] <- geo_label$GeologicSegment[j]
    }
  }
  if (is.null(df$GeologicSegment[i])) {
    df$GeologicSegment[i] <- 'Unlabeled'
  }
}

# ------------------------------------------------------------------------------
# SAVE DF
# ------------------------------------------------------------------------------

# drop rows with missing values
rf_df <- drop_na(df)

# save as csv
write.csv(rf_df,'../results/rf_df.csv', row.names = FALSE)

