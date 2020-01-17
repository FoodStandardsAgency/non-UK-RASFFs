# Non-UK RASFFs model construction.
source('helpers.r')

# Load the data.
df_data_raw <- load_rasffs(columns='*', table='rasff_data')
df_countries_raw <- load_rasffs(columns='*', table='rasff_countries')
df_details_raw <- load_rasffs(columns='*', table='rasff_details')
df_hazards_raw <- load_rasffs(columns='*', table='rasff_hazards')