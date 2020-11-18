import xarray as xr
ds = xr.open_dataset('../../test/testoutput/owp_snow_obs.nc')
df = ds.to_dataframe().rename(columns={
    "snow_depth@ObsValue": "ObsValue",
    "snow_depth@ObsError": "ObsError",
    "snow_depth@PreQC": "PreQC",
    "time@MetaData": "time",
    "latitude@MetaData": "latitude",
    "longitude@MetaData": "longitude",
    "datetime@MetaData": "datetime",
    "variable_names@VarMetaData": "variable_name"})
df['datetime'] = df['datetime'].str.decode("utf-8")
df['variable_name'] = df['variable_name'].str.decode("utf-8")
df = df.drop('time', axis='columns')
df.to_csv('../../test/testinput/owp_snow_obs_prelim.csv', index=False)
