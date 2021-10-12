import os
import torch
import pandas
import geopandas
import re
import math
import datetime
#import numpy as np
#import matplotlib.pyplot as plt
from torch.utils.data import Dataset, DataLoader, random_split

class IIASAChangeDataset(Dataset):
    """IIASA 2015-2018 land cover change + Landsat 8 time series dataset."""

    def __init__(self, change_reference_file, timeseries_file,
                 bands=["Global_SR_B1", "Global_SR_B2", "Global_SR_B3", "Global_SR_B4",
                        "Global_SR_B5", "Global_SR_B6", "Global_SR_B7"],
                 classes=["bare", "burnt", "crops", "fallow_shifting_cultivation",
                          "grassland", "lichen_and_moss", "not_sure", "shrub",
                          "snow_and_ice", "tree", "urban_built_up", "water", "wetland_herbaceous"],
                 torch=True
                ):
        """
        Args:
            change_reference_file (string): CSV file that includes multiple years of observations of land cover change.
            timeseries_file (string): GeoPackage that includes Landsat 8 time series data.
            bands (list of strings): which bands to load.
            classes (list of strings): which classes to load.
            torch (bool): whether the output is for PyTorch implementation (True) or for Python BFAST Monitor (False).
        """
        self.change_reference = pandas.read_csv(change_reference_file)
        self.timeseries = [geopandas.read_file(timeseries_file, layer=band) for band in bands]
        self.bands = bands
        self.classes = classes
        self.torch = torch
        self.change_ref_end_years = self.change_reference.loc[self.change_reference["reference_year"] > 2015] # Exclude the first year, i.e. 2015
        
        # Get dates by parsing column names
        dates = []
        for column in self.timeseries[0].columns:
            result = re.search('X[0-9][0-9][0-9][0-9]\.[0-9][0-9]\.[0-9][0-9]', column)
            if result is not None:
                dates.append(result[0])
        self.dates = pandas.to_datetime(dates, format="X%Y.%m.%d")

    def __len__(self):
        return len(self.change_ref_end_years)

    # idx is an index of the change reference, each year is separate for BFAST Monitor
    def __getitem__(self, idx):
        if torch.is_tensor(idx):
            idx = idx.tolist()
            
        sample_id = self.change_ref_end_years["sample_id"].iloc[idx]
        this_year = self.change_ref_end_years["reference_year"].iloc[idx]
        fractions_this_year = self.change_ref_end_years[self.classes].iloc[idx]
        all_year_data = self.change_reference.loc[self.change_reference["sample_id"] == sample_id]
        fractions_last_year = all_year_data.loc[all_year_data["reference_year"] == (this_year - 1), self.classes].iloc[0]
        
        fraction_change = fractions_this_year - fractions_last_year
        
        # Make change a boolean based on whether more than 50% changed or not
        boolean_change = sum(abs(fraction_change)) > 50*2
        
        # Select the time series (all bands) of the right sample
        ts_list = []
        for band_idx in range(0, len(self.bands)):
            ts = self.timeseries[band_idx].loc[self.timeseries[band_idx]["sample_id"] ==
                                               str(sample_id)].filter(regex='X[0-9][0-9][0-9][0-9]\.[0-9][0-9]\.[0-9][0-9]').iloc[0]
            # Some time series are a bit longer, remove the last entry
            ts = ts[0:(len(self.dates))]
            ts.index = self.dates
            ts.name = self.bands[band_idx]
            ts_list.append(ts)
        ts_df = pandas.concat(ts_list, axis=1)
        
        
        if self.torch:
            # Cut the time series to the year of interest
            ts_df = ts_df.loc[str(int(this_year))]
            
            # Convert everything into tensors
            ts_tensor = torch.as_tensor(ts_df.to_numpy())
            boolean_change = torch.tensor(boolean_change)
        else:
            # Cut the time series off at the end of the monitoring period (mind the ":")
            ts_df = ts_df.loc[:str(int(this_year))]
            # Output dates: the monitor period start as well as all timestamps
            this_year = (datetime.datetime(int(this_year), 1, 1, 0, 0), list(ts_df.index.to_pydatetime()))
            # Calculate NDVI
            ndvi = ((ts_df["Global_SR_B5"] - ts_df["Global_SR_B4"]) / (ts_df["Global_SR_B5"] + ts_df["Global_SR_B4"]))
            # Format output as an ndarray of dimensions length, width, 1
            ts_tensor = ndvi.to_numpy().reshape(-1, 1, 1)

        sample = {'timeseries': ts_tensor, 'change': boolean_change, 'year': this_year, 'id': sample_id}

        return sample

# Test individual time series
MyData = IIASAChangeDataset(change_reference_file = "../../data/raw/Data_Global_quoted.csv",
                            timeseries_file = "../../data/raw/IIASAChange20152018_Landsat8_TS.gpkg")
MyData[442]["timeseries"] # Transpose or not?
MyData[442]["timeseries"][:,1]
MyData[442]["change"]
MyData[442]["year"]
MyData[442]["id"]

# Split into 30-70%

Holdout = random_split(MyData, [math.floor(len(MyData)*0.7), math.ceil(len(MyData)*0.3)])

train_dataloader = DataLoader(Holdout[0], batch_size=64, shuffle=True)
test_dataloader = DataLoader(Holdout[1], batch_size=64, shuffle=True)

next(iter(train_dataloader))
