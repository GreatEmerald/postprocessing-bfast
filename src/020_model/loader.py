import os
import torch
import pandas
import geopandas
import re
#import numpy as np
#import matplotlib.pyplot as plt
from torch.utils.data import Dataset, DataLoader

class IIASAChangeDataset(Dataset):
    """IIASA 2015-2018 land cover change + Landsat 8 time series dataset."""

    def __init__(self, change_reference_file, timeseries_file,
                 bands=["Global_SR_B1", "Global_SR_B2", "Global_SR_B3", "Global_SR_B4",
                        "Global_SR_B5", "Global_SR_B6", "Global_SR_B7", "Global_ST_B10"],
                 classes=["bare", "burnt", "crops", "fallow_shifting_cultivation",
                          "grassland", "lichen_and_moss", "not_sure", "shrub",
                          "snow_and_ice", "tree", "urban_built_up", "water", "wetland_herbaceous"]
                ):
        """
        Args:
            change_reference_file (string): CSV file that includes multiple years of observations of land cover change.
            timeseries_file (string): GeoPackage that includes Landsat 8 time series data.
        """
        self.change_reference = pandas.read_csv(change_reference_file)
        self.timeseries = [geopandas.read_file(timeseries_file, layer=band) for band in bands]
        self.bands = bands
        self.classes = classes
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

        sample = {'timeseries': ts_df, 'change': fraction_change, 'year': this_year}

        return sample

MyData = IIASAChangeDataset(change_reference_file = "../../data/raw/Data_Global_quoted.csv",
                            timeseries_file = "../../data/raw/IIASAChange20152018_Landsat8_TS.gpkg")
MyData[100]["timeseries"].plot()
MyData[100]["change"]
