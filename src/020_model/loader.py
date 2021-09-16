import os
import torch
#import pandas as pd
import geopandas
#import numpy as np
#import matplotlib.pyplot as plt
from torch.utils.data import Dataset, DataLoader

class IIASAChangeDataset(Dataset):
    """IIASA 2015-2018 land cover change + Landsat 8 time series dataset."""

    def __init__(self, change_reference_file, timeseries_file,
                 bands=["Global_SR_B1", "Global_SR_B2", "Global_SR_B3", "Global_SR_B4",
                        "Global_SR_B5", "Global_SR_B6", "Global_SR_B7", "Global_ST_B10"]):
        """
        Args:
            change_reference_file (string): CSV file that includes multiple years of observations of land cover change.
            timeseries_file (string): GeoPackage that includes Landsat 8 time series data.
        """
        self.change_reference = pd.read_csv(change_reference_file)
        self.timeseries = [geopandas.read_file(timeseries_file, layer=band) for band in bands]
        # Make into an array

    def __len__(self):
        return len(self.change_reference)

    def __getitem__(self, idx):
        if torch.is_tensor(idx):
            idx = idx.tolist()

        img_name = os.path.join(self.root_dir,
                                self.landmarks_frame.iloc[idx, 0])
        image = io.imread(img_name)
        landmarks = self.landmarks_frame.iloc[idx, 1:]
        landmarks = np.array([landmarks])
        landmarks = landmarks.astype('float').reshape(-1, 2)
        sample = {'image': image, 'landmarks': landmarks}

        if self.transform:
            sample = self.transform(sample)

        return sample

change_reference_file = "../../data/raw/Data_Global_quoted.csv"
timeseries_file = "../../data/raw/IIASAChange20152018_Landsat8_TS.gpkg"
