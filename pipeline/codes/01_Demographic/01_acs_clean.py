import pandas as pd
import os

n_drive = "N:/Transfer/KSkvoretz/AHRQ/data/01_Demographic/cleaned"

acs = pd.read_csv(os.path.join(n_drive, "ACS_cleaned.csv"))

# columns to keep