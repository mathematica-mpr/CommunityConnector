import pandas as pd

prev = pd.read_csv(os.path.join(output, "cms_prev_data.csv"))
spend = pd.read_csv(os.path.join(output, "cms_spend_data.csv"))

print(prev.head())
print(spend.head())