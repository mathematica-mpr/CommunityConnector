import pandas as pd

final_dict = pd.read_csv('data/inter_data_dictionary.csv')

final_dict.to_csv('data/final_data_dictionary.csv', index = False)