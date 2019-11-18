import pandas as pd 
pd.set_option('display.max_colwidth', -1)
pd.set_option('display.max_rows', -1)
pd.set_option('display.max_columns', 30)
from sodapy import Socrata

# Dialysis Facility Compare

client = Socrata("data.medicare.gov",
                 "WzCiIEqAIN57WaGqsXaeGyZmw")
# TODO: make sure rows > limit
dial_fac = client.get("23ew-n7w9", limit=10000)
dial_fac_df = pd.DataFrame.from_records(dial_fac)
print(dial_fac_df.shape)

# limit to colorado
dial_fac_df = dial_fac_df[dial_fac_df['state'] == "CO"]
dial_fac_df = dial_fac_df[['provider_number','facility_name','five_star','address_line_1','city','state','zip','county','phone_number','profit_or_non_profit_','chain_owned','chain_organization',
'late_shift_','_of_dialysis_stations','offers_in_center_hemodialysis','offers_in_center_peritoneal_dialysis','offers_home_hemodialysis_training',
'mortality_rate_facility','readmission_rate_facility','standardized_hospitalization_ratio']]

dial_fac_df.to_csv('data/locations_dialysis_facility.csv', index = False)

# Physician_Compare_National_Downloadable_File - limit to Primary specialty == Nephrology
# Line 1 Street address
# limit to colorado
client = Socrata("data.medicare.gov", "WzCiIEqAIN57WaGqsXaeGyZmw")
phys = client.get("mj5m-pzi6", limit=3000000)
phys_df = pd.DataFrame.from_records(phys)
print(phys_df.shape)
phys_df = phys_df[phys_df['st'] == "CO"]
phys_df = phys_df[phys_df['pri_spec'] == 'NEPHROLOGY']
phys_df.drop(['num_org_mem','assgn','ln_2_sprs','suff'], axis = 1, inplace = True)
print(phys_df.shape)

phys_df.to_csv('data/locations_physicians.csv', index = False)