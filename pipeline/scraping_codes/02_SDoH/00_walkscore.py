#import packages
from bs4 import BeautifulSoup
import os
import requests
import json
import re
import pandas as pd

#FIND THE codes
file_path = 'data/raw/'

# location files for mapping from zip code --> FIPS code
uszips_path = file_path + 'uszips.csv'
ZIP_TRACT_092019_path = file_path + 'ZIP_TRACT_092019.csv'
ZIP_COUNTY_FIPS_path = file_path + 'ZIP-COUNTY-FIPS_2017-06.csv'

#reading in Data
#uszip_df read in selecting columns
uszips_df = pd.read_csv(uszips_path)[['city','zip']]

#ZIP_TRACT_092019_df read in and selecting columns
ZIP_TRACT_092019_df = pd.read_csv(ZIP_TRACT_092019_path)[['zip','tract']]
# add leading zero to tract
ZIP_TRACT_092019_df['tract'] = "0" + ZIP_TRACT_092019_df['tract'].apply(str)

#ZIP_COUNTY_FIPS load data and select
ZIP_COUNTY_FIPS_df = pd.read_csv(ZIP_COUNTY_FIPS_path)[['ZIP','COUNTYNAME','STATE','STCOUNTYFP']]
#lowercase the column names
ZIP_COUNTY_FIPS_df.columns = map(str.lower, ZIP_COUNTY_FIPS_df.columns)
#selecting state == CO
ZIP_COUNTY_FIPS_df = ZIP_COUNTY_FIPS_df.query('state == "CO"')

#ACS - using this to get population to find largest city within a fips code
def api_pull(variable):
    url = f'https://api.census.gov/data/2017/acs/acs5?key={key}&get=NAME,group({variable})&for=tract:*&in=state:08'
    response = requests.get(url)
    formattedResponse = json.loads(response.text)
    data = pd.DataFrame(formattedResponse)
    return data

key = 'e2675475712b1694b49a734a05e1ece105f45be4'
variable = 'B01003'
co = api_pull(variable)
co = pd.DataFrame(co[1:])              
co.columns = ["NAME","GEO_ID","B01003_001E","B01003_001M","NAME_2","B01003_001MA","B01003_001EA","state","county","tract"]

#selcting and mutating columns
co['tract'] = co['state'].astype(str)+ co['county'].astype(str)+ co['tract'].astype(str)
census_pop_df = co[['tract','B01003_001E', "NAME"]]
census_pop_df.columns = ["tract","population","NAME"]

#MERGE
tract_county = pd.merge(ZIP_COUNTY_FIPS_df, ZIP_TRACT_092019_df, on='zip')
tract_county['tract']= tract_county['tract'].astype(str) 
tract_county_pop = pd.merge(tract_county, census_pop_df, on = 'tract')

#FIND LARGEST ZIP
largest_tract = tract_county_pop.sort_values('population', ascending=False).drop_duplicates(['stcountyfp'])
largest_tract = pd.merge(largest_tract, uszips_df, on = 'zip')

#Creating URLS to scrape from for each city
fips_urls_df = largest_tract [['stcountyfp', 'city']]
fips_urls_df.columns = ['fips', 'city']
fips_urls_df['urls'] = "https://www.walkscore.com/CO/"+ fips_urls_df['city']

#WEBSCRAPER 
#URLS come from the top

#OUTFILE
out_scores_path= "data/cleaned/02_SDoH/" + 'out_scores.csv'

#this is the dictionary
fips_scores = {}

def num_extraction (score_string, scr_full_text):
    '''extracts number from alt test string'''    
    if score_string in scr_full_text:
        score = re.findall('\d+', scr_full_text)
        return score[0]

def imagine_to_score_convert (url):
    '''takes url and outputs the three scores'''    
    #gets the website
    result = requests.get(url)
    #stores the content of website
    src = result.text
    #Souping the Website
    soup = BeautifulSoup (src, 'html.parser')
    
    #isolate image tags
    image_tags = soup.find_all('img')
    #setting default values
    walk_score = "notfound"
    trans_score = "notfound"
    bike_score = "notfound"

    for image_tag  in image_tags:
        scr_text = image_tag.get('src')
        if scr_text is not None:
            result = num_extraction ("//pp.walk.sc/badge/walk/score/", scr_text) 
            if result is not None:
                walk_score = result
            result = num_extraction ("//pp.walk.sc/badge/transit/score/", scr_text) 
            if result is not None:
                trans_score = result
            result = num_extraction ("//pp.walk.sc/badge/bike/score/", scr_text) 
            if result is not None:
                bike_score = result
    return walk_score, trans_score, bike_score

    #main
for row in fips_urls_df.iterrows():
    fips_code = row[1]["fips"]
    website = row[1]["urls"]
    walk_score, trans_score, bike_score = imagine_to_score_convert (website)
    fips_scores[fips_code] = {}
    fips_scores[fips_code]["walk_score"] = walk_score
    fips_scores[fips_code]["trans_score"] = trans_score
    fips_scores[fips_code]["bike_score"] = bike_score

#writing outfile
df = pd.DataFrame(fips_scores).T
df.sort_index(inplace = True)

df.to_csv(out_scores_path)