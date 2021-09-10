# CFB QBR Downloader
# By: Akshay Easwaran <akeaswaran@me.com>
# 
# Big thanks to @nntrn for the gist for NFL QBR data: https://gist.github.com/nntrn/ee26cb2a0716de0947a0a4e9a157bc1c#athletes


import requests
import pandas as pd
import os

base_lang_suffix = "?lang=en&region=us"

def generate_qbr_url(year, week):
    return f"http://sports.core.api.espn.com/v2/sports/football/leagues/college-football/seasons/{year}/types/2/weeks/{week}/qbr/10000"

def generate_athlete_prefix(year):
    return f"http://sports.core.api.espn.com/v2/sports/football/leagues/college-football/seasons/{year}/athletes/"

def generate_team_prefix(year):
    return f"http://sports.core.api.espn.com/v2/sports/football/leagues/college-football/seasons/{year}/teams/"

event_prefix = "http://sports.core.api.espn.com/v2/sports/football/leagues/college-football/events/"

athlete_name_map = dict()
def retrieve_athlete_name(year, athleteId):
    if (athleteId in athlete_name_map.keys()):
        print(f"found cached name for athlete {athleteId}")
        return athlete_name_map[athleteId]
    
    print(f"no cached name for athlete {athleteId}, polling espn")
    url = f"https://sports.core.api.espn.com/v2/sports/football/leagues/college-football/seasons/{year}/athletes/{athleteId}"
    r = requests.get(url)
    data = r.json()
    name = data["fullName"]
    athlete_name_map[athleteId] = name
    return name

team_map = dict()
def retrieve_team_name(year, teamId):
    if (teamId in team_map.keys()):
        print(f"found cached name for team {teamId}")
        return team_map[teamId]
    
    print(f"no cached name for team {teamId}, polling espn")
    url = f"http://sports.core.api.espn.com/v2/sports/football/leagues/college-football/seasons/{year}/teams/{teamId}?lang=en&region=us"
    r = requests.get(url)
    data = r.json()
    name = data["abbreviation"]
    team_map[teamId] = name
    return name

def process_qbr_json(json, year, week):
    if ("items" not in json.keys()):
        return []
    
    qbr_records = json["items"]
    for record in qbr_records:
        record["athlete_id"] = record["athlete"]["$ref"].replace(generate_athlete_prefix(year), "").replace(base_lang_suffix, "")
        record["athlete_name"] = retrieve_athlete_name(year, record["athlete_id"])
        
        record["team_id"] = record["team"]["$ref"].replace(generate_team_prefix(year), "").replace(base_lang_suffix, "")
        record["team_abbreviation"] = retrieve_team_name(year, record["team_id"])

        record["game_id"] = record["event"]["$ref"].replace(event_prefix, "").replace(base_lang_suffix, "")
        
        record["year"] = year
        record["week"] = week

        actual_qbr_pieces = record["splits"]["categories"][0]["stats"]
        for stat in actual_qbr_pieces:
            record[stat["abbreviation"]] = stat["value"]
        
        del record["athlete"]
        del record["team"]
        del record["event"]
        del record["season"]
        del record["splits"]
    return qbr_records

def write(df, path):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    df.to_csv(path, index=False)

parent_qbr_df = pd.DataFrame()
for yr in range(2004, 2021):
    for wk in range(1, 16):
        print(f"Looking for QBR data for year {yr}, week {wk}")
        url = generate_qbr_url(yr, wk)
        r = requests.get(url)
        data = r.json()
        if (data != None and len(data) != 0):
            print(f"Found QBR data for year {yr}, week {wk}, parsing")
            qbr_dict = process_qbr_json(data, yr, wk)
            qbr_df = pd.DataFrame(qbr_dict)
            # write(qbr_df, f"qbr/{yr}/{wk}.csv")
            parent_qbr_df = parent_qbr_df.append(qbr_df)
            # save it
            # print(qbr_dict)
        else:
            print(f"QBR Data not found for year {yr} and week {wk}")
    print(f"Done with year {yr}")

print(f"Writing composite file with {len(parent_qbr_df)} records...")
write(parent_qbr_df, "./composite.csv")
print(f"Done writing composite file with {len(parent_qbr_df)} records.")