# CFB QBR Downloader
# By: Akshay Easwaran <akeaswaran@me.com>
# 
# Big thanks to @nntrn for the gist for NFL QBR data: https://gist.github.com/nntrn/ee26cb2a0716de0947a0a4e9a157bc1c#athletes
# Also thanks to @jthomasmock for a better link https://github.com/jthomasmock/espnscrapeR/blob/master/R/get_nfl_qbr.R#L66


import requests
import pandas as pd
import os

base_lang_suffix = "?lang=en&region=us"

def generate_qbr_url(year, week, seasontype):
    return f"https://site.web.api.espn.com/apis/fitt/v3/sports/football/college-football/qbr?qbrType=weeks&season={year}&week={week}&seasontype={seasontype}&isqualified=true&limit=1000"

def generate_backup_qbr_url(year, week, seasontype):
    return f"https://sports.core.api.espn.com/v2/sports/football/leagues/college-football/seasons/{year}/types/{seasontype}/weeks/{week}/qbr/10000?limit=1000"

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
    url = f"https://sports.core.api.espn.com/v2/sports/football/leagues/college-football/seasons/{year}/athletes/{athleteId}?lang=en&region=us"
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
    url = f"https://sports.core.api.espn.com/v2/sports/football/leagues/college-football/seasons/{year}/teams/{teamId}?lang=en&region=us"
    r = requests.get(url)
    data = r.json()
    name = data["abbreviation"]
    team_map[teamId] = name
    return name

def process_qbr_json(json, year, week, seasontype):
    if ("athletes" not in json.keys()):
        return []
    
    qbr_records = json["athletes"]
    total_new_rec = []
    for record in qbr_records:
        new_rec = dict()
        new_rec["athlete_id"] = record["athlete"]["id"]
        new_rec["athlete_name"] = record["athlete"]["displayName"]

        # cache for later
        athlete_name_map[new_rec["athlete_id"]] = new_rec["athlete_name"]
        
        new_rec["team_id"] = record["athlete"]["teamId"]
        new_rec["team_abbreviation"] = record["athlete"]["teamShortName"]
        
        # cache for later
        team_map[new_rec["team_id"]] = new_rec["team_abbreviation"]

        new_rec["game_id"] = record["game"]["id"]
        
        new_rec["season"] = year
        new_rec["week"] = week
        new_rec["season_type"] = seasontype
        # new_rec["opponent"] = record["game"]["teamOpponent"]["abbreviation"]

        actual_qbr_pieces = record["categories"][0]["totals"]
        zipped_pieces = zip(json["categories"][0]["labels"], actual_qbr_pieces)
        for (stat, value) in zipped_pieces:
            new_rec[stat] = value
        
        total_new_rec.append(new_rec)
    return total_new_rec

def process_backup_qbr_json(json, year, week, seasontype):
    if ("items" not in json.keys()):
        return []
    
    qbr_records = json["items"]
    total_new_rec = []
    for record in qbr_records:
        new_rec = dict()
        new_rec["athlete_id"] = record["athlete"]["$ref"].replace(generate_athlete_prefix(year), "").replace(base_lang_suffix, "")
        new_rec["athlete_name"] = retrieve_athlete_name(year, new_rec["athlete_id"])

        new_rec["team_id"] = record["team"]["$ref"].replace(generate_team_prefix(year), "").replace(base_lang_suffix, "")
        new_rec["team_abbreviation"] = retrieve_team_name(year, new_rec["team_id"])

        new_rec["game_id"] = record["event"]["$ref"].replace(event_prefix, "").replace(base_lang_suffix, "")

        new_rec["season"] = year
        new_rec["week"] = week
        new_rec["season_type"] = seasontype

        actual_qbr_pieces = record["splits"]["categories"][0]["stats"]
        for stat in actual_qbr_pieces:
            new_rec[stat["abbreviation"]] = stat["value"]
        
        total_new_rec.append(new_rec)
    return total_new_rec

def use_backup_qbr_process(year, week, seasontype):
    url = generate_backup_qbr_url(year, week, seasontype)
    r = requests.get(url)
    data = r.json()
    return process_backup_qbr_json(data, year, week, seasontype)

def write(df, path):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    df.to_csv(path, index=False)

parent_qbr_df = pd.DataFrame()
for yr in range(2004, 2022):
    for wk in range(-1, 16):
        if (wk == 0):
            continue

        seasontype = 3 if (wk == -1) else 2
        adjWk = 1 if (wk == -1) else wk
        print(f"Looking for QBR data for year {yr}, week {adjWk}, seasontype {seasontype}")
        url = generate_qbr_url(yr, adjWk, seasontype)
        r = requests.get(url)
        data = r.json()
        if (data != None and len(data) != 0):
            if ("athletes" not in data.keys()):
                print(f"Found did not find data for year {yr}, week {adjWk}, seasontype {seasontype} in normal URL, using backup")
                qbr_rec = use_backup_qbr_process(yr, adjWk, seasontype)
            else:        
                print(f"Found QBR data for year {yr}, week {adjWk}, seasontype {seasontype}, parsing")
                qbr_rec = process_qbr_json(data, yr, adjWk, seasontype)

            if (len(qbr_rec) > 0):
                print(f"cleaned {len(qbr_rec)} records for {yr}, week {adjWk}, seasontype {seasontype}, adding to composite")
                qbr_df = pd.DataFrame(qbr_rec)
                parent_qbr_df = parent_qbr_df.append(qbr_df, sort=False)
            else:
                print(f"No records for {yr}, week {wk}, seasontype {seasontype}, skipping")
        else:
            print(f"QBR Data not found for year {yr} and week {wk}, seasontype {seasontype}")
    print(f"Done with year {yr}")

print(f"Writing composite file with {len(parent_qbr_df)} records...")
parent_qbr_df.dropna(axis=1, inplace=True)
write(parent_qbr_df, "./composite_qbr.csv")
print(f"Done writing composite file with {len(parent_qbr_df)} records.")
