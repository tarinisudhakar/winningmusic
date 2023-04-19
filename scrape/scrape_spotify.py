import spotipy 
from spotipy.oauth2 import SpotifyClientCredentials
import csv
import os
import pandas as pd
import numpy as np 
from collections import OrderedDict


INPUT_DIR = os.path.join("scrape", "billboard_top_songs.csv")

#Authentication - without user
cid = "ENTER HERE"
secret = "ENTER HERE"

client_credentials_manager = SpotifyClientCredentials(client_id=cid, client_secret=secret)
sp = spotipy.Spotify(client_credentials_manager = client_credentials_manager)

#Import tracks from .csv
bbcsv = pd.read_csv(INPUT_DIR)
df2 = bbcsv[['title', 'artist']].copy()

df2.replace('\'','', regex=True, inplace=True)
df2.replace('\"','', regex=True, inplace=True)
#df2.replace('','', regex=True, inplace=True)
#df2.replace('[^A-Za-z\s]','', regex=True, inplace=True)

sample = df2.head(1500)

#Get Track ID for single case in case of errors
artist = sample['artist'][22]
track = sample['title'][22]
track_id = sp.search(q='artist:' + artist + ' track:' + track, type='track', market = 'US')
features_to_df = np.array(track_features)[0]
features_to_df = OrderedDict(features_to_df)
feature_values = features_to_df.values()


#Function for all data
def get_spotify_features(track, artist):
    #Search for Spofity song ID 
    songs=sp.search(q='track:'+track+' '+'artist:'+artist+'*' , type='track', market = 'US')
    items = songs['tracks']['items']
    if len(items) ==0:
        return 'None'
    else:
        track = items[0]
        song_id = str(track["id"])
        #Use ID to get Song features
        track_features=sp.audio_features(song_id)
        if len(track_features[0]) <18:
            return([0]*len(feature_values))
        else:
            features_to_df = np.array(track_features)[0]
            #Order Dictionary
            features_to_df = OrderedDict(features_to_df)
            #Get Dictionary values
            feature_values = features_to_df.values()
            return(feature_values)

dfx = sample.apply(lambda x: get_spotify_features(x['title'], x['artist']), axis=1)
df1 = pd.DataFrame(dfx.tolist())
df1.columns = ['danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness', 'instrumentalness', 
'liveness', 'valence', 'tempo', 'type', 'id', 'uri', 'track_href', 'analysis_url', 'duration_ms', 'time_signature']

#Merge dataframes 
sample = sample.join(df1)

merged_df = pd.merge(bbcsv, sample, on = ['title', 'artist'])

merged_df.isnull().values.any()

print(merged_df)

#os.makedirs('../data', exist_ok=True)  
#>>> merged_df.to_csv('../data/out5.csv')
