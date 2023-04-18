import json
import csv

def main():
    dataset = []
    data = json.load(open("data.json"))
    years = list(range(35,66)) # 1980-2015
    dataset = [['year', 'position', 'title', 'artist', 'pos_sentiment',
                'neg_sentiment', 'neut_sentiment','compound_sentiment',
                'f_k_grade', 'flesch_index', 'fog_index', 'num_syllables',
                'difficult_words', 'num_dupes', 'num_words', 'num_lines',
                'tags']]

    for i in years:
        
        songs = list(range(len(data[i]['songs'])))
        for index in songs:
            song_data = []
            song_data.append(data[i]['songs'][index]['year'])
            song_data.append(data[i]['songs'][index]['pos'])
            song_data.append(str(data[i]['songs'][index]['title']))
            song_data.append(data[i]['songs'][index]['artist'])
            song_data.append(data[i]['songs'][index]['sentiment']['pos'])
            song_data.append(data[i]['songs'][index]['sentiment']['neg'])
            song_data.append(data[i]['songs'][index]['sentiment']['neu'])
            song_data.append(data[i]['songs'][index]['sentiment']['compound'])
            song_data.append(data[i]['songs'][index]['f_k_grade'])
            song_data.append(data[i]['songs'][index]['flesch_index'])
            song_data.append(data[i]['songs'][index]['fog_index'])
            song_data.append(data[i]['songs'][index]['num_syllables'])
            song_data.append(data[i]['songs'][index]['difficult_words'])
            song_data.append(data[i]['songs'][index]['num_dupes'])
            song_data.append(data[i]['songs'][index]['num_words'])
            song_data.append(data[i]['songs'][index]['num_lines'])
            #song_data.append(data[i]['songs'][index]['tags'])
            song_data.append(', '.join(data[i]['songs'][index]['tags']))
            dataset.append(song_data)

    with open("billboard_top_songs.csv", "w", newline="") as x:
        writer = csv.writer(x)
        writer.writerows(dataset)
main()
    
    
