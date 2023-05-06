# What goes into a record-breaking song? 
*Predicting Billboard Top 100 winners using Spotify data and lyrics of songs* 


How do you make a hit song? Intuitively, a hit song should have a good beat, a catchy hook, and not-too-complicated lyrics. But we can do better than that, given the amount of data surrounding us. Spotify has been able to break a song into quantitative features such as energy, tempo, acoustics, and much more. 

Using these features for songs released between 1985 to 2015, we predict whether a song will make the Billboard Top 100 in the USA. We run both a logistic regression and random forests model. Unfortunately, while both models have a high total accuracy, they are unable to predict well whether a song will be a hit. Random forests get 5% as the best sensitivity rate.

We also evaluate how features of a Top 10 Billboard hit change over this time period, including lyric analysis. We observe that PCA does manage to group songs by lyric and acoustic features representing the popular music genres of each decade. Our biggest contribution is compiling a dataset with songs from 1958 to 2015, containing Spotify audio features and lyric features for Billboard hit songs.
