# Racial Bias Serie A
 
This repository contains the replication material for “Racial Bias in Fans and Officials: Evidence from the Italian Serie A” published in Sociology. Below, I describe each of the files in this repository.

## Manuscript

All models are run on the Final_Merged.csv datastet.
To obtain all tables and figures in the paper:
- run code.R

## Robustness Checks

For materials in the online appendix:
- run robustness.R  

# Codebook

- Player.x: name of player
- Squad.x: Serie A team in which the player has played the most minutes in a given year
- Nation: nationality of the player
- Season.x: Serie A season (2009-10 to 2020-21)
- Skin: skin tone (1 - lightest to 20 - darkest)
- Position: Goalkeeper (GK), Defender (DF), Midfielder (MF), Forward (FW)
- yellow_cards_overall: number of yellow cards (source Footystats)
- red_cards_overall: number of red cards (source Footystats)
- VAR: 1 if seasons 2017-18 or 2018-19 (post-VAR, before COVID)
- Mins: minutes played
- Fouls: number of fouls committed (source WhoScored)
- Fls: number of fouls committed (source FBREF)
- TotalAttemptedTackles: number of attempted tackles (source WhoScored)
