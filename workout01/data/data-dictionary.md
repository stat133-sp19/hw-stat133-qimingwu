###Data Dictonary for the .csv player data files

|    Column Name    |  Data Type  |                                          Description                                       |
|-------------------|-------------|--------------------------------------------------------------------------------------------|
|     team_name     |  Character  |                                  Name of the player's team                                 |
|     game_date     |  Character  |                                      Date of the game                                      |
|       season      |   Integer   |                                     Season of the game                                     |
|       period      |   Integer   |                      Period of the game in which the shot is attempted                     |
| minutes_remaining |   Integer   |                   Number of minutes remaining when this shot is attempted                  |
| seconds_remaining |   Integer   |                   Number of seconds remaining when this shot is attempted                  |
|  shot_made_flag   |  Character  |                             Whether this shot is made or missed                            |
|    action_type    |  Character  |                              Basketball moves used by player                               |
|     shot_type     |  Character  |              Whether the shot is a 2-point field goal, or a 3-point field goal             |
|   shot_distance   |   Integer   |                          Distance to the basket (measured in feet)                         |
|      opponent     |  Character  |                                    Opponent of the game                                    |
|         x         |   Integer   | Horizontal component of the court coordinates (measured in inches) where the shot occurred |
|         y         |   Integer   |  Vertical component of the court coordinates (measured in inches) where the shot occurred  |
