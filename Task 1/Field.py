# Import Modules
import warnings

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import plotly.graph_objects as go
import scipy as sp
import seaborn as sns
from PIL import Image
from plotly.subplots import make_subplots
from scipy import stats
warnings.filterwarnings('ignore')

# Read FIFA 19 dataset and show info and describe columns from it.
df = pd.read_csv("../Datasets/data.csv", index_col="Unnamed: 0")
print(df.head())
# get column names.
col = df.columns
print(col)
# dimensions of data
print(df.shape)
# info for all dataset columns_name, dataType, null_count
df.info()
# description of data min, max, mean, std values for all columns
df.describe()
# count number of rows that have null value in every columns.
df.isnull().sum()

# Data Visualisation

# Calculate top 10 countries sorted by most players in the game
# group data by Nationality and sort it by number of players to get most countries having players.
national_players = df[['Nationality', "ID"]].groupby(by=['Nationality'], as_index=False).count().sort_values(
    "ID", ascending=False)
national_players.rename(columns={'Nationality': "country", 'ID': 'player_count'}, inplace=True)
national_players = national_players.reset_index()
national_players = national_players.drop(["index"], axis=1)
national_players.head(10)

# Slicing first 10 rows from country player_count dataset
player_count = national_players.iloc[0:10, 1]
nation = national_players.iloc[0:10, 0]

# select seaborn style of chart to make display easy on the eyes.
plt.style.use("seaborn")
# create bar chart
plt.bar(nation, player_count)
plt.xticks(rotation=45)
plt.title('Top 10 Countries that have players in FIFA 19')
plt.show()

# Show Distribution of Age for all players
# slicing Age column and group it and count no. of players that have same age for all ages.
player_ages = df[['Age', "ID"]].groupby(by=['Age'], as_index=False).count().sort_values("ID", ascending=False)
player_ages.rename(columns={'ID': 'count'}, inplace=True)
player_ages = player_ages.reset_index().drop(["index"], axis=1)
player_ages.head()

# display histogram of age for all players and fit a normal distribution line for it.
_, bins, _ = plt.hist(df.Age, bins=df.Age.max() - df.Age.min(), label="Age with no. of player")
mu, sigma = sp.stats.norm.fit(df.Age)
best_fit_line = sp.stats.norm.pdf(bins, mu, sigma)
plt.plot(bins, df.shape[0] * best_fit_line, label="fit_line", color="red")

plt.title('Distribution of Age of players in FIFA 19')
plt.ylabel("Number of players")
plt.xlabel("Age of players")
plt.legend()
plt.show()

# Preferred foot analysis.
# count number of left and right foot preferred players
preferred_foot = df.groupby("Preferred Foot")["Preferred Foot"].count()
print(preferred_foot)

# plot pie chart to display the percentage for the preferred foot
plt.pie(preferred_foot, labels=["left", "right"], explode=[0.1, 0], autopct='%1.2f%%', colors=["#ea157a", "#0089af"])
plt.legend()
plt.show()

# Show positions with the most number of players
# count number of players for every position in playground that have players and sort it.
player_position = df[["Position", "ID"]].groupby(by=['Position'], as_index=False).count().sort_values("ID",
                                                                                                      ascending=False)
player_position.rename(columns={'ID': 'count'}, inplace=True)
player_position = player_position.reset_index().drop(["index"], axis=1)
player_position.head()

# plot bar chart to display the number of players for every position.
plt.figure(figsize=(15, 7))
plt.bar(player_position["Position"], player_position["count"])
plt.xticks(rotation=70)
plt.title("Player's Position Distribution", color="black")
plt.show()

# Top 10 players for ST, GK, LW, RF Position

ST_position = df[df["Position"] == "ST"].sort_values("Overall", ascending=False)[["Name", "Overall"]]
ST_position = ST_position.iloc[:10, :]

GK_position = df[df["Position"] == "GK"].sort_values("Overall", ascending=False)[["Name", "Overall"]]
GK_position = GK_position.iloc[:10, :]

LW_position = df[df["Position"] == "LW"].sort_values("Overall", ascending=False)[["Name", "Overall"]]
LW_position = LW_position.iloc[:10, :]

RF_position = df[df["Position"] == "RF"].sort_values("Overall", ascending=False)[["Name", "Overall"]]
RF_position = RF_position.iloc[:10, :]


# function to plot bar chart for top 10 players in selected positions.
def draw(df, color, position, ax):
    plt.style.use('tableau-colorblind10')
    sns.barplot(df["Name"], df["Overall"], color=color, ax=ax).set_title("Top 10 " + position + " players",
                                                                         fontsize=14)
    ax.set_xticklabels(ax.get_xticklabels(), rotation=40)


# plot 4 figures that display Top 10 players in ST, GK, LW, RF positions.
fig, axes = plt.subplots(nrows=2, ncols=2, figsize=[20, 15])

draw(GK_position, "#e91e63", "GK", axes[0, 0])
draw(ST_position, "#0089af", "ST", axes[0, 1])
draw(LW_position, "#1ab39f", "LW", axes[1, 0])
draw(RF_position, "#72bd35", "RF", axes[1, 1])
plt.show()


# Distribution of all player's value and calculate The average value of players.
# function that converts value column of players to numeric.
def getValue(df):
    new = []
    for i in df:
        i = i.strip("€")
        if "K" in i:
            i = i.strip("K")
            new.append(float(i) * 1000)
        elif "M" in i:
            i = i.strip("M")
            new.append(float(i) * (10 ** 6))
        else:
            new.append(0.0)

    return new


# convert value columns to numeric and calculate the average value.
lis = getValue(df.Value.values)
print("The average value of players in the world = ", round(np.average(np.array(lis)) / 10 ** 6, 2), "M")

# plot histogram of values to show distribution of it.
plt.hist(lis, bins=100)
plt.show()

sns.boxplot(x=getValue(df.Value.values))
plt.show()

# Overall rating distribution and most fit line for it.
# plot the distribution of overall rating.
plt.figure(figsize=(15, 7))
sns.countplot(df.Overall, label="overall_hist", color="#c81067")
plt.title("Overall rating distribution for all Players")
plt.legend()
plt.show()

# plot the distribution of overall rating and get most fit line for it.
_, bins, _ = plt.hist(df.Overall, bins=(df.Overall.max() - df.Overall.min()), label="overall_hist", color="#0093bc")
mu, sigma = sp.stats.norm.fit(df.Overall)
best_fit_line = sp.stats.norm.pdf(bins, mu, sigma)
plt.plot(bins, df.shape[0] * best_fit_line, label="fit_line", color="red")
plt.title("Overall ratings histogram")
plt.legend()
plt.show()

# Make Analysis for Real Madrid Club.
# select Real Madrid player from data
real_Madrid_players = df[df.Club == "Real Madrid"]
real_Madrid_players = real_Madrid_players[["Name", "Age", "Nationality", "Value", "Release Clause", "Overall"]]
real_Madrid_players.rename(columns={'Release Clause': 'Release'}, inplace=True)

# convert value column to numeric.
real_Madrid_players.Release = getValue(real_Madrid_players.Release)

# sort them by Release column.
real_Madrid_players.sort_values("Release", inplace=True, ascending=False)
real_Madrid_players = real_Madrid_players.reset_index()
real_Madrid_players.drop("index", axis=1, inplace=True)
real_Madrid_players.head(10)

print("Overall mean value for Real Madrid Team = ", round(real_Madrid_players.Overall.mean()), "%")

print("Release Corr. Overall= ", round(real_Madrid_players.Release.corr(real_Madrid_players.Overall), 2))

# plot chart for.
plt.figure(figsize=(14, 8))
plt.bar(real_Madrid_players.Name[:20], real_Madrid_players.Release[:20], width=0.8,
        label="Player Vs Value")
plt.title("Real Madrid Release Values")
plt.xticks(rotation=40)
plt.xlabel("player name")
plt.ylabel("value")
plt.legend()
plt.show()


# Calculate the relation between weight and height for all players(corr. and distribution)
# function that convert weight to numeric.
def get_weight(weight):
    new = []
    for i in weight:
        if "lbs" in str(i):
            new.append(i.strip("lbs"))
    return new


# function that convert height to numeric.
def get_height(height):
    new = []
    for i in height:
        if "'" in str(i):
            new.append(i.replace("'", "."))
    return new


# drop nan values form weight and height columns and convert it to numeric.
weight_height = df[["Weight", "Height"]].dropna(how="any")
weight_height.Weight = get_weight(list(weight_height.Weight.values))
weight_height.Height = get_height(list(weight_height.Height.values))

# plot scatter for  weight column.
weight_height.sort_values("Weight", ascending=True, inplace=True)
plt.figure(figsize=(15, 10))
plt.scatter(weight_height.Weight, weight_height.Height)
plt.xticks(rotation=40)
plt.show()

weight_height.Weight = weight_height.Weight.astype("float64")
weight_height.Height = weight_height.Height.astype("float64")
print("correlation between Weight and Height of players=", round(weight_height.Weight.corr(weight_height.Height), 2))

# plot the distribution of weight.
plt.hist(weight_height.Weight, bins=40)
plt.show()

# plot distribution of Height columns.
plt.hist(weight_height.Height, bins=40)
plt.show()
stats.mode(weight_height.Height)
print(weight_height.Height.shape)

# Top 10 expensive teams in the world
# drop nan from needed columns and group by Clubs and sort it by sum op player values.
club_value_df = df[["Club", "Release Clause"]].dropna(how="any")
club_value_df.columns = ["club", "value"]
club_value_df.value = getValue(club_value_df.value)
club_value_df = club_value_df.groupby(by=['club'], as_index=False).sum().sort_values(by="value", ascending=False)
club_value_df.reset_index().drop("index", axis=1).head(10)

# plot most 10 team have expensive players.
plt.figure(figsize=(12, 7))
plt.style.use("seaborn")
plt.bar(club_value_df.club[:10], club_value_df.value[:10], label="club with value")
plt.title("Top 10 most expensive teams in the world", fontsize=15)
plt.xlabel("Teams")
plt.ylabel("Value")
plt.xticks(rotation=40)
plt.legend()
plt.show()

# Calculate ATTRIBUTE DETAILS for any player you want.
# attribute dictionary key attribute and values skills columns for every attribute.
attribute_dict = {"shooting": ["Positioning", "Finishing", "ShotPower", "LongShots", "Volleys", "Penalties"],
                  "passing": ["Vision", "Crossing", "FKAccuracy", "ShortPassing", "LongPassing", "Curve"],
                  "dribbling": ["Agility", "Balance", "Reactions", "BallControl", "Dribbling", "Composure"],
                  "defending": ["Interceptions", "HeadingAccuracy", "Marking", "StandingTackle", "SlidingTackle"],
                  "physical": ["Jumping", "Stamina", "Strength", "Aggression"]}


# function that calculate attribute for any player: need using player index
def calculate_attribute(dataframe, player_index):
    allcols = []

    for i in attribute_dict.values():
        allcols.extend(i)

    player_observation = dataframe.loc[player_index, allcols].astype("int64")
    player_skills = []

    for i in attribute_dict.keys():
        lis = attribute_dict.get(i)
        player_skills.append(int(sum(player_observation[lis]) / len(player_observation[lis])))

    return {i.upper() + ": " + str(j) + "%": j for i, j in zip(attribute_dict.keys(), player_skills)}


# function get skills values for any attribute.
def get_attributes_values(attribute, observation):
    return observation.loc[attribute_dict.get(attribute)].astype("int64")


# function that plot player attribute: need index of player skills.
def plot_player_attribute(player_index, observation, skills):
    colors = ['#03a309', "#a3037e", "#fd3689", "#ded118", "#474bc9"]
    go.Figure()
    fig = make_subplots(rows=1, cols=5)
    # create skills bar chart
    for key, skill_name, color_i, column in zip(attribute_dict.keys(), skills, colors, range(1, 6)):
        values = get_attributes_values(key, observation)
        fig.add_trace(go.Bar(x=values, y=attribute_dict.get(key), name=skill_name,
                             marker=go.bar.Marker(color=color_i, line=dict(color="#454545", width=1)), orientation="h",
                             width=0.5, text=values, textposition='auto'), row=1, col=column)

    # read image
    img = Image.open("../faces/" + str(player_index) + ".png")
    # Add image
    fig.add_layout_image(dict(source=img, xref="paper", yref="paper",
                              x=1, y=1.5, sizex=0.5, sizey=0.5, xanchor="right", yanchor="top"))
    # update layout properties
    fig.update_layout(autosize=False, height=300, width=2300, bargap=0.5, bargroupgap=0.3, barmode="overlay",
                      hovermode="x", margin=dict(r=0, l=0, b=0, t=100),
                      title=(
                          {'text': observation["Name"] + " ATTRIBUTE DETAILS", 'y': 0.9, 'x': 0.5, 'xanchor': 'right',
                           'yanchor': 'top'}))
    fig.update_xaxes(range=[0, 100])
    fig.show()


# Player Attributes select player index form dataset
# draw attribute details for MESSI.
player_index = 0
player_skills = calculate_attribute(df, player_index)
plot_player_attribute(player_index, df.iloc[player_index], list(player_skills.keys()))

# draw attribute details for RONALDO.
player_index = 1
player_skills = calculate_attribute(df, player_index)
plot_player_attribute(player_index, df.iloc[player_index], list(player_skills.keys()))

# draw attribute details for Neymar Jr
player_index = 2
player_skills = calculate_attribute(df, player_index)
plot_player_attribute(player_index, df.iloc[player_index], list(player_skills.keys()))

# draw attribute details for M.SALAH.
player_index = 26
player_skills = calculate_attribute(df, player_index)
plot_player_attribute(player_index, df.iloc[player_index], list(player_skills.keys()))


# Radar Plot for player attribute's Details
# function plot radar diagram for any player, need player skills and player name.
# noinspection PyTypeChecker
def plot_player_radar(skills, player_name):
    fig = go.Figure()

    fig.add_trace(go.Scatterpolar(r=list(skills.values()), theta=list(skills.keys()), fill='toself',
                                  name=player_name, line_color='darkviolet', ))

    fig.update_layout(polar=dict(radialaxis=dict(visible=False, range=[0, 100])), showlegend=True)
    fig.show()


# draw attribute Details radar chart for RONALDO.

player_index = 1
player_skills = calculate_attribute(df, player_index)
plot_player_radar(player_skills, df.iloc[player_index]["Name"])

# draw attribute Details radar chart for M.SALAH.
player_index = 26
player_skills = calculate_attribute(df, player_index)
plot_player_radar(player_skills, df.iloc[player_index]["Name"])

# Radar plot for player's skills.
# select player, skills columns and convert it to numeric.
player_index = 0
col = ['Crossing', 'Finishing', 'ShortPassing', 'SprintSpeed', 'Stamina', 'Strength', 'Vision', 'Acceleration',
       'BallControl']
observation = df.loc[player_index, col].astype("int64")

# use to concat skills with it's value.
categories = []
for i in col:
    categories.append(i + ": " + str(int(observation[i])) + "%")

# plot MESSI skills radar.
# noinspection PyRedeclaration
fig = go.Figure()
# noinspection PyTypeChecker
fig.add_trace(go.Scatterpolar(r=observation, theta=categories, fill='toself',
                              name=df.iloc[player_index]["Name"], line_color="red"))
fig.update_layout(polar=dict(radialaxis=dict(visible=False, range=[0, 100])), showlegend=True)
fig.show()

# Show the best squad in the world by choosing Lineup.
# player position in each line in playground.
position = {"defender": ["RB", "LB", "CB", "LCB", "RCB", "RWB", "RDM", "CDM", "LDM", "LWB"],
            "midfielder": ["RM", "LM", "CM", "LCM", "RCM", "RAM", "CAM", "LAM"],
            "attacker": ["RW", "RF", "CF", "LF", "LW", "RS", "ST", "LS"]
            }
lineup = [3, 4, 3]


# function that get best squad in the world based on your Lineup.
def get_best_squad(lineup):
    best_squad = df[df.Position == "GK"].sort_values("Overall", ascending=False).iloc[0:1]
    for j, k in zip(position.keys(), range(3)):
        best = []
        for i in position.get(j):
            best.append(df[df.Position == i].sort_values(["Overall", "Potential"], ascending=[False, False]).iloc[0])
        best = pd.DataFrame(best).sort_values(["Overall", "Potential"], ascending=[False, False])
        best = best.iloc[0:lineup[k]]
        best_squad = pd.concat([best_squad, best])
    return best_squad


# get best squad on the world based on lineup which you select.
best_squad = get_best_squad(lineup)
best_squad.reset_index(inplace=True)
player_index = list(best_squad.loc[:, ["index"]].values.reshape(11, ))
best_squad.drop("index", axis=1, inplace=True)
print(best_squad)

# Plot the best squad on playground based on Lineup [3,4,3].
# location of player on chart.
location_3_4_3 = {0: [150, 80],
                  1: [150, 145],
                  2: [220, 145],
                  3: [80, 145],
                  4: [60, 300],
                  5: [150, 230],
                  6: [240, 300],
                  7: [150, 320],
                  8: [60, 400],
                  9: [150, 450],
                  10: [240, 400]
                  }
# Create figure
fig = go.Figure()

# Constants
img_width = 900  # 900
img_height = 1200  # 1200
scale_factor = 0.4

# add plot
# noinspection PyTypeChecker
fig.add_trace(
    go.Scatter(x=[0, img_width * scale_factor], y=[0, img_height * scale_factor], mode="markers", marker_opacity=0))

# Configure axes
fig.update_xaxes(visible=False, range=[0, img_width * scale_factor])

# disable y-axis visible
fig.update_yaxes(visible=False, range=[0, img_height * scale_factor], scaleanchor="x")

# add player image
for i in range(11):
    img = Image.open("../faces/" + str(i) + ".png")
    fig.add_layout_image(dict(x=location_3_4_3[i][0], y=location_3_4_3[i][1], sizex=60, sizey=60, xref="x",
                              yref="y", opacity=1.0, layer="above", source=img))
# Add background image
img = Image.open("../field.jpg")
fig.add_layout_image(dict(x=0, sizex=img_width * scale_factor, y=img_height * scale_factor,
                          sizey=img_height * scale_factor, xref="x", yref="y", opacity=1.0, layer="below",
                          sizing="stretch", source=img))
# Configure other layout
fig.update_layout(width=img_width * scale_factor, height=img_height * scale_factor,
                  margin={"l": 0, "r": 0, "t": 50, "b": 0},
                  title_font_size=15, title_font_family="Dosis",
                  title=({'text': "---Best Squad in The World for Lineup[3,4,3]---",
                          'y': 0.95, 'x': 0.5, 'xanchor': 'center', 'yanchor': 'top', }))
fig.show()
