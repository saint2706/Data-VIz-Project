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
import plotly.express as px

warnings.filterwarnings("ignore")

# Read FIFA 19 dataset and show info and describe columns from it.
df = pd.read_csv(r"Datasets\data.csv", index_col="Unnamed: 0")


# Data Visualisation

# Calculate top 10 countries sorted by most players in the game
# group data by Nationality and sort it by number of players to get most countries having players.
national_players = (
    df[["Nationality", "ID"]]
    .groupby(by=["Nationality"], as_index=False)
    .count()
    .sort_values("ID", ascending=False)
)
national_players.rename(
    columns={"Nationality": "country", "ID": "player_count"}, inplace=True
)
national_players = national_players.reset_index()
national_players = national_players.drop(["index"], axis=1)


# Slicing first 10 rows from country player_count dataset
player_count = national_players.iloc[0:10, 1]
nation = national_players.iloc[0:10, 0]

# select seaborn style of chart to make display easy on the eyes.
temp_df = pd.DataFrame(
    list(zip(list(player_count), list(nation))), columns=["PlayerCount", "Nation"]
)
temp_df.to_csv(r"Outputs\MostPlayersNation.csv")
fig = px.bar(
    temp_df,
    x="Nation",
    y="PlayerCount",
    color="PlayerCount",
    title="Top Countries with Most Players",
)
fig.show()