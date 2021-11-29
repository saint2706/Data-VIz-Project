import warnings

import dash
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from dash import dcc
from dash import html
from dash.dependencies import Input, Output
from plotly.subplots import make_subplots

warnings.filterwarnings("ignore")
app = dash.Dash()  # initialising dash app

df = pd.read_csv(r"Datasets\data.csv", index_col="Unnamed: 0")


def top_10_cunts():
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
    # temp_df.to_csv(r"Outputs\MostPlayersNation.csv")
    fig = px.bar(
        temp_df,
        x="Nation",
        y="PlayerCount",
        color="PlayerCount",
        title="Top Countries with Most Players",
    )
    return fig


attribute_dict = {
    "shooting": [
        "Positioning",
        "Finishing",
        "ShotPower",
        "LongShots",
        "Volleys",
        "Penalties",
    ],
    "passing": [
        "Vision",
        "Crossing",
        "FKAccuracy",
        "ShortPassing",
        "LongPassing",
        "Curve",
    ],
    "dribbling": [
        "Agility",
        "Balance",
        "Reactions",
        "BallControl",
        "Dribbling",
        "Composure",
    ],
    "defending": [
        "Interceptions",
        "HeadingAccuracy",
        "Marking",
        "StandingTackle",
        "SlidingTackle",
    ],
    "physical": ["Jumping", "Stamina", "Strength", "Aggression"],
}


def calculate_attribute(dataframe, player_index):
    allcols = []

    for i in attribute_dict.values():
        allcols.extend(i)

    player_observation = dataframe.loc[player_index, allcols].astype(
        "int64")
    player_skills = []

    for i in attribute_dict.keys():
        lis = attribute_dict.get(i)
        player_skills.append(
            int(sum(player_observation[lis]) /
                len(player_observation[lis]))
        )

    return {
        i.upper() + ": " + str(j) + "%": j
        for i, j in zip(attribute_dict.keys(), player_skills)
    }

    # function get skills values for any attribute.


def get_attributes_values(attribute, observation):
    return observation.loc[attribute_dict.get(attribute)].astype("int64")

    # function that plot player attribute: need index of player skills.


def plot_player_attribute(observation, skills):
    colors = ["#03a309", "#a3037e", "#fd3689", "#ded118", "#474bc9"]
    go.Figure()
    fig = make_subplots(rows=1, cols=5)
    # create skills bar chart
    for key, skill_name, color_i, column in zip(
        attribute_dict.keys(), skills, colors, range(1, 6)
    ):
        values = get_attributes_values(key, observation)
        fig.add_trace(
            go.Bar(
                x=values,
                y=attribute_dict.get(key),
                name=skill_name,
                marker=go.bar.Marker(
                    color=color_i, line=dict(color="#454545", width=1)
                ),
                orientation="h",
                width=0.5,
                text=values,
                textposition="auto",
            ),
            row=1,
            col=column,
        )

    # update layout properties
    fig.update_layout(
        autosize=False,
        height=300,
        width=2300,
        bargap=0.5,
        bargroupgap=0.3,
        barmode="overlay",
        hovermode="x",
        margin=dict(r=0, l=0, b=0, t=100),
        title=(
            {
                "text": observation["Name"] + " Attribute Details",
                "y": 0.9,
                "x": 0.5,
                "xanchor": "right",
                "yanchor": "top",
            }
        ),
    )
    fig.update_xaxes(range=[0, 100])
    return fig


def preffoot():
    preferred_foot = df.groupby("Preferred Foot")[
        "Preferred Foot"].count().to_list()

    temp_df = pd.DataFrame(
        list(zip(["Left", "Right"], preferred_foot)), columns=["Foot", "Count"]
    )
    # temp_df.to_csv(r"Outputs\PreferredFootAnalysis.csv")
    # plot pie chart to display the percentage for the preferred foot
    fig = px.pie(temp_df, values="Count", names="Foot",
                 title="Preferred Foot Analysis")
    fig.update_traces(textposition="inside",
                      textinfo="percent+label", pull=[0.25, 0])
    return fig


def posAnal():
    player_position = (
        df[["Position", "ID"]]
        .groupby(by=["Position"], as_index=False)
        .count()
        .sort_values("ID", ascending=False)
    )

    player_position.rename(columns={"ID": "count"}, inplace=True)
    player_position = player_position.reset_index().drop(["index"], axis=1)

    # plot bar chart to display the number of players for every position.
    temp_df = pd.DataFrame(
        list(zip(list(player_position["Position"]),
                 list(player_position["count"]))),
        columns=["Position", "Count"],
    )
    # temp_df.to_csv(r"Outputs\PlayerPositionAnalysis.csv")
    fig = px.bar(
        temp_df,
        x="Position",
        y="Count",
        color="Position",
        title="Player's Position Distribution",
    )
    return fig


# noinspection PyShadowingNames
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


def overRate():
    fig = px.histogram(
        df,
        x="Overall",
        color="Overall",
        title="Overall rating distribution for all players",
    )
    return fig


def top10expensive():
    club_value_df = df[["Club", "Release Clause"]].dropna(how="any")

    club_value_df.columns = ["club", "value"]
    club_value_df.value = getValue(club_value_df.value)
    club_value_df = (
        club_value_df.groupby(by=["club"], as_index=False)
        .sum()
        .sort_values(by="value", ascending=False)
    )
    club_value_df.reset_index().drop(
        "index", axis=1).to_csv(r"Outputs\TopExpensive.csv")

    # plot most 10 team have expensive players.
    fig = px.bar(
        club_value_df[:20],
        x="club",
        y="value",
        title="Top 20 most expensive teams in the world",
        color="value",
        labels={"club": "Clubs", "value": "Values"},
    )
    return fig


app.layout = html.Div(id='parent', children=[
    html.H1(id='H1', children='Dashboard 101', style={'textAlign': 'center',
                                                      'marginTop': 'auto', 'marginBottom': 'auto'}),
    dcc.Dropdown(id='dropdown', options=[
        {'label': 'L. Messi Attribute chart', 'value': 0},
        {'label': 'Ronaldo Attribute chart', 'value': 1},
        {'label': 'Neymar Jr Attribute chart', 'value': 2},
        {'label': 'M. Salah Attribute chart', 'value': 26},
        {'label': 'top 10 countries with maximum players', 'value': 'top10'},
        {'label': 'Preferred foot analysis', 'value': 'footfetish'},
        {'label': 'Position Analyis', 'value': 'posAnal'},
        {'label': 'OverAll Rating Distribution', 'value': 'overRate'},
        {'label': 'expensive clubs', 'value': 'mehengaai'}
    ], value=0),
    dcc.Graph(id='attributes')
]
)


@app.callback(Output(component_id='attributes', component_property='figure'),
              [Input(component_id='dropdown', component_property='value')])
def graph_update(dropdown_value):
    if dropdown_value in [0, 1, 2, 26]:
        return plot_player_attribute(
            df.iloc[dropdown_value], list(calculate_attribute(df, dropdown_value).keys()))
    if dropdown_value == 'top10':
        return top_10_cunts()
    if dropdown_value == 'footfetish':
        return preffoot()
    if dropdown_value == 'posAnal':
        return posAnal()
    if dropdown_value == 'overRate':
        return overRate()
    if dropdown_value == 'mehengaai':
        return top10expensive()


if __name__ == '__main__':
    app.run_server()
