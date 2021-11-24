import dash
from dash import html
from dash import dcc
import plotly.graph_objects as go
import plotly.express as px
import warnings
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import scipy as sp
import seaborn as sns
from PIL import Image
from plotly.subplots import make_subplots
import plotly.express as px
from dash.dependencies import Input, Output
warnings.filterwarnings("ignore")
app = dash.Dash()  # initialising dash app

df = pd.read_csv(r"Datasets\data.csv", index_col="Unnamed: 0")

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


def plot_player_attribute(player_index, observation, skills):
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


app.layout = html.Div(id='parent', children=[
    html.H1(id='H1', children='Styling using html components', style={'textAlign': 'center',
                                                                      'marginTop': 40, 'marginBottom': 40}),

    dcc.Dropdown(id='dropdown', options=[{'label': 'L. Messi', 'value': 0}, {
                 'label': 'Ronaldo', 'value': 1}, {'label': 'Neymar Jr', 'value': 2}, {'label': 'M. Salah', 'value': 26}], value=0),
    dcc.Graph(id='attributes')
]
)


@app.callback(Output(component_id='attributes', component_property='figure'),
              [Input(component_id='dropdown', component_property='value')])
def graph_update(dropdown_value):
    return plot_player_attribute(
        dropdown_value, df.iloc[dropdown_value], list(calculate_attribute(df, dropdown_value).keys()))


if __name__ == '__main__':
    app.run_server()
