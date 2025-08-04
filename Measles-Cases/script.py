# ============================ #
#
# UK / Europe Measles Cases Jan 2020 – Jun 2025
#
# Data from Tidy Tuesday:
# https://github.com/rfordatascience/tidytuesday
# 2025-06-24
#
# ============================ #

# Import libraries
import os
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.image as mpimg
from matplotlib.offsetbox import OffsetImage, AnnotationBbox
import seaborn as sns

# Set working directory to script location
script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

# ---------- #
# Data Preparation
# ---------- #

# Read in data with iso3 country names as index
cases_month = pd.read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_month.csv", index_col="iso3")
# print(cases_month)

# Add a datetime column
cases_month["date"] = pd.to_datetime(dict(
    year=cases_month["year"],
    month=cases_month["month"],
    day=1
))
# cases_month["month_format"] = cases_month["date"].dt.strftime("%b %Y")

# Subset rows for UK and from Jan 2020 – Jun 2025
country_iso3 = ["GBR"]
cases_month = cases_month.loc[country_iso3]
cases_month.columns

# Subset rows for Jan 2020 – Jun 2025
year = 2015
cases_month_uk = cases_month.loc[lambda df: (df["year"] >= year)]

# Subset columns
columns = ["country","year","date","measles_total"]
cases_month = cases_month[columns]
cases_month_uk = cases_month_uk[columns]

# ---------- #
# Logo
# ---------- #

# Read in PNG
logo_png = mpimg.imread("../misc/logo-darkblue.png")

# Create an OffsetImage
imagebox = OffsetImage(logo_png, zoom=0.02)

# Position in top-right corner
image = AnnotationBbox(
    imagebox, (0.99, 0.99),
    xycoords="axes fraction",
    frameon=False,
    box_alignment=(1, -0.3)
)

# ---------- #
# Parameters
# ---------- #

# Seaborn theme
sns.set_theme()

# Font family
plt.rcParams["font.family"] = "Verdana"

# ---------- #
# UK Time Series
# ---------- #

# Plot
fig, ax = plt.subplots(figsize=(9,6))
sns.lineplot(
    data=cases_month_uk,
    x="date",
    y="measles_total",
    color="#E24A33",
    marker="o",
    markersize=5,
    linewidth=1.5,
    ax = ax
)

# Custom x-axis ticks every Jan from 2015 to 2025
ticks_x = pd.date_range(
    start=min(cases_month_uk["date"]),
    end=max(cases_month_uk["date"]),
    freq="12MS"
)

# Format x-axis
ax.set_xticks(ticks_x)
# plt.xticks(rotation=45)
ax.set_xlabel("")

# Format x-axis labels ("Jan 2020", "Jan 2021", etc.)
date_format = mdates.DateFormatter("%b\n%Y")
ax.xaxis.set_major_formatter(date_format)

# Format y-axis
ax.set_ylabel("Cases", labelpad=10)

# Remove vertical x-axis grid lines
# plt.grid(axis="x", visible=False)

# Add logo image to axes
ax.add_artist(image)

# Title
ax.text(x=0, y=1.10, s="Total Measles Cases 2015–2025", transform=ax.transAxes, fontsize=15, fontweight="bold")

# Subtitle
ax.text(x=0, y=1.03, s="United Kingdom", transform=ax.transAxes, fontsize=13)

# Credit
ax.text(x=0, y=-0.20, s="Source: World Health Organisation", ha="left", transform=ax.transAxes, fontsize=8)
ax.text(x=1, y=-0.20, s="Note: Provisional monthly data obtained on 2025-06-12", ha="right", transform=ax.transAxes, fontsize=8)

# Show or Save plot
plt.tight_layout()
# plt.show()
plt.savefig("Measles_cases_uk.png", dpi=300, bbox_inches="tight")