
# Generating Traffic Accident Visualizations for American Cities and Towns

### Created by Michael Yohannes

This R Shiny application aims to provide car accident visualizations for
many cities throughout the United States in an automatically generated
manner, such that it can be easily accessed and used for academic
purposes. To run this application, open RStudio and input the following
commands:

``` r
library(shiny)
runGitHub(repo = "GEO3PROJECT", username = "michaelyohannes123", subdir = "project", ref="main")
```
Note that after running these commands, when the R Shiny application is loaded, the basemap of the heatmap visualization will not appear. To have it appear, download this project and modify line 67 of the app.R file (located in the project folder), by replacing "Sys.getenv("MAPBOX_KEY")" with your personal Mapbox API key. 

## Background

With traffic accidents being a consistent problem society faces in the
modern world, it is necessary for local governments to enact policy
initiatives to mitigate the number of accidents in their locality.
Having data easily accessible in such a way that one could assess the
extent to which certain parts of a locality have more accidents than
other areas, could be useful in generating specific local ordinances and
policies that pinpoint areas with high number of traffic accidents, and
try to reduce that area’s dangerousness in the long term. As a result, I
created an RShiny application where a user can get access to dynamically
generated maps detailing the spatial distribution of car accidents for
areas throughout a given place in the United States. The aim of this
project is to streamline traffic accident data visualizations for cities
across the United States, such that its ease of accessibility enables
future research on the socioeconomic factors behind the traffic accident
distributions in American cities.

Understanding the specific spatial distribution of traffic accidents
throughout a place can enable future socioeconomic-driven analysis
specific to that place. Broader socioeconomic disparities in society
have been shown to influence accident trends. For example, poorer
individuals tend to have lower quality vehicles that could result in an
increased likelihood of vehicle crashes, compared to the wealthier
residents. Researchers found in Montreal that lower income areas had 4.3
times more driving-related injuries than wealthier areas, attributing
the disparity to a less adequate road infrastructure in the poorer
communities (Morency et al., 2012). Similarly, the city of Minneapolis
commissioned a research study on their roads to see the extent to which
certain areas were prone to accidents. They found that areas deemed as
ACP50, which means that greater than or equal to 50% of an area is
composed of minorities, tend to have higher pedestrian crashes per
capita compared to non-ACP50 areas in the city (Kimley-Horn and
Associates, Inc.).

## Data Sources and Spatial Scale

The application data was based on
[this](https://www.kaggle.com/sobhanmoosavi/us-accidents) nationwide
dataset of 3 million accidents in the United States (from 2016-2020),
which was developed by researchers at the Ohio State University. The
accidents dataset was accumulated over that four year time span through
retrieving real time traffic API data across the country. Each accident
record has a plethora of attributes, such as start latitude-longitude
coordinates (under WGS 84 coordinate system), place of occurrence, time
of accident, severity (in relation to traffic impact), and fahrenheit
temperature at the time of accident. The accident records can all be
downloaded as a large .csv file, which allows me to retrieve specific
subsections of the data. It’s important to note that the dataset is
meant for non commercial and educational purposes, which is why I also
intended to make the application strictly for academic purposes, and as
such cited the datasets. After finding the dataset from Kaggle, I
downloaded it and from there developed this application.

There is a discussion to be had regarding the spatial scale, given how
throughout this paper I used the word ‘place’ instead of ‘city.’ This is
because the analysis scale of this project is at the city or town level,
depending on the nature of the place available in the data set. There’s
a massive difference in infrastructure and subsequently accidents data
between a sprawling urban center like New York City and a place like
Apple Valley, CA, which would be considered more so a as regular or
small town than a city. For the case of making that argument, I define a
city in this case to be a large important urban-centric town. Yet these
places mentioned can be searched in the application individually, and
have their accidents data be retrieved. The phenomenon that we are
assessing in this project through the dataset used, being accidents,
occurs everywhere around the world at any given moment in time, due to
the prevalence of vehicles throughout the world. Since the dataset
strictly encapsulates accident data in the United States, we can also
claim the phenomenon scale to be at the scope of the United States.

## Methods Used

I developed the project as an R Shiny application, importing the
following libraries: plotly, shinyjs, tmap, sf, tidyverse, mapdeck, and
data.table. The initial step was to efficiently read the accidents data
set, which was done through retrieving it as a .RData file. This was
also done to account for Github’s restriction on pushing large files
like the original .csv dataset. Through fluidPage I structured the UI,
having the place input at the top for the user to allow them to
immediately generate visualizations, and have the visualizations output
below the input boxes. Searching for a place is done through a selectize
input on the server-side to enable a faster retrieval of the thousands
of cities. Once a place is selected and the retrieve data button is
clicked, I use observeEvent to respond to the input place selected.
Dividing the input place into name, county, and state components, I
filtered the accidents dataset for accidents that occurred in that
place, and converted those filtered records into point features through
st\_as\_sf.

Once the retrieve data button is clicked, I display another input where
the user can focus on a specific accident record attribute. I decided to
have the default selection be general mode, where I just display a tmap
mapping of the accident points and also, through mapdeck, create an
associated heatmap for the accident points to better show the data’s
spatial distribution. Since mapdeck has a Mapbox base map, I also needed
to get a Mapbox API key. Responding to a change in input for the data
aspect input box, is also done through observeEvent. Specifying options
aside from general mode results in the application of a color palette
vector that is modified to fit the features of the filtered data. For
example, if there were no accidents that have a 5 severity designation,
then I would remove the color red from palette options to ensure that
red is associated only with the 5 level designation relative to the
other severity levels. This makes it such that the color designations
for the tmap and bar chart color coding is fixed. I made the bar chart
functionality plotly, and displayed this functionality when the
temperature, severity, or civil twilight data focus option is selected.
For the severity bar chart, I count the number of accident records that
fulfill each of the 1-5 severity level categorizations, showing the
distribution of accident records by severity level. For the civil
twilight bar chart, I similarly count the number of accident records
that meet a specific category, with the possible categories in this case
being “Day” and “Night.” The temperature bar chart differs from the
other two, in that I calculated the mean temperatures of accidents in
relation to the severity level categorization that an accident falls
under. This was done to assess if there’s a correlation between
temperature and the severity of an accident in impeding traffic. For
example, perhaps accidents of a higher severity occur in more extreme
weather relative to the lower severity accidents, with the extreme
temperatures affecting the road environment, and affecting an accident’s
impact.

One final aspect of the application’s development worth mentioning is
that throughout the application code, I used renderText to discuss
relevant tidbits regarding the data, as well as instructions.

## Results

The dashboard begins with an input asking the user to enter a specific
place and select the corresponding option. Let us take New York City as
a case study. Upon searching for New York City and then clicking the
retrieve data button, you get the following tmap output, depicting the
accident points found in New York City from 2016 to 2020, as well as a
heatmap of the points distribution:

<img src="https://github.com/michaelyohannes123/GEO3PROJECT/blob/main/p6cap.PNG" width="100%" />

<img src="https://github.com/michaelyohannes123/GEO3PROJECT/blob/main/p7cap.PNG" width="100%" />

Note that initially general mode is the data focus selection by default.
You can adjust the selection to focus on temperature, accident severity
(in regards to traffic impacts), or civil twilight (refers to the time
of day of accident in terms of either day or night). Looking at first
the temperature aspect, we will get the modification or creation of the
following visuals as so:

<img src="https://github.com/michaelyohannes123/GEO3PROJECT/blob/main/p10cap.PNG" width="100%" />

<img src="https://github.com/michaelyohannes123/GEO3PROJECT/blob/main/p11cap.PNG" width="100%" />

Here we modified tmap by color coding the points in relation to the
fahrenheit temperature during the accident. The bar chart was also
added, displaying the mean temperature during the accident in relation
to the accident severity level. If the civil twilight focus was selected
instead, we would get the following visual changes:

<img src="https://github.com/michaelyohannes123/GEO3PROJECT/blob/main/p12cap.PNG" width="100%" />

<img src="https://github.com/michaelyohannes123/GEO3PROJECT/blob/main/p13cap.PNG" width="100%" />

Here we again modified tmap by color coding the points, albeit in
relation to the civil twilight instead. Furthermore, the bar chart
instead details the number of accidents that are under each of the civil
twilight categorization. Finally, we have the severity focus option,
which would look like the following:

<img src="https://github.com/michaelyohannes123/GEO3PROJECT/blob/main/p8cap.PNG" width="100%" />

<img src="https://github.com/michaelyohannes123/GEO3PROJECT/blob/main/p9cap.PNG" width="100%" />

Here we modified tmap by color coding the points in relation to accident
severity level and added a bar chart that details the accidents count
distribution by severity level.

The options just described, of selecting different data focus modes,
aims to work for each place input, and is the main source of user
interactivity with this visualization generator application.

## Discussion

The many possible place options to acquire data from makes it such that
analysis can widely vary depending on what one is interested to research
in. For example, through this application one could compare the accident
distributions from a specific set of cities and conduct analysis on
that. Additionally, much of the analysis in understanding the underlying
factors behind the spatial distribution of accidents is place specific.
This is because places vary in relevant factors like road
infrastructure, weather, and population, which could affect accident
trends. Additionally, targeted proposed solutions to mitigating these
accidents are often specific to local government and community
initiatives. Though, with the application aiming to provide a good sense
of the spatial distribution of the data, one could make many associated
findings for specific cities in question. This is especially the case
for cities with a larger quantity of data, as evident by a place like
New York City that we saw in the previous section. Adding the bar charts
helps account for the points overlap that may make certain cities appear
with a higher proportion of a certain data aspect than in actuality. We
can see this in cities like Columbus, OH, with severity specified as the
data focus:

<img src="https://github.com/michaelyohannes123/GEO3PROJECT/blob/main/p3cap.PNG" width="100%" />

Visually it appears that the accidents of a severity 2 are greater in
number than accidents of a severity 3, due to the accidents of severity
2 being more spread out throughout the city compared to accidents of
severity 3, which appeared more concentrated in certain main central
routes in the city. To see the data more clearly, I wrote some test code
to split up the data to show only data of one of the severity levels
displayed, for severity levels 2 and 3:

<img src="https://github.com/michaelyohannes123/GEO3PROJECT/blob/main/p1cap.PNG" width="100%" />

<img src="https://github.com/michaelyohannes123/GEO3PROJECT/blob/main/p2cap.PNG" width="100%" />

The seemingly larger quantity of severity 2 data compared to severity 3
data may initially seem to contradict with the bar chart distribution as
follows:

<img src="https://github.com/michaelyohannes123/GEO3PROJECT/blob/main/p4cap.PNG" width="100%" />

As seen through the bar chart the number of severity 3 accidents is
actually greater than severity 2 accidents. This case study of Columbus,
OH helps illustrate the usefulness of including the bar chart. Though
the severity 2 data is more spread out, the severity 3 data is heavily
concentrated in certain areas to the point of being in greater number
than the severity 2 data. For understanding the variation between
severity level distributions, it is important to remember that accident
severity does measure the physical damage caused by an accident on the
cars and people, but rather the severity measures the impact of the
accident on traffic. As such, it is not necessarily counterintuitive to
have more accidents that result in a greater effect on traffic,
especially if the road infrastructure is plagued with problematic
bottleneck routes that cause accidents to have an outsized influence on
traffic.

## Conclusion

Given the sheer number of available cities, it is extremely difficult to
check for a problematic absence of data that could result in the
application running into errors. This results in having to be more
careful to write fairly efficient and cautious code to make the
application be not too time consuming for the user, and also not make
too many assumptions about the accident records themselves (i.e. they
likely miss certain relevant data). Steps can be taken to improve the
speed of the code when retrieving the accidents data, as there is a
noticeable time duration taken to retrieve larger cities data. This is
why I scrapped the idea of letting the user select the data on a year by
year basis after numerous attempts, as it led to the application
becoming much slower in handling the data of large cities. Perhaps by
modifying the code to be more efficient in data retrieval, I could have
the tmap point visualization operate as a more interactive year by year
timeline. Overall I believe this application has much room for expansion
through means such as adding new accident data aspects that the user can
focus on, and expanding the current bar chart options. Through these
future modifications the application will get closer to fulfilling its
intended purpose, as being a helpful tool to learn about the spatial
distribution of accidents and the data nuances for places all across the
United States, and subsequently look beyond the data through extensive
research.

## Sources

Moosavi, Sobhan, Mohammad Hossein Samavatian, Srinivasan Parthasarathy,
and Rajiv Ramnath. “A Countrywide Traffic Accident Dataset.”, 2019.

Moosavi, Sobhan, Mohammad Hossein Samavatian, Srinivasan Parthasarathy,
Radu Teodorescu, and Rajiv Ramnath. “Accident Risk Prediction based on
Heterogeneous Sparse Data: New Dataset and Insights.” In proceedings of
the 27th ACM SIGSPATIAL International Conference on Advances in
Geographic Information Systems, ACM, 2019.

Morency, P., Gauvin, L., Plante, C., Fournier, M. & Morency, C. (2012,
June 1). “Neighborhood Social Inequalities in Road Traffic Injuries: The
Influence of Traffic Volume and Road Design”, American Journal of Public
Health 102, no. 6: pp. 1112-1119.
<https://doi.org/10.2105/AJPH.2011.300528>

Kimley-Horn and Associates, Inc. (2017). 2017 City of Minneapolis
Pedestrian Crash Study.
<https://lims.minneapolismn.gov/Download/RCA/2877/Minneapolis-Pedestrian-CrashStudy_2017.pdf>
