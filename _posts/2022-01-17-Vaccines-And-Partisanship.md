preprocess
   replace "IMAGE: ([^;\n]+);?(.*)" -> "<center><img src=\"https://raw.githubusercontent.com/kavigupta/covid-and-partisanship/main/images/\\1.png\" \\2 /></center>"
   pass ../_scripts/download_include.py
---
layout: post
title: Vaccines and Partisanship
comments: True
tags:
- draft
---

[Much](https://www.nytimes.com/2021/09/27/briefing/covid-red-states-vaccinations.html) [has](https://www.cnn.com/2021/11/16/politics/covid-19-vaccine-partisanship/index.html) [been](https://www.nationalreview.com/corner/get-the-shot-fellow-right-wingers/) [written](https://www.npr.org/2021/12/06/1057344561/anti-vaccine-activists-political-conference-trump-republicans) about the vaccine polarization in the United States and how Republicans in particular are driving vaccine hesitancy.

Unfortunately, we unfortunately do not have joint data on vaccine uptake and voting (i.e., the number of vaccinated democrats), as vaccinations are not recorded alongside voting record. The best approximation we have to this is a small number of national polls on the subject. We do, however, have data on vaccine uptake and voting separately, for almost all of the counties in the United States. In this article, We use demographic-based regression to estimate the number of vaccinated and unvaccinated Democrats and Republicans at the county level.

As a teaser, here's what an election map looks like if only vaccinated people voted.

IMAGE: vaccinated

<!-- end excerpt -->
 
# Methodology

We use a demographic regression to predict the four variables by county: unvaccinated democrats, unvaccinated republicans, vaccinated democrats, and vaccinated republicans; fitting this function to try to match current vaccine totals and and 2020 election results as closely as possible.

Specifically, we took the latent demographics that [bot_2024](https://twitter.com/bot_2024) uses to predict elections, and assigned each one our four variables. The latent demographics from bot_2024 are designed to be politically relevant since they were trained to predict the last 5 elections, and as such we can use them reasonably in this kind of analysis. Additionally, since we have just one set of parameters we are using across all counties, we have a smaller number of parameters than variables, meaning that our model has to learn to interpolate and learn a reasonable function.

In practice, our model is decent but not perfect at predicting the actual voteshare and vaccination rates.

IMAGE: accuracy

We can improve our results by just snapping them to the values we do know, modifying our 4 numbers minimally so that they match up with the vaccine uptake and democratic voteshare numbers we have. We also use our technique, with an appropriate correction for the state level vaccination rates, to produce the vaccine rates for VT, GA, neither of which is reporting accurate county level covid data (as per [the New York Times page on vaccinations](https://www.nytimes.com/interactive/2020/us/covid-19-vaccine-doses.html)).

# National results, and a sanity check

As a quick check to see if our results are reasonable, we can look at what our predictions come out to at the national level.

download_include: https://raw.githubusercontent.com/kavigupta/covid-and-partisanship/main/tables/topline.html

Our model predicts that 87.4% of Democrats and 50.4% of Republicans are vaccinated. This roughly lines up with October Polling from [KFF](https://www.kff.org/coronavirus-covid-19/poll-finding/importance-of-partisanship-predicting-vaccination-status/) which says that 83% of Democrats and 40% of Republicans are vaccinated. It also roughly lines up with an August poll from [NBC news](https://www.nbcnews.com/politics/meet-the-press/nbc-news-poll-shows-demographic-breakdown-vaccinated-u-s-n1277514) that says that 88% of Democrats and 55% of Republicans are vaccinated.

Since my demography/political geography technique relies on an entirely different approach to the polling results, the rough correspondence provides some confidence that both are correct.

# How is each party being vaccinated?

IMAGE: overall

If you've been following vaccine data, you're probably familiar with this map, which shows the number of people vaccinated by county and state. We have added in my estimates for GA, VT, and the few rural counties in CA that don't have any data to this map.


This is presented as an election map for reasons you will see later, but it is not very interesting in this case. If "Vaccinated" were a party it would win in a 538-0 landslide, as expected given that 69.61% of American adults are vaccinated. We can also see that in almost every county, vaccination rates are above 50%, but there are many counties where vaccines drop below 50%. Apart from the exceptions noted above, this map is entirely based on real CDC data.

IMAGE: democrats

Now, we can start looking at the maps produced by my model. If you take a look at the vaccination rates only among Democrats, we can see that they are much higher across the board, with only a few counties being below 50%.


IMAGE: republicans

Alternatively, looking at vaccination rates among Republicans, we can see that the numbers are substantially lower across the board. In many states, Republicans are less than 50% vaccinated, making the corresponding election map surprisingly competitive. Notably, Republicans in the deep South and the Mountain West are much less likely to be vaccinated, but also the Republicans in some important metro areas such as LA County and Cook County (which contains Chicago) are less than 50% vaccinated.

# How do vaccinated and unvaccinated people vote?

IMAGE: vaccinated

Repeating the map from the top of the article, if only vaccinated people voted, the Democrats would win in a massive D+30.68 landslide, winning all but 8 states and gaining 500 electoral votes. The effect is especially pronounced in the South, where Republicans are far less likely to be vaccinated.

IMAGE: unvaccinated

On the other hand, if only unvaccinated people voted, the Republicans would dominate in an even larger R+57.19 landslide, winning every single state, and only losing the District of Columbia. The only counties Democrats would win would be a couple rich, white, and highly liberal counties, such as Mendocino, Marin, Sonoma, or Santa Cruz, California, or several parts of Vermont, heavily black and liberal areas in and around DC on the Maryland side, and a few of the Massachusetts coastal islands that I suspect are a data error on the part of the CDC.

# Which kind of person are you most likely to run into?

IMAGE: all_four

One question you might be asking yourself is "if I were to walk up to a random person in my county/state, which of the four categories would they fall into?" That question is answered by the above map. The shades on the map represent each category, with darker shades representing greater increases over the next category.

For most counties, including Republican ones, the answer is the person you run into is most likely to be a Vaccinated Democrat, since almost all Democrats are vaccinated, while Republicans are split between vaccinated and not. You can see the latter's effect in the checkerboard of counties represented by each of the two types of Republicans. The only counties where unvaccinated Democrats are the largest category are the Massachusetts islands, which, as I mentioned earlier, I am pretty sure are a data error.

# Aren't Republican politicians afraid of losing voters to Covid Deaths?

I have seen the above question asked repeatedly, by pundits and twitter users alike. One way to help answer this is to look at how much the effect really is. Below is a table of the amount of swing I estimate each state would experience if there were a million post-available-vaccine covid deaths;  for brevity I have only included swing states. 


download_include: https://raw.githubusercontent.com/kavigupta/covid-and-partisanship/main/tables/electoral_effect.html

Even in the million deaths scenario, no state has its result changed by more than 0.26%. In 2020, the closest state was Georgia with a margin of 0.24%, followed by Arizona with a margin of 0.31%, followed by Wisconsin, with a margin of 0.63%. So, in the extreme scenario depicted here, no states would have flipped in 2020, but it could easily flip a close race in the future, as these states could shift 0.5% to the right for 2022 or 2024.

The million deaths scenario is one in which Republicans continue to not get vaccinated and we have several waves that are as deadly as the summer delta wave, post vaccine. There have been about 400 thousand covid deaths since the vaccine was widely available in early summer, and it is possible that there will be 600 thousand more over the next year. This is however probably worse than the median case, and even then, the electoral impacts of negative headlines around covid and a weakened economy are more than enough to make up that difference for 2022, if not 2024.

*To step away from the analysis in this article for a moment, writing the previous paragraph sent a chill down my spine, and not just because I casually referred to the deaths of a million people in terms of its marginal effect on an election. I can't help but shake the impression that I am not the first person to do this analysis, calculating out the electoral impacts of allowing antivaccination to fester. Do I think the operatives and elected officials of the Republican Party are deliberately stoking antivaccination for electoral gain? Probably not. But do I think they would be trying harder to promote vaccinations in conservative spaces if the electoral calculus above spelled doom? I can't reasonably answer anything but yes.*

# Links

If you want the underlying data for the maps, you can find that at [this spreadsheet](https://raw.githubusercontent.com/kavigupta/covid-and-partisanship/main/csvs/inferred_breakdown.csv). The code for this project can be found at [this repository](https://github.com/kavigupta/covid-and-partisanship) but it is a complete mess and will require reworking to work on anyone's computer but mine.

Instead, if you have questions on methodology, you should probably contact me. You can find my twitter at [@notkavi](https://twitter.com/notkavi), or email me at `vaccinesparty` at `kavigupta.org` if you have any questions.

Special thanks to Lakshya Jain [@lxeagle](https://twitter.com/lxeagle17) for helping me with the data and editing for this project.