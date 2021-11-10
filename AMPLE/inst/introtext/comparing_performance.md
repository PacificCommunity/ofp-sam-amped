[//]: # (Information and instructions for the Comparing Performance app)
[//]: # (comparing_performance.md)
[//]: # (Author: Finlay Scott (SPC) <finlays@spc.int>)
[//]: # (Soundtrack: Assassin's Creed 4: Black Flag (Original Game Soundtrack) by Brian Tyler)
[//]: # (Distributed under the terms of the GNU General Public License GPL (>= 3))

# Comparing the performance of HCRs

## Introduction

This is the third app in the **AMPLE** package. The first app, **Introduction to Harvest Control Rules (HCRs)** explores how HCRs work.
If you are unfamiliar with HCRs, you should start there.

Before an HCR is adopted and put into operation many candidate HCRs are tested.
Their performance is compared using *performance indicators* (PIs) and the preferred HCR is selected.

In this app you will be testing and comparing the performances of multiple HCRs using PIs.

The basic process is:

1. Select the shape of an HCR using the controls in the left-hand panel.
2. Run a projection to generate the performance indicators for that HCR by pressing the **Project** button.
3. Inspect the performance indicators and, if you like them, add the HCR to the basket by pressing **Add HCR to basket**.
4. Go back to step 1 and try another HCR.

When you have enough HCRs (at least 3) in your basket you will use the performance indicators to compare them (in the **Compare results** tab).

## How it works

The first page of the app looks very similar to the **Measuring performance** app, with time series plots of catch, biomass and relative CPUE, as well as the HCR shape.

In the **Measuring performance** app you ran one projection, or replicate, at a time.
Here, when you run a projection, by pressing the **Project** button, 250 replicates will be run (this may take a few seconds).
Biological variability is already switched on (the level can be adjusted in the Setting page) so that uncertainty is considered in the results.

After pressing **Project** a table of *performance indicators* for that HCR is shown under the HCR plot.
This is the same table as seen in the **Measuring performance** app.

Seven performance indicators are calculated:

| Performance indicator | Description |
|:-------|:-------|
| Biomass | The biomass relative to the unfished biomass.|
| Probability of being above the LRP | This reflects the of risk of stock being overfished. |
| Catch | The expected catches. |
| Relative CPUE | The CPUE relative to the CPUE in the last historical year. |
| Relative effort | The fishing effort relative to the effort in the last historical year. |
| Catch stability | How much the catches change over time. A value of 1 means that the catches are very stable and do not change at all. A low value, close to 0, means that the catches fluctuate a lot over time (probably not a good thing). |
| Proximity to the TRP | How close the biomass is to the TRP on average. A value of 1 means that the biomass is always at the TRP. A low value close to 0 means that the biomass spends a lot of time being much higher, or lower, than the TRP. |

<br />

Generally, for most indicators, the higher the value the better (i.e. higher catches and higher catch stability are assumed to be better than lower catches and catch levels that change a lot over time).
However, higher fishing effort is not necessarily better as it may mean higher costs of fishing.
Similarly, higher biomass might not be better. If the biomass is too high, it may mean you could have fished more.

After choosing a HCR shape and pressing **Project** you should inspect the performance indicators in the table.
If you like the look of the performance indicators and think that the HCR is a possible candidate for adoption click on the **Add HCR to basket** to button.
You can give the HCR a name in the text box if you want.

Repeat these steps until you have at least 3 HCRs in your basket.

## Comparing performance

When you have sufficient HCRs in your basket it is time to compare their performance.

Click on the *Compare results* tab at the top of the page.
You will see several bar charts.
Each window is a bar chart of the average value of a performance indicator for each HCR in the basket for each time period (short-, medium- or long-term).
This makes it easy to compare the average performance of the HCRs in the different time periods.

As mentioned above, there are seven performance indicators.
This means that there is a lot of information to process (seven time periods, with three time periods for as many HCRs as are in your basket).

To help, you can de-select performance indicators using the menu on the left.
You will need to decide which of these performance indicators are the most important (which will depend on your fishery objectives) and only focus on those by de-selecting those that are less important.
For example, if *CPUE* (catch rates) and *catch stability* are your most important consideration, de-select all other performance indicators leaving just two.

It is also possible to de-select HCRs using the menu on the left.
This allows you to eliminate HCRs that do not perform as well as others and do not meet your objectives.
By progressively eliminating HCRs you will eventually arrive at the one that you want to adopt (unless none of them make the grade - in which case go back to the **HCR selection** tab and try out some more HCRs.)

## Bar charts, box plots and tables

As well as the bar charts, you can view as box plots. Go to the **Performance indicators - box plots** tab at the top of the app.
You can now see the range of values for each indicator, not just the average value.

Additionally, you can view the results in a table in the **Performance indicators - table** tab.

