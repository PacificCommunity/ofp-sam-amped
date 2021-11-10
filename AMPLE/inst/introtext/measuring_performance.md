[//]: # (Information and instructions for the Measuring Performance app)
[//]: # (measuring_performance.md)
[//]: # (Author: Finlay Scott (SPC) <finlays@spc.int>)
[//]: # (Soundtrack: I Sometimes Dream of Glue by Luke Haines)
[//]: # (Distributed under the terms of the GNU General Public License GPL (>= 3))

# Measuring performance

## Introduction

This is the second app in the **AMPLE** package. The first app, **Introduction to Harvest Control Rules (HCRs)** explores how HCRs work.
If you are unfamiliar with HCRs, you should start there.

This app explores how the performance of a proposed HCR can be measured.
Particular focus is put on the impact of uncertainty on the performance.

Before a HCR is adopted for use in the real world, its performance is tested and evaluated using computer simulations (known as Management Strategy Evaluation - MSE).
During these evaluations, the performance of a proposed HCR is measured using a range of indicators, known as performance indicators (PIs).

PIs measure things like expected catches and catch rates in the future and the probability of the stock falling below the Limit Reference Point.
These indicators should relate to the management objectives of the fishery, e.g. good economic performance, stock sustainability,etc.

We saw in the *Introduction to HCRs* app that uncertainty, such as biological variability, can affect the performance of a HCR.
For this reason, when evaluating an HCR many hundreds of projections are run.
Each projection is known as a replicate and, due to the uncertainty, each replicate will be different.

To fully understand the performance of the HCR it is necessary to consider all of the replicates together, as this gives a better idea of how the HCR will perform in the real world.

Performance indicators are calculated for reach replicate.
Summaries, such as the average and ranges of values, are presented to allow managers and stakeholders decide which HCR they prefer.

## Using the app

The layout of this app is slightly different to the **Introduction to HCRs** app.
The HCR is still at the top right, and the controls for changing the shape are still in the left-hand panel.

There are now three time series plots to the left of the HCR. They are *catch*, *biomass* and *relative CPUE*.
Catch and biomass we saw in the previous app.

*Relative CPUE* is the catch-per-unit of fishing effort (CPUE, sometimes called the catch rate), relative to the CPUE at the last historical value. 
This gives it a value of 1 in the last historical time step.
Using a relative measure like this makes it easier to compare the future values with those in the past.

In the **Introduction to HCRs** app, each time you clicked **Advance**, you projected forward by a single time step.
To run a full projection you had to click **Advance** many times.

In this app when you click **Run projection** you will run a full projection for all time steps (try it).
The process of the HCR setting the catch limits using the estimated biomass is exactly the same as with with previous app, only here a full projection is run (to save you from clicking over and over).

Every time you click **Run projection**, a new projection, or replicate, will be run.
The first time you click **Run projection** a table of results will appear underneath the HCR.
This table tells you the biomass, catch and relative CPUE in the final year of the projection.
Every time you run a projection a new row is added to the top of the table that gives the results for that replicate. 
The first row of the table tells you the average value and the range of values across all the replicates you have run.
The range of values is not the minimum and maximum values, but the range in which 90% of the values fell into (so it includes most of the full range, but ignores any odd, far out values).

## Including variability

When you first open the app, there is no uncertainty in the projections.
This means that each projection you run, and every row in the results table, will be exactly the same.
This is not very helpful and does give not a complete picture as to the performance of the HCR.

As we saw in the previous app, uncertainty can be included in the projection in two ways: through biological variability and estimation error.

Biological variability is the natural variability in the biological processes of the stock (e.g. recruitment and growth).
Increasing the variability will increase the 'bumpiness' of the stock trajectory as the numbers of fish in the next time period will be affected by this random variability.

Estimation error is also important.
The true stock status (e.g. abundance) of a fish stock is never known (because it is impossible to count all of the fish!).
Instead, the stock status must be estimated, for example, by using a stock assessment model.

It is important to note that an HCR can only use the *estimated* value of stock status, not the true value (which is never known).
This means that if the biomass is estimated poorly, the resulting catch limit set by the HCR may not be appropriate.
For example, if the estimated biomass is higher than the true biomass, the catch limit set by the HCR might be too high.

The variability options are initially turned off.
The options can be seen by clicking on the **Show variability options** box.

## Running projections with variability

A good start point is to set the biological variability to 0.2.
Each projection will now be different, as the biological processes will be slightly different in each one.
Click **Run projection** several times to see this.
In the time series plots of catch, biomass and relative CPUE, the most recent projection is in black, the previous ones are grey.

Each row of the results table will be slightly different. 
The average and range of values are shown in the first row of the table.
You will see that each time a projection is run this row gets updated.

When you reach 50 or more replicates (that's a lot of clicking!) you will see that the plots of catch, biomass and relative CPUE change slightly.
Instead of lots of grey wiggly lines for each replicate, you get a grey ribbon.
The width of the ribbon represents the range of values. Most (90%) of the the grey wiggly lines will fit within this ribbon.
Using a ribbon makes interpreting the results easier than trying to make sense of loads of individual lines.
Also plotted on the ribbon is the last replicate (solid line) and the average value (dashed line).

When you have enough replicates you will see that, as you add more, the average and range values in the results table no longer change very much (around 50 might be enough, more if you want...).
This means that although each replicate is different, the summary results across all replicates have settled down.
You can now look at the *performance indicators*.

## Performance indicators

If you click on the **Performance indicators** button the results table changes.
You will see a table of performance indicators calculated across all the replicates you have run.

Each row in the table is a different performance indicator and measures some aspect of the HCR's performance.

The previous table only gave us results in the final year.
However, to fully understand the performance of an HCR it is important to fully evaluate it over the full projection period.
The performance indicator table gives the results over three different time periods: short-, medium- and long-term.
These time periods can be seen on the time series plots of catch, biomass and relative CPUE as vertical dashed lines.

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

Try different HCRs and see how they perform.
You will find that not all HCRs will be good at all performance indicators. Some may have higher catches, while having lower CPUE (catch rates), for example.
Another consideration is how the performance indicators change over time.
Some may be better in the short-term than the long-term, for example.

Managers and stakeholders will prefer the HCRs that have the best chance of meeting the fishery objectives.
However, different managers and stakeholders may have different objectives.
Finding an HCR that everyone is happy with may not be easy and will require discussion and compromise.

See the next app, **Comparing performance** for more information.



