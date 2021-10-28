# Measuring performance

## Introduction

This is the second app in the **AMPLE** package. The first app, *Introduction to Harvest Control Rules (HCRs)* explores how HCRs work.
If you are unfamiliar with HCRs, you should start there.

This app explores how the performance of a proposed HCR can be measured.
In particular, the impact of uncertainty on the HCR is investigated.

Before a HCR is adopted for use in the real world its performance is tested and evaluated using computer simulations (known as Management Strategy Evaluation - MSE).
During these evaluations, the performance of a proposed HCR is measured using a range of indicators, known as performance indicators.
They measure things like expected catches and catch rates in the future and the probability of the stock falling below the Limit Reference Point.
These indicators should relate to the management objectives of the fishery, e.g. good economic performance, stock sustainability,etc.

We saw in the *Introduction to HCRs* app that uncertainty, such as biological variability,  can affect the performance of a HCR.
For this reason, when evaluating an HCR the evaluations are performed many hundreds of times.
Each evaluation is known as a replicate, or an indicator.
Due to the uncertainty, each replicate will be different.
To fully understand the performance of the HCR it is necessary to consider all of the replicates together, as this gives a better idea of how the HCR will perform in the real world.

Performance indicators are calculated for reach replicate and summaries, such as the average and ranges of values are presented.

## Using the app

The layout of this app is slightly different to the **Introduction to HCRs** app.
The HCR is still at the top right, and the controls for changing the shape are still in the left hand panel.
There are now three time series plots to the left of the HCR. They are *catch*, *biomass* and *relative CPUE*.
Catch and biomass we saw in the previous app.
*Relative CPUE* is the catch-per-unit of fishing effort (CPUE, sometimes called the catch rate), relative to the CPUE at the last historical value. 
This gives it a value of 1 in the last historical time step.
Using a relative measure allows to easily compare the future values with those in the past.

In the **Introduction to HCRs** app, each time you clicked **Advance**, you projected forward by a single time step.
In this app when you click **Run projection** you will run a full projection for all time steps (try it).
The process of the HCR setting the catch limits using the estimated biomass is exactly the same as with with previous app, only here a full projection is run (to save you from clicking over and over).

Everytime you click **Run projection**, a new projection, or replicate, will be run.
When you run a projection, a new line will appear in the table underneath the HCR that tells you biomass, catch and relative CPUE in the final time step of the projection.
A row at the bottom of the table tells you average value and range of values across all the replicates you have run.
The range of values is not the minimum and maximum values, but the range in which 90% of the values fell into it (so it ignores any odd, far out values).

## Including variability

When you first open the app, there is no variability in the projections.
This means that each replicate will be exactly the same. This is not very helpful.

As we saw in the previous app, variability can be included in the projection in two ways:

* through biological variability
* through estimation variability (where the estimated value of the stock status used by the HCR is different to the 'true' stock status)

These options are initially turned off.
The options can be seen by clicking on the **Show variability options** box.


        p("Biological productivity variability represents the variability of the natural procesess of the stock, for example growth and natural mortality. Increasing the variability will increase the 'bumpiness' of the stock trajectory. As biological variability is always encountered in fisheries it is essential that a selected HCR is robust to the variability."),
        p("Estimation error simulates the difference between the true level of the stock biomass and the estimated level. Unfortunately, the true abundance of a fish stock is never known. Instead, estimates of abundance are made, for example using stock assessment models. The HCR uses the estimated biomass, not the true biomass. This means that the catch limit that is set by the HCR is based on estimated biomass. If the biomass is estimated poorly the resulting catch limit set by the HCR may not be appropriate."),
        p("Here, estimation error is modelled using two different processes: random error and consistent bias (positive or negative). The bias represents situations where the biomass is consistently over or under estimated."),
        p("When estimation error is active the biomass plot shows two lines. The black line shows the true biomass, the blue line shwows the estimated biomass. It is the blue line that feeds the HCR. Increasing the estiamation bias and variability will increase the difference between these lines."),
        h1("Tutorial"),
        p("A more detailed tutorial can be found at these links:"),
        a("Tutorial (pdf)",target="_blank",href= "introIndicators.pdf"), 
        br(),
        a("Tutorial (html)",target="_blank",href="introIndicators.html") 
