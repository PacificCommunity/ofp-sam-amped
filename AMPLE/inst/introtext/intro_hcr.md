[//]: # (Information and instructions for the Introduction to HCRs app)
[//]: # (intro_hcr.md)
[//]: # (Author: Finlay Scott (SPC) <finlays@spc.int>)
[//]: # (Soundtrack: Dear Esther (Original Game Soundtrack) by Jessica Curry)
[//]: # (Distributed under the terms of the GNU General Public License GPL (>= 3))

# Harvest Control Rules

## Introduction

This is the first app in the **AMPLE** package.
It is the best to start if you are unfamiliar with HCRs.

This app attempts to introduce the basic ideas behind a Harvest Control Rule (HCR).
An HCR is part of a Management Procedure, along with the data collection and estimation method.
In this app we only focus on HCRs.

You can see that there are three plots: catch (top left), biomass (bottom left) and the HCR (the top right). 

The current HCR can be seen in the top right window of the app as the red line.
The default HCR in the app is a *Threshold catch"* style of HCR (sometimes called a hockey stick).
It determines the catch limit in the next time step based on the status on the stock.
The shape of the HCR is set using four parameters that can be adjusted using the sliders in the left-hand panel:
*Blim*, *Belbow*, *Cmin* and *Cmax*.
Changing the parameters changes the shape of the HCR (try it!) and will reset the current projection.

The *Threshold catch* HCR takes the *estimated* value of biomass as the input.
The biomass can be seen in the biomass plot at the bottom left of the app.
Here, the biomass has been scaled to be between 0 and 1, where 0 is no biomass (and no stock!) and 1 is the biggest biomass possible (occurring when in there is no fishing).

You can see how the current level of biomass is used in the HCR by following the blue arrow from the biomass plot to the HCR plot.
The HCR uses the biomass input to set the catch limit in the next time step.
The vertical blue dashed line on the HCR plot shows the current estimated value of the stock biomass.
Where this vertical blue line meets the red line of the HCR gives the next catch limit.
The horizontal blue dashed line is the catch limit set for the next time step.

The horizontal, blue dashed line is also shown on the catch plot.
It shows what the catch will be in the next time step.

## How it works

When you start the app there are already several years of historical catch and biomass data.
We can start moving into the future, one year at a time.
In each year, the HCR will set a new catch limit, based on the current estimated biomass.
The process moves anti-clockwise around the main panel.

Pressing the **Advance** button steps the projection forward by one time step.
Fishing happens by catching the catch limit set by the HCR (you should see that the next catch goes to where the blue dashed line was).
The stock responds to being fished and the biomass is updated to the next year based on the level of fishing and the population dynamics of the stock.
The HCR then uses the current biomass of the stock to set the next catch limit.

By repeatedly pressing the **Advance** button you can see how the stock level changes and how the catch limit is set by HCR, depending on the stock level.

Note that the shape of the HCR does not change through the projection.
You decide at the beginning of the projection what the shape of the HCR should be (a big decision!), then see what happens as it is applied every year.

The ghosts of past catch limits are shown as grey dashed lines on the catch plot and as grey dots on the HCR plot. These allow you to see which parts of the HCR have been active. 

Pressing the **Reset** button resets the projection. Choose another shape for your HCR and try again.

# Variability

Variability is an important factor when considering how HCRs perform.
The real world is very noisy and full of uncertainty. It is important that this uncertainty is included when considering how an HCR might perform.
When you have no variability in the projection, you will get the same projection each time (if the shape of the HCR is kept the same).

Variability can be included in the projection in two ways: biological variability in the stock dynamics (e.g. natural variability in recruitment) and through differences between the estimated level of stock biomass and the 'true' level.

These options are initially turned off. The options can be seen by clicking on the **Show variability options** box.

Biological variability is the natural variability in the biological processes of the stock (e.g. recruitment and growth).
Increasing the variability will increase the 'bumpiness' of the stock trajectory.
For example, increase it to 0.2 and run some projections.
You will get a different projection each time.
As biological variability is always encountered in fisheries it is important that a selected HCR is robust to the variability.

Estimation variability is also important.
The true stock status (e.g. abundance) of a fish stock is never known (because it is impossible to count all of the fish!).
Instead, the stock status must be estimated, for example by using a stock assessment model.

It is important to note that an HCR can only use the *estimated* value of stock status, not the true value (which is never known).
This means that if the biomass is estimated poorly, the resulting catch limit set by the HCR may not be appropriate.
For example, if the estimated biomass is higher than the true biomass, the catch limit set by the HCR might be too high.

Estimation variability and estimation bias is used in this app to simulate the estimation process.
They can be adjusted to create differences between the true level of the biomass and the estimated level.
Estimation bias represents situations where the biomass is consistently over or under estimated (e.g. my estimated biomass is always 10% higher than the true value).
Estimation variability is just a crude way of introducing a random difference between the true and estimated values.

When estimation variability or bias is active the biomass plot shows two lines.
The black line shows the true biomass, the blue line shows the estimated biomass.
It is the blue line that feeds the HCR.
Increasing the estimation bias and variability will increase the difference between these lines.

If you change the values for estimation variability and bias you will see changes in the performance of the HCR.
This is why HCRs are designed to be used with a specific estimation process, so you consider the estimation process when designing the HCR.
It's also why testing the HCR using simulations is very important.

