# Performance Indicators and Management Procedures Explorer (PIMPLE)

## What is PIMPLE?

PIMPLE is a tool for exploring and comparing the performance of alternative candidate Management Procedures (MPs).

A Management Procedure has three components:

1. Data collection
2. Analytical method (stock assessment model)
3. Harvest Control Rule (HCR)

For these results, the data collection and stock assessment model are the same for each MP, only the HCRs are different.
The goal is to find the HCR that is most likely to meet your management objectives.

The performance of each HCR is measured using different performance indicators (PIs).
PIMPLE can be used to explore and compare the PIs in a number of ways, using different plots and tables.
This allows trade-offs between the different HCRs to be evaluated.

The focus should be on the *relative* performance of the different MPs, e.g. MP X results in higher catches than MP Y.

## Model Areas

The model used for the evaluations has five areas as seen here:

![](figures/plot_assessment_boundaries.png)

## Performance Indicators

Currently there are seven PIs included in PIMPLE.
The numbers relate to their position in the original table of indicators. These numbers have been kept for convenience.

The average values of the PIs are calculated over three time periods:

* Short-term: 2016-2024
* Medium-term: 2025-2033
* Long-term: 2034-2042

With the exception of SB/SBF=0, all of the indicators have been scaled so that 'bigger is better', i.e. the larger the value, the better the MP is thought to be performing for that PI.

Some of the indicators are calculated over different model areas, or only calculated for certain subsets of fisheries and areas.

### SB/SBF=0

Ranges between 0 and 1.

SB/SBF=0 is the the ratio of the adult biomass to the adult biomass in the absence of fishing (sometimes known as depletion).
It is a measure of stock status and can be compared to the Limit Reference Point (0.2) and the Target Reference Point (0.5, interim).

### 1. Probability of SB/SBF=0 being above the Limit Reference Point (LRP)

This indicator ranges between 0 and 1.
The higher the value, the smaller the chance of the stock falling below the LRP.
For example, a value of 1 means that there is no chance of falling below the LRP and a value of 0.9 means that there is a 1 in 10 (0.1) chance of falling below the LRP.

Higher values are preferred, depending on the level of risk that managers and stakeholders are prepared to accept.

### 3. Catches

The catches are presented as relative to the average catches in the period 2013 to 2015.
They are calculated over several groupings, based on area and fishery.

* Total catch over all areas and fisheries
* Purse seine catches in areas 2, 3 and 5
* Catches from all fisheries in individual areas

### 4. Relative Catch per Unit Effort (CPUE)

The CPUE is only calculated for the purse seines in areas 2, 3 and 5 (excluding the associated purse seines in area 5).
The CPUE is presented as relative to the CPUE in 2010.

### 6. Catch stability

Catch stability measures how variable catches are in time. 
The indicator ranges between 0 and 1. A value of 1 means that the catches do not change over time, i.e. they are constant.
A value of 0 means that the catches vary relatively strongly over time.

The indicator has been calculated over the same groupings as described for PI 3 (catches).

Note that although the stability of catches can be compared across time different periods and HCRs, it cannot be compared between area groupings. 
For example, you cannot compare the stabilities in area 1 to the stabilities in area 2.

This indicator has also been calculated as the variability of relative catches (catches relative to the average catches in the period 2013 to 2015) over time.
The higher the variability, the more the catches change over time.
A variability of 0 means that the catches are not changing over time.

### 7. Effort stability

This is similar to PI 6 (catch stability).
It measures the stability of effort relative to the effort in 2010.
It is also possible to investigate the variability, as well as the stability.

As with PI 4 (Relative CPUE), this indicator is only calculated for the purse seines in areas 2, 3 and 5 (excluding the associated purse seines in area 5).

### 8. Proximity of SB/SBF=0 to the Target Reference Point (TRP)

This indicator measures how close, on average, the SB/SBF=0 was to the TRP in each year.
The higher the value of the indicator, the closer SB/SBF=0 was to the TRP in each year, on average.
A value of 1 means that the SB/SBF=0 was exactly at the TRP in each year.
A value of 0 means that the SB/SBF=0 was as far from the TRP as possible in each year.

Note that if the average value of SB/SBF=0 over a range of years is close to the TRP it does not necessarily mean that SB/SBF=0 was close to the TRP during those years.
For example, if SB/SBF=0 oscillated between 0.3 and 0.7 each year, the average value would be 0.5 but it would never have been very close to 0.5 in any year.

## Plot Types



          
