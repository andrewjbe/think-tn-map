# PDFs

## General
* ~~I updated the description/definition of many metrics the last time I sent the master file of data. Can you make sure that the descriptions/definitions are updated throughout the PDFs? Reattached for convenience.~~
    * ~~Remove the 1 and 1.1, 1.2 markers alongside section headers on all three PDFs~~
    * ~~Our official term for this project is the “State of the Counties Dashboard” Can you make the title on all three PDFs say “2024 ThinkTennessee State of the Counties Dashboard” instead of Data Map~~
        * ~~Make sure that there is no space between Think and Tennessee~~

## One County, All Metrics
* ~~Can you make sure that any metrics with percentage values have a single decimal point? And any metrics with dollar values do not include any decimals/cents?~~
    * ~~Can you unabbreviate Avg. and make sure it says Average in the column headers. Just want to be clear.~~

## One Metric, All Counties (Metric Summary)
* ~~Relabel “value in county” column to  “County Value”~~
* ~~Can you unabbreviate Avg. and make sure it says Average in the column headers. Just want to be clear.~~
* ~~Can you swap the county rank so that it is the second column from the left, and “County Value” is the third column from the left?~~
    * ~~The title of this one underneath the header should say Metric Summary to match the header title. In other words, they should both say Metric Summary in the header and in bold.~~

## County Fact Sheet
* ~~Generally, the font size is too large, the graphics take up too much space, and the spacing is a little too wide. It’s currently at five pages, and I think we’d like to narrow it down to four or three. Here are some steps to take that can help, but feel free to take other steps.~~
    * ~~On the first page, lets change the opening header from “Sevier County Summary” to “Sevier County Demographics”, and go straight to the graphics. We can cut out the report overview language for the sake of brevity. We can also cut out the introductory sentences in each paragraph.~~
    * ~~We need to make a way to scale down the charts, they currently take up too much space on each page. We can narrow the width of each chart, for example, by including a value label for each chart element, as opposed to displaying the y axis labels.~~ 
        * ~~Ideally, for example, Economy and Working Families and Debt and Household Finances should be on one page. Maybe we can reorient the sections and condense them this way?~~
    * ~~Demographics + Civic Engagement and Criminal Justice (pg. 1)~~
    * ~~Economy and Working Families + Debt and Household Finance (pg. 2)~~
    * ~~Affordability (pg. 3)~~
* ~~Make sure that all percentages have single decimal point and dollar values have no decimals.~~
* Any raw number should be rounded to a whole number (e.g. Election Day Polling Places to Voters Ratio and Work Hours for a 2BR Rental Unit,  but not Debt to Income Ratio or mean travel time to work)

# WEBSITE

* ~~Can you move the comparison metric button to enable the comparison function to the top of the compare stats window?~~
* ~~And change the language of the button to “Enable Comparison Tool”~~


MAP SCALES

    * Can you toy with some of the color scales for the following maps, Im not really okay with how they are being displayed.
        * High values in one county depress the color variation for all other counties
            * Health - Black Uninsured Rate (A county with an uninsured rate below 10% should not be the same color as a county with a rate greater than or equal to 10%)
            Housing - Average Home Sale Price ( A home sale price of $300k or so should not be the same color as a county with $200k, so I guess try to divide them up into specific categories by $100k increase between each color)
            Housing - Renter Growth - African Americans (Counties with a 7% growth and a 55% growth have the same color. Can we run it without Cumberland and Claiborne and add those manually to the good green color after the fact?)
            Housing - Renter Growth - Hispanics/Latinos (Counties with a 13% growth have the same color as a 55% growth. This metric is difficult because these are estimates so they swing wildly, can you do the same within and remove Hardin and Lewis and try to even out/redistribute the colors, then add those back in?)
            Economy - GDP Growth (maybe 5% intervals would display more variation?)
            Economy - Median Household Income (a county with 40k should not be the same color as a county with a median HH income of 50k, same with one county at 60k and others at 70k)
            Environment & Energy - Renewable Energy Production (Counties with a 0 MW production should not be the same color as a county that generates 30 MW, or a county that generates 100MW)
            Criminal Justice & the Courts - Prison Capacity ( counties with 91% capacity are the same color as those with 52% capacity)
            Elections - Nonprofit Giving (counties with $500 per capita and $200 per capita should not be the same color)
            Elections - Provisional Ballots Rejected
        Color scale is just off slightly
            Post High School Educational Attainment (too many counties are red just because Williamson has attainment rates. Can we adjust it to be a little more middle of the road)