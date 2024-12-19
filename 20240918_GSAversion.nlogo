;; 3spire 30-5-2023
;; Project for this version: Find out why and remove - if any - artefacts causing households to have very large negative incomes
;; author: Aleid Sunniva Teeuwen


;; try:

;; Improving:
;; - add change to fallow as a strategy --> works but nobody chooses it
;; - select different households each repetition

;; Debugging:
;; -

;; Cleaning
;; - remove unused characteristics


extensions [nw csv table]

globals[
  yvar ; temporal variability of yield (sd in %)
  ymu ; trend towards lower or higher yields? mean increase/decrease over the simulation period (in %)
  ymult ; deviation from expected yield (in %)

  pvar ; same but for price
  pmu ; "
  pmult;

  forgetfullness; vector with values 0.5, 0.75, 0.9 for example <- HOW TO USE THESE IN AN ASK command? GET?
  area-field; area of one field agent
  cowiter; dummy used in loop function
  cowid; dummy used in loop function
  all-cattle; dummy list used in loop function
  gini
  sorted-weights
  selected-weights

  ; financial
  global-maize-price
  global-veg-price
  global-butter-price
  global-coffee-price
  global-supplementary-feed-price
  global-cow-sell-price
  global-cow-purchase-price
  global-bull-sell-price
  global-heifer-sell-price
  global-oxen-sell-price
  global-coffeeplantation-price
  global-herbicides-price
  global-fertiliser-price

  ; technical
  milk-to-butter-conversion-factor
  utilisable-pasture
  utilisable-maizestover
  utilisable-vegresidues
  utilisable-coffeehusk
  utilisable-coffeeleaves
  feed-from-maize
  feed-from-coffee
  maize-yield-low
  maize-yield-moderate
  maize-yield-high
  maize-yield-veryhigh
  veg-yield-low
  veg-yield-moderate
  veg-yield-high
  veg-yield-veryhigh
  coffee-yield-low-establishment
  coffee-yield-moderate-establishment
  coffee-yield-high-establishment
  coffee-yield-veryhigh-establishment
  coffee-yield-low-initiation
  coffee-yield-moderate-initiation
  coffee-yield-high-initiation
  coffee-yield-veryhigh-initiation
  coffee-yield-low-full
  coffee-yield-moderate-full
  coffee-yield-high-full
  coffee-yield-veryhigh-full
  coffee-yield-low-ageing
  coffee-yield-moderate-ageing
  coffee-yield-high-ageing
  coffee-yield-veryhigh-ageing
  maize-N-moderate
  maize-N-high
  maize-N-veryhigh
  veg-N-moderate
  veg-N-high
  veg-N-veryhigh
  coffee-N-moderate
  coffee-N-high
  coffee-N-veryhigh

  list-of-three ; dummy
]

breed [households household]
breed [fields field]
breed [cattle cow]
breed [cities city]
breed [members member] ; household members

undirected-link-breed [mycows mycow]
undirected-link-breed [mycalves mycalf]
undirected-link-breed [myfields myfield]
undirected-link-breed [myfarms myfarm]
undirected-link-breed [mymembers mymember]
undirected-link-breed [friendships friendship]
directed-link-breed [influencers influencer]

friendships-own[weight]

households-own [

  ;;household characteristics
  name
  household-size;
  candidate-weight; needed to form friendships
  network-size;
  children;
  elders;
  adults;
  savings;
  food-requirement;
  workload
  region
  woreda

  ;; farm inputs
  fertiliser;

  ;; productive means
  labourcap; max hours a household can work per week
  tlu;
  herd;
  oxen;
  cows;
  calves;
  number_fields;
  farmland; area of agricultural land
  maizeland; area grown with maize
  vegetableland; area grown with maize
  privatepasture; area grown with pasture
  coffeeland; area grown with coffee
  fallowland; area left fallow

  ;; choices
  chosen; is yes when a strategy has been chosen, remain no otherwise
  chosen-strategy; a string with the chosen strategy
  testing-strategy; a string with the strategy a household is testing out at that moment or tested out last
  adopted-feed?; yes if feed has been adopted
  order-strategies; a list of the scores of the strategies in the order
  loop-order;
  known-strategies; a list of know strategies
  possible-strategies; a list of strategies that are "allowed" (no restrictions make them impossible)
  strat-dummy
  pos; to keep track of which position strategies should have in order-strategies list
  testincomeoutcomes; when households go through the loop, the expected income outcomes are stored in this list --> order is the same as the loop-order
  testfoodoutcomes; when households go through the loop, the expected food outcomes are stored in this list --> order is the same as the loop-order
  pastureincomeoutcomes
  pasturefoodoutcomes

  ;; aspirational outcomes
  maizeproduction; production of maize current year
  vegproduction; production of maize current year
  milkproduction; production of milk current year
  milk-consumed
  butterproduction; production of butter current year
  coffeeproduction; production of coffee current year
  coffeesales ; how much farmers earn from coffee without substracting costs
  feedproduction;
  maizeprofit; profit from maize sales current year
  maizesales; earnings from sales of maize current year
  maizepurchase; the maize farmers purchase (only if they do not grow enough themselves)
  maize-consumed
  maize-left; need it for now to work out code
  veg-left;
  milk-left;
  maizecost; cost of producing maize
  vegprofit; profit from vegetable sales current year
  vegsales; earnings from sales of vegetables current year
  vegcost; the cost of producing vegetables current year
  veg-consumed; vegetables consumed annually, kg
  butterprofit; profit from butter sales current year (supplementary feed subtracted)
  supplementaryfeedcost ; price of supplementary feed per year
  butter-consumed; the amount of butter consumed by household (kg per year)
  buttersales; earnings from butter sales without subtracting costs
  cowsales; earnings from selling cows (if this option is chosen)
  cowcost; cost of buying new cattle
  bullsales; profit from bull sales current year
  oxensales; profit from oxen sales current year
  coffeeprofit; profit from coffee sales current year
  coffeecost; cost of producing coffee current year
  coffee-consumed; coffee consumed by household per year kg
  laboursum; the sum of the labour cost of all activities
  value-of-consumed-products ; the yearly economic value of the products that households consume
  food-purchases
  costs-of-production
  earnings-from-sales

  leisure-outcome-t3; aspiration outcome three years ago
  leisure-outcome-t2; aspiration outcome two years ago
  leisure-outcome-t1start; aspiration outcome one years ago
  leisure-outcome-t1end; aspiration outcome end of time step
  leisure-outcomes-past

  food-outcome-t3; aspiration outcome three years ago
  food-outcome-t2; aspiration outcome two years ago
  food-outcome-t1start; aspiration outcome one years ago
  food-outcome-t1end; aspiration outcome end of time step
  food-outcomes-past

  income-outcome-t3; aspiration outcome three years ago
  income-outcome-t2; aspiration outcome two years ago
  income-outcome-t1start; aspiration outcome one years ago
  income-outcome-t1end; aspiration outcome end of time step
  income-outcomes-past

  ;; aspiration thresholds
  leisure-threshold-t3; thresholds three years ago
  leisure-threshold-t2; thresholds two years ago
  leisure-threshold-t1start; thresholds one years ago
  leisure-threshold-t1end; thresholds at end of this years time step
  leisure-thresholds-past

  food-threshold-t3; thresholds three years ago
  food-threshold-t2; thresholds two years ago
  food-threshold-t1start; thresholds one years ago
  food-threshold-t1end; thresholds at end of this years time step
  food-thresholds-past

  income-threshold-t3; thresholds three years ago
  income-threshold-t2; thresholds two years ago
  income-threshold-t1start; thresholds one years ago
  income-threshold-t1end; thresholds at end of this years time step
  income-thresholds-past

  leisure-threshold-mean; weighted mean of past thresholds
  food-threshold-mean; weighted mean of past thresholds
  income-threshold-mean; weighted mean of past thresholds

  food-relativeimportance; relative importance of food aspiration compared to other aspirations
  income-relativeimportance;
  leisure-relativeimportance;

  weighted-utilities

  ;; expected aspirational outcomes calculated when assessing different strategies
  test-maizeproduction
  test-maizesales
  test-maizecost
  test-maizeconsume
  test-vegproduction
  test-vegsales
  test-vegcost
  test-vegconsume
  test-feedproduction
  test-herd
  test-milkproduction
  test-milkconsumed
  test-butterproduction
  test-buttersales
  test-cowcost
  test-feedcost
  test-cowsales
  test-bullsales
  test-oxensales
  test-coffeeproduction
  test-coffeesales
  test-coffeecost
  leisure-expectation
  leisure-expectations ; expectations of all strategies stored in a list
  food-expectation
  food-expectations
  income-expectation
  income-expectations

  ; aspiration gaps
  income-gap
  food-gap
  leisure-gap
  satisficed; yes if all gaps > 0, no otherwise

  ; experience-weights
  ew-income
  ew-food
  ew-leisure

]

fields-own[
  field-size;
  current-land-use; maize, coffee, pasture, or pasture
  past-land-uses; vector with land uses of past years
  coffee-age; -1 if not coffee, otherwise age in years
  owned-by ; who-id or -1 if not owned by anyone
  yield; production per ha
  production; production in total (not per ha)
  labour;
  fertiliser; can be 0 (none), 88 (moderate), 145 (high), or 209 (very high)
  herbicides; true or false

  ; test characteristics for assessing expected outcomes of strategies
  test-production
  test-yield
]

cattle-own[
  manure; in kg
  age;
  castrated;
  value;
  feed-quantity;
  supplementary-feed; yes or no
  pasture-quantity;
  productivity;
  milkyield;
  pregnant; yes or no
  sex; male or female
  in-lactation; yes or no
  lactation; the number of lactations the cow has had
  dry;
  calf; the number of calves the cow has had
  bought? ; will be true if the cattle were purchased and false if the cattle owned at the beginning of the simulation or born
]

patches-own [
  ; soil
  ; climate
  farmstead ; 1 if there is a household on it, 0 otherwise
  afield ; 1 if there is a field on it, 0 otherwise
]

cities-own [
  xcoord ; because I don't want to actually place the city on the map
  ycoord ; because I don't want to actually place the city on the map
  seed-price-modern ;
  cattle-prices
  milk-prices
  butter-prices
  maize-price
]

members-own[lifestage]

;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup                                    ; Model initialisation

  clear-all
  file-close-all
  reset-ticks

  if fix-seed? [ random-seed Repetition-nr] ; If model user chooses fix-seed, then the random seed is the same across repetitions
  test-lu                                   ; Test if the filled in global values are OK. If they are not, the simulation will stop and the user will get a warning

  print ""
  print word "NEW RUN started at " date-and-time
  print "=============================================================================="

  fix-global-variables                      ; Set all pre-defined global variables

  determine-climate-market-lu               ; Determine temporal trends and variability in yields and prices, and land use --> depending on model users choice (Draw-random?) these are either drawn from wide but conceivable and representative ranges (if TRUE), or the input of the model user is used directly (if FALSE)

  ifelse Fix-chars? [                       ; Read households with household characteristics from csv.
    read-fixed-households-from-csv          ; If model user chooses to Fix-chars, then the household characteristics drawn from fitted distributions will be fixed
  ][
    read-households-from-csv                ; Otherwise, they will be different each simulation run (or each repetition if the seed is fixed)
  ]

  create-fields-cattle-hhmembers            ; Create fields, cattle and household members in line with household characteristics as read from csv

  assign-stochastic-hhchars                 ; Here, all chaaracteristics drawn from fitted distrubtions (aspiration-thresholds, relative importance and network size) are determined

  read-fixed-matrix                         ; Here a csv file containing the similarity (or difference) between all households
  select-friends                            ; Here households are connected, based on homophily, forming a social network

  ; write to file
  report-initial-data                       ; Here the initial conditions of this simulation run and the households in it are noted down. This will only happen if write-file?

  reset-ticks

  print "setup done"
end

;; RUN SIMULATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  print word "year:" ticks
  ask households [

    ; STEP 1: update aspirations
    update-aspiration-outcomes                                             ; Update aspiration outcomes
    update-aspiration-thresholds                                           ; Update aspiration thresholds
    calc-new-aspiration-thresholds                                         ; Calculate new aspiration thresholds

  ]

  ; STEP 2: Determine order of livelihood improvement strategies
  ask households [                                                         ; Start with clean slate; households have not yet chosen a strategy
    set chosen "no"
    set chosen-strategy 0
    set color white
  ]

  ; Determine which order households should loop through strategies in. They will consider the strategies that are most similar to their current management first. Strategies unknown to households will not be considered. If two or more strategies are equally similar, they will be ordered randomly.
  if (Strategy-order = "defined-by-similarity") [determine-loop-order]
  if (Strategy-order = "random") [ask households [set loop-order shuffle known-strategies]]

  ask households with [satisficed = "no"][
    foreach loop-order [                                                   ; Remove strategies that the farmers do not know
      x -> if member? x known-strategies = false [set loop-order remove-item (position x loop-order) loop-order]
    ]
  ]

  ; All households that are not yet satisficed will loop through strategies till they find a satisficing solution.

  ;ask households [if who = test-hh  [print (word "is household " who " satisficed? " satisficed)]]

  ask households with [satisficed = "yes"][
    set-testing-vars-na
  ]

  ask households with [satisficed = "no"][
    if (Search-method = "random draw") [choose-random]
    if (Search-method = "sequential loop") [choose-strategy-satisficing2]
    if (Search-method = "optimised selection")  [choose-the-best]
  ]

  ask cattle with [color != orange] [
    set color ifelse-value (sex = "female")[pink + 2][sky + 2]]            ; Restore color changed when looping through and testing different strategies

  ; STEP 5: Perform strategies --> what is the climate and market like this year?
  let ymu-t (ymu / 24) * ticks
  let pmu-t (pmu / 24) * ticks

  set ymult random-normal ymu-t yvar
  set pmult random-normal pmu-t pvar
  set ymult ifelse-value ymult < -1 [-1][ymult] ; negative yields are not possible --> min yield = 0
  set pmult ifelse-value pmult < -1 [-1][pmult] ; we assume that farmers will not pay money for people to take their produce --> min income from sales = 0

  perform-chosen-management                                                ; Implement the baseline management and (if any) strategies selected in the previous step

  ; update characteristics
  update-hh-characteristics                                                ; Land use and livestock
  calc-aspirational-outcomes                                               ; Leisure, income & and other financial indicators, and food self-sufficiency

  ; STEP 6: Simulate the transfusion of knowledge through farmers social networks
  share-knowledge

  ;calc-gini                                                               ; Calculate population level inequality (gini-index). This does not work because incomes can be negative. Need to find a different indicator or adapt it to allow for negative incomes

  tick

  if write-file? [
    let tickfile (word "D:/Data/Personal/TeeuwenAS/3spire/Netlogo_Output/SAtickfiles_quicker/" ; path on remote
      substring Strategy-order 0 3 "_"
      substring Search-method 0 3 "_"
      (ifelse-value Utility-calculation = "rank-sum" ["RS"] Utility-calculation = "weighted-sum" ["WS"] Utility-calculation = "cobb-douglas+" ["CBt"] Utility-calculation = "cobb-douglas*" ["CBx"] Utility-calculation = "cobb-douglasln" ["CBln"] [Utility-calculation]) "_"
      (ifelse-value Threshold-type = "dynamic" ["dyn"] Threshold-type = "static" ["stat"] Threshold-type = "infinite" ["inf"]) "_"
      (ifelse-value Threshold-adjustment = "internal only" ["in"] Threshold-adjustment = "internal and external" ["ie"] Threshold-adjustment = "external only" ["ex"]) "-"
      Income? "_" Food? "_" Leisure? ".txt") ; Income? Food? and Leisure? combined are the aspirational dimensions deemed relevant by user = parameter/process

    file-open tickfile
    write-to-tickfile false
    file-close
  ]                                           ; Record model and household characteristics each year and write to a csv file.


end

;; FUNCTIONS used in setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to fix-global-variables

  ; prices (global)
  set global-maize-price 10                                       ; average domestic maize price per kg over the last years (2009-2018) was 4.8 birr (Minten 2020b, p. 290), but the since the conflict and the drought it has been reported being >20 birr at wholesale (http://www.egte-ethiopia.com/en/in-t-market/commodity-statistics.html)
  set global-veg-price 30;
  set global-butter-price 200 ;                                   ; 112 etb according to Gebremedhin 2014. "Developing the butter value chain in Ethiopia". Adjusted upwards to account for inflation
  set global-coffee-price 140                                     ; according to Diro 2019 ; parameter
  set global-supplementary-feed-price 142.92 * 12                 ; price of buying supplementary feed
  set global-cow-sell-price 5286                                  ; source: socio-economic household survey 2018-2019, r-script: cattle_prices.R || before it was 600 = what farmers are likely to get for selling cow at the local market (= lower end of price range), source: Anteneh 2010 ; parameter (seems quite cheap, adjust for inflation?)
  set global-cow-purchase-price 6090                              ; source: socio-economic household survey 2018-2019, r-script: cattle_prices.R || before it was 1600 = cost of buying a cow at the local market (= higher end of price range), source: Anteneh 2010 ; parameter (seems quite cheap, adjust for inflation?)
  set global-bull-sell-price 8376                                 ; source: socio-economic household survey 2018-2019, r-script: cattle_prices.R || before it was 6000 but I am not sure where I got that number from
  set global-heifer-sell-price 3510                               ; source: socio-economic household survey 2018-2019, r-script: cattle_prices.R
  set global-oxen-sell-price 8730                                 ; source: socio-economic household survey 2018-2019, r-script: cattle_prices.R || before it was 10000 but I am not sure where I got that number from
  set global-coffeeplantation-price 79921                         ; etb per ha --> reference = Diro 2019 ; parameter
  set global-herbicides-price 3000                                ; etb per L and for 1 L per ha, source: https://addisfortune.news/govt-targets-agrochemical-market-shake-up-importers-incensed/
  set global-fertiliser-price 23.6                                ; etb per kg N, source: Assefa 2021

  ; other (global)
  set milk-to-butter-conversion-factor 16.5                       ; unit = kg. 26 = # weeks in 6 months (assuming that the herd is synchronised), 5 = minimum amount (L) needed for butter making per week. ; About 16.5 litres of milk is required to produce a kilogram of butter (Anteneh 2010, p. 23)
  set utilisable-pasture 0.75 * 2                                 ; see sources on slide 20 C:\Users\TeeuwenAS\OneDrive - Universiteit Twente\Twente\Thesis shared - ABM value chains food security\Meetings\20221011_Supervision meeting
  set utilisable-maizestover 0.58                                 ; utilisable tonnes of stover (DM) per tonne of grain (FM) (Berhanu 2007)93
  set utilisable-vegresidues 0
  set utilisable-coffeehusk 1.05                                  ; utilisable tonnes of husk (DM) per tonne of beans (FM), as a by-product from dry processing of coffee cherries. Husk includes dried pulp, exocarp and hulls ()
  set maize-yield-low 2.718 * 1000                                ; yield units = kg per ha. Source maize yields: van Dijk 2020 & Assefa 2021
  set maize-yield-moderate 3.311 * 1000
  set maize-yield-high 4.356 * 1000
  set maize-yield-veryhigh 4.664 * 1000
  set veg-yield-low 14.9 * 1000                             ; Look back what the source for these were, see ppt presentation C:\Users\TeeuwenAS\OneDrive - Universiteit Twente\Supervision meeting add fertiliser, Sigaye (2022)
  set veg-yield-moderate 38.2 * 1000
  set veg-yield-high 41.9 * 1000
  set veg-yield-veryhigh 56 * 1000
  set coffee-yield-low-establishment 50.34                        ; Look back what the source for these were, see ppt presentation C:\Users\TeeuwenAS\OneDrive - Universiteit Twente\Supervision meeting add fertiliser
  set coffee-yield-moderate-establishment 98
  set coffee-yield-high-establishment 84
  set coffee-yield-veryhigh-establishment 142.2
  set coffee-yield-low-initiation 654.36
  set coffee-yield-moderate-initiation 1218
  set coffee-yield-high-initiation 1512
  set coffee-yield-veryhigh-initiation 1564
  set coffee-yield-low-full 906.04
  set coffee-yield-moderate-full 1694
  set coffee-yield-high-full 2016
  set coffee-yield-veryhigh-full 1864.44
  set coffee-yield-low-ageing 453.02
  set coffee-yield-moderate-ageing 1260
  set coffee-yield-high-ageing 1512
  set coffee-yield-veryhigh-ageing 1121.08
  set maize-N-moderate 88                          ; kg N per ha, Assefa 2021 & "Supervision meeting add fertiliser.pptx"
  set maize-N-high 145                             ; kg N per ha, Assefa 2021 & "Supervision meeting add fertiliser.pptx"
  set maize-N-veryhigh 209                         ; kg N per ha, Assefa 2021 & "Supervision meeting add fertiliser.pptx"
  set veg-N-moderate 46                            ; kg N per ha, Sigaye 2022 & "Supervision meeting add fertiliser.pptx"
  set veg-N-high 92                                ; kg N per ha, Sigaye 2022 & "Supervision meeting add fertiliser.pptx"
  set veg-N-veryhigh 138                           ; kg N per ha, Sigaye 2022 & "Supervision meeting add fertiliser.pptx"
  set coffee-N-moderate 155                        ; kg N per ha, Dawid 2018 & "Supervision meeting add fertiliser.pptx"
  set coffee-N-high 321                            ; kg N per ha, Dawid 2018 & "Supervision meeting add fertiliser.pptx"
  set coffee-N-veryhigh 472                        ; kg N per ha, Dawid 2018 & "Supervision meeting add fertiliser.pptx"
  set forgetfullness (list remember-t3 remember-t2 remember-t1)
  set area-field 1

end

to determine-climate-market-lu

  ; Determine temporal trends and variability in yields and prices, and land use

  ifelse Draw-random? [                     ; If the model user chooses to Draw-random, then random yield and price-trends, and standard deviations around those trends will be drawn from ranges of -100% to +100% and 0% to 100%, respectively.
                                            ; Land use will also be drawn randomly, though maizeland has to be >=25% and all other land uses >=5% to be representative

    ; yields and prices
    set ymu ifelse-value Time-trend? [(random 201 - 100) / 100][0] ; realistic range ~ from -30 to +40 because literature I found projected % changes between these values, see Thomas 2019 and Degife 2020
    set yvar ifelse-value Variability? [random 101 / 100][0] ; between 0 and 100
    set pmu ifelse-value Time-trend? [(random 201 - 100) / 100][0]
    set pvar ifelse-value Variability? [random 101 / 100][0]

    ; b and optimism
    set b-no random 101 / 100 ; between zero and 1
    set optimism (80 + random 71) / 100 ; between 0.8 and 1.5

    ; initial land-use
    set chance-maize      25 + random 61 ; between 0.25 and 0.85
    print word "chance maize " chance-maize
    set chance-pasture    5 + random (86 - chance-maize)
    print word "chance pasture " chance-pasture
    set chance-veg        5 + random (91 - chance-maize - chance-pasture)
    print word "chance veg " chance-veg
    set chance-coffee     5 + random (96  - chance-maize - chance-pasture - chance-veg)
    print word "chance coffee " chance-coffee

    set chance-maize chance-maize / 100
    set chance-pasture chance-pasture / 100
    set chance-veg chance-veg / 100
    set chance-coffee chance-coffee / 100

  ][                                        ; Otherwise, the values the model user filled in will be used

    set ymu ifelse-value Time-trend? [Yield-trend / 100][0] ; For the sensitivity analysis this ranges from -100 to +100. Realistic range may be smaller, -30 to +40, literature I found projected % changes between these values, see Thomas 2019 and Degife 2020
    set yvar ifelse-value Variability? [Yield-variability / 100][0]

    set ymu ifelse-value Time-trend? [Price-trend / 100][0]
    set pvar ifelse-value Variability? [Price-variability / 100][0]
  ]

end

to read-households-from-csv
  file-close-all                                                           ; Close all open files

  let netlogopopfile ifelse-value Different-hhs? [(word "Netlogo_Input/netlogopop" Population-nr ".csv") ] ["Netlogo_Input/netlogopop.csv"]
  print netlogopopfile

  if not file-exists? netlogopopfile [
    user-message (word netlogopopfile "does not exist! Check your repository")
    stop
  ]

  file-open netlogopopfile ; open the file with the turtle data

  while [ not file-at-end? ] [                                             ; We'll read all the data in a single loop
    let data csv:from-row file-read-line                                   ; here the CSV extension grabs a single line and puts the read data in a list
                                                                           ; now we can use that list to create a turtle with the saved properties
    if (item 0 data != "hhid")[                                            ; this is the header
      create-households 1 [
        set name item 0 data
        set xcor    item 1 data
        set ycor    item 2 data
        set size    item 3 data
        set color   item 4 data
        set heading item 5 data
        set shape   item 6 data
        set household-size item 7 data
        set children item 8 data
        set elders item 9 data
        set adults item 10 data
        set tlu item 11 data
        set oxen item 12 data
        set cows item 13 data
        set calves item 14 data
        set farmland item 15 data
        set number_fields item 16 data
        set food-outcome-t1end item 17 data                                ; = maize consumption
        set workload item 18 data                                          ; this is in hours per household per week
        set region item 19 data
        set woreda item 20 data
      ]
    ]
  ]

  file-close                                                             ; make sure to close the file
end

to read-fixed-households-from-csv
  file-close-all                                                           ; Close all open files

  let netlogopopfile ifelse-value Different-hhs? [(word "Netlogo_Output/households" Population-nr ".csv") ] [(word "Netlogo_Output/households1.csv")]
  print netlogopopfile

  if not file-exists? netlogopopfile [
    user-message (word netlogopopfile "does not exist! Check your repository")
    stop
  ]

  file-open netlogopopfile ; open the file with the turtle data

  while [ not file-at-end? ] [                                             ; We'll read all the data in a single loop
    let data csv:from-row file-read-line                                   ; here the CSV extension grabs a single line and puts the read data in a list
                                                                           ; now we can use that list to create a turtle with the saved properties
    if (item 0 data != "hhid")[                                            ; this is the header
      create-households 1 [
        set name item 0 data
        set xcor    item 1 data
        set ycor    item 2 data
        set size    item 3 data
        set color   item 4 data
        set heading item 5 data
        set shape   item 6 data
        set household-size item 7 data
        set children item 8 data
        set elders item 9 data
        set adults item 10 data
        set tlu item 11 data
        set oxen item 12 data
        set cows item 13 data
        set calves item 14 data
        set farmland item 15 data
        set number_fields item 16 data
        set workload item 17 data                                          ; this is in hours per household per week
        set region item 18 data
        set woreda item 19 data
        set network-size item 20 data
        set income-outcome-t1end item 21 data
        set food-outcome-t1end item 22 data
        set leisure-outcome-t1end item 23 data
        set income-threshold-t1start item 24 data
        set food-threshold-t1start item 25 data
        set leisure-threshold-t1start item 26 data
        set income-relativeimportance item 27 data
        set food-relativeimportance item 28 data
        set leisure-relativeimportance item 29 data
      ]
    ]
  ]

  file-close                                                             ; make sure to close the file
end

to create-fields-cattle-hhmembers

  ask households[
    set pcolor grey
    ask patch-here [
      sprout-cattle round [tlu] of myself
      sprout-fields [number_fields] of myself
      sprout-members [household-size] of myself
      set farmstead 1
    ]
  ]

  ; Assign characteristics to the cattle
  ask households [
    let perc_cows ifelse-value (calves + oxen + cows) > 0 [               ; Determine how many percentage of the cattle should be cows --> parameter perc_cows
      cows / (calves + oxen + cows)][0]
    let perc_oxen ifelse-value (calves + oxen + cows) > 0 [               ; Determine how many percentage of the cattle should be oxen --> parameter perc_cows
      oxen / (calves + oxen + cows)][0]

    ask cattle-here [                                                     ; Assign default generic cattle characteristics
      set shape "cow"
      set dry "yes"
      set castrated "no"
      set supplementary-feed "no"
      set bought? "no"
      create-mycows-with other households-here                            ; Link cattle to household owning them
    ]

    ask n-of floor (perc_cows * tlu) cattle-here [                        ; Assign characteristics to cows
      set sex "female"
      set age (random 12 + 3)                                             ; --> parameter age
      set color pink + 2
      forward 3
    ]

    ask n-of floor (perc_oxen * tlu) cattle-here [                        ; Assign characteristics to oxen
      set sex "male"
      set castrated "yes"
      set age random 7 + 1                                                ; Male cattle are only kept till they are maximum 7 years old. After that they are no longer useful; eaten or sold
      set color sky + 2
      forward 3
    ]

    ask cattle-here[                                                      ; The remaining cattle are calves. Assign them calf characteristics
      set sex item random 2 ["female" "male"]
      set age random 3
      ifelse sex = "female" [set color pink + 2] [set color sky + 2]
      forward 3
    ]
  ]

  ask n-of (count cattle with [                                           ; Cows are pregnant approximately every other year between the age of 3 and 11
    sex = "female" and age < 11 and age > 2] / 2) cattle with [sex = "female" and age < 11 and age > 2] [set pregnant "yes"]
  ask cattle with [(age = 4 or age = 5) and pregnant != "yes" and         ; After pregnancy, they give birth and then lactate. Cows generally do not birth more than four calves
    sex = "female"][set lactation 1 set in-lactation "yes" set calf 1]
  ask cattle with [(age = 6 or age = 7) and pregnant != "yes" and sex = "female"][set lactation 2 set in-lactation "yes"  set calf 2]
  ask cattle with [(age = 8 or age = 9) and pregnant != "yes" and sex = "female"][set lactation 3 set in-lactation "yes"  set calf 3]
  ask cattle with [(age = 10 or age = 11) and pregnant != "yes" and sex = "female"][set lactation 4 set in-lactation "yes"  set calf 4]
  ask cattle [set size age / 8 + 1]

  ; Assign characteristics to fields
  ask households [ask fields-here[set field-size [farmland] of myself / [number_fields] of myself]] ; assuming all fields are equally large

  ask fields [
    set shape "square"
    set color white
    set pcolor grey
    create-myfields-with other households-here
    forward 1
    position-fields
    set fertiliser "low"
    set herbicides false
  ]

  let n-fields count fields
  ask n-of floor (n-fields * chance-maize) fields [ set color yellow set current-land-use "maize"] ; maize fields
  ask n-of floor ((n-fields * chance-pasture) - 1) fields with [color != yellow] [ set color green set current-land-use "pasture"] ; pasture fields
  ask n-of floor ((n-fields * chance-veg) - 1) fields with [color != yellow and color != green] [ set color orange set current-land-use "vegetable"] ; pasture fields
  ask n-of floor ((n-fields * chance-coffee) - 1) fields with [color != yellow and color != green and color != orange] [ set color brown set current-land-use "coffee" set coffee-age random 29 + 1] ; pasture fields
  ask fields with [color != yellow and color != green and color != orange and color != brown] [ set color white set current-land-use "fallow"] ; coffee fields

  ; Assign characteristics to households (cattle and field-related)
  ask households [
    ;; characteristics
    set savings 0
    set maizeland sum [field-size] of in-myfield-neighbors with [current-land-use = "maize"]
    set vegetableland sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable"]
    set privatepasture sum [field-size] of in-myfield-neighbors with [current-land-use = "pasture"]
    set coffeeland sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee"]
    set fallowland sum [field-size] of in-myfield-neighbors with [current-land-use = "fallow"]
    set herd count in-mycow-neighbors
    set oxen count in-mycow-neighbors with [sex = "male" and castrated = "yes"]

    ;; assign memory to households
    set known-strategies [] ; empty list
    set order-strategies [0 0 0 0 0 0 0 0 0 0 0 0]; the number of zeros should equal the number of strategy options
    set strat-dummy 0

  ]

  ask households [set known-strategies fput "stick to current" known-strategies]
  ask households [set known-strategies fput "abandon land" known-strategies]
  ask households with [maizeland > 0][set known-strategies fput "change to maize" known-strategies]
  ask households with [vegetableland > 0][set known-strategies fput "change to vegetable" known-strategies]
  ask households with [privatepasture > 0][set known-strategies fput "change to pasture" known-strategies]
  ask households with [herd > 0 and privatepasture <= 0][set known-strategies fput "change to pasture" known-strategies]
  ask households with [coffeeland > 0][set known-strategies fput "plant coffee" known-strategies]
  ask households with [herd > 0][set known-strategies fput "buy cow" known-strategies]
  ask households with [herd > 0][set known-strategies fput "sell cow" known-strategies]
  ask n-of (round Number-of-households * 0.27) households with [herd > 0][set known-strategies fput "buy feed" known-strategies] ; parameter here = 0.27 knowledge-of-supplementary-feed
  ask n-of (round Number-of-households * 0.80) households [set known-strategies fput "buy fertiliser" known-strategies] ; parameter here = 0.80, just a wild guess for now, do research!
  ask n-of (round Number-of-households * 0.16) households with [member? "buy fertiliser" known-strategies] [
    set known-strategies fput "buy herbicides" known-strategies
    set known-strategies fput "buy both fertiliser and herbicides" known-strategies
  ] ; parameter here = 0.16, Banchayehu 2021

  ask households [set known-strategies shuffle known-strategies]

  ask households [set labourcap adults * 365] ; person days per year

  ; attribute household members to households
  assign-members
  ask members[forward 1]

  ; create city and set cattle prices
  ask n-of 1 patches[ sprout-cities 1 ]

  ask cities[
    set shape "house two story"
    set size 5
    set color grey
    create-myfarms-with households [set hidden? true]
    set hidden? true
  ]

end

to assign-stochastic-hhchars
  ask households [                          ;; assign baseline aspiration values to households
    assign-aspiration-outcomes
    assign-aspiration-thresholds
    if not Fix-chars? [
      assign-relativeimportance
      ; there is no data for social network in the socio-economic household survey data, but we found another survey that had. In this survey, we found that social network ~ poisson with lambda = 2.29 + 0.3 * household members + 0.00005 * income. We will use this model to estimate social network size. See script social_network.R
      let lambda 2.29 + 0.3 * household-size + 0.00005 * income-outcome-t1end
      set network-size random-poisson lambda
    ]
  ]
end

to position-fields ; assumption
  while [any? other fields-here or any? other households-here]            ; This gives a nice layout where fields do not overlap and are placed randomly in the proximity of the household owning them
  [ fd 1 ]
  setxy round xcor round ycor                                             ; The center of fields is put in the center of the patch underneath, gives a tidy look
end

to assign-aspiration-outcomes
  if not Fix-chars? [
    calc-expected-outcome                                           ; We did not have data about initial income from the survey data, so used the other data (land use and herd size and composition) to calculate their expected income
    set income-outcome-t1end income-expectation
  ]

  set income-outcome-t1start income-outcome-t1end
  set income-outcome-t2 income-outcome-t1start
  set income-outcome-t3 income-outcome-t2

  set food-outcome-t1start food-outcome-t1end                           ; Food self-sufficiency outcome is based on survey data
  set food-outcome-t2 food-outcome-t1start
  set food-outcome-t3 food-outcome-t2

  if not Fix-chars? [
    let workcap_persondays adults * 7                                     ; Number of adults is based on survey data --> parameter worcap_persondays
    let workload_persondays workload / 8                                  ; Workload is given in hours per household per week in the survey, transform to person days per week by dividing by 8 hours per day
    set leisure-outcome-t1end workcap_persondays - workload_persondays    ; Leisure days per household per week
  ]

  set leisure-outcome-t1start leisure-outcome-t1end
  set leisure-outcome-t2 leisure-outcome-t1start
  set leisure-outcome-t3 leisure-outcome-t2
end

to assign-aspiration-thresholds
  if not Fix-chars? [
    set income-threshold-t1start income-outcome-t1start + random-exponential 0.86 * 100000 ; Fitted based on the data of Daniel Mekonnen (see Initial_aspiration.ppt + current_vs_aspired_surveydata.R)
    if income-threshold-t1start <= 0 [set income-threshold-t1start 0.01]
  ]

  if (Threshold-type = "infinite")[set income-threshold-t1start 10 ^ 100] ; this is an increadibly high number that is impossible for the households to achieve

  set income-threshold-t1end income-threshold-t1start
  set income-threshold-t2 income-threshold-t1end
  set income-threshold-t3 income-threshold-t2

  if not Fix-chars? [
    set food-threshold-t1start food-outcome-t1start + random-normal 6.91 26.99 ; Fitted based on data of Ermias Tesfaye (see Initial_aspiration.ppt + current_vs_aspired_surveydata.R)
    if food-threshold-t1start <= 0 [set food-threshold-t1start 0.01]
  ]

  if (Threshold-type = "infinite")[set food-threshold-t1start 10 ^ 100] ; this is an increadibly high number that is impossible for the households to achieve

  set food-threshold-t1end food-threshold-t1start
  set food-threshold-t2 food-threshold-t1end
  set food-threshold-t3 food-threshold-t2

  if not Fix-chars? [
    set leisure-threshold-t1start leisure-outcome-t1start + random-normal 0.98 1.68 ; Fitted based on data of Ermias Tesfaye (see Initial_aspiration.ppt + current_vs_aspired_surveydata.R)
    if leisure-threshold-t1start <= 0 [set leisure-threshold-t1start 0.01]
  ]

  if (Threshold-type = "infinite")[set leisure-threshold-t1start 10 ^ 100] ; this is an increadibly high number that is impossible for the households to achieve

  set leisure-threshold-t1end leisure-threshold-t1start
  set leisure-threshold-t2 leisure-threshold-t1end
  set leisure-threshold-t3 leisure-threshold-t2
end

to assign-relativeimportance                                              ; Relative importances are drawn from distributions fitted on the data of Ermias Tesfaye, see ~/Twente/slides with model info/Relative importance of asirational dimensions.pptx
  let three-random (list (random-poisson 8.29) (random-poisson 10.98) (random-poisson 3.71))
  let three-sum sum three-random
  set food-relativeimportance ((item 0 three-random) / three-sum) * 100 ; parameter
  set income-relativeimportance ((item 1 three-random) / three-sum) * 100 ; parameter
  set leisure-relativeimportance ((item 2 three-random) / three-sum) * 100 ; paramete
end

to assign-members
  ask households [                                                        ; Assign members to households, and give them characteristics
    ask members-here [
      set shape "person"
      create-mymembers-with other households-here
      set color brown
      set size 0.5
    ]

    ask n-of adults members-here [set lifestage "adult"]                  ; Number of adults, children and elders are based on survey data
    ask n-of children members-here [set lifestage "child"]
    ask n-of elders members-here [set lifestage "elderly"]
  ]
end

to import-city-prices                                                     ; I am not using this for the time being
  ask cities[
    set cattle-prices []
    set milk-prices []
    set butter-prices []
  ]

  file-open "prices_testing.csv"
  while [not file-at-end?][
    let prices csv:from-row file-read-line
    ask cities[
      set cattle-prices lput item 1 prices cattle-prices
      set milk-prices lput item 2 prices milk-prices
      set butter-prices lput item 3 prices butter-prices
    ]
  ]
  file-close
end

to read-matrix
  file-close-all                                                          ; Close all open files

  if not file-exists? "Netlogo_Input/simmatrix.csv" [                     ; This is a matrix which juxtaposes all households and gives their similarity (based on household characteristics) a score
    user-message "Netlogo_Input/simmatrix.csv' does not exist! :("
    stop
  ]

  let data csv:from-file "Netlogo_Input/simmatrix.csv"
  foreach range 100 [                                                     ; Then in a foreach loop, the friendship between those households is given a weight equal to the similarity score
    x -> let connections item x data
    ask households with [name = x][
      let other-names remove-item x range 100;
      foreach other-names[
        y -> let connection item y connections
        create-friendships-with other households with [name = y][
          set weight connection
          set hidden? true
        ]
      ]
    ]
  ]
end

to read-fixed-matrix
  file-close-all

  let matfile ifelse-value Fix-chars? [(word "Netlogo_Input/popsimmatrix" Population-nr ".csv")] [(word "Netlogo_Input/simmatrix" Population-nr ".csv")]
  print matfile

  if not file-exists? matfile [
    user-message word matfile " does not exist! :("
    stop
  ]

  let data csv:from-file matfile
  let hhnames item 0 data

  foreach range 100 [
    x ->
    let connections item (x + 1) data
    let hhname item x hhnames

    ask households with [name = hhname][
      let other-names remove hhname hhnames
      foreach other-names [
        y ->
        let connection position y hhnames
        create-friendships-with other households with [name = y][
          set weight connection
          set hidden? true
        ]
      ]
    ]
  ]

end

to select-friends
  foreach range 100 [                                                     ; Then let households select n best friends where n = network-size, which we have from survey data know is best described as 2.29 + 0.3 * hhmembers + 0.00005 * income, see script social_networks.R
    x ->
    ask households with [name = x][
      let candidates other households with [count in-influencer-neighbors < network-size]
      ask candidates [set candidate-weight [weight] of out-friendship-to myself]

     set sorted-weights reverse sort [candidate-weight] of candidates
     set selected-weights take network-size sorted-weights

     ask candidates[if member? ([weight] of out-friendship-to myself) selected-weights [
        create-influencer-to myself [set color sky - 3]]
      ]
    ]
  ]
end

to report-initial-data
  if write-file? [

    let file (word "D:/Data/Personal/TeeuwenAS/3spire/Netlogo_Output/SArunfiles_quicker/" ; path on remote
      substring Strategy-order 0 3 "_"
      substring Search-method 0 3 "_"
      (ifelse-value Utility-calculation = "rank-sum" ["RS"] Utility-calculation = "weighted-sum" ["WS"] Utility-calculation = "cobb-douglas+" ["CBt"] Utility-calculation = "cobb-douglas*" ["CBx"] Utility-calculation = "cobb-douglasln" ["CBln"] [Utility-calculation]) "_"
      (ifelse-value Threshold-type = "dynamic" ["dyn"] Threshold-type = "static" ["stat"] Threshold-type = "infinite" ["inf"]) "_"
      (ifelse-value Threshold-adjustment = "internal only" ["in"] Threshold-adjustment = "internal and external" ["ie"] Threshold-adjustment = "external only" ["ex"]) "-"
      Income? "_" Food? "_" Leisure? ".txt") ; ; Income? Food? and Leisure? combined are the aspirational dimensions deemed relevant by user = parameter/process

    print file

    if not is-string? file [error (word file "is not a string")]
    if not file-exists? file [
      file-open file
      write-to-runfile true
      file-close
    ]

    if file-exists? file [
      carefully [ file-open file ] [ print word "Warning: could not open" file  ]
      write-to-runfile false
      file-close
    ]


    let tickfile (word "D:/Data/Personal/TeeuwenAS/3spire/Netlogo_Output/SAtickfiles_quicker/" ; change this back to "Netlogo_Output/files_written_auto/" after SA
      substring Strategy-order 0 3 "_"
      substring Search-method 0 3 "_"
      (ifelse-value Utility-calculation = "rank-sum" ["RS"] Utility-calculation = "weighted-sum" ["WS"] Utility-calculation = "cobb-douglas+" ["CBt"] Utility-calculation = "cobb-douglas*" ["CBx"] Utility-calculation = "cobb-douglasln" ["CBln"] [Utility-calculation]) "_"
      (ifelse-value Threshold-type = "dynamic" ["dyn"] Threshold-type = "static" ["stat"] Threshold-type = "infinite" ["inf"]) "_"
      (ifelse-value Threshold-adjustment = "internal only" ["in"] Threshold-adjustment = "internal and external" ["ie"] Threshold-adjustment = "external only" ["ex"]) "-"
      Income? "_" Food? "_" Leisure? ".txt")

    if not is-string? tickfile [error (word tickfile "is not a string")]
    if not file-exists? tickfile [
      file-open tickfile
      write-to-tickfile true
      file-close
    ]

    if file-exists? tickfile [
      carefully [file-open tickfile] [print word "Warning: could not open" tickfile ]
      write-to-tickfile false
      file-close
    ]

  ]
end

;; FUNCTIONS used in go procedures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Main functions
to update-aspiration-outcomes                                            ; No sub-functions
    set income-outcome-t3 income-outcome-t2
    set income-outcome-t2 income-outcome-t1start
    set income-outcome-t1start income-outcome-t1end

    set food-outcome-t3 food-outcome-t2
    set food-outcome-t2 food-outcome-t1start
    set food-outcome-t1start food-outcome-t1end

    set leisure-outcome-t3 leisure-outcome-t2
    set leisure-outcome-t2 leisure-outcome-t1start
    set leisure-outcome-t1start leisure-outcome-t1end
end

to update-aspiration-thresholds                                          ; No sub-functions
    set income-threshold-t3 income-threshold-t2
    set income-threshold-t2 income-threshold-t1start
    set income-threshold-t1start income-threshold-t1end
    set income-thresholds-past (list (income-threshold-t3) (income-threshold-t2) (income-threshold-t1end))

    set food-threshold-t3 food-threshold-t2
    set food-threshold-t2 food-threshold-t1start ;
    set food-threshold-t1start food-threshold-t1end ;
    set food-thresholds-past (list food-threshold-t3 food-threshold-t2 food-threshold-t1end)

    set leisure-threshold-t3 leisure-threshold-t2
    set leisure-threshold-t2 leisure-threshold-t1start
    set leisure-threshold-t1start leisure-threshold-t1end
    set leisure-thresholds-past (list leisure-threshold-t3 leisure-threshold-t2 leisure-threshold-t1end)
end

to calc-new-aspiration-thresholds                                   ; No sub-functions
  ; calculate aspiration means first
  let my-forgetfullness-sum sum forgetfullness
  let my-memory-time memory-time
  let my-forgetfullness forgetfullness

  let temp1  (1 / my-forgetfullness-sum )

  let temp2 (map * my-forgetfullness income-thresholds-past)
  set income-threshold-mean sum map [ i -> temp1 * i ] temp2

  let temp4 (map * my-forgetfullness food-thresholds-past)
  set food-threshold-mean sum map [ i -> temp1 * i ] temp4

  let temp3 (map * my-forgetfullness leisure-thresholds-past)
  set leisure-threshold-mean sum map [i -> temp1 * i] temp3

  ; income
  set income-gap income-threshold-mean - income-outcome-t1start

  if Threshold-type = "dynamic" [
    set income-threshold-t1end income-threshold-t1start * (optimism + external-adjustment pmult) - internal-adjustment income-gap
    if income-threshold-t1end < 0 [set income-threshold-t1end 0.01]]

  ; food
  set food-gap food-threshold-mean - food-outcome-t1start

  if Threshold-type = "dynamic" [
    set food-threshold-t1end food-threshold-t1start * (optimism + external-adjustment ymult) - internal-adjustment food-gap
    if food-threshold-t1end < 0 [set food-threshold-t1end 0.01]
    if food-threshold-t1end > (1 * (365 / 56) * household-size) [set food-threshold-t1end (1 * (365 / 56) * household-size)]] ; this is 3600 calories per day per person ( a lot! )

  ; leisure
  set leisure-gap leisure-threshold-mean - leisure-outcome-t1start

  if Threshold-type = "dynamic" [
    set leisure-threshold-t1end leisure-threshold-t1start * (optimism + external-adjustment 0 ) - internal-adjustment leisure-gap
    if leisure-threshold-t1end < 0 [set leisure-threshold-t1end 0.01]
    if leisure-threshold-t1end > (5 * household-size) [set leisure-threshold-t1end 7 * household-size]] ; assume max desired leisure is 5 person days per person, and that more leisure would not be desired but underemployment

  ; set gap to zero if aspiration is turned off
  if not Income? [set income-gap 0]
  if not Food? [set food-gap 0]
  if not Leisure? [set leisure-gap 0]

  ifelse (income-gap > 0) or (food-gap > 0) or (leisure-gap > 0)[set satisficed "no"][set satisficed "yes"]

end

to determine-loop-order                                                  ; No sub-functions
    ask households with [satisficed = "no"][
    let strategy-options ["stick to current" "abandon land" "change to pasture" "change to maize" "change to vegetable" "plant coffee" "buy cow" "buy feed" "buy fertiliser" "sell cow" "buy herbicides" "buy both fertiliser and herbicides"]
    let similarity-of-sticktocurrent               [2  2  2  2  2  2  2  2  2  2  2  2]
    let similarity-of-abandonland                  [2  2  2  1  0 -2 -1 -1 -2  1 -1 -1]
    let similarity-of-changetopasture              [2  2  2  0 -2 -2  2  1 -2  0 -2 -2]
    let similarity-of-changetomaize                [2  1 -1  2 -1 -2  1  1 -1  0  1  1]
    let similarity-of-changetovegs           [2  0 -2 -1  2 -1 -1 -1 -1  0  1  1]
    let similarity-of-changetocoffee               [2 -2 -2 -2 -1  2  1 -1  0  0  1  1]
    let similarity-of-buycow                       [2 -1  2  1 -2  1  2  2  1 -2 -2 -2]
    let similarity-of-buyfeed                      [2 -1  1  1 -1  0  2  2  2 -2 -2  2]
    let similarity-of-buyfertiliser                [2 -2 -2 -1 -1  0  1  2  2  0  2  2]
    let similarity-of-sellcow                      [2  1  0  0  0  0 -2 -2  0  2 -1 -1]
    let similarity-of-buyherbicides                [2 -1 -2  1  1  1 -2 -2  2 -1  2  2]
    let similarity-of-buyfertiliserherbicides      [2 -1 -2  1  1  1 -2 -2  2 -1  2  2]

     if member? "stick to current" known-strategies [
      set order-strategies (map + order-strategies similarity-of-sticktocurrent) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "abandon land" known-strategies [
      set order-strategies (map + order-strategies similarity-of-abandonland) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "change to pasture" known-strategies [
      set order-strategies (map + order-strategies similarity-of-changetopasture) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "change to maize" known-strategies [
      set order-strategies (map + order-strategies similarity-of-changetomaize) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "change to vegetable" known-strategies [
      set order-strategies (map + order-strategies similarity-of-changetovegs) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "plant coffee" known-strategies [
      set order-strategies (map + order-strategies similarity-of-changetovegs) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "buy cow" known-strategies [
      set order-strategies (map + order-strategies similarity-of-buycow) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "buy feed" known-strategies [
      set order-strategies (map + order-strategies similarity-of-buyfeed) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "buy fertiliser" known-strategies [
      set order-strategies (map + order-strategies similarity-of-buyfertiliser) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "sell cow" known-strategies [
      set order-strategies (map + order-strategies similarity-of-sellcow) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "buy herbicides" known-strategies [
      set order-strategies (map + order-strategies similarity-of-buyherbicides) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    if member? "buy both fertiliser and herbicides" known-strategies [
      set order-strategies (map + order-strategies similarity-of-buyfertiliserherbicides) ;sum of existing order strategies list and the similarity list for this specific strategy
    ]

    ; if who = test-hh [print (word "order options: " order-strategies)]
    let randomvalues (list (random-float 0.8 - 0.4) (random-float 0.8 - 0.4) (random-float 0.8 - 0.4) (random-float 0.8 - 0.4) (random-float 0.8 - 0.4) (random-float 0.8 - 0.4) (random-float 0.8 - 0.4) (random-float 0.8 - 0.4) (random-float 0.8 - 0.4) (random-float 0.8 - 0.4) (random-float 0.8 - 0.4) (random-float 0.8 - 0.4))
    let distinct-order  (map + order-strategies randomvalues)
    ; if who = test-hh [print (word "order options distinctly: " distinct-order)]

    let sorted-order reverse sort distinct-order
    ; if who = test-hh [print (word "sorted order: " sorted-order)]

    set loop-order []
    foreach sorted-order [
      x -> let position-strat position x distinct-order
      set loop-order lput (item position-strat strategy-options) loop-order
    ]
  ]
end

to set-testing-vars-na
  set test-feedcost "NA"
  set test-cowcost "NA"
  set test-maizecost "NA"
  set test-vegcost "NA"
  set test-coffeecost "NA"
  set test-maizesales "NA"
  set test-vegsales "NA"
  set test-coffeesales "NA"
  set test-oxensales "NA"
  set test-bullsales "NA"
  set test-cowsales 0
  set test-herd "NA"
  set income-expectation "NA"
  set leisure-expectation "NA"
  set food-expectation "NA"
  set chosen-strategy "NA"

end

to-report compare-expectations-to-thresholds ; Income? Food? and Leisure? combined are the aspirational dimensions deemed relevant by user = parameter/process
  if not Income? and not Leisure? and not Food? [report true]
  if not Income? and not Leisure? and     Food? [report food-expectation >= food-threshold-mean]
  if not Income? and     Leisure? and not Food? [report leisure-expectation >= leisure-threshold-mean]
  if not Income? and     Leisure? and     Food? [report leisure-expectation >= leisure-threshold-mean and food-expectation >= food-threshold-mean]
  if     Income? and not Leisure? and not Food? [report income-expectation >= income-threshold-mean]
  if     Income? and     Leisure? and not Food? [report income-expectation >= income-threshold-mean and leisure-expectation >= leisure-threshold-mean]
  if     Income? and not Leisure? and     Food? [report income-expectation >= income-threshold-mean and food-expectation >= food-threshold-mean]
  if     Income? and     Leisure? and     Food? [report income-expectation >= income-threshold-mean and leisure-expectation >= leisure-threshold-mean and food-expectation >= food-threshold-mean]
end

to-report internal-adjustment [gap]
  report (ifelse-value
    (Threshold-adjustment = "internal only") [ b-no * gap]
    (Threshold-adjustment = "internal and external")[ b-no * gap]
    (Threshold-adjustment = "external only")[ 0 ]
  )
end

to-report external-adjustment [mult]
  report (ifelse-value
    (Threshold-adjustment = "internal only") [ 0 ]
    (Threshold-adjustment = "internal and external")[ (mult * 0.1) ]  ; ymult and pmult are between -1 and +1. Multiply by 0.1 so that the adjustment is significant but not crazy
    (Threshold-adjustment = "external only")[ (mult * 0.1) ]
    )
end

to choose-strategy-satisficing2

  loop-through-strategies
  ifelse chosen-strategy = 0 [set color red][set color green - 2]

  if (color = red and chosen-strategy = 0)[
    choose-the-best
  ]

end

to choose-strategy-satisficing
  ; if who = test-hh  [print "have entered the big for loop, will now loop through strategies first time"]

    loop-through-strategies
    ifelse chosen-strategy = 0 [set color red][set color green - 2]        ; Households that find a satisficing solution are turned light green. Unsatisficed households are turned red.
    set cowiter 0

    let aspiration-thresholds-before-loop
            (list (income-threshold-mean)                                  ; Store pre-downadjustment aspiration thresholds in list
                  (food-threshold-mean) (leisure-threshold-mean))

    ; While households could not find a satisficing strategy, they will look for close-to-satificing strategies. This is realised by gradually lowering their aspiration thresholds.
    while [color = red and chosen-strategy = 0 and                         ; Have not found a strategy
           cowiter < 20 and                                                ; They will lower 20 times, after that, they go to more drastic means
           (income-threshold-mean != 0 or                                  ; Thresholds cannot be lowered below 0
            food-threshold-mean != 0 or
            leisure-threshold-mean != 0)][

      set income-threshold-mean income-threshold-mean -                    ; How much the thresholds are lowered, depends on the relative importance of the aspirational dimension
          (item 0 aspiration-thresholds-before-loop * ( income-relativeimportance / 100) * 0.1 )
      set food-threshold-mean food-threshold-mean -
          (item 1 aspiration-thresholds-before-loop * ( food-relativeimportance / 100) * 0.1 )
      set leisure-threshold-mean leisure-threshold-mean -
          (item 2 aspiration-thresholds-before-loop * ( leisure-relativeimportance / 100) * 0.1 )

      ; if who = test-hh  [print word "looping through strategies in while loop, iteration = " cowiter]

      loop-through-strategies
      ifelse chosen-strategy = 0 [set color red][set color green - 2] ;;her
      set cowiter cowiter + 1
    ]

    ; While households could not find a close-to-satisficing strategy by down-adjusting their aspiration thresholds, do something more drastic: drop one or several aspiration thresholds
    let cow-id 0
    set income-threshold-mean item 0 aspiration-thresholds-before-loop
    set food-threshold-mean item 1 aspiration-thresholds-before-loop
    set leisure-threshold-mean item 2 aspiration-thresholds-before-loop

    while [color = red and cow-id < 6] [
      set cow-id cow-id + 1
      if cow-id = 1 [
        let aspiration-thresholds-dropped drop-least-important             ; Drop the least important aspiration dimension (1 1 0)
        set income-threshold-mean item 0 aspiration-thresholds-dropped
        set food-threshold-mean item 1 aspiration-thresholds-dropped
        set leisure-threshold-mean item 2 aspiration-thresholds-dropped
      ]

      if cow-id = 2 [
        let aspiration-thresholds-dropped drop-2ndleast-important          ; Drop the 2nd least important aspiration dimension (1 0 1)
        set income-threshold-mean item 0 aspiration-thresholds-dropped
        set food-threshold-mean item 1 aspiration-thresholds-dropped
        set leisure-threshold-mean item 2 aspiration-thresholds-dropped
      ]

      if cow-id = 3 [
        let aspiration-thresholds-dropped keep-only-most-important         ; Drop the two least important aspiration dimensions (1 0 0)
        set income-threshold-mean item 0 aspiration-thresholds-dropped
        set food-threshold-mean item 1 aspiration-thresholds-dropped
        set leisure-threshold-mean item 2 aspiration-thresholds-dropped
      ]

      if cow-id = 4 [
        let aspiration-thresholds-dropped drop-most-important              ; Drop the most important aspiration dimension (0 1 1)
        set income-threshold-mean item 0 aspiration-thresholds-dropped
        set food-threshold-mean item 1 aspiration-thresholds-dropped
        set leisure-threshold-mean item 2 aspiration-thresholds-dropped
      ]

      if cow-id = 5 [
        let aspiration-thresholds-dropped keep-only-2ndmost-important      ; Drop most and least important aspiration dimensions (0 1 0)
        set income-threshold-mean item 0 aspiration-thresholds-dropped
        set food-threshold-mean item 1 aspiration-thresholds-dropped
        set leisure-threshold-mean item 2 aspiration-thresholds-dropped
      ]

      if cow-id = 6 [
        let aspiration-thresholds-dropped keep-only-least-important        ; Drop 2nd least and least important aspiration dimensions (0 0 1)
        set income-threshold-mean item 0 aspiration-thresholds-dropped
        set food-threshold-mean item 1 aspiration-thresholds-dropped
        set leisure-threshold-mean item 2 aspiration-thresholds-dropped
      ]

      ; if who = test-hh  [print word "looping through strategies in while loop where aspiration dimensions are dropped, iteration = " cow-id]

      loop-through-strategies
      ; if who = test-hh [print (word "hh " who " expected cowsales: " test-cowsales " || right after loop where dimensions are dropped")]
      ifelse chosen-strategy = 0 [set color red][set color green - 2]

    ]

    ; if who = test-hh [print (word "hh " who " expected cowsales: " test-cowsales " || right before resetting thresholds")]

    set income-threshold-mean item 0 aspiration-thresholds-before-loop
    set food-threshold-mean item 1 aspiration-thresholds-before-loop
    set leisure-threshold-mean item 2 aspiration-thresholds-before-loop

    ; if who = test-hh [print (word "hh " who " test-cowsales: " test-cowsales " || right after resetting thresholds")]      ;; here it is 3510

    ; this is just a test to see what happens
    set cowsales test-cowsales
    ; if who = test-hh [print (word "** hh " who " cowsales: " cowsales " || right after resetting thresholds")]  ; 3510
    ; this is just a test to see what happens
end

to choose-random
  ;if order-strats is random, then a random strategy will be selected. if order-strats is defined-by-similarity, the most similar strategy will be chosen

  foreach loop-order [
    x ->
    ;if who = test-hh [print ""]
    ;if who = test-hh [print (word "checking: " x)]

    ; Stick to current --------------------------------------------------------------------->
    if (x = "stick to current")[
      set chosen-strategy "stick to current"
    ]

    ; Abandon land ------------------------------------------------------------------------->
    if (x = "abandon land")[
      if (fallowland <= farmland)[
        if (privatepasture > 0)[ set chosen-strategy "abandon pasture"]
        if (maizeland > 0)[ set chosen-strategy "abandon maize"]
        if (vegetableland > 0)[ set chosen-strategy "abandon vegetable"]
        if (count in-myfield-neighbors with [coffee-age > 7 and current-land-use = "coffee"] > 0)[set chosen-strategy "abandon coffee"] ; --> parameter possible-to-change-from-coffee-after
        ]
      ]

    ; Change to pasture -------------------------------------------------------------------->
    if (x = "change to pasture")[

      if (privatepasture <= farmland)[
        if (fallowland > 0)[ set chosen-strategy "change from fallow to pasture" ]
        if (maizeland > 0)[ set chosen-strategy "change from maize to pasture"  ]
        if (vegetableland > 0)[set chosen-strategy "change from vegetable to pasture" ]
        if (count in-myfield-neighbors with [coffee-age > 7 and current-land-use = "coffee"] > 0)[set chosen-strategy "change from coffee to pasture"] ; --> parameter possible-to-change-from-coffee-after
      ]
    ]

    ; Change to vegetable ------------------------------------------------------------------>
    if (x = "change to vegetable")[
      if (vegetableland <= farmland)[
        if (fallowland > 0)[ set chosen-strategy "change from fallow to vegetable" ]
        if (maizeland > 0)[ set chosen-strategy "change from maize to vegetable" ]
        if (privatepasture > 0)[set chosen-strategy "change from pasture to vegetable" ]
        if (count in-myfield-neighbors with [coffee-age > 7 and current-land-use = "coffee"] > 0)[set chosen-strategy "change from coffee to vegetable"] ; --> parameter possible-to-change-from-coffee-after
     ]
    ]

    ; Change to maize --------------------------------------------------------------------->
    if (x = "change to maize")[
      if (maizeland <= farmland)[
        if (fallowland > 0)[ set chosen-strategy "change from fallow to maize"]
        if (vegetableland > 0)[ set chosen-strategy "change from vegetable to maize" ]
        if (privatepasture > 0 )[ set chosen-strategy "change from pasture to maize" ]
          if (count in-myfield-neighbors with [coffee-age > 7 and current-land-use = "coffee"] > 0)[set chosen-strategy "change from coffee to maize" ] ; --> parameter possible-to-change-from-coffee-after
      ]
    ]

    ; Plant coffee ------------------------------------------------------------------------->
    if (x = "plant coffee")[
      if (count in-myfield-neighbors with [coffee-age > 7 and current-land-use = "coffee"] > 0)[ ; --> parameter possible-to-change-from-coffee-after
        if (fallowland > 0)[
          change-from-fallow-to-coffee-test
          let estimated-cost (costs-of-production +                     ; calculate the expected cost of establishing coffee
                                global-coffeeplantation-price * sum [field-size] of in-myfield-neighbors with [color = grey])

          if (estimated-cost <= savings)[ set chosen-strategy "change from fallow to coffee" ]

          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color white
              set current-land-use "fallow"]
        ]

      if (privatepasture > 0)[
          change-from-pasture-to-coffee-test
          let estimated-cost (costs-of-production +                     ; calculate the expected cost of establishing coffee
                                global-coffeeplantation-price * sum [field-size] of in-myfield-neighbors with [color = grey])

          if (estimated-cost <= savings)[ set chosen-strategy "change from pasture to coffee" ]

          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color green
              set current-land-use "pasture"]
        ]

      if (maizeland > 0)[
          change-from-maize-to-coffee-test

          let estimated-cost (costs-of-production +                     ; calculate the expected cost of establishing coffee
                                global-coffeeplantation-price * sum [field-size] of in-myfield-neighbors with [color = grey])

          if (estimated-cost <= savings)[ set chosen-strategy "change from maize to coffee" ]

          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color yellow
              set current-land-use "maize"]
        ]

      if (vegetableland > 0)[
          change-from-veg-to-coffee-test

          let estimated-cost (costs-of-production +                     ; calculate the expected cost of establishing coffee
                                global-coffeeplantation-price * sum [field-size] of in-myfield-neighbors with [color = grey])

          if (estimated-cost <= savings)[ set chosen-strategy "change from vegetable to coffee" ]

          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color orange
              set current-land-use "vegetable"]
        ]
      ]
    ]

    ; Buy cow ------------------------------------------------------------------------->
    if (x = "buy cow")[
      let mycapacity floor (((feedproduction * 1000) / 365 ) / 3.15) ; 3.15 = parameter feed-lowerthreshold-nosup

      if (savings >= (costs-of-production + global-cow-purchase-price) and
         (mycapacity > herd or adopted-feed? = "yes"))[
        set chosen-strategy "buy cow"
      ]
    ]

    ; Buy feed ------------------------------------------------------------------------>
    if (x = "buy feed")[
      let cows-on-supplement count in-mycow-neighbors with [supplementary-feed = "yes"]
      let on-supplement ifelse-value (cows-on-supplement > 0)["yes"]["no"] ; 0 = parameter feed-lowerthreshold-sup
      let estimated-cost costs-of-production + (global-supplementary-feed-price * herd)

      if (savings >= estimated-cost and
          herd > 0 and
          on-supplement = "no")[
        set chosen-strategy "buy feed"
      ]
    ]

    ; Buy fertiliser ------------------------------------------------------------------------>
    if (x = "buy fertiliser")[
      let eligible-fields count in-myfield-neighbors with [current-land-use = "maize"] +
                          count in-myfield-neighbors with [current-land-use = "vegetable"] +
                          count in-myfield-neighbors with [current-land-use = "coffee"]
      let maxed-out-fields count in-myfield-neighbors with [fertiliser = "very high"]

      if (eligible-fields > 0 and
        maxed-out-fields < eligible-fields)[
        buy-fertiliser-test
        let fert-cost calc-cost-of-fertiliser-test
        if (savings >= fert-cost + costs-of-production)[ set chosen-strategy "buy fertiliser" ]
        ask in-myfield-neighbors [                                                       ; Reverse the change
            set fertiliser (ifelse-value
              fertiliser = "testing moderate"  ["low"]
              fertiliser = "testing high"      ["moderate"]
              fertiliser = "testing very high" ["high"]
                                               [fertiliser]
            )
          ]
      ]

    ]

    ; Sell cow ------------------------------------------------------------------------>
    if (x = "sell cow")[
      let mycapacity floor (((feedproduction * 1000) / 365) / 3.15) ; 3.15 = parameter feed-lowerthreshold-nosup
      if (count in-mycow-neighbors with [calf < 4 and sex = "female"] > 0 and
          savings <= 0 and
          mycapacity < herd)[
        set chosen-strategy "sell cow"
      ]
    ]

    ; Buy herbicides ------------------------------------------------------------------------>
    if (x = "buy herbicides")[
      let eligible-fields count in-myfield-neighbors with [current-land-use = "maize" and fertiliser != "low"] +
                          count in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser != "low"] +
                          count in-myfield-neighbors with [current-land-use = "coffee" and fertiliser != "low"]
      let maxed-out-fields count in-myfield-neighbors with [herbicides = true]
      if (eligible-fields > 0 and maxed-out-fields < eligible-fields)[
        buy-herbicides-test

        let herbicide-cost sum [field-size] of in-myfield-neighbors with [herbicides = "testing true"] * global-herbicides-price
        if herbicide-cost + costs-of-production <= savings[
         set chosen-strategy "buy herbicides"
        ]
        ask in-myfield-neighbors with [herbicides = "testing true"][set herbicides false]              ; Reverse the change
      ]
    ]

    ; buy both fertiliser and herbicides ------------------------------------------------------------------------>
    if (x = "buy both fertiliser and herbicides")[
      let eligible-fields count in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "low"] +
                          count in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "low"] +
                          count in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "low"]
      let maxed-out-fields count in-myfield-neighbors with [herbicides = true]

      if (eligible-fields > 0 and maxed-out-fields < eligible-fields)[
        buy-fertiliser-and-herbicides-test

        let herbicide-cost sum [field-size] of in-myfield-neighbors with [herbicides = "testing true"] * global-herbicides-price
        let fert-cost calc-cost-of-fertiliser-test
        if herbicide-cost + fert-cost + costs-of-production <= savings[
          set chosen-strategy "buy both fertiliser and herbicides"
        ]
        ask in-myfield-neighbors with [herbicides = "testing true"][set herbicides false]              ; Reverse the change
        ask in-myfield-neighbors with [fertiliser = "testing moderate"][set fertiliser "low"]
      ]
    ]

  ]

  implement-chosen-strategy

end

to choose-the-best
  set income-expectations (list "")
  set food-expectations (list "")
  set leisure-expectations (list "")
  set possible-strategies (list "")

  ;if who = test-hh [print (word "household: " who)]


  foreach known-strategies [
    x ->
    ;if who = test-hh [print ""]
    ;if who = test-hh [print (word "checking: " x)]

    ; Stick to current --------------------------------------------------------------------->
    if (x = "stick to current")[

      ;if who = test-hh [print (word "income expectation:" item 0 report-expected-outcomes)]

      set income-expectations lput item 0 report-expected-outcomes income-expectations
      set food-expectations lput item 1 report-expected-outcomes food-expectations
      set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
      set possible-strategies lput x possible-strategies
    ]

    ; Abandon land ------------------------------------------------------------------------->
    if (x = "abandon land")[
      if (fallowland < farmland)[
        if (privatepasture > 0)[
          change-from-pasture-to-fallow-test

          ;if who = test-hh [print (word "income expectation:" item 0 report-expected-outcomes)]




          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "abandon pasture" possible-strategies
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color green
              set current-land-use "pasture"]
          ]
          if (maizeland > 0)[
          change-from-maize-to-pasture-test

          ;if who = test-hh [print (word "income expectation:" item 0 report-expected-outcomes)]



          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "abandon maize" possible-strategies
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color yellow
              set current-land-use "maize"]
          ]
          if (vegetableland > 0)[
          change-from-veg-to-fallow-test

          ;if who = test-hh [print (word "income expectation:" item 0 report-expected-outcomes)]



          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "abandon vegetable" possible-strategies
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color orange
              set current-land-use "vegetable"]
          ]
          if (count in-myfield-neighbors with [coffee-age > 7 and current-land-use = "coffee"] > 0)[ ; --> parameter possible-to-change-from-coffee-after
          change-from-coffee-to-fallow-test

          ;if who = test-hh [print (word "income expectation:" item 0 report-expected-outcomes)]



          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "abandon coffee" possible-strategies
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color brown
              set current-land-use "coffee"]
          ]
        ]
      ]

    ; Change to pasture -------------------------------------------------------------------->
    if (x = "change to pasture")[

      if (privatepasture < farmland)[
        if (fallowland > 0)[
          change-from-fallow-to-pasture-test

          ;if who = test-hh [print (word "income expectation:" item 0 report-expected-outcomes)]



          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "change from fallow to pasture" possible-strategies
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color white
              set current-land-use "fallow"]
          ]
        if (maizeland > 0)[
          change-from-maize-to-pasture-test

          ;if who = test-hh [print (word "income expectation:" item 0 report-expected-outcomes)]



          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "change from maize to pasture" possible-strategies
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color yellow
              set current-land-use "maize"]
          ]
        if (vegetableland > 0)[
          change-from-veg-to-pasture-test

          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "change from vegetable to pasture" possible-strategies
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color orange
              set current-land-use "vegetable"]
          ]
        if (count in-myfield-neighbors with [coffee-age > 7 and current-land-use = "coffee"] > 0)[ ; --> parameter possible-to-change-from-coffee-after
          change-from-coffee-to-pasture-test

          ;if who = test-hh [print (word "income expectation:" item 0 report-expected-outcomes)]



          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "change from coffee to pasture" possible-strategies
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color brown
              set current-land-use "coffee"]
          ]
      ]
    ]

    ; Change to vegetable ------------------------------------------------------------------>
    if (x = "change to vegetable")[
      if (vegetableland < farmland)[
        if (fallowland > 0)[
          change-from-fallow-to-veg-test

          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "change from fallow to vegetable" possible-strategies
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color white
              set current-land-use "fallow"]
          ]
        if (maizeland > 0)[
          change-from-maize-to-veg-test

          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "change from maize to vegetable" possible-strategies
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color yellow
              set current-land-use "maize"]
          ]
        if (privatepasture > 0)[
          change-from-pasture-to-veg-test

          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "change from pasture to vegetable" possible-strategies
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color green
              set current-land-use "pasture"]
          ]
        if (count in-myfield-neighbors with [coffee-age > 7 and current-land-use = "coffee"] > 0)[ ; --> parameter possible-to-change-from-coffee-after
          change-from-coffee-to-veg-test

          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "change from coffee to vegetable" possible-strategies
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color brown
              set current-land-use "coffee"]
          ]
      ]
    ]

    ; Change to maize --------------------------------------------------------------------->
    if (x = "change to maize")[
      if (maizeland < farmland)[
        if (fallowland > 0)[
          change-from-fallow-to-maize-test

          ;if who = test-hh [print "change from fallow to maize"]
          ;if who = test-hh [print (word "income expectation:" item 0 report-expected-outcomes)]

          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "change from fallow to maize" possible-strategies
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color white
              set current-land-use "fallow"]
          ]
        if (vegetableland > 0)[
          change-from-veg-to-maize-test

          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "change from vegetable to maize" possible-strategies
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color orange
              set current-land-use "vegetable"]
          ]
        if (privatepasture > 0)[
          change-from-pasture-to-maize-test

          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "change from pasture to maize" possible-strategies
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color green
              set current-land-use "pasture"]
          ]
        if (count in-myfield-neighbors with [coffee-age > 7 and current-land-use = "coffee"] > 0)[ ; --> parameter possible-to-change-from-coffee-after
          change-from-coffee-to-maize-test

          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "change from coffee to maize" possible-strategies
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color brown
              set current-land-use "coffee"]
          ]
      ]
    ]

    ; Plant coffee ------------------------------------------------------------------------->
    if (x = "plant coffee")[
      if (coffeeland < farmland)[
        if (fallowland > 0)[
          change-from-fallow-to-coffee-test



          let estimated-cost (costs-of-production +                     ; calculate the expected cost of establishing coffee
                                global-coffeeplantation-price * sum [field-size] of in-myfield-neighbors with [color = grey])

          if (estimated-cost <= savings)[
            set income-expectations lput item 0 report-expected-outcomes income-expectations
            set food-expectations lput item 1 report-expected-outcomes food-expectations
            set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
            set possible-strategies lput "change from fallow to coffee" possible-strategies
          ]
          ;print (word "reverse change from fallow to coffee")
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color white
              set current-land-use "fallow"]
        ]

      if (privatepasture > 0)[
          change-from-pasture-to-coffee-test

          ;if who = test-hh [print (word "income expectation:" item 0 report-expected-outcomes)]



          let estimated-cost (costs-of-production +                     ; calculate the expected cost of establishing coffee
                                global-coffeeplantation-price * sum [field-size] of in-myfield-neighbors with [color = grey])
          if (estimated-cost <= savings)[
            set income-expectations lput item 0 report-expected-outcomes income-expectations
            set food-expectations lput item 1 report-expected-outcomes food-expectations
            set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
            set possible-strategies lput "change from pasture to coffee" possible-strategies
          ]
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color green
              set current-land-use "pasture"]
        ]

      if (maizeland > 0)[
          change-from-maize-to-coffee-test

          ;if who = test-hh [print (word "income expectation:" item 0 report-expected-outcomes)]



          let estimated-cost (costs-of-production +                     ; calculate the expected cost of establishing coffee
                                global-coffeeplantation-price * sum [field-size] of in-myfield-neighbors with [color = grey])
          if (estimated-cost <= savings)[
            set income-expectations lput item 0 report-expected-outcomes income-expectations
            set food-expectations lput item 1 report-expected-outcomes food-expectations
            set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
            set possible-strategies lput "change from maize to coffee" possible-strategies
          ]
          ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color yellow
              set current-land-use "maize"]
        ]

      if (vegetableland > 0)[
          change-from-veg-to-coffee-test

          ;if who = test-hh [print (word "income expectation:" item 0 report-expected-outcomes)]



          let estimated-cost (costs-of-production +                     ; calculate the expected cost of establishing coffee
                                global-coffeeplantation-price * sum [field-size] of in-myfield-neighbors with [color = grey])
          if (estimated-cost <= savings)[
            set income-expectations lput item 0 report-expected-outcomes income-expectations
            set food-expectations lput item 1 report-expected-outcomes food-expectations
            set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
            set possible-strategies lput "change from vegetable to coffee" possible-strategies
        ]
        ask in-myfield-neighbors with [color = grey][                                      ; Reverse the change
              set color orange
              set current-land-use "vegetable"]
        ]
      ]
    ]

    ; Buy cow ------------------------------------------------------------------------->
    if (x = "buy cow")[
      let mycapacity floor (((feedproduction * 1000) / 365 ) / 3.15) ; 3.15 = parameter feed-lowerthreshold-nosup

      if (savings >= (costs-of-production + global-cow-purchase-price) and
         (mycapacity > herd or adopted-feed? = "yes"))[
        buy-cow-test

        ;if who = test-hh [print (word "income expectation:" item 0 report-expected-outcomes)]



        set income-expectations lput item 0 report-expected-outcomes income-expectations
        set food-expectations lput item 1 report-expected-outcomes food-expectations
        set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
        set possible-strategies lput "buy cow" possible-strategies
        ask in-mycow-neighbors with [bought? = "yes" and age = 4][die]                    ; Reverse the change
      ]
    ]

    ; Buy feed ------------------------------------------------------------------------>
    if (x = "buy feed")[
      let cows-on-supplement count in-mycow-neighbors with [supplementary-feed = "yes"]
      let on-supplement ifelse-value (cows-on-supplement > 0)["yes"]["no"]
      let estimated-cost costs-of-production + (global-supplementary-feed-price * herd)

      if (savings >= estimated-cost and
          herd > 0 and
          on-supplement = "no")[
        buy-feed-test

        ;if who = test-hh [print (word "income expectation:" item 0 report-expected-outcomes)]



        set income-expectations lput item 0 report-expected-outcomes income-expectations
        set food-expectations lput item 1 report-expected-outcomes food-expectations
        set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
        set possible-strategies lput "buy feed" possible-strategies
        set adopted-feed? "no"
        set test-feedcost 0
        ask in-mycow-neighbors with [supplementary-feed = "testing"][                      ; Reverse the change
          set supplementary-feed "no"]
      ]
    ]

    ; Buy fertiliser ------------------------------------------------------------------------>
    if (x = "buy fertiliser")[
      let eligible-fields count in-myfield-neighbors with [current-land-use = "maize"] +
                          count in-myfield-neighbors with [current-land-use = "vegetable"] +
                          count in-myfield-neighbors with [current-land-use = "coffee"]
      let maxed-out-fields count in-myfield-neighbors with [fertiliser = "very high"]

      if (eligible-fields > 0 and
        maxed-out-fields < eligible-fields)[
        buy-fertiliser-test
        let fert-cost calc-cost-of-fertiliser-test
        if (savings >= fert-cost + costs-of-production)[

          ;if who = test-hh [print (word "income expectation:" item 0 report-expected-outcomes)]



          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "buy fertiliser" possible-strategies
        ]
        ask in-myfield-neighbors [                                                       ; Reverse the change
            set fertiliser (ifelse-value
              fertiliser = "testing moderate"  ["low"]
              fertiliser = "testing high"      ["moderate"]
              fertiliser = "testing very high" ["high"]
                                               [fertiliser]
            )
          ]
      ]

    ]

    ; Sell cow ------------------------------------------------------------------------>
    if (x = "sell cow")[
      let mycapacity floor (((feedproduction * 1000) / 365) / 3.15) ; 3.15 = parameter feed-lowerthreshold-nosup
      if (count in-mycow-neighbors with [calf < 4 and sex = "female"] > 0 and
          savings <= 0 and
          mycapacity < herd)[
        sell-cow-test

        ;if who = test-hh [print (word "income expectation:" item 0 report-expected-outcomes)]

        set income-expectations lput item 0 report-expected-outcomes income-expectations
        set food-expectations lput item 1 report-expected-outcomes food-expectations
        set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
        set possible-strategies lput "sell cow" possible-strategies
        ask in-mycow-neighbors with [color = red][set color pink + 2]                                                ; Reverse the change
        set test-herd herd
      ]
    ]

    ; Buy herbicides ------------------------------------------------------------------------>
    if (x = "buy herbicides")[
      let eligible-fields count in-myfield-neighbors with [current-land-use = "maize" and fertiliser != "low"] +
                          count in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser != "low"] +
                          count in-myfield-neighbors with [current-land-use = "coffee" and fertiliser != "low"]
      let maxed-out-fields count in-myfield-neighbors with [herbicides = true]
      if (eligible-fields > 0 and maxed-out-fields < eligible-fields)[
        buy-herbicides-test

        ;if who = test-hh [print (word "income expectation:" item 0 report-expected-outcomes)]

        let herbicide-cost sum [field-size] of in-myfield-neighbors with [herbicides = "testing true"] * global-herbicides-price
        if herbicide-cost + costs-of-production <= savings[
          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "buy herbicides" possible-strategies
        ]
        ask in-myfield-neighbors with [herbicides = "testing true"][set herbicides false]              ; Reverse the change
      ]
    ]

    ; Buy both fertiliser and herbicides ------------------------------------------------------------------------>
    if (x = "buy both fertiliser and herbicides")[
      let eligible-fields count in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "low"] +
                          count in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "low"] +
                          count in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "low"]
      let maxed-out-fields count in-myfield-neighbors with [herbicides = true]

      if (eligible-fields > 0 and maxed-out-fields < eligible-fields)[
        buy-fertiliser-and-herbicides-test

        ;if who = test-hh [print (word "income expectation:" item 0 report-expected-outcomes)]

        let herbicide-cost sum [field-size] of in-myfield-neighbors with [herbicides = "testing true"] * global-herbicides-price
        let fert-cost calc-cost-of-fertiliser-test
        if herbicide-cost + fert-cost + costs-of-production <= savings[
          set income-expectations lput item 0 report-expected-outcomes income-expectations
          set food-expectations lput item 1 report-expected-outcomes food-expectations
          set leisure-expectations lput item 2 report-expected-outcomes leisure-expectations
          set possible-strategies lput "buy both fertiliser and herbicides" possible-strategies
        ]
        ask in-myfield-neighbors with [herbicides = "testing true"][set herbicides false]              ; Reverse the change
        ask in-myfield-neighbors with [fertiliser = "testing moderate"][set fertiliser "low"]
      ]
    ]

  ]

    ; remove "" from list
    set income-expectations remove "" income-expectations
    set food-expectations remove "" food-expectations
    set leisure-expectations remove "" leisure-expectations
    set possible-strategies remove "" possible-strategies

    if length possible-strategies > 0 [
      ; calculate min and max per dimension
      let min-income min income-expectations
      let max-income max income-expectations
      let min-food min food-expectations
      let max-food max food-expectations
      let min-leisure min leisure-expectations
      let max-leisure max leisure-expectations

      ; scale expectations per dimension
      let income-scaled ifelse-value (max-income != min-income) [map [i -> 100 * (i - min-income) / (max-income - min-income)] income-expectations] [map [i -> i * 0] income-expectations] ;
      let food-scaled ifelse-value (max-food != min-food) [map [i -> 100 * (i - min-food) / (max-food - min-food)] food-expectations][map [i -> i * 0] food-expectations]
      let leisure-scaled ifelse-value (max-leisure != min-leisure) [map [i -> 100 * (i - min-leisure) / (max-leisure - min-leisure)] leisure-expectations][map [i -> i * 0] leisure-expectations]

      ; calculate weighted utility sums

    if(Utility-calculation = "rank-sum")[
      let income-rank (3 - position income-relativeimportance sort (list income-relativeimportance food-relativeimportance leisure-relativeimportance))
      let food-rank (3 - position food-relativeimportance sort (list income-relativeimportance food-relativeimportance leisure-relativeimportance))
      let leisure-rank (3 - position leisure-relativeimportance sort (list income-relativeimportance food-relativeimportance leisure-relativeimportance))

      set weighted-utilities (map [[a b c] -> a * (ifelse-value Income? [1] [0]) * income-rank + ; Income? Food? and Leisure? combined are the aspirational dimensions deemed relevant by user = parameter/process
                                              b * (ifelse-value Food? [1] [0]) * food-rank +
                                              c * (ifelse-value Leisure? [1] [0]) * leisure-rank] income-scaled food-scaled leisure-scaled)] ; rank-sum

    if(Utility-calculation = "weighted-sum")[
      set weighted-utilities (map [[a b c] -> a * (ifelse-value Income? [1] [0]) * income-relativeimportance +
                                              b * (ifelse-value Food? [1] [0]) * food-relativeimportance +
                                              c * (ifelse-value Leisure? [1] [0]) * leisure-relativeimportance] income-scaled food-scaled leisure-scaled)] ; weighted-sum

    if(Utility-calculation = "cobb-douglas+")[
      set weighted-utilities (map [[a b c] -> (ifelse-value Income? [1] [0]) * a ^ (income-relativeimportance / 100 ) +                                                               ; cobb-douglas+
                                              (ifelse-value Food? [1] [0]) * b ^ (food-relativeimportance / 100 ) +
                                              (ifelse-value Leisure? [1] [0]) * c ^ (leisure-relativeimportance / 100 )] income-scaled food-scaled leisure-scaled)]

    if(Utility-calculation = "cobb-douglas*")[
      set weighted-utilities (map [[a b c] -> (ifelse-value Income? [1] [0]) * a ^ (income-relativeimportance / 100 ) *                                                               ; cobb-douglas*
                                              (ifelse-value Food? [1] [0]) * b ^ (food-relativeimportance / 100 ) *
                                              (ifelse-value Leisure? [1] [0]) * c ^ (leisure-relativeimportance / 100 )] income-scaled food-scaled leisure-scaled)]

    if(Utility-calculation = "cobb-douglasln")[
      set weighted-utilities (map [[a b c] -> (ifelse-value Income? [1] [0]) * (income-relativeimportance / 100 ) * log (a + 1) 10 +                                                 ; cobb-douglas ln
                                              (ifelse-value Food? [1] [0]) * (income-relativeimportance / 100 ) * log ( b + 1)  10 +
                                              (ifelse-value Leisure? [1] [0]) * (leisure-relativeimportance / 100 ) * log (c + 1) 10 ] income-scaled food-scaled leisure-scaled)]

      set weighted-utilities (map [i -> precision i 3] weighted-utilities)
      let max-utility max weighted-utilities

      ; implement strategy with the highest utility
      let max-position position max-utility weighted-utilities
      let current-position position "stick to current" possible-strategies
      set chosen-strategy ifelse-value (item current-position weighted-utilities = max-utility) ["stick to current"][item max-position possible-strategies]
      implement-chosen-strategy
    ]

end

to implement-chosen-strategy
  if chosen-strategy = "abandon pasture" [ ;1
    change-from-pasture-to-fallow-test
    ask in-myfield-neighbors with [color = grey][set color white]
    set chosen-strategy "abandon land"
  ]
  if chosen-strategy = "abandon maize" [ ;2
    change-from-maize-to-fallow-test
    ask in-myfield-neighbors with [color = grey][set color white]
    set chosen-strategy "abandon land"
  ]
  if chosen-strategy = "abandon vegetable" [ ;3
    change-from-veg-to-fallow-test
    ask in-myfield-neighbors with [color = grey][set color white]
    set chosen-strategy "abandon land"
  ]
  if chosen-strategy = "abandon coffee" [ ;4
    change-from-coffee-to-fallow-test
    ask in-myfield-neighbors with [color = grey][set color white]
    set chosen-strategy "abandon land"
  ]
  if chosen-strategy = "change from fallow to pasture" [ ;1
    change-from-fallow-to-pasture-test
    ask in-myfield-neighbors with [color = grey][set color green]
    set chosen-strategy "change to pasture"
  ]
  if chosen-strategy = "change from maize to pasture" [ ;2
    change-from-maize-to-pasture-test
    ask in-myfield-neighbors with [color = grey][set color green]
    set chosen-strategy "change to pasture"
  ]
  if chosen-strategy = "change from vegetable to pasture" [ ;3
    change-from-veg-to-pasture-test
    ask in-myfield-neighbors with [color = grey][set color green]
    set chosen-strategy "change to pasture"
  ]
  if chosen-strategy = "change from coffee to pasture" [ ;4
    change-from-coffee-to-pasture-test
    ask in-myfield-neighbors with [color = grey][set color green]
    set chosen-strategy "change to pasture"
  ]
  if chosen-strategy = "change from fallow to vegetable" [ ;1
    change-from-fallow-to-veg-test
    ask in-myfield-neighbors with [color = grey][set color orange]
    set chosen-strategy "change to vegetable"
  ]
  if chosen-strategy = "change from maize to vegetable" [ ;2
    change-from-maize-to-veg-test
    ask in-myfield-neighbors with [color = grey][set color orange]
    set chosen-strategy "change to vegetable"
  ]
  if chosen-strategy = "change from pasture to vegetable" [ ;3
    change-from-pasture-to-veg-test
    ask in-myfield-neighbors with [color = grey][set color orange]
    set chosen-strategy "change to vegetable"
  ]
  if chosen-strategy = "change from coffee to vegetable" [ ;4
    change-from-coffee-to-veg-test
    ask in-myfield-neighbors with [color = grey][set color orange]
    set chosen-strategy "change to vegetable"
  ]
  if chosen-strategy = "change from fallow to maize" [ ;1
    change-from-fallow-to-maize-test
    ask in-myfield-neighbors with [color = grey][set color yellow]
    set chosen-strategy "change to maize"
  ]
  if chosen-strategy = "change from vegetable to maize" [ ;2
    change-from-veg-to-maize-test
    ask in-myfield-neighbors with [color = grey][set color yellow]
    set chosen-strategy "change to maize"
  ]
  if chosen-strategy = "change from pasture to maize" [ ;3
    change-from-pasture-to-maize-test
    ask in-myfield-neighbors with [color = grey][set color yellow]
    set chosen-strategy "change to maize"
  ]
  if chosen-strategy = "change from coffee to maize" [ ;4
    change-from-coffee-to-maize-test
    ask in-myfield-neighbors with [color = grey][set color yellow]
    set chosen-strategy "change to maize"
  ]
  if chosen-strategy = "change from fallow to coffee" [ ;1
    change-from-fallow-to-coffee-test
    ask in-myfield-neighbors with [color = grey][set color brown]
    set chosen-strategy "plant coffee"
  ]
  if chosen-strategy = "change from pasture to coffee" [ ;2
    change-from-pasture-to-coffee-test
    ask in-myfield-neighbors with [color = grey][set color brown]
    set chosen-strategy "plant coffee"
  ]
  if chosen-strategy = "change from maize to coffee" [ ;3
    change-from-maize-to-coffee-test
    ask in-myfield-neighbors with [color = grey][set color brown]
    set chosen-strategy "plant coffee"
  ]
  if chosen-strategy = "change from vegetable to coffee" [ ;4
    change-from-veg-to-coffee-test
    ask in-myfield-neighbors with [color = grey][set color brown]
    set chosen-strategy "plant coffee"
  ]
  if chosen-strategy = "buy cow" [
    buy-cow-test
    ask in-mycow-neighbors with [bought? = "yes" and age = 4][set color pink + 2]
  ]
  if chosen-strategy = "buy feed" [
    buy-feed-test
    set adopted-feed? "yes"                                       ; this strategy is adopted once, then kept and implemented automatically for new cattle
    ask in-mycow-neighbors with [supplementary-feed = "testing"][
      set supplementary-feed "yes"]
  ]
  if chosen-strategy = "buy fertiliser" [
    buy-fertiliser-test
    ask in-myfield-neighbors [set fertiliser remove "testing " fertiliser]
  ]
  if chosen-strategy = "sell cow" [
    sell-cow-test
    ask cattle with [color = red][die]
  ]
  if chosen-strategy = "buy herbicides" [
    buy-herbicides-test
    ask in-myfield-neighbors with [herbicides = "testing true"] [set herbicides true]
  ]
  if chosen-strategy = "buy both fertiliser and herbicides" [
    buy-fertiliser-and-herbicides-test
    ask in-myfield-neighbors with [herbicides = "testing true"] [set herbicides true]
    ask in-myfield-neighbors with [fertiliser = "testing moderate"] [set fertiliser "moderate"]
  ]

end

to loop-through-strategies                                               ; Sub-functions: calc-expected-outcome, change-from-x-to-x-test, buy-cow-test, buy-feed-test, buy-fertiliser-test

  ; if who = test-hh [print "_______________________________________________________________"]

   foreach loop-order [                                                  ; loop through farm management options and check if they will fullfill aspirations

          x ->                                                           ; it will run this loop for each x in the list loop-order. loop-order is a sorted list with all the options available to households. the length of the list differs per household.
                                                                         ; a household may have chosen a strategy before it has gone through the whole list. in this case, it will still finish the loop, but not choose anything because a condition for
                                                                         ; choosing is that chosen-strategy = 0.
    ; reset (testing things)
    set test-herd herd  ; this is needed for when cattle strategy is not chosen
    ;set test-cowsales 0
    set testing-strategy ""
    if chosen-strategy = 0 [calc-expected-outcome]

    ; STICK TO CURRENT?
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    if (x = "stick to current" and chosen-strategy = 0)[
      set testing-strategy "stick to current"
                                                                         ; No conditions need to be met to continue with current management, and no test conditions need to be changed
      calc-expected-outcome                                              ; Calculate expected outcomes resulting from the current management

      ; change ifelse function to allow switching on and off

      ifelse compare-expectations-to-thresholds [
        set chosen-strategy "stick to current"                           ; If they do, implement current management
      ][
        ; do nothing                                                     ; Else, do nothing
      ]


    ]

    ; ABANDON LAND?                                                      ; Check for consecutive land uses whether it is worthwhile to abandon, starting with pasture, maize, vegetables and then coffee.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    if (x = "abandon land" and chosen-strategy = 0)[
      set testing-strategy "abandon land"

      if (fallowland <= farmland) [                                      ; If all land has been abandoned already, it's not possible to abandom more

        if (chosen-strategy = 0 and privatepasture > 0)[                 ; First, check if it is possible to change from PASTURE to FALLOW?????????????????????
          change-from-pasture-to-fallow-test                             ; Change a pasture into fallow
          calc-expected-outcome                                          ; Calculate expected outcome resulting from the current manamgeent + the change

          ifelse compare-expectations-to-thresholds [
            set chosen-strategy "abandon land"                           ; If they do, implement change
            ask in-myfield-neighbors with [color = grey][
              set color white]
          ][
            ask in-myfield-neighbors with [color = grey][
              set color green                                            ; Else, reverse the change
              set current-land-use "pasture"]
          ]
        ]

        if (chosen-strategy = 0 and maizeland > 0)[                      ; Second, check if it is possible to change from MAIZE to FALLOW?????????????????????
          change-from-maize-to-fallow-test                               ; Change a maize field into fallow
          calc-expected-outcome                                          ; Calculate expected outcomes resulting from the current management + the change

          ifelse compare-expectations-to-thresholds [
            set chosen-strategy "abandon land"                           ; If they do, implement change
            ask in-myfield-neighbors with [color = grey][
              set color white]
          ][
            ask in-myfield-neighbors with [color = grey][
              set color yellow                                           ; Else, reverse the change
              set current-land-use "maize"]
            ]

        ]
      ]

      if (chosen-strategy = 0 and vegetableland > 0)[                    ; Third, check if it is possible to change from VEGETABLE to FALLOW?????????????????????
        change-from-veg-to-fallow-test                             ; Change a vegetable field into fallow
        calc-expected-outcome                                            ; Calculate expected outcomes resulting from the current management + the change

        ifelse compare-expectations-to-thresholds [
          set chosen-strategy "abandon land"                             ; If they do, implement change
          ask in-myfield-neighbors with [color = grey][
            set color white]
        ][
          ask in-myfield-neighbors with [color = grey][
            set color orange                                             ; Else, reverse the change
            set current-land-use "vegetable"]
        ]
      ]

      if (chosen-strategy = 0 and coffeeland > 0 and                     ; Fourth and last, check if it is possible to change from COFFEE to FALLOW?????????????????????
          count in-myfield-neighbors with [coffee-age > 7 and            ; also check whether the fields are more tan 7 years old, otherwise they cannot be changed into another land use --> parameter possible-to-change-from-coffee-after
                                           current-land-use = "coffee"] > 0) [
        change-from-coffee-to-fallow-test                                ; Change a coffee field into fallow
        calc-expected-outcome                                            ; Calculate expected outcomes resulting from the current management + the change

        ifelse compare-expectations-to-thresholds [
          set chosen-strategy "abandon land"                             ; If they do, implement change
          ask in-myfield-neighbors with [color = grey][
            set color white]
        ][
          ask in-myfield-neighbors with [color = grey][
            set color brown                                              ; Else, reverse the change
            set current-land-use "coffee" ]
        ]
      ]

    ]

    ; CHANGE TO PASTURE?                                                 ; Check for consecutive land uses whether it is possible to change from that land use to pasture, starting with fallow, maize, vegetables and then coffee.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    if (x = "change to pasture" and chosen-strategy = 0) [
      set testing-strategy "change to pasture"
      ; if who = test-hh [print (word "household " who " is testing: " x " | " testing-strategy)]

        if farmland > privatepasture [                                   ; if all of the farmers land has already been changed to pasture, changing to pasture is not possible anymore

          if (chosen-strategy = 0 and fallowland > 0) [                  ; first, check if it is possible to change from FALLOW to PASTURE?????????????????????

            change-from-fallow-to-pasture-test                           ; change a fallow field into pasture
            calc-expected-outcome                                        ; calculate expected outcomes resulting from the current management + the change

            ifelse compare-expectations-to-thresholds [

              set chosen-strategy "change to pasture"                    ; If they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color green
            ]
            ][ ask in-myfield-neighbors with [color = grey][
                set color white set current-land-use "fallow"            ; Else, reverse change
            ]]
          ]


      if (chosen-strategy = 0 and maizeland > 0) [                       ; second, check if it is possible to change from MAIZE to PASTURE?????????????????????

          change-from-maize-to-pasture-test                            ; change a maize field into pasture
          calc-expected-outcome                                        ; calculate expected outcomes resulting from the current management + the change

            ifelse compare-expectations-to-thresholds [

              set chosen-strategy "change to pasture"                    ; If they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color green
            ]
            ][ ask in-myfield-neighbors with [color = grey][
                set color yellow set current-land-use "maize"            ; Else, reverse change
            ]]
          ]


        if (chosen-strategy = 0 and vegetableland > 0) [                 ; third, check if it is possible to change from VEGETABLES to PASTURE??????????????
            change-from-veg-to-pasture-test                        ; change a vegetable field into pasture
            calc-expected-outcome                                        ; calculate expected outcomes resulting from the current management + the change

            ifelse compare-expectations-to-thresholds [
            set chosen-strategy "change to pasture"                      ; if they do, implement change
            ask in-myfield-neighbors with [color = grey][
              set color green
            ]
            ][ask in-myfield-neighbors with [color = grey][
              set color orange set current-land-use "vegetable"          ; else, reverse change
          ]]
        ]



        if (chosen-strategy = 0 and coffeeland > 0 and                  ; fourth and last, check if it is possible to change from COFFEE to PASTURE??????????????
            count in-myfield-neighbors with [coffee-age > 7 and         ; also check whether the fields are more tan 7 years old, otherwise they cannot be changed into another land use --> parameter possible-to-change-from-coffee-after
                                             current-land-use = "coffee"] > 0) [
            change-from-coffee-to-pasture-test                          ; change a coffee field into pasture
            calc-expected-outcome                                       ; calculate expected outcomes resulting from the current management + the change

            ifelse compare-expectations-to-thresholds [
            set chosen-strategy "change to pasture"                     ; if they do, implement change
            ask in-myfield-neighbors with [color = grey][
              set color green
            ]
            ][ask in-myfield-neighbors with [color = grey][
              set color brown set current-land-use "coffee"]]           ; else, reverse change
          ]
        ]
      ]

      ; CHANGE TO VEGETABLE?                                            ; check for consecutive land uses whether it is possible to change from that land use to vegetable, starting with fallow, maize, pasture, and then coffee.
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      if (x = "change to vegetable" and chosen-strategy = 0)[
      set testing-strategy "change to vegetable"
      ; if who = test-hh  [print (word "household " who " is testing: " x " | " testing-strategy)]

      if farmland > vegetableland [                                     ; if all of the farmers land has already been changed to vegetable, changing to vegetable is not possible anymore

        if (chosen-strategy = 0 and fallowland > 0)[                    ; first, check if it is possible to change from FALLOW to VEGETABLE??????????????????
            change-from-fallow-to-veg-test                        ; change a fallow field into vegetable
            calc-expected-outcome                                       ; calculate expected outcomes resulting from the current management + the change

            ifelse compare-expectations-to-thresholds [
              set chosen-strategy "change to vegetable"                 ; if they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color orange
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color white set current-land-use "fallow"]]           ; else, reverse change
          ]

          if (chosen-strategy = 0 and maizeland > 0)[                   ; second, check if it is possible to change from MAIZE to VEGETABLE??????????????????
            change-from-maize-to-veg-test                         ; change a maize field into vegetable
            calc-expected-outcome                                       ; calculate expected outcomes resulting from the current management + the change

            ifelse compare-expectations-to-thresholds [
              set chosen-strategy "change to vegetable"                 ; if they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color orange
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color yellow set current-land-use "maize"]]           ; else, reverse change
          ]

          if (chosen-strategy = 0 and privatepasture > 0)[              ; third, check if it is possible to change from PASTURE to VEGETABLE???????????????????
            change-from-pasture-to-veg-test                       ; change a pasture into vegetable
            calc-expected-outcome                                       ; calculate expected outcomes resulting from the current management + the change


            ifelse compare-expectations-to-thresholds [
              set chosen-strategy "change to vegetable"                 ; if they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color orange
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color green set current-land-use "pasture"]]          ; else, reverse change
          ]

          if (chosen-strategy = 0 and coffeeland  > 0 and               ; fourth, and last, check if it is possible to change from COFFEE to VEGETABLE???????????????????
              count in-myfield-neighbors with [coffee-age > 7 and       ; also check whether the fields are more tan 7 years old, otherwise they cannot be changed into another land use --> parameter possible-to-change-from-coffee-after
                                               current-land-use = "coffee"] > 0) [
            change-from-coffee-to-veg-test                        ; change a coffee field into vegetable
            calc-expected-outcome                                       ; calculate expected outcomes resulting from the current management + the change


            ifelse compare-expectations-to-thresholds [
              set chosen-strategy "change to vegetable"                 ; if they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color orange
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color brown set current-land-use "coffee"]]           ; else, reverse change
          ]
        ]
      ]

      ; CHANGE TO MAIZE?                                                ; check for consecutive land uses whether it is possible to change from that land use to maize, starting with fallow, vegetable, pasture, and then coffee.
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      if (x = "change to maize" and chosen-strategy = 0)[
      set testing-strategy "change to maize"
        ; if who = test-hh  [print (word "household " who " is testing: " x " | " testing-strategy)]

        if farmland > maizeland [                                       ; if all of the farmers land has already been changed to maize, changing to maize is not possible anymore


          if (chosen-strategy = 0 and fallowland > 0)[                  ; first check if it is possible to change from FALLOW to MAIZE????????????????????
            change-from-fallow-to-maize-test                            ; change a fallow field into maize
            calc-expected-outcome                                       ; calculate expected outcomes resulting from the current management + the change


            ifelse compare-expectations-to-thresholds [
              set chosen-strategy "change to maize"                     ; if they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color yellow
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color white set current-land-use "fallow"]]           ; else, reverse change
          ]

          if (chosen-strategy = 0 and vegetableland > 0)[               ; second, check if it is possible to change from VEGETABLE to MAIZE????????????????????
            change-from-veg-to-maize-test                         ; change a vegetable field into maize
            calc-expected-outcome                                       ; calculate expected outcomes resulting from the current management + the change

            ifelse compare-expectations-to-thresholds [
              set chosen-strategy "change to maize"                     ; if they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color yellow
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color orange set current-land-use "vegetable"]]       ; else, reverse change
          ]

          if (chosen-strategy = 0 and privatepasture > 0)[              ; third, check if it is possible to change from PASTURE to MAIZE????????????????????????
            change-from-pasture-to-maize-test                           ; change a pasture into maize
            calc-expected-outcome                                       ; calculate expected outcomes resulting from the current management + the change

            ifelse compare-expectations-to-thresholds [
              set chosen-strategy "change to maize"                     ; if they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color yellow
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color green set current-land-use "pasture"]]          ; else, reverse change
          ]

          if (chosen-strategy = 0 and coffeeland > 0 and                ; fourth, and last, check if it is possible to change from COFFEE to MAIZE????????????????????????
              count in-myfield-neighbors with [coffee-age > 7 and       ; also check whether the fields are more tan 7 years old, otherwise they cannot be changed into another land use --> parameter possible-to-change-from-coffee-after
                                               current-land-use = "coffee"] > 0) [
            change-from-coffee-to-maize-test                            ; change a coffee field into maize
            calc-expected-outcome                                       ; calculate expected outcomes resulting from the current management + the change

            ifelse compare-expectations-to-thresholds [
              set chosen-strategy "change to maize"                     ; if they do, implement change
              ask in-myfield-neighbors with [color = grey][
              set color yellow
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color brown set current-land-use "coffee"]]           ; else, reverse change
          ]
        ]
      ]

      ; PLANT COFFEE?                                                   ; check for consecutive land uses whether it is possible to change from that land use to coffee, starting with fallow, pasture, maize, and then vegetable.
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      if (x = "plant coffee" and chosen-strategy = 0)[
       set testing-strategy "plant coffee"
        ;if who = test-hh  [print (word "household " who " is testing: " x " | " testing-strategy)]

        if farmland > coffeeland [                                      ; if all of the farmers land has already been changed to maize, changing to maize is not possible anymore

          if (chosen-strategy = 0 and fallowland > 0)[                  ; first, check if it is possible to change from FALLOW to COFFEE????????????????????
            change-from-fallow-to-coffee-test                           ; change a fallow field into coffee
            calc-expected-outcome                                       ; calculate expected outcomes resulting from the current management + the change

            let estimated-cost (costs-of-production +                   ; calculate the expected cost of establishing coffee
                                global-coffeeplantation-price * sum [field-size] of in-myfield-neighbors with [color = grey])

            ifelse compare-expectations-to-thresholds and
                   savings >= estimated-cost [                          ; also check if the general production costs + the expected cost of establishing the plantation can be covered by savings
              set chosen-strategy "plant coffee"                        ; if they do, and the farmers have sufficient savings, implement change
              ask in-myfield-neighbors with [color = grey][
              set color brown set coffee-age 0
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color white set current-land-use "fallow"]]           ; else, reverse change
          ]

          if (chosen-strategy = 0 and privatepasture > 0)[              ; second, check if it is possible to change from PASTURE to COFFEE????????????????????
            change-from-pasture-to-coffee-test                          ; change a pasture into coffee
            calc-expected-outcome                                       ; calculate expected outcomes resulting from the current management + the change

            let estimated-cost (costs-of-production +                   ; calculate the expected cost of establishing coffee
                                global-coffeeplantation-price * sum [field-size] of in-myfield-neighbors with [color = grey])

            ifelse compare-expectations-to-thresholds and
                   savings >= estimated-cost [                          ; also check if the general production costs + the expected cost of establishing the plantation can be covered by savings
              set chosen-strategy "plant coffee"                        ; if they do, and the farmers have sufficient savings, implement change
              ask in-myfield-neighbors with [color = grey][
              set color brown set coffee-age 0
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color green set current-land-use "pasture"]]          ; else, reverse change
          ]

          if (chosen-strategy = 0 and maizeland > 0)[                   ; third, check if it is possible to change from MAIZE to COFFEE????????????????????????
            change-from-maize-to-coffee-test                            ; change a maize field into coffee
            calc-expected-outcome                                       ; calculate expected outcomes resulting from the current management + the change

            let estimated-cost (costs-of-production +                   ; calculate the expected cost of establishing coffee
                                global-coffeeplantation-price * sum [field-size] of in-myfield-neighbors with [color = grey])

            ifelse compare-expectations-to-thresholds and
                   savings >= estimated-cost [                          ; also check if the general production costs + the expected cost of establishing the plantation can be covered by savings
              set chosen-strategy "plant coffee"                        ; if they do, and the farmers have sufficient savings, implement change
              ask in-myfield-neighbors with [color = grey][
              set color brown set coffee-age 0
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color yellow set current-land-use "maize"]]           ; else, reverse change
          ]

          if (chosen-strategy = 0 and vegetableland > 0)[               ; fourth, and last, check if it is possible to change from VEGETABLE to COFFEE????????????????????????
            change-from-veg-to-coffee-test                        ; change a vegetable field into coffee
            calc-expected-outcome                                       ; calculate expected outcomes resulting from the current management + the change

          let estimated-cost (costs-of-production +                     ; calculate the expected cost of establishing coffee
                                global-coffeeplantation-price * sum [field-size] of in-myfield-neighbors with [color = grey])

            ifelse compare-expectations-to-thresholds and
                   savings >= estimated-cost [                          ; also check if the general production costs + the expected cost of establishing the plantation can be covered by savings
              set chosen-strategy "plant coffee"                        ; if they do, and the farmers have sufficient savings, implement change
              ask in-myfield-neighbors with [color = grey][
              set color brown set coffee-age 0
            ]
            ][ ask in-myfield-neighbors with [color = grey][
              set color orange set current-land-use "vegetable"]]       ; else, reverse change
          ]
        ]
      ]

      ; BUY COW?                                                        ; check if buying a cow can satisfice farmers
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      if (x = "buy cow" and chosen-strategy = 0)[
       set testing-strategy "buy cow"
       ; if who = test-hh  [print (word "household " who " is checking whether it can afford: " x " | " testing-strategy)]
      let mycapacity floor (((feedproduction * 1000) / 365 )  / 3.15) ; 3.15 = parameter feed-lowerthreshold-nosup

      if (savings > (costs-of-production + global-cow-purchase-price) and ; farmers need to have enough savings to cover the additional cost of buying a cow
          (mycapacity > herd or adopted-feed? = "yes")) [                ; need to be able to feed the new cow
        ; if who = test-hh  [print (word "household " who " can afford: " x " | " testing-strategy " so proceeds to testing")]
         buy-cow-test                                                   ; buy a cow
         calc-expected-outcome                                          ; calculate expected outcomes resulting from the current management + the change

       ifelse compare-expectations-to-thresholds [
        set chosen-strategy "buy cow"                                   ; if they do, and the farmers have sufficient savings, implement change
       ; if who = test-hh [print (word "hh " who " expected cowcost: " test-cowcost " || accept buy-cow-test, keep change")]
          ask in-mycow-neighbors with [bought? = "yes" and age = 4][
            set color pink + 2]
       ][                                                              ; else, reverse change
          set test-cowcost 0
         ; if who = test-hh [print (word "hh " who " expected cowcost: " test-cowcost " || reverse buy-cow-test")]
          ask in-mycow-neighbors with [bought? = "yes" and age = 4][
            die
          ]
       ]
     ]
   ]

    ; BUY FEED?                                                         ; check if feeding cattle supplementary feed can satisfice. all cattle are given if this choice is made, and once adopted, the choice cannot be reversed.
                                                                        ; future offspring will also be fed supplementary feed.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    if (x = "buy feed" and chosen-strategy = 0)[
      set testing-strategy "buy feed"
      ;if who = test-hh  [print (word "household " who " is testing: " x " | " testing-strategy)]

      let cows-on-supplement count in-mycow-neighbors with [supplementary-feed = "yes"]
      let on-supplement ifelse-value (cows-on-supplement > 0)["yes"]["no"]
      let estimated-cost costs-of-production +
                         (global-supplementary-feed-price * herd)       ; calculate the expected cost of feeding the herd supplementary feed

      ;if who = test-hh  [print (word "household " who " needs " estimated-cost ", has " savings)]

    if (savings >= estimated-cost and                                   ; need to be able to buy the feed
        herd > 0 and                                                    ; need to have cattle to feed
        on-supplement = "no")[                                          ; and need to not already be feeding supplementary feed
        buy-feed-test                                                   ; adopt supplementary feeding
        calc-expected-outcome                                           ; calculate expected outcomes resulting from the current management + the change

        ifelse compare-expectations-to-thresholds [
          set chosen-strategy "buy feed"                                ; if they do, and the farmers have sufficient savings, implement change
          set adopted-feed? "yes"                                       ; this strategy is adopted once, then kept and implemented automatically for new cattle
          ask in-mycow-neighbors with [supplementary-feed = "testing"][
            set supplementary-feed "yes"]
        ][
          set adopted-feed? "no"
          set test-feedcost 0
          ask in-mycow-neighbors with [supplementary-feed = "testing"][
            set supplementary-feed "no"
          ]                                ; else, reverse change
        ]
      ]
    ]

    ; BUY FERTILISER?                                                   ; check if applying more fertiliser can satisfice. households can choose to apply more fertiliser to one field per tick, every field with arable crops is eligible.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    if (x = "buy fertiliser" and chosen-strategy = 0)[
      set testing-strategy "buy fertiliser"

                                                                        ; number of fields that are eligible for application of fertiliser
      let eligible-fields count in-myfield-neighbors with [current-land-use = "maize"] +
                          count in-myfield-neighbors with [current-land-use = "vegetable"] +
                          count in-myfield-neighbors with [current-land-use = "coffee"]
                                                                        ; number of fields have received the maximum amount of fertiliser
      let maxed-out-fields count in-myfield-neighbors with [fertiliser = "very high"]

      if (eligible-fields > 0 and                                       ; there need to be fields that can benefit from fertiliser application
          maxed-out-fields < eligible-fields ) [                        ; and there need to be fields that have not already been maxed out

        buy-fertiliser-test
        let fert-cost calc-cost-of-fertiliser-test                      ; calculate cost of "testing" field only

        ;if who = test-hh [ifelse (fert-cost + costs-of-production) <= savings [print (word "has enough savings to buy fertiliser")][print (word "does NOT have enough savings to buy fertiliser")]]

        ifelse (fert-cost + costs-of-production) <= savings [           ; there needs to be enough money to buy fertiliser
          calc-expected-outcome

          ifelse compare-expectations-to-thresholds [
            set chosen-strategy "buy fertiliser"                        ; if they do, and if the hh has sufficient savings, implement change
            ask in-myfield-neighbors [
              set fertiliser remove "testing " fertiliser]              ; take away "testing " from string so that only the fertiliser level remains (low, moderate, high or very high)
          ][
            ask in-myfield-neighbors [                                  ; else, reverse change after checking expectations
              set fertiliser (ifelse-value
                fertiliser = "testing moderate"         ["low"]
                fertiliser = "testing high"             ["moderate"]
                fertiliser = "testing very high"        ["high"]
                                                        [fertiliser]
              )
            ]
          ]
        ][
          ask in-myfield-neighbors [                                   ; else, reverse change from before checking expectations because hh does not have enough money to buy fertiliser
              set fertiliser (ifelse-value
              fertiliser = "testing moderate"             ["low"]
              fertiliser = "testing high"                 ["moderate"]
              fertiliser = "testing very high"            ["high"]
                                                          [fertiliser]
              )
            ]
        ]
      ]
    ]

    ; SELL COW?                                                       ; check if selling cattle can help households fulfill their aspirations
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    if (x = "sell cow" and chosen-strategy = 0)[
      set testing-strategy "sell cow"
      ; if who = test-hh  [print (word "household " who " is checking whether it can: " x " | " testing-strategy)]
      let mycapacity floor (((feedproduction * 1000) / 365 )  / 3.15) ; 3.15 = parameter feed-lowerthreshold-nosup

      if (count in-mycow-neighbors with [calf < 4 and sex = "female"] > 0 and (savings <= 0 or mycapacity < herd))[
        sell-cow-test
        calc-expected-outcome

        ifelse compare-expectations-to-thresholds [
          set chosen-strategy "sell cow"
          ; if who = test-hh [print (word "hh " who " expected cowsales: " test-cowsales " || accept sell-cow-test, keep change")]
          ask cattle with [color = red][die]
        ][
          ask cattle with [color = red][set color pink + 2]
          set test-cowsales 0
        ]

      ]

    ]

    ; BUY HERBICIDES                                                      ; check if selling cattle can help households fulfill their aspirations
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    if (x = "buy herbicides" and chosen-strategy = 0)[
      set testing-strategy "buy herbicides"
      ;print (word "testing " testing-strategy)

      let eligible-fields count in-myfield-neighbors with [current-land-use = "maize" and fertiliser != "low"] +
                          count in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser != "low"] +
                          count in-myfield-neighbors with [current-land-use = "coffee" and fertiliser != "low"]
      let maxed-out-fields count in-myfield-neighbors with [herbicides = true]

      if (eligible-fields > 0 and maxed-out-fields < eligible-fields)[

        buy-herbicides-test
        let herbicide-cost sum [field-size] of in-myfield-neighbors with [herbicides = "testing true"] * global-herbicides-price

        ifelse (herbicide-cost + costs-of-production) <= savings [
          ifelse compare-expectations-to-thresholds [
            calc-expected-outcome
            ask in-myfield-neighbors with [herbicides = "testing true"][set herbicides true]
            set chosen-strategy "buy herbicides"
          ][
            ask in-myfield-neighbors with [herbicides = "testing true"][set herbicides false]
          ]
        ][
          ask in-myfield-neighbors with [herbicides = "testing true"][set herbicides false]
        ]
      ]
    ]

    ; buy both fertiliser and herbicides                                                      ; check if selling cattle can help households fulfill their aspirations
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    if (x = "buy both fertiliser and herbicides" and chosen-strategy = 0)[
      set testing-strategy "buy herbicides"

      let eligible-fields count in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "low"] +
                          count in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "low"] +
                          count in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "low"]
      let maxed-out-fields count in-myfield-neighbors with [herbicides = true]

      if (eligible-fields > 0 and maxed-out-fields < eligible-fields)[

        buy-fertiliser-and-herbicides-test
        let herbicide-cost sum [field-size] of in-myfield-neighbors with [herbicides = "testing true"] * global-herbicides-price
        let fert-cost calc-cost-of-fertiliser-test                 ; calculate cost of "testing" field only

        ifelse herbicide-cost + fert-cost + costs-of-production <= savings [
          calc-expected-outcome
          ifelse compare-expectations-to-thresholds [
            ask in-myfield-neighbors with [herbicides = "testing true"][set herbicides true]
            ask in-myfield-neighbors with [fertiliser = "testing moderate"][set fertiliser "moderate"]
            set chosen-strategy "buy both fertiliser and herbicides"
          ][
            ask in-myfield-neighbors with [herbicides = "testing true"][set herbicides false]
            ask in-myfield-neighbors with [fertiliser = "testing moderate"][set fertiliser "low"]
          ]
        ][
          ask in-myfield-neighbors with [herbicides = "testing true"][set herbicides false]
          ask in-myfield-neighbors with [fertiliser = "testing moderate"][set fertiliser "low"]
        ]
      ]
    ]
  ] ; end loop

end

to perform-chosen-management                                             ; Sub-functions: calc-maize-prod, calc-maize-profit, calc-vegetable-prod, calc-vegetable-profit, calc-feed-production, age-cattle, ...
                                                                         ;                give-new-calves-supplementary-feed, underfed-die, calc-milk-prod, milk-to-butter, calc-butter-profit, make-oxen, sell-cattle
                                                                         ;                age-coffee, calc-coffee-prod, calc-coffee-profit, calc-labour
  calc-maize-prod
  calc-veg-prod
  calc-feed-production
  age-cattle
  give-new-calves-supplementary-feed
  underfed-die
  calc-milk-prod

  calc-food-consume

  milk-to-butter
  make-oxen
  sell-cattle
  age-coffee
  calc-coffee-prod

  calc-maize-profit
  calc-veg-profit
  calc-butter-profit
  calc-coffee-profit

  calc-labour
end

to update-hh-characteristics                                             ; No sub-functions
  ask households [
    ; land use
    set maizeland sum [field-size] of in-myfield-neighbors with [current-land-use = "maize"]
    set vegetableland sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable"]
    set privatepasture sum [field-size] of in-myfield-neighbors with [current-land-use = "pasture"]
    set coffeeland sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee"]
    set fallowland sum [field-size] of in-myfield-neighbors with [current-land-use = "fallow"]
    ; cattle
    set herd count in-mycow-neighbors
  ]
end

to calc-aspirational-outcomes                                       ; No sub-functions
  ask households [
    set value-of-consumed-products maize-consumed * global-maize-price + maizepurchase / 1.2 + (milk-consumed / milk-to-butter-conversion-factor) * global-butter-price + butter-consumed * global-butter-price + coffee-consumed * global-coffee-price + veg-consumed * global-veg-price; birr per household per year
    set food-purchases maizepurchase ; birr per household per year
    set costs-of-production maizecost + vegcost + coffeecost + supplementaryfeedcost + cowcost; for all products except butter we haven't added costs yet
    set earnings-from-sales maizesales + vegsales + buttersales + bullsales + oxensales + coffeesales + cowsales

    set income-outcome-t1end earnings-from-sales - costs-of-production; can be negative if households have to buy maize (negative maizeprofit), assumes same price for selling and buying, this is not so realistic
    set savings savings + income-outcome-t1end - food-purchases
    ;set food-outcome-t1end maize-consumed / 56
    set food-outcome-t1end ifelse-value (maize-left > 0) [(maizeproduction - maize-left) / 56] [maizeproduction / 56]
    set leisure-outcome-t1end ((labourcap - laboursum)) / 56 ; leisure days per week
  ]
end

to share-knowledge
  ask households with [chosen-strategy != 0] [                             ; If households did not choose a new strategy, there is no new knowledge to share
    ask out-influencer-neighbors [                                         ; Out-influencer-neighbors of x are households that are influenced by a household x
      ifelse member? [chosen-strategy] of myself known-strategies                      ; Check of the neighbors already know the strategy that the household chose
      [set known-strategies known-strategies]                              ; No change
      [set known-strategies lput [chosen-strategy] of myself known-strategies]         ; New strategy is added to neighbors knowledge
    ]
  ]
end

to calc-gini                                                        ; No sub-functions
  let g0_10 sum sublist sort [income-outcome-t1start] of households ceiling (Number-of-households * 0.00) ceiling (Number-of-households * 0.10) / sum [income-outcome-t1start] of households
  let g10_25 sum sublist sort [income-outcome-t1start] of households ceiling (Number-of-households * 0.10) ceiling (Number-of-households * 0.25) / sum [income-outcome-t1start] of households
  let g25_50 sum sublist sort [income-outcome-t1start] of households ceiling (Number-of-households * 0.25) ceiling (Number-of-households * 0.50) / sum [income-outcome-t1start] of households
  let g50_75 sum sublist sort [income-outcome-t1start] of households ceiling (Number-of-households * 0.50) ceiling (Number-of-households * 0.75) / sum [income-outcome-t1start] of households
  let g75_90 sum sublist sort [income-outcome-t1start] of households ceiling (Number-of-households * 0.75) ceiling (Number-of-households * 0.90) / sum [income-outcome-t1start] of households
  let g90_100 sum sublist sort [income-outcome-t1start] of households ceiling (Number-of-households * 0.90) ceiling (Number-of-households * 1.00) / sum [income-outcome-t1start] of households

  set gini (0.5 * g0_10 * 10) +  (0.5 * g10_25 * 15 + g0_10 * 15) + (0.5 * g25_50 * 25 + g10_25 * 25) + (0.5 * g50_75 * 25 + g25_50 * 25) + (0.5 * g75_90 * 15 + g50_75 * 15) + (0.5 * g90_100 * 10 + g75_90 * 10)
end

to write-to-file
  if ticks = 0 [
  file-print ( ; column names
    word

    ; basic
      "household;hhid;year;Strategy_order;Make_decisions;Utility_calculation;Threshold_type;Internal_external"
      ";time_of_run;optimism;b_no;remember_t3;remember_t2;remember_t1"
      ";run_number;region;woreda;Population;Repetition"

    ; aspiration outcomes
      ";income_outcome_t1start;income_outcome_t1end;income_outcome_t2;income_outcome_t3"
      ";leisure_outcome_t1start;leisure_outcome_t1end;leisure_outcome_t2;leisure_outcome_t3"
      ";food_outcome_t1start;food_outcome_t1end;food_outcome_t2;food_outcome_t3"
      ";satisficed"

     ; aspiration related outcomes
      ";savings;value_of_consumed-products;food_purchases;costs_of_production;earnings_from_sales;maize_consumed"

     ; disaggregated
      ";maizecost;vegetablecost;coffeecost;supplementaryfeedcost;cowcost"
      ";maizesales;vegetablesales;coffeesales;buttersales;oxensales;bullsales;cowsales"

     ; expected costs
      ";test_maizecost;test_vegetablecost;test_coffeecost;test_feedcost;test_cowcost"
      ";test_maizesales;test_vegetablesales;test_coffeesales;test_buttersales;test_oxensales;test_bullsales;test_cowsales"

    ; aspiration thresholds
      ";income_threshold_mean;income_threshold_t1start;income_threshold_t1end;income_threshold_t2;income_threshold_t3;income_on"
      ";leisure_threshold_mean;leisure_threshold_t1start;leisure_threshold_t1end;leisure_threshold_t2;leisure_threshold_t3;leisure_on"
      ";food_threshold_mean;food_threshold_t1start;food_threshold_t1end;food_threshold_t2;food_threshold_t3;food_on"

    ; aspiration gaps
      ";incomegap"
      ";leisuregap"
      ";foodgap"

    ; aspiration relative importance
      ";relativeimportanceincome"
      ";relativeimportanceleisure"
      ";relativeimportancefood"

    ; experience weight (ew)
      ";ew_income"
      ";ew_leisure"
      ";ew_food"

    ; chosen strategies
      ";chosen_strategy;known_strategies"
      ";income_expectation;leisure_expectation;food_expectation"
      ";income_expectationS;leisure_expectationS;food_expectationS;weighted_utilities;possible_strategies"

    ; household land use
      ";fields;maizeland;vegetableland;coffeeland;privatepasture;fallowland;farmland"
      ";maize_0fert_0herb;maize_1fert_0herb;maize_1fert_1herb;maize_2fert_0herb;maize_2fert_1herb;maize_3fert_0herb;maize_3fert_1herb"
      ";veg_0fert_0herb;veg_1fert_0herb;veg_1fert_1herb;veg_2fert_0herb;veg_2fert_1herb;veg_3fert_0herb;veg_3fert_1herb"
      ";coffee_0fert_0herb;coffee_1fert_0herb;coffee_1fert_1herb;coffee_2fert_0herb;coffee_2fert_1herb;coffee_3fert_0herb;coffee_3fert_1herb"
      ";unfertilisedland"
      ";moderatefertilisedland"
      ";highlyfertilisedland"
      ";veryhighlyfertilisedland"
      ";maizefertilisedland"
      ";vegetablefertilisedland"
      ";coffeefertilisedland"

    ; food production
      ";maizeproduction;vegetableproduction;coffeeproduction;milkproduction"

    ; livestock
     ";herd"
     ";on_supplementary_feed"
     ";adopted_feed"

    ; household characteristics
      ";hhmembers;adults;elderly;children;network_size"

    ; temporal variability
      ";price_sd;price_mean;price_multiplier;yield_sd;yield_mean;yield_multiplier"

    ; check whether colnames and values match
      ";match_check"


    )
  ]

  foreach sort households [ t ->
    ask t [
      file-print (
        word

        ; basic
        self " ;" name " ;" ticks " ;" Strategy-order " ;" Search-method " ;" Utility-calculation " ;" Threshold-type " ;" Threshold-adjustment
        " ;" date-and-time " ;" optimism " ;" b-no " ;" remember-t3 " ;" remember-t2 " ;" remember-t1
        " ;" behaviorspace-run-number " ;" region " ;" woreda " ;" Population-nr " ;" Repetition-nr

        ; aspiration outcomes
        " ;" income-outcome-t1start  " ;" income-outcome-t1end  " ;" income-outcome-t2  " ;" income-outcome-t3
        " ;" leisure-outcome-t1start " ;" leisure-outcome-t1end " ;" leisure-outcome-t2 " ;" leisure-outcome-t3
        " ;" food-outcome-t1start    " ;" food-outcome-t1end    " ;" food-outcome-t2    " ;" food-outcome-t3
        " ;" satisficed

        ; aspiration related outcomes
        " ;" savings " ;" value-of-consumed-products " ;" food-purchases " ;" costs-of-production " ;" earnings-from-sales " ;" maize-consumed

        ; disaggregated
        " ;" maizecost " ;" vegcost " ;" coffeecost " ;" supplementaryfeedcost " ;" cowcost
        " ;" maizesales " ;" vegsales " ;" coffeesales " ;" buttersales " ;" oxensales " ;" bullsales " ;" cowsales

        ; expected costs
        " ;" test-maizecost " ;" test-vegcost " ;" test-coffeecost " ;" test-feedcost " ;" test-cowcost
        " ;" test-maizesales " ;" test-vegsales " ;" test-coffeesales " ;" test-buttersales " ;" test-oxensales " ;" test-bullsales " ;" test-cowsales

        ; aspiration thresholds
        " ;" income-threshold-mean  " ;" income-threshold-t1start  " ;" income-threshold-t1end  " ;" income-threshold-t2  " ;" income-threshold-t3 " ;" Income?
        " ;" leisure-threshold-mean " ;" leisure-threshold-t1start " ;" leisure-threshold-t1end " ;" leisure-threshold-t2 " ;" leisure-threshold-t3 " ;" Leisure?
        " ;" food-threshold-mean    " ;" food-threshold-t1start    " ;" food-threshold-t1end    " ;" food-threshold-t2    " ;" food-threshold-t3 " ;" Food?

        ; aspiration gaps
        " ;" income-gap
        " ;" leisure-gap
        " ;" food-gap

        ; aspiration relative importance
        " ;" income-relativeimportance
        " ;" leisure-relativeimportance
        " ;" food-relativeimportance

        ; experience weight (ew)
        " ;" ew-income
        " ;" ew-leisure
        " ;" ew-food

        ; chosen-strategies
        " ;" chosen-strategy " ;" known-strategies
        " ;" income-expectation " ;" leisure-expectation " ;" food-expectation
        " ;" income-expectations " ;" leisure-expectations " ;" food-expectations " ;" weighted-utilities " ;" possible-strategies

        ; household land use
        " ;" count in-myfield-neighbors " ;" maizeland " ;" vegetableland " ;" coffeeland " ;" privatepasture " ;" fallowland

        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "low" and herbicides = false]      ; 0 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "moderate" and herbicides = false] ; 1 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "moderate" and herbicides = true]  ; 1 1
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "high" and herbicides = false]     ; 2 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "high" and herbicides = true]      ; 2 1
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "very high" and herbicides = false]; 3 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "very high" and herbicides = true] ; 3 1

        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "low" and herbicides = false]      ; 0 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "moderate" and herbicides = false] ; 1 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "moderate" and herbicides = true]  ; 1 1
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "high" and herbicides = false]     ; 2 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "high" and herbicides = true]      ; 2 1
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "very high" and herbicides = false]; 3 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "very high" and herbicides = true] ; 3 1

        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "low" and herbicides = false]      ; 0 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "moderate" and herbicides = false] ; 1 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "moderate" and herbicides = true]  ; 1 1
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "high" and herbicides = false]     ; 2 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "high" and herbicides = true]      ; 2 1
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "very high" and herbicides = false]; 3 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "very high" and herbicides = true] ; 3 1

        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser = "low"] ; area not fertilised
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser = "moderate"] ; area moderately fertilised
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser = "high"] ; area highly fertilised
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser = "very high"] ; area very highly fertilised
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser != "low" and current-land-use = "maize"] ; maize area not fertilised
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser != "low" and current-land-use = "vegetable"] ; vegetable area not fertilised
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser != "low" and current-land-use = "coffee"] ; coffee area not fertilised

        ; food production
        " ;" maizeproduction " ;" vegproduction " ;" coffeeproduction " ;" milkproduction

        ; livestock
        " ;" herd
        " ;" count in-mycow-neighbors with [supplementary-feed = "yes"] ; herd on supplementary feed? yes/no bc either all are or none are
        " ;" adopted-feed?

        ; temporal variability
        " ;" pvar " ;" pmu " ;" pmult " ;" yvar " ;" ymu " ;" ymult

        ; check whether colnames and values match
        " ;match_check"

      )
    ]
  ]
end

to write-to-runfile [write-header?]
  if write-header? [
  file-print ( ; column names
    word

    ; runID
      "runID;"

    ; basic
      "household;hhid;Aspiration_adaptation;Strategy_order;Make_decisions;Utility_calculation;Threshold_type;Internal_external"
      ";time_of_run;optimism;b_no;remember_t3;remember_t2;remember_t1"
      ";run_number;region;woreda;Population;Repetition"

    ; aspiration relative importance
      ";relativeimportanceincome"
      ";relativeimportanceleisure"
      ";relativeimportancefood"

    ; experience weight (ew)
      ";ew_income"
      ";ew_leisure"
      ";ew_food"

    ; household land use
      ";fields;farmland"

    ; household characteristics
      ";hhmembers;adults;elderly;children;network_size"

    ; temporal variability
      ";price_sd;price_mean;yield_sd;yield_mean"

    ; check whether colnames and values match
      ";match_check"

    )
 ]

  foreach sort households [ t ->
    ask t [
      file-print (
        word

        ; runID
        (word substring Strategy-order 0 3 "_"
          substring Search-method 0 3 "_"
          (ifelse-value Utility-calculation = "rank-sum" ["RS"] Utility-calculation = "weighted-sum" ["WS"] Utility-calculation = "cobb-douglas+" ["CBt"] Utility-calculation = "cobb-douglas*" ["CBx"] Utility-calculation = "cobb-douglasln" ["CBln"] [Utility-calculation]) "_"
          (ifelse-value Threshold-type = "dynamic" ["dyn"] Threshold-type = "static" ["stat"] Threshold-type = "infinite" ["inf"]) "_"
          (ifelse-value Threshold-adjustment = "internal only" ["in"] Threshold-adjustment = "internal and external" ["ie"] Threshold-adjustment = "external only" ["ex"]) "_"
          substring word "I" Income? 0 2 "_" substring word "F" Food? 0 2 "_" substring word "L" Leisure? 0 2 ;shorten true and false to <I|F|L>t and<I|F|L>f
          "_p" Population-nr "_r" Repetition-nr) " ;"

        ; basic
        self " ;" name " ;" Strategy-order " ;" Search-method " ;" Utility-calculation " ;" Threshold-type " ;" Threshold-adjustment
        " ;" date-and-time " ;" optimism " ;" b-no " ;" remember-t3 " ;" remember-t2 " ;" remember-t1
        " ;" behaviorspace-run-number " ;" region " ;" woreda " ;" Population-nr " ;" Repetition-nr

        ; aspiration relative importance
        " ;" income-relativeimportance
        " ;" leisure-relativeimportance
        " ;" food-relativeimportance

        ; experience weight (ew)
        " ;" ew-income
        " ;" ew-leisure
        " ;" ew-food

        ; household land use
        " ;" count in-myfield-neighbors " ;" farmland

        ; household characteristics
        " ;" household-size " ;" adults " ;" elders " ;" children " ;" network-size

        ; temporal variability
        " ;" pvar " ;" pmu " ;" yvar " ;" ymu

        ; check whether colnames and values match
        " ;match_check"

      )
    ]
  ]

end

to write-to-tickfile [write-header?]
  if write-header? [
  file-print ( ; column names
    word

    ; runID
      "runID;time_of_run;"

    ; basic
      "hhid;year"

    ; aspiration outcomes
      ";income_outcome_t1start;income_outcome_t1end;income_outcome_t2;income_outcome_t3"
      ";leisure_outcome_t1start;leisure_outcome_t1end;leisure_outcome_t2;leisure_outcome_t3"
      ";food_outcome_t1start;food_outcome_t1end;food_outcome_t2;food_outcome_t3"
      ";satisficed"

     ; aspiration related outcomes
      ";savings;value_of_consumed-products;food_purchases;costs_of_production;earnings_from_sales;maize_consumed"

     ; disaggregated
      ";maizecost;vegetablecost;coffeecost;supplementaryfeedcost;cowcost"
      ";maizesales;vegetablesales;coffeesales;buttersales;oxensales;bullsales;cowsales"

    ; aspiration thresholds
      ";income_threshold_mean;income_threshold_t1start;income_threshold_t1end;income_threshold_t2;income_threshold_t3"
      ";leisure_threshold_mean;leisure_threshold_t1start;leisure_threshold_t1end;leisure_threshold_t2;leisure_threshold_t3"
      ";food_threshold_mean;food_threshold_t1start;food_threshold_t1end;food_threshold_t2;food_threshold_t3"

    ; aspiration gaps
      ";incomegap"
      ";leisuregap"
      ";foodgap"

    ; chosen strategies
      ";chosen_strategy;known_strategies"
      ";income_expectation;leisure_expectation;food_expectation"
      ";income_expectationS;leisure_expectationS;food_expectationS;weighted_utilities;possible_strategies"

    ; household land use
      ";maizeland;vegetableland;coffeeland;privatepasture;fallowland;farmland"
      ";maize_0fert_0herb;maize_1fert_0herb;maize_1fert_1herb;maize_2fert_0herb;maize_2fert_1herb;maize_3fert_0herb;maize_3fert_1herb"
      ";veg_0fert_0herb;veg_1fert_0herb;veg_1fert_1herb;veg_2fert_0herb;veg_2fert_1herb;veg_3fert_0herb;veg_3fert_1herb"
      ";coffee_0fert_0herb;coffee_1fert_0herb;coffee_1fert_1herb;coffee_2fert_0herb;coffee_2fert_1herb;coffee_3fert_0herb;coffee_3fert_1herb"
      ";unfertilisedland"
      ";moderatefertilisedland"
      ";highlyfertilisedland"
      ";veryhighlyfertilisedland"
      ";maizefertilisedland"
      ";vegetablefertilisedland"
      ";coffeefertilisedland"

    ; food production
      ";maizeproduction;vegetableproduction;coffeeproduction;milkproduction"

    ; livestock
     ";herd"
     ";on_supplementary_feed"
     ";adopted_feed"

    ; temporal variability
      ";price_multiplier;yield_multiplier"

    ; check whether colnames and values match
      ";match_check"


    )
  ]
  ; file-print (word "---------- Tick Number: " ticks "-----------")
  ;; use SORT so the turtles print their data in order by who number,
  ;; rather than in random order
  foreach sort households [ t ->
    ask t [
      file-print (
        word

        ; runID
        (word substring Strategy-order 0 3 "_"
          substring Search-method 0 3 "_"
          (ifelse-value Utility-calculation = "rank-sum" ["RS"] Utility-calculation = "weighted-sum" ["WS"] Utility-calculation = "cobb-douglas+" ["CBt"] Utility-calculation = "cobb-douglas*" ["CBx"] Utility-calculation = "cobb-douglasln" ["CBln"] [Utility-calculation]) "_"
          (ifelse-value Threshold-type = "dynamic" ["dyn"] Threshold-type = "static" ["stat"] Threshold-type = "infinite" ["inf"]) "_"
          (ifelse-value Threshold-adjustment = "internal only" ["in"] Threshold-adjustment = "internal and external" ["ie"] Threshold-adjustment = "external only" ["ex"]) "_"
          substring word "I" Income? 0 2 "_" substring word "F" Food? 0 2 "_" substring word "L" Leisure? 0 2 ;shorten true and false to <I|F|L>t and<I|F|L>f
          "_p" Population-nr "_r" Repetition-nr) " ;"

        date-and-time " ;"

        ; basic
        name " ;" ticks

        ; aspiration outcomes
        " ;" income-outcome-t1start  " ;" income-outcome-t1end  " ;" income-outcome-t2  " ;" income-outcome-t3
        " ;" leisure-outcome-t1start " ;" leisure-outcome-t1end " ;" leisure-outcome-t2 " ;" leisure-outcome-t3
        " ;" food-outcome-t1start    " ;" food-outcome-t1end    " ;" food-outcome-t2    " ;" food-outcome-t3
        " ;" satisficed

        ; aspiration related outcomes
        " ;" savings " ;" value-of-consumed-products " ;" food-purchases " ;" costs-of-production " ;" earnings-from-sales " ;" maize-consumed

        ; disaggregated
        " ;" maizecost " ;" vegcost " ;" coffeecost " ;" supplementaryfeedcost " ;" cowcost
        " ;" maizesales " ;" vegsales " ;" coffeesales " ;" buttersales " ;" oxensales " ;" bullsales " ;" cowsales

        ; aspiration thresholds
        " ;" income-threshold-mean  " ;" income-threshold-t1start  " ;" income-threshold-t1end  " ;" income-threshold-t2  " ;" income-threshold-t3
        " ;" leisure-threshold-mean " ;" leisure-threshold-t1start " ;" leisure-threshold-t1end " ;" leisure-threshold-t2 " ;" leisure-threshold-t3
        " ;" food-threshold-mean    " ;" food-threshold-t1start    " ;" food-threshold-t1end    " ;" food-threshold-t2    " ;" food-threshold-t3

        ; aspiration gaps
        " ;" income-gap
        " ;" leisure-gap
        " ;" food-gap

        ; chosen-strategies
        " ;" chosen-strategy " ;" known-strategies
        " ;" income-expectation " ;" leisure-expectation " ;" food-expectation
        " ;" income-expectations " ;" leisure-expectations " ;" food-expectations " ;" weighted-utilities " ;" possible-strategies

        ; household land use
        " ;" maizeland " ;" vegetableland " ;" coffeeland " ;" privatepasture " ;" fallowland " ;" farmland

        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "low" and herbicides = false]      ; 0 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "moderate" and herbicides = false] ; 1 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "moderate" and herbicides = true]  ; 1 1
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "high" and herbicides = false]     ; 2 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "high" and herbicides = true]      ; 2 1
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "very high" and herbicides = false]; 3 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "very high" and herbicides = true] ; 3 1

        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "low" and herbicides = false]      ; 0 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "moderate" and herbicides = false] ; 1 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "moderate" and herbicides = true]  ; 1 1
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "high" and herbicides = false]     ; 2 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "high" and herbicides = true]      ; 2 1
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "very high" and herbicides = false]; 3 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "very high" and herbicides = true] ; 3 1

        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "low" and herbicides = false]      ; 0 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "moderate" and herbicides = false] ; 1 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "moderate" and herbicides = true]  ; 1 1
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "high" and herbicides = false]     ; 2 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "high" and herbicides = true]      ; 2 1
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "very high" and herbicides = false]; 3 0
        " ;" sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "very high" and herbicides = true] ; 3 1

        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser = "low"] ; area not fertilised
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser = "moderate"] ; area moderately fertilised
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser = "high"] ; area highly fertilised
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser = "very high"] ; area very highly fertilised
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser != "low" and current-land-use = "maize"] ; maize area not fertilised
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser != "low" and current-land-use = "vegetable"] ; vegetable area not fertilised
        " ;" sum [field-size] of in-myfield-neighbors with [fertiliser != "low" and current-land-use = "coffee"] ; coffee area not fertilised

        ; food production
        " ;" maizeproduction " ;" vegproduction " ;" coffeeproduction " ;" milkproduction

        ; livestock
        " ;" herd
        " ;" count in-mycow-neighbors with [supplementary-feed = "yes"] ; herd on supplementary feed? yes/no bc either all are or none are
        " ;" adopted-feed?

        ; temporal variability
        " ;" pmult " ;" ymult

        ; check whether colnames and values match
        " ;match_check"

      )
    ]
  ]
end

; Sub functions

; Used in loop-through-strategies
to calc-expected-outcome                                                ; Sub-functions: calc-maize-prod-test, calc-maize-consume-profit-test, calc-veg-prod-test, calc-veg-profit-test, calc-feed-prod-test, ...
                                                                        ;                underfed-die-test, calc-milk-prod-test, milk-to-butter-test, calc-butter-profit-test, sell-cattle-test, calc-coffee-prod-test,
                                                                        ;                calc-coffee-profit-test, calc-labour-test
 calc-maize-prod-test
 calc-veg-prod-test
 calc-feed-prod-test
 underfed-die-test ; buy cow here if that is the chosen strategy
 calc-milk-prod-test

 calc-maize-profit-test
 set food-expectation ifelse-value (maize-left > 0) [(test-maizeproduction - maize-left) / 56] [test-maizeproduction / 56]
 calc-veg-profit-test

 milk-to-butter-test
 calc-butter-profit-test
 sell-cattle-test

 calc-coffee-prod-test
 calc-coffee-profit-test
 calc-labour-test

 let earnings-from-sales-test test-maizesales + test-vegsales + test-buttersales + test-bullsales + test-oxensales + test-cowsales + buy-cow-expected-sales-test + test-coffeesales
 let costs-of-production-test test-maizecost + test-vegcost + test-cowcost + test-feedcost + test-coffeecost
 set income-expectation earnings-from-sales-test - costs-of-production-test; can be negative if households have to buy maize (negative maizeprofit), assumes same price for selling and buying, this is not so realistic

end

to-report report-expected-outcomes
 calc-maize-prod-test
 calc-veg-prod-test
 calc-feed-prod-test
 underfed-die-test ; buy cow here if that is the chosen strategy
 calc-milk-prod-test

 calc-maize-profit-test
 set food-expectation ifelse-value (maize-left > 0) [(test-maizeproduction - maize-left) / 56] [test-maizeproduction / 56]
 calc-veg-profit-test

 milk-to-butter-test
 calc-butter-profit-test
 sell-cattle-test

 calc-coffee-prod-test
 calc-coffee-profit-test
 calc-labour-test

 let earnings-from-sales-test test-maizesales + test-vegsales + test-buttersales + test-bullsales + test-oxensales + test-cowsales + buy-cow-expected-sales-test + test-coffeesales
 let costs-of-production-test test-maizecost + test-vegcost + test-cowcost + test-feedcost + test-coffeecost
 set income-expectation earnings-from-sales-test - costs-of-production-test; can be negative if households have to buy maize (negative maizeprofit), assumes same price for selling and buying, this is not so realistic

 report (list income-expectation food-expectation leisure-expectation)

end


to change-from-pasture-to-fallow-test
  ask one-of in-myfield-neighbors with [current-land-use = "pasture"][
    set color grey
    set current-land-use "fallow"
  ]
end

to change-from-maize-to-fallow-test
  ask one-of in-myfield-neighbors with [current-land-use = "maize"][
    set color grey
    set current-land-use "fallow"
  ]
end

to change-from-veg-to-fallow-test
  ask one-of in-myfield-neighbors with [current-land-use = "vegetable"][
    set color grey
    set current-land-use "fallow"
  ]
end

to change-from-coffee-to-fallow-test
  ask one-of in-myfield-neighbors with [current-land-use = "coffee" and coffee-age > 7][ ; --> parameter possible-to-change-from-coffee-after
    set color grey
    set current-land-use "fallow"
  ]
end

to change-from-fallow-to-pasture-test
    ask one-of in-myfield-neighbors with [current-land-use = "fallow"][
      set color grey
      set current-land-use "pasture"
    ]
end

to change-from-maize-to-pasture-test
    ask one-of in-myfield-neighbors with [current-land-use = "maize"][
      set color grey
      set current-land-use "pasture"
    ]
end

to change-from-veg-to-pasture-test
    ask one-of in-myfield-neighbors with [current-land-use = "vegetable"][
      set color grey
      set current-land-use "pasture"
    ]
end

to change-from-coffee-to-pasture-test
    ask one-of in-myfield-neighbors with [current-land-use = "coffee" and coffee-age > 7][ ; --> parameter possible-to-change-from-coffee-after
      set color grey
      set current-land-use "pasture"
    ]
end

to change-from-fallow-to-veg-test
    ask one-of in-myfield-neighbors with [current-land-use = "fallow"][
      set color grey
      set current-land-use "vegetable"
    ]
end

to change-from-maize-to-veg-test
    ask one-of in-myfield-neighbors with [current-land-use = "maize"][
      set color grey
      set current-land-use "vegetable"
    ]
end

to change-from-pasture-to-veg-test
  ask one-of in-myfield-neighbors with [current-land-use = "pasture"][
    set color grey
    set current-land-use "vegetable"
  ]
end

to change-from-coffee-to-veg-test
    ask one-of in-myfield-neighbors with [current-land-use = "coffee" and coffee-age > 7][ ; --> parameter possible-to-change-from-coffee-after
      set color grey
      set current-land-use "vegetable"
    ]
end

to change-from-fallow-to-maize-test
    ask one-of in-myfield-neighbors with [current-land-use = "fallow"][
      set color grey
      set current-land-use "maize"
    ]
end

to change-from-veg-to-maize-test
    ask one-of in-myfield-neighbors with [current-land-use = "vegetable"][
      set color grey
      set current-land-use "maize"
    ]
end

to change-from-pasture-to-maize-test
    ask one-of in-myfield-neighbors with [current-land-use = "pasture"][
      set color grey
      set current-land-use "maize"
    ]
end

to change-from-coffee-to-maize-test
    ask one-of in-myfield-neighbors with [current-land-use = "coffee" and coffee-age > 7][ ; --> parameter possible-to-change-from-coffee-after
      set color grey
      set current-land-use "maize"
    ]
end

to change-from-fallow-to-coffee-test
    ask one-of in-myfield-neighbors with [current-land-use = "fallow"][
      set color grey
      set current-land-use "coffee"
    ]
end

to change-from-pasture-to-coffee-test
    ask one-of in-myfield-neighbors with [current-land-use = "pasture"][
      set color grey
      set current-land-use "coffee"
    ]
end

to change-from-maize-to-coffee-test
    ask one-of in-myfield-neighbors with [current-land-use = "maize"][
      set color grey
      set current-land-use "coffee"
    ]
end

to change-from-veg-to-coffee-test
    ask one-of in-myfield-neighbors with [current-land-use = "vegetable"][
      set color grey
      set current-land-use "coffee"
    ]
end

to buy-cow-test
  hatch-cattle 1 [
    set color orange
    set size 6
    set bought? "yes"
    set age 4
    set sex "female"
    set dry ifelse-value (random-float 1 < 0.5) ["yes"]["no"]
    set pregnant dry
    set in-lactation ifelse-value (pregnant = "yes")["no"]["yes"]
    set supplementary-feed ifelse-value (any? in-mycow-neighbors with [supplementary-feed = "yes"])["yes"]["no"]
  ]

  ;ask cattle-here[create-mycows-with other households-here]
  create-mycows-with other cattle-here with [bought? = "yes" and age = 4][set color orange]
  set test-herd count in-mycow-neighbors
end

to buy-feed-test
  ask in-mycow-neighbors with [supplementary-feed != "yes"] [
    set supplementary-feed "testing"
  ]
end

to buy-fertiliser-test
  ;print (word "entering buy-fertiliser-test for household: " who)
  if any? (in-myfield-neighbors with [(current-land-use != "fallow" and current-land-use != "pasture") and (fertiliser = "low" or fertiliser = "moderate" or fertiliser = "high")])
    [
      ask one-of (in-myfield-neighbors with [
        (current-land-use != "fallow" and current-land-use != "pasture") and (fertiliser = "low" or fertiliser = "moderate" or fertiliser = "high")])[

        set fertiliser (ifelse-value
          fertiliser = "low" ["testing moderate"]
          fertiliser = "moderate" ["testing high"]
          fertiliser = "high" ["testing very high"]
        )

        ;print (word fertiliser " fertiliser for " current-land-use " field " [who] of self " of household " [who] of myself)
      ]
    ]
end

to buy-herbicides-test
  ask one-of in-myfield-neighbors with [(current-land-use = "maize" or current-land-use = "vegetable" or current-land-use = "coffee") and fertiliser != "low" and not herbicides][
    set herbicides "testing true"
  ]
end

to buy-fertiliser-and-herbicides-test
  ask one-of in-myfield-neighbors with [(current-land-use = "maize" or current-land-use = "vegetable" or current-land-use = "coffee") and fertiliser = "low" and not herbicides][
    set herbicides "testing true"
    set fertiliser "testing moderate"
  ]
end

to sell-cow-test
   ask one-of in-mycow-neighbors with [calf < 4 and sex = "female"][
    set color red]
  set test-herd herd - 1
end


; Used in perform-chosen-management

to calc-maize-prod ;

  ask fields with [current-land-use = "maize"][

    (ifelse
      fertiliser = "low" [set yield  maize-yield-low + maize-yield-low * ymult] ; yield units = kg per ha  source yield: van Dijk 2020 ; parameter maize-yield-low = 2.718
      fertiliser = "moderate" [set yield maize-yield-moderate + maize-yield-moderate * ymult] ; for yields > 0, we calculated based on mpp formula estimated based on Assefa 2021 ; parameter maize-yield-medium = 3.331
      fertiliser = "high" [set yield  maize-yield-high + maize-yield-high * ymult] ; parameter maize-yield-high = 4.356
      fertiliser = "very high" [set yield  maize-yield-veryhigh + maize-yield-veryhigh * ymult] ; parameter maize-yield-veryhigh = 4.664
    )
    if herbicides [ set yield yield * 1.09 ]; Assefa 2021
    set production yield * field-size ; production unit = kg

  ]

  ask households with [maizeland > 0][ ;ask household 12 [ask in-myfield-neighbors [show production]]
    set maizeproduction sum [production] of in-myfield-neighbors with [current-land-use = "maize"]; unit = kg
  ]

end

to calc-food-consume
  ; first calculate maize need
  ; assume 60% of calories should come from maize
  ; assume men need 3000 calories, women 2000 calories and children 1500 calories
  ; if aspiration is to consume more than that, however (food-threshold-t1start), then we use that instead of requirement
  ; for now we have no age or gender distribution, so assume on average calorie need is 2000 per household member, meaning each household member needs to consume 1200 calories from maize. This is >2x as much maize as they normally eat, but maize also represents other cereals and enset, which together are the main part of diet (Berhane 2011b, p.1)

  ask households [
    let adult-requirement adults * (2200 + 2900) / 2 * 365
    let elderly-requirement elders * (2300 + 1900) / 2 * 365
    let child-requirement children * (1300 + 1800 + 2000) / 3 * 365
    let hh-requirement adult-requirement + elderly-requirement + child-requirement

    set food-requirement hh-requirement * 0.86 * 0.8 ; see calc-test-maize-consume-profit-test formula for explanation of assumptions
    let maize-requirement food-requirement / 3640 ; unit = kg per hh/year, https://agsci.colostate.edu/smallholderagriculture/ethiopia-diet-analysis/

    ifelse maize-requirement > (food-threshold-t1start * 56) [
      set maize-left maizeproduction - maize-requirement
      set maize-consumed maize-requirement
    ][
      set maize-left maizeproduction - (food-threshold-t1start * 56)
      set maize-consumed (food-threshold-t1start * 56) ; per yea
    ]

  ]
end

to calc-maize-profit

  ask households[

    let local-maize-price global-maize-price + global-maize-price * pmult

    ifelse maize-left >= 0 [
      set maizeprofit maize-left * local-maize-price ; parameter maize-price = 10 ; average domestic maize price per kg over the last years (2009-2018) was 4.8 birr (Minten 2020b, p. 290), but the since the conflict and the drought it has been reported being >20 birr at wholesale (http://www.egte-ethiopia.com/en/in-t-market/commodity-statistics.html)
      set maizepurchase 0
    ][
      set maizeprofit 0
      set maizepurchase abs maize-left * local-maize-price * 1.2 ; parameter for now assuming that market price is 20% higher than farm-gate price
    ]

    set maizesales maizeprofit

    ; subtract cost of herbicides
    let herbicide-area sum [field-size] of in-myfield-neighbors with [herbicides and current-land-use = "maize"]
    let herbicide-cost global-herbicides-price * herbicide-area

    ; subtract cost of fertiliser
    set maizecost calc-cost-of-fertiliser in-myfield-neighbors with [current-land-use = "maize"] + herbicide-cost
    set maizeprofit maizeprofit - maizecost

  ]
end

to calc-veg-prod
  ask fields with [current-land-use = "vegetable"][
    ; set yield 107 * 100; kg per ha, see CSA 2012, THE FEDERAL DEMOCRATIC REPUBLIC OF ETHIOPIA  CENTRAL STATISTICAL AGENCY AGRICULTURAL SAMPLE SURVEY  2011 / 2012 (2004 E.C.) <- onion
    (ifelse
      fertiliser = "low" [set yield veg-yield-low + veg-yield-low * ymult] ; parameter veg-yield-low = 14.9
      fertiliser = "moderate" [set yield veg-yield-moderate + veg-yield-moderate * ymult] ; parameter veg-yield-medium = 38.2
      fertiliser = "high" [set yield veg-yield-high + veg-yield-high * ymult] ; parameter veg-yield-high = 41.9
      fertiliser = "very high" [set yield veg-yield-veryhigh + veg-yield-veryhigh * ymult] ; parameter veg-yield-veryhigh = 56.0
    )
    if herbicides [set yield yield * 1.09]
    set production yield * field-size
  ] ; kg per ha

  ask households with [vegetableland > 0][
    set vegproduction sum [production] of in-myfield-neighbors with [current-land-use = "vegetable"] ; unit = kg
  ]

end

to calc-veg-profit
  ask households[

    let local-veg-price global-veg-price + global-veg-price * pmult

    let veg-need (household-size / 5) * 5 * 12 ; unit = kg / year, https://agsci.colostate.edu/smallholderagriculture/ethiopia-diet-analysis
    set veg-left vegproduction - veg-need
    ifelse veg-left >= 0 [
      set vegprofit veg-left * local-veg-price
      set veg-consumed veg-need
    ][
      set vegprofit 0
      set veg-consumed vegproduction
    ] ;

    set vegsales vegprofit

    ; subtract cost of herbicides
    let herbicide-area sum [field-size] of in-myfield-neighbors with [herbicides and current-land-use = "vegetable"]
    let herbicide-cost global-herbicides-price * herbicide-area

    ; subtract costs of fertiliser
    set vegcost calc-cost-of-fertiliser in-myfield-neighbors with [current-land-use = "vegetable"] + herbicide-cost
    set vegprofit vegprofit - vegcost
  ]
end

to calc-feed-production
  ask households [
    set feedproduction ; in tonnes DM per year
                      privatepasture * utilisable-pasture + ; 2 tonnes per ha, of which 75% is utilisable
                      maizeproduction * utilisable-maizestover + ; Total stover yield in DM is on average 1.39 tonnes DM per tonne 1.67 tonnes of maize yield (grain) (Berhanu 2007). This makes the stover to grain ratio 0.84. Stover is, however, not just used for feed, but also fuel and construction. About 69% of the residue produced is used as feed (Berhanu 2007). 0.84 * 0.69 = 0.58.
                      (coffeeproduction / 1000) * utilisable-coffeehusk ; divide coffee production by 1000 because it is given in kg not tonnes
    set feedproduction feedproduction + feedproduction * ymult
  ]
end

to age-cattle

  ; grow
  ask cattle [
    set age age + 1
    set size (age / 8) + 1
  ]

  ; die
  ask cattle with [age = 14][die]

  ; pregnant cattle -> birth + milk
  ask cattle with [pregnant = "yes"][
    set pregnant "no more"
    if calf < 4 [set calf calf + 1] ; parameter die-after-n-calves-born
    set dry "no"
    set in-lactation "soon"
    set pcolor white
  ]

  ; sprount calves
  ask patches with [pcolor = white][sprout-cattle 1 set pcolor black]

  ; assign sex to calves
  ask n-of (count cattle with [shape = "default"] / 2) cattle with [shape = "default"] [set sex "female" set color pink + 2]
  ask cattle with [sex != "female" and shape = "default"] [set sex "male" set color sky + 2]

  ask cattle with [shape = "default"][
    set shape "cow"
    set age 0
    set dry "yes"
    set calf 0
    set lactation 0
    set size age / 8 + 1
    set bought? "no"
    create-mycalves-with other cattle-here with [pregnant = "no more"]; links calves to their mothers
    forward 2
  ]

  ; link new calves to households
  set cowiter -1
  set all-cattle sort([who] of cattle)
  while[cowiter < length [who] of cattle - 1]
  [
   set cowiter cowiter + 1
   set cowid item cowiter all-cattle ; problem below is cause by friendships and influencer links. This means there are more households 2 distance from cows, and that households will own the cows of their friends.
    (foreach sort([who] of households) n-values length sort([who] of households) [cowid] [ [a b] ->
      ask household a [if nw:distance-to cow b = 2 [ ; first check if the network distance between household and calf = 2
      let pathcheck word item 1 nw:turtles-on-path-to cow b " is a hh? "     ; then check if path is household -> cow -> calf and not household -> household -> cow
      if not member? "household" pathcheck [
          create-mycow-with cow b]

    ] ] ])
  ]

  ; get cattle that have been in-lactation to become pregnant again
  ask cattle with [in-lactation = "yes"][
   set in-lactation "no more"
   if lactation < 4 [set lactation lactation + 1]
   set dry "yes"
  ]

  ; cattle that get pregnant for the first time
  ask n-of (count cattle with [sex = "female" and age = 3] / 2) cattle with [sex = "female" and age = 3][set pregnant "yes"] ; half get pregnant at three years old
  ask cattle with [sex = "female" and age = 4 and in-lactation != "yes"][set pregnant "yes"] ; others get pregnant at four years old

  ; get cattle that have been pregnant before (and were lactating previous year) pregnant
  ask cattle with [sex = "female" and in-lactation = "no more" and calf < 4][set pregnant "yes"] ; parameter die-after-n-calves-born

  ; set cattle that gave birth in-lactation
  ask cattle with [in-lactation = "soon"][set in-lactation "yes"]

end

to give-new-calves-supplementary-feed
  ask households with [adopted-feed? = "yes"][
      ask in-mycow-neighbors [set supplementary-feed "yes"]
  ]
end

to underfed-die
  ask households [
    ;let cows-on-supplement count in-mycow-neighbors with [supplementary-feed = "yes"]
    ;let on-supplement ifelse-value (cows-on-supplement > 0)["yes"]["no"]
    set herd count in-mycow-neighbors

    ; on low-quality diet
    if (herd > 0 and adopted-feed? != "yes" )[
      let capacity floor(((feedproduction * 1000) / 365 ) / 3.15) ; feedproduction * 1000 --> feed production in kg ; feed production / 365 --> feed production per day ; 3.15 = parameter feed-lowerthreshold-nosup
      let doomed herd - capacity
      if doomed > 0 [ask n-of doomed in-mycow-neighbors [
        die]
    ]
  ]
    ; on high-quality diet no cows will die due to supplementary feed
]
end

to calc-milk-prod
  ask households with [herd > 0] [
    ask in-mycow-neighbors [
      let tonnes-year ([feedproduction] of myself / [herd] of myself)
      let kg-day (tonnes-year * 1000) / 365
      set feed-quantity kg-day
    ]
  ]

  ; milk yield of cattle on low-quality diet
  ask cattle with [in-lactation = "yes" and supplementary-feed != "yes"]; with [some age criteria]
  [
    (ifelse
      feed-quantity <= 3.15 [set milkyield 0 ] ;set color red 3.15 = parameter feed-lowerthreshold-nosup
      feed-quantity > 3.15 and feed-quantity < 20 [set milkyield -2.11 + 0.67 * feed-quantity ] ;set color green 20 = parameter feed-upperthreshold-nosup, -2.11 = milk-intercept-nosup, 0.67 = milk-slop-nosup
      feed-quantity >= 20 [set milkyield -2.11 + 0.67 * 20 ] ; max set color grey 20 = parameter feed-upperthreshold-nosup, -2.11 = milk-intercept-nosup, 0.67 = milk-slop-nosup
    )
  ]

  ; milk yield of cattle on high-quality diet
  ask cattle with [in-lactation = "yes" and supplementary-feed = "yes"]; with [some age criteria]
  [
    (ifelse
      feed-quantity <= 0 [set milkyield 0 ] ;set color red 0 = parameter feed-lowerthreshold-sup
      feed-quantity > 0 and feed-quantity < 20 [set milkyield 1.38 + 0.74 * feed-quantity ] ;set color green 20 = parameter feed-upperthreshold-sup 1.38 = milk-intercept-nosup, 0.74 = milk-slop-nosup
      feed-quantity >= 20 [set milkyield 1.38 + 0.74 * 20 ] ; max ; set color grey 20 = parameter feed-upperthreshold-sup 1.38 = milk-intercept-nosup, 0.74 = milk-slop-nosup
    )
  ]

  ask households with [herd > 0]
  [
    set milkproduction sum [milkyield] of in-mycow-neighbors
  ]

end

to milk-to-butter
  ask households [
    let milk-need 0.1 * 365 * household-size; unit milk-need = L/household per day
    set milk-left milkproduction - milk-need

    ifelse milk-left > 56 * 5 [ ; 5 is a parameter --> butter-volume-needed; Gebremedhim (2014)
      set butterproduction milk-left / milk-to-butter-conversion-factor
      set milk-consumed milk-need
    ] [
      set butterproduction 0
      set milk-consumed milkproduction
    ] ; unit = kg. 26 = # weeks in 6 months (assuming that the herd is synchronised), 5 = minimum amount (L) needed for butter making per week.
  ]
end

to calc-butter-profit

  ask households with [milkproduction > 0]
  [

    let local-butter-price global-butter-price + global-butter-price * pmult

    let butter-left butterproduction ;- butter-requirement
    ifelse butter-left > 0 [
      set butterprofit butter-left * local-butter-price
      set butter-consumed 0 ;butter-requirement
    ][
      set butterprofit 0
      set butter-consumed ifelse-value butterproduction > 0 [butterproduction][0]
    ]

    set buttersales butterprofit
  ]

  ask households [

    ; subtract costs of buying cattle
    set cowcost ifelse-value (chosen-strategy = "buy cow") [global-cow-purchase-price][0]

    ; subtract costs of supplementary feed
    let cows-on-supplementary-feed count in-mycow-neighbors with [supplementary-feed = "yes"]
    set supplementaryfeedcost ifelse-value adopted-feed? = "yes" [cows-on-supplementary-feed * global-supplementary-feed-price] [0]

    ; calculate profit from butter sales subtracting the cost of feed and new cattle
    set butterprofit ifelse-value milkproduction > 0 [buttersales - supplementaryfeedcost - cowcost][0 - supplementaryfeedcost - cowcost]
    ;set butterprofit buttersales - supplementaryfeedcost - cowcost
  ]

end

to make-oxen
  ; assume oxen can start ploughing at age two
  ask households with [herd > 0 and count in-mycow-neighbors with [sex = "male"] > 0][
    let males count in-mycow-neighbors with [sex = "male"]
    if count in-mycow-neighbors with [castrated = "yes"] < 2 [ ; assume
      ask n-of 1 in-mycow-neighbors with [sex = "male"][
        set castrated "yes"
      ]
    ]
    ask in-mycow-neighbors with [castrated = "yes" and age > 1][set color 87]
  ]

  ask households[set oxen count in-mycow-neighbors with [castrated = "yes" and age > 1]]

end

to sell-cattle
  ; consume old dairy cattle
  ask households with [count in-mycow-neighbors with [calf = 4] > 0][ ; parameter die-after-n-calves-born
    ask in-mycow-neighbors with [calf = 4][die]
  ]

  ; sell 2 year old bulls
  ask households with [count in-mycow-neighbors with [sex = "male" and castrated = "no" and age > 1] = 0][
    set bullsales 0
  ]

  ask households with [count in-mycow-neighbors with [sex = "male" and castrated = "no" and age > 1] > 0][
    let to-sell count in-mycow-neighbors with [sex = "male" and castrated = "no" and age > 1]
    set bullsales to-sell * global-bull-sell-price ;etb
    ask in-mycow-neighbors with [sex = "male" and castrated = "no" and age > 1][die]
  ]

  ; sell 7 year old oxen
  ask households with [count in-mycow-neighbors with [castrated = "yes" and age > 6] = 0] [
    set oxensales 0
  ]

  ask households with [count in-mycow-neighbors with [castrated = "yes" and age > 6] > 0][
    let to-sell count in-mycow-neighbors with [castrated = "yes" and age > 6]
    set oxensales to-sell * global-oxen-sell-price ; etb
    ask in-mycow-neighbors with [castrated = "yes" and age > 6][die]
  ]

  ; sell cows if this option is chosen
  ; ask households [set cowsales test-cowsales]

end

to age-coffee
  ask fields with [current-land-use = "coffee"][
    set coffee-age coffee-age + 1
    if coffee-age > 30 [set current-land-use "fallow" set color white]
  ]
  ask households with [count in-myfield-neighbors with [current-land-use = "coffee"] * area-field <= 0][set coffeeproduction 0 set coffeeprofit 0]
end

to calc-coffee-prod
  ask fields with [current-land-use = "coffee"][

    if coffee-age <= 3 [ ;set yield 50.34
      (ifelse
        fertiliser = "low" [set yield coffee-yield-low-establishment]
        fertiliser = "moderate" [set yield coffee-yield-moderate-establishment]
        fertiliser = "high" [set yield coffee-yield-high-establishment]
        fertiliser = "very high" [set yield coffee-yield-veryhigh-establishment]
      )
    ] ;kg per ha
    if coffee-age >= 3 and coffee-age <= 6 [
      (ifelse
        fertiliser = "low" [set yield coffee-yield-low-initiation]
        fertiliser = "moderate" [set yield coffee-yield-moderate-initiation]
        fertiliser = "high" [set yield coffee-yield-high-initiation]
        fertiliser = "very high" [set yield coffee-yield-veryhigh-initiation]
      )
    ] ;kg per ha
    if coffee-age >= 7 and coffee-age <= 27 [
      (ifelse
        fertiliser = "low" [set yield coffee-yield-low-full]
        fertiliser = "moderate" [set yield coffee-yield-moderate-full]
        fertiliser = "high" [set yield coffee-yield-high-full]
        fertiliser = "very high" [set yield coffee-yield-veryhigh-full]
      )
    ] ;kg per ha
    if coffee-age >= 28 and coffee-age <= 30 [; set yield 453.02
      (ifelse
        fertiliser = "low" [set yield coffee-yield-low-ageing]
        fertiliser = "moderate" [set yield coffee-yield-moderate-ageing]
        fertiliser = "high" [set yield coffee-yield-high-ageing]
        fertiliser = "very high" [set yield coffee-yield-veryhigh-ageing]
      )
    ] ;kg per ha

    if herbicides [set yield yield * 1.09]

    set yield yield + yield * ymult ; add randomness

    set production yield * field-size
  ]

  ask households with [coffeeland > 0][
    set coffeeproduction sum [production] of in-myfield-neighbors with [current-land-use = "coffee"] ; unit = kg
  ]
end

to calc-coffee-profit
  ask households with [coffeeproduction > 0][

    let local-coffee-price global-coffee-price + global-coffee-price * pmult

    let coffee-requirement 0.2 * 12 * household-size ; according to https://agsci.colostate.edu/smallholderagriculture/ethiopia-diet-analysis/ ; parameter
    let coffee-left coffeeproduction - coffee-requirement
    ifelse coffee-left > 0 [
      set coffeeprofit coffee-left * local-coffee-price
      set coffee-consumed coffee-requirement
    ][
      set coffeeprofit 0
      set coffee-consumed ifelse-value coffeeproduction > 0 [coffeeproduction][0]
    ] ;according to Diro 2019 ; parameter

    set coffeesales coffeeprofit

    ; subtract planting and maintanance costs
    let phaseI (sum [field-size] of in-myfield-neighbors with [coffee-age = 1]) * global-coffeeplantation-price ; etb per ha --> reference = Diro 2019 ; parameter
    let phaseII (sum [field-size] of in-myfield-neighbors with [coffee-age = 2  and coffee-age = 3]) * 19053 ; etb per ha ; parameter = coffeecost-phase2
    let phaseIII (sum [field-size] of in-myfield-neighbors with [coffee-age >= 4  and coffee-age <= 8]) * 22039 ; etb per ha ; parameter = coffeecost-phase3
    let phaseIV (sum [field-size] of in-myfield-neighbors with [coffee-age >= 9  and coffee-age <= 12]) * 18247 ; etb per ha ; parameter = coffeecost-phase4
    let phaseV (sum [field-size] of in-myfield-neighbors with [coffee-age >= 13]) * 19843 ; etb per ha ; parameter
    let coffeecost-maintain phaseI + phaseII + phaseIII + phaseIV + phaseV

    set maizesales maizeprofit

    ; subtract cost of herbicides
    let herbicide-area sum [field-size] of in-myfield-neighbors with [herbicides and current-land-use = "coffee"]
    let herbicide-cost global-herbicides-price * herbicide-area

    ; subtract fertiliser costs
    let coffeecost-fertiliser calc-cost-of-fertiliser in-myfield-neighbors with [current-land-use = "coffee"]
    set coffeecost coffeecost-maintain + coffeecost-fertiliser + herbicide-cost
    set coffeeprofit coffeeprofit - coffeecost
  ]

  ask households with [coffeeland <= 0][set coffeecost 0 set coffeesales 0]
end

to calc-labour
  ask households [

    let maizelabour sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "low"] * 41 + ; parameter 41 = maize-labour-needed
                    sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser != "low" and not herbicides] * 41 + 3 + ; parameter 3 = fertiliser-labour-needed
                    sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser != "low" and herbicides] * 41 + 3 - 12 ; parameter -12 = herbicides-labour-needed

    ;let coffeelabour coffeeland * 132 ; person days per year ; parameter coffee-labour-needed = 132; de Souza (2012)
    let coffeelabour sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "low"] * 132 + ; parameter 132 = coffee-labour-needed
                     sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser != "low" and not herbicides] * (132 + 3) + ; parameter 3 = fertiliser-labour-needed
                     sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser != "low" and herbicides] * (132 + 3 - 12) ; parameter -12 = herbicides-labour-needed


    let coffeeinitialisationlabour sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and coffee-age = 0] * 51 ; person days to establish plantation first year (de Souza 2012) ; coffee-initialisation-labour-needed = 51

    let veglabour sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "low"] * (41 + (41 * 0.15)) + ; parameter 41 + (41 * 0.15) = veg-labour-needed
                        sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser != "low" and not herbicides] * (41 + (41 * 0.15) + 3) +  ; parameter 3 = fertiliser-labour-needed
                        sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser != "low" and herbicides] * (41 + (41 * 0.15) + 3 - 12) ; parameter -12 = herbicides-labour-needed

    let cattlelabour (2.55 - 0.037 * herd) * herd ; parameter cattle-labour-needed-intercept = 2.55 parameter cattle-labour-needed-slope = - 0.037
    let pasturelabour 0.5 ; ; parameter: pasture-labour-needed. Educated guess, could not find any numeric value in literature
    set laboursum maizelabour + coffeelabour + coffeeinitialisationlabour + veglabour + cattlelabour + pasturelabour    ; labour days per year
  ]
end

; Used in calc-expected-outcome

to calc-maize-prod-test ;
  ask in-myfield-neighbors with [current-land-use = "maize"][
    ;set test-yield 3.4 * 1000; kg per ha, see Assefa et. al. 2021, Usage and Impacts of Technologies and Management Practices in Ethiopian Smallholder Maize Production <- later, allow for spatial differences based on agro-climate and differences based on inputs
    (ifelse
      fertiliser = "low" [set test-yield  maize-yield-low ] ; yield units = kg per ha  source yield: van Dijk 2020
      fertiliser = "moderate" or fertiliser = "testing moderate" [set test-yield  maize-yield-moderate] ; for yields > 0, we calculated based on mpp formula estimated based on Assefa 2021
      fertiliser = "high" or fertiliser = "testing high" [set test-yield  maize-yield-high]
      fertiliser = "very high" or fertiliser = "testing very high" [set test-yield  maize-yield-veryhigh]
  )
    if (herbicides = true or herbicides = "testing true") [set test-yield test-yield * 1.09]
    set test-production test-yield * field-size ; unit = kg
  ]

  set test-maizeproduction sum [test-production] of in-myfield-neighbors with [current-land-use = "maize"]; unit = kg
  ;if who = test-hh [print (word "test-maizeproduction = " test-maizeproduction)]

end

to calc-food-consume-test
  let adult-requirement adults * (2200 + 2900) / 2 * 365 ; unit = calories per year (Claro 2010 https://www.scielo.br/j/csp/a/KVZN6Bp7Wx633qkXKTzG4Gh/?format=pdf&lang=en)
  let elderly-requirement elders * (2300 + 1900) / 2 * 365
  let child-requirement children * (1300 + 1800 + 2000) / 3 * 365
  let hh-requirement adult-requirement + elderly-requirement + child-requirement
  set food-requirement hh-requirement * 0.86 * 0.8; 0.86 = typical proportion of caloric intake coming from staple grains (https://agsci.colostate.edu/smallholderagriculture/ethiopia-diet-analysis/); 0.8 = amount households are willing to consumption smoothe; households commonly lower their intake in order to survive without selling of assets
  let maize-requirement food-requirement / 3640 ; conversion to kg hh/year (https://agsci.colostate.edu/smallholderagriculture/ethiopia-diet-analysis/)

  ifelse maize-requirement > (food-threshold-t1start * 56) [
    set maize-left test-maizeproduction - maize-requirement
    set test-maizeconsume maize-requirement
  ][
    set maize-left test-maizeproduction - (food-threshold-t1start * 56)
    set test-maizeconsume (food-threshold-t1start * 56)
  ]
end

to calc-maize-profit-test
  ifelse maize-left >= 0 [
    set test-maizesales maize-left * global-maize-price ; parameter maize-price = 10 ; average domestic maize price per kg over the last years (2009-2018) was 4.8 birr (Minten 2020b, p. 290), but the since the conflict and the drought it has been reported being >20 birr at wholesale (http://www.egte-ethiopia.com/en/in-t-market/commodity-statistics.html)
    let test-maizepurchase 0
  ][
    set test-maizesales 0
    let test-maizepurchase abs maize-left * global-maize-price * 1.2 ; parameter for now assuming that market price is 20% higher than farm-gate price
  ]

  ; subtract cost of fertiliser
  let herbicide-area sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and (herbicides = true or herbicides = "testing true")]
  let herbicide-cost herbicide-area * global-herbicides-price

  ; subtract cost of fertiliser
  set test-maizecost calc-cost-of-fertiliser in-myfield-neighbors with [current-land-use = "maize"] + herbicide-cost
  if any? in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "testing"] [set test-maizecost test-maizecost + calc-cost-of-fertiliser-test]

end

to calc-veg-prod-test ;
  ask in-myfield-neighbors with [current-land-use = "vegetable"][
    (ifelse
      fertiliser = "low" [set test-yield veg-yield-low]
      fertiliser = "moderate" or fertiliser = "testing moderate" [set test-yield veg-yield-moderate]
      fertiliser = "high" or fertiliser = "testing high" [set test-yield veg-yield-high]
      fertiliser = "very high" or fertiliser = "testing very high" [set test-yield veg-yield-veryhigh]
    )
    if (herbicides = true or herbicides = "testing true") [set test-yield test-yield * 1.09]
    set test-production test-yield * field-size ; unit = kg
  ]

  set test-vegproduction sum [test-production] of in-myfield-neighbors with [current-land-use = "vegetable"]; unit = kg

end

to calc-veg-profit-test
  let veg-need (household-size / 5) * 5 * 12 ; unit = kg/year, https://agsci.colostate.edu/smallholderagrculture/ethiopia-diet-analysis
  set veg-left test-vegproduction - veg-need

  ifelse veg-left > 0 [set test-vegsales veg-left * global-veg-price][set test-vegsales 0]

  ; subtract cost of fertiliser
  let herbicide-area sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and (herbicides = true or herbicides = "testing true")]
  let herbicide-cost herbicide-area * global-herbicides-price

  ; subtract cost of fertiliser
  set test-vegcost calc-cost-of-fertiliser in-myfield-neighbors with [current-land-use = "vegetable"] + herbicide-cost; fields with fertiliser
  set test-vegcost test-vegcost + calc-cost-of-fertiliser-test  ; fields testing fertiliser

end

to calc-coffee-prod-test
  ask fields with [current-land-use = "coffee"][ ; here I choose the yield associated with phase III, which is the yield at full production
    ; set yield 906.04 ; yield at max production
    (ifelse
        fertiliser = "low" [set test-yield 906.04]
        fertiliser = "moderate" [set test-yield 1694]
        fertiliser = "high" [set test-yield 2016]
        fertiliser = "very high" [set test-yield 1864.44]
      )
    if (herbicides = true or herbicides = "testing true") [
      set test-yield test-yield * 1.09
    ]
    set production test-yield * field-size
  ]
  set test-coffeeproduction sum [production] of in-myfield-neighbors with [current-land-use = "coffee"] ; unit = kg
end

to calc-feed-prod-test
  set test-feedproduction sum [field-size] of in-myfield-neighbors with [current-land-use = "pasture"] * utilisable-pasture +
                          test-maizeproduction * utilisable-maizestover +
                          (test-coffeeproduction / 1000) * utilisable-coffeehusk ; in tonnes DM per year
end

to underfed-die-test

    set test-cowcost ifelse-value testing-strategy = "buy cow" [global-cow-purchase-price / 10] [0] ; divided by ten because the costs are payed off over the entire lifetime of the cow, not the same year only
    set test-feedcost ifelse-value (testing-strategy = "buy feed" or adopted-feed? = "yes") [(global-supplementary-feed-price * count in-mycow-neighbors)] [0]

    let sell-heifer?    ifelse-value any? in-mycow-neighbors with [color = red and calf < 1][1][0]
    let sell-young-cow? ifelse-value any? in-mycow-neighbors with [color = red and calf = 1][1][0]
    let sell-cow?       ifelse-value any? in-mycow-neighbors with [color = red and calf > 1][1][0]

    set test-cowsales sell-heifer? * global-heifer-sell-price +           ; female cattle that have not yet given birth yet are heifers
                      sell-young-cow? * global-cow-sell-price * 2 +       ; price of young milk cows = 2x price of older milk cows, source: Vernooij 2010
                      sell-cow? * global-cow-sell-price                   ; price of milk cows

     ; if who = test-hh [ print (word "who = " who " this is inside the underfed-die-test, test-cowsales = " test-cowsales)]

    ; only cows on low quality diet risk dying
    if (count in-mycow-neighbors with [color != red] > 0 and testing-strategy != "buy feed" and adopted-feed? != "yes") [
      let doomed ceiling(((test-feedproduction * 1000) / 365 ) / 3.15) ; 3.15 = parameter feed-lowerthreshold-nosup
      if doomed > count in-mycow-neighbors with [color != red] [set doomed count in-mycow-neighbors with [color != red]] ; can happen due to rounding up
      ask n-of doomed in-mycow-neighbors with [color != red] [set color yellow] ; I don't want them to die when I am testing, so turn them yellow instead
    ]

end

to calc-milk-prod-test
    ask in-mycow-neighbors with [color != yellow and color != red][ ; yellow cows are doomed to die of under-nutrition, red cows are sold, orange cows are bought
      ifelse ([count in-mycow-neighbors] of myself = 0)[
        set feed-quantity 0][
        let tonnes-year ([feedproduction] of myself / [count in-mycow-neighbors] of myself)
        let kg-day (tonnes-year * 1000) / 365
        set feed-quantity kg-day]
    ]

  ; milk yield with low-quality diet
  ask cattle with [in-lactation = "yes" and color != yellow and color != red and color != orange and supplementary-feed = "no"]; with [some age criteria]
  [
    (ifelse
      feed-quantity <= 3.15 [set milkyield 0 ] ;set color red 3.15 = parameter feed-lowerthreshold-nosup
      feed-quantity > 3.15 and feed-quantity < 20 [set milkyield -2.11 + 0.67 * feed-quantity ] ;set color green 20 = parameter feed-upperthreshold-nosup, -2.11 = milk-intercept-nosup, 0.67 = milk-slop-nosup
      feed-quantity >= 20 [set milkyield -2.11 + 0.67 * 20 ] ; this is max;  set color grey 20 = parameter feed-upperthreshold-sup, -2.11 = milk-intercept-nosup, 0.67 = milk-slop-nosup
    )
  ]

  ; milk yield with high-quality diet
  ask cattle with [in-lactation = "yes" and color != yellow and color != red and color != orange and (supplementary-feed = "testing" or supplementary-feed = "yes")]; with [some age criteria]
  [
    (ifelse
      feed-quantity <= 0 [set milkyield 0 ] ;set color red 0 = parameter feed-lowerthreshold-sup
      feed-quantity > 0 and feed-quantity < 20 [set milkyield 1.38 + 0.74 * feed-quantity ] ;set color green 20 = parameter feed-upperthreshold-sup 1.38 = milk-intercept-nosup, 0.74 = milk-slop-nosup
      feed-quantity >= 20 [set milkyield 1.38 + 0.74 * 20 ] ; this is max;  set color grey 20 = parameter feed-upperthreshold-sup 1.38 = milk-intercept-nosup, 0.74 = milk-slop-nosup
    )
  ]

  if count in-mycow-neighbors with [color != yellow and color != red] > 0 [ set test-milkproduction sum [milkyield] of in-mycow-neighbors ]
  if testing-strategy = "stick with current" and who = test-hh [print (word "milk production current = " test-milkproduction)]
  if testing-strategy = "buy feed" and who = test-hh [print (word "milk production supplementary feed = " test-milkproduction)]

end

to milk-to-butter-test
  set milk-left test-milkproduction
   ifelse milk-left > 56 * 5 [set test-butterproduction milk-left / milk-to-butter-conversion-factor] [set test-butterproduction 0] ; unit = kg. 26 = # weeks in 6 months (assuming that the herd is synchronised), 5 = minimum amount (L) needed for butter making per week.
  ; About 16.5 litres of milk is required to produce a kilogram of butter (Anteneh 2010, p. 23)
end

to calc-butter-profit-test
  let butter-left test-butterproduction ;- butter-requirement
  ifelse butter-left > 0 [set test-buttersales (butter-left * global-butter-price)][set test-buttersales 0]
end

to sell-cattle-test

  ; sell 2 year old bulls
  if count in-mycow-neighbors with [sex = "male" and castrated = "no" and age > 1] = 0 [ set test-bullsales 0 ]
  if count in-mycow-neighbors with [sex = "male" and castrated = "no" and age > 1] > 0[
    let to-sell count in-mycow-neighbors with [sex = "male" and castrated = "no" and age > 1]
    set test-bullsales to-sell * global-bull-sell-price ;etb
  ]

  ; sell 7 year old oxen
  if count in-mycow-neighbors with [castrated = "yes" and age > 6] = 0 [ set test-oxensales 0 ]
  if count in-mycow-neighbors with [castrated = "yes" and age > 6] > 0[
    let to-sell count in-mycow-neighbors with [castrated = "yes" and age > 6]
    set test-oxensales to-sell * global-oxen-sell-price ; etb
    ]
end

to-report buy-cow-expected-sales-test
  ifelse testing-strategy = "buy cow" [
    ifelse count in-mycow-neighbors with [color != orange and in-lactation = "yes"] > 0 [
      let average-buttersales-percow ( test-buttersales / count in-mycow-neighbors with [color != orange and sex = "female"] ) * 4 ; this is the amount of milk she is expected to produce over her life
      let exp-bullsales ( round sum (list random-float 1 random-float 1 random-float 1 random-float 1) ) * global-bull-sell-price  ; this is the number of bulls she is expected to sell (0-4)
      report (average-buttersales-percow + exp-bullsales) / 10                                                                     ; this is the average income from sales over her life
    ][
      ask in-mycow-neighbors with [in-lactation = "yes" and color = orange and supplementary-feed = "no"][
        (ifelse
          feed-quantity <= 3.15 [set milkyield 0 ] ;set color red 3.15 = parameter feed-lowerthreshold-nosup
          feed-quantity > 3.15 and feed-quantity < 20 [set milkyield -2.11 + 0.67 * feed-quantity ] ;set color green 20 = parameter feed-upperthreshold-nosup, -2.11 = milk-intercept-nosup, 0.67 = milk-slop-nosup
          feed-quantity >= 20 [set milkyield -2.11 + 0.67 * 20 ] ; this is max;  set color grey 20 = parameter feed-upperthreshold-nosup, -2.11 = milk-intercept-nosup, 0.67 = milk-slop-nosup
        )
      ]

      ; milk yield with high-quality diet
      ask cattle with [in-lactation = "yes" and color = orange and (supplementary-feed = "testing" or supplementary-feed = "yes")]; with [some age criteria]
      [
        (ifelse
          feed-quantity <= 0 [set milkyield 0 ] ;set color red 0 = parameter feed-lowerthreshold-sup
          feed-quantity > 0 and feed-quantity < 20 [set milkyield 1.38 + 0.74 * feed-quantity ] ;set color green 20 = parameter feed-upperthreshold-sup 1.38 = milk-intercept-nosup, 0.74 = milk-slop-nosup
          feed-quantity >= 20 [set milkyield 1.38 + 0.74 * 20 ] ; this is max;  set color grey 20 = parameter feed-upperthreshold-sup 1.38 = milk-intercept-nosup, 0.74 = milk-slop-nosup
        )
      ]

      set test-milkproduction sum [milkyield] of in-mycow-neighbors with [color = orange]

      let milk-requirement 0.1 * 365 * household-size; unit = L/household member per year
      let milks-left test-milkproduction - milk-requirement
      ifelse milks-left > 56 * 5 [set test-butterproduction milks-left / milk-to-butter-conversion-factor] [set test-butterproduction 0] ; unit = kg. 26 = # weeks in 6 months (assuming that the herd is synchronised), 5 = minimum amount (L) needed for butter making per week.

      let butter-requirement 0.2 * 12 * household-size
      let butter-left test-butterproduction - butter-requirement
      ifelse butter-left > 0 [set test-buttersales (butter-left * global-butter-price)][set test-buttersales 0]

      let average-buttersales-percow test-buttersales * 4                                                                           ; this is the amount of milk she will produce over her life
      let exp-bullsales ( round sum (list random-float 1 random-float 1 random-float 1 random-float 1) ) * global-bull-sell-price   ; this is the number of bulls she will sell (0-4)
      report (average-buttersales-percow + exp-bullsales) / 10                                                                      ; this is the average income from sales over her life
    ]
  ][
    report 0
  ]
end

to calc-coffee-profit-test
  let coffee-requirement 0.2 * 12 * household-size ; according to https://agsci.colostate.edu/smallholderagriculture/ethiopia-diet-analysis/
  let coffee-left test-coffeeproduction - coffee-requirement
  ifelse coffee-left > 0 [set test-coffeesales coffee-left * global-coffee-price][set test-coffeesales 0]
  ; if who = test-hh [print (word "coffee left: " coffee-left)]
  ; if who = test-hh [print (word "test coffee sales: " test-coffeesales)]

  ; subtract cost of fertiliser
  let herbicide-area sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and (herbicides = true or herbicides = "testing true")]
  let herbicide-cost herbicide-area * global-herbicides-price

  ; subtract establishment costs, production costs and fertiliser costs
  let establish-cost global-coffeeplantation-price * sum [field-size] of in-myfield-neighbors with [color = grey and current-land-use = "coffee"] / 29 ; 29 = lifetime of plantation
  let production-cost 22309 * sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee"] + herbicide-cost           ; cost at peak production; coffeecost-phase3 (Diro 2019)

  set test-coffeecost establish-cost + production-cost + calc-cost-of-fertiliser in-myfield-neighbors with [current-land-use = "coffee"]
  if any? in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "testing"] [set test-coffeecost test-coffeecost + calc-cost-of-fertiliser-test]
  ; if who = test-hh [print (word "test coffee cost: " test-coffeecost)]
end

to calc-labour-test

  let maizelabour sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser = "low"] * 41 + ; parameter 41 = maize-labour-needed
                  sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser != "low" and herbicides = false] * 41 + 3 + ; parameter 3 = fertiliser-labour-needed
                  sum [field-size] of in-myfield-neighbors with [current-land-use = "maize" and fertiliser != "low" and herbicides != false] * 41 + 3 - 12 ; parameter -12 = herbicides-labour-needed

  let coffeelabour sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser = "low"] * 132 + ; parameter 132 = coffee-labour-needed
                   sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser != "low" and herbicides = false] * (132 + 3) + ; parameter 3 = fertiliser-labour-needed
                   sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and fertiliser != "low" and herbicides != false] * (132 + 3 - 12) ; parameter -12 = herbicides-labour-needed

  let coffeeinitialisationlabour sum [field-size] of in-myfield-neighbors with [current-land-use = "coffee" and color = grey] * 51 ; person days to establish plantation first year ; coffee-initialisation-labour-needed = 51

  let veglabour sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser = "low"] * (41 + (41 * 0.15)) + ; parameter 41 + (41 * 0.15) = vegetable-labour-needed
                        sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser != "low" and herbicides = false] * (41 + (41 * 0.15) + 3) + ; parameter 3 = fertiliser-labour-needed
                        sum [field-size] of in-myfield-neighbors with [current-land-use = "vegetable" and fertiliser != "low" and herbicides != false] * (41 + (41 * 0.15) + 3 - 12) ; parameter -12 = herbicides-labour-needed



  let cattlelabour (2.55 - 0.037 * count in-mycow-neighbors) * count in-mycow-neighbors ;
  let pasturelabour 0.5 ; parameter: pasture-labour-needed. Educated guess, could not find any numeric value in literature
  let testlaboursum maizelabour + coffeelabour + coffeeinitialisationlabour + veglabour + cattlelabour + pasturelabour
  set leisure-expectation ((labourcap - testlaboursum)) / 56 ; leisure days per week
end

;; REPORTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report get-aspiration-ranks

  let n-unique table:length table:counts (list (income-relativeimportance) (food-relativeimportance) (leisure-relativeimportance))

  if n-unique < 3
  [
    set income-relativeimportance income-relativeimportance + random-float 1 - 0.5
    set leisure-relativeimportance leisure-relativeimportance + random-float 1 - 0.5
    set food-relativeimportance food-relativeimportance + random-float 1 - 0.5
  ]

  let income-rank position income-relativeimportance sort (list (income-relativeimportance) (food-relativeimportance) (leisure-relativeimportance)) + 1 ; will be 1 if most important, 2 if second most important and 3 if least important
  let leisure-rank position leisure-relativeimportance sort (list (income-relativeimportance) (food-relativeimportance) (leisure-relativeimportance)) + 1 ; will be 1 if most important, 2 if second most important and 3 if least important
  let food-rank position food-relativeimportance sort (list (income-relativeimportance) (food-relativeimportance) (leisure-relativeimportance)) + 1 ; will be 1 if most important, 2 if second most important and 3 if least important

  let ranks (list income-rank leisure-rank food-rank)
  report ranks
end

to-report drop-least-important
  ;get aspiration ranks
  let aspiration-ranks get-aspiration-ranks
  let aspiration-drop replace-item (position 3 aspiration-ranks) aspiration-ranks 0 ; the last (3rd) ranks is replaced with 0 ---------------------> 1 2 0
  let aspiration-id replace-item (position 2 aspiration-drop) aspiration-drop 1 ; the second best (2nd) rank is replaced with 1----------------> 1 1 0
  report (map * (list income-threshold-mean food-threshold-mean leisure-threshold-mean) aspiration-id)

end

to-report drop-2ndleast-important
  ;get aspiration ranks
  let aspiration-ranks get-aspiration-ranks
  let aspiration-drop replace-item (position 2 aspiration-ranks) aspiration-ranks 0 ; the last (3rd) ranks is replaced with 0 ---------------------> 1 0 3
  let aspiration-id replace-item (position 3 aspiration-drop) aspiration-drop 1 ; the second best (2nd) rank is replaced with 1----------------> 1 0 1
  report (map * (list income-threshold-mean food-threshold-mean leisure-threshold-mean) aspiration-id)
end

to-report keep-only-most-important
  ;get aspiration ranks
  let aspiration-ranks get-aspiration-ranks
  let aspiration-drop replace-item (position 3 aspiration-ranks) aspiration-ranks 0 ; the last (3rd) ranks is replaced with 0 ---------------------> 1 2 0
  let aspiration-id replace-item (position 2 aspiration-drop) aspiration-drop 0 ; the second best (2nd) rank is replaced with 1----------------> 1 1
  report (map * (list income-threshold-mean food-threshold-mean leisure-threshold-mean) aspiration-id)
end

to-report drop-most-important
  ;get aspiration ranks
  let aspiration-ranks get-aspiration-ranks
  let aspiration-drop1 replace-item (position 1 aspiration-ranks) aspiration-ranks 0 ; the last (3rd) ranks is replaced with 0 ---------------------> 1 0 3
  let aspiration-drop replace-item (position 2 aspiration-drop1) aspiration-drop1 1 ; the last (3rd) ranks is replaced with 0 ---------------------> 1 0 3
  let aspiration-id replace-item (position 3 aspiration-drop) aspiration-drop 1 ; the second best (2nd) rank is replaced with 1----------------> 1 0 1
  report (map * (list income-threshold-mean food-threshold-mean leisure-threshold-mean) aspiration-id)
end

to-report keep-only-2ndmost-important
  ;get aspiration ranks
  let aspiration-ranks get-aspiration-ranks
  let aspiration-drop1 replace-item (position 1 aspiration-ranks) aspiration-ranks 0 ; the last (3rd) ranks is replaced with 0 ---------------------> 1 0 3
  let aspiration-drop replace-item (position 2 aspiration-drop1) aspiration-drop1 1 ; the last (3rd) ranks is replaced with 0 ---------------------> 1 0 3
  let aspiration-id replace-item (position 3 aspiration-drop) aspiration-drop 0 ; the second best (2nd) rank is replaced with 1----------------> 1 0 1
  report (map * (list income-threshold-mean food-threshold-mean leisure-threshold-mean) aspiration-id)
end

to-report keep-only-least-important
  ;get aspiration ranks
  let aspiration-ranks get-aspiration-ranks
  let aspiration-drop1 replace-item (position 1 aspiration-ranks) aspiration-ranks 0 ; the last (3rd) ranks is replaced with 0 ---------------------> 1 0 3
  let aspiration-drop replace-item (position 2 aspiration-drop1) aspiration-drop1 0 ; the last (3rd) ranks is replaced with 0 ---------------------> 1 0 3
  let aspiration-id replace-item (position 3 aspiration-drop) aspiration-drop 1 ; the second best (2nd) rank is replaced with 1----------------> 1 0 1
  report (map * (list income-threshold-mean food-threshold-mean leisure-threshold-mean) aspiration-id)
end

to-report calc-cost-of-fertiliser-test

  let fert-cost sum [(ifelse-value
    fertiliser = "testing moderate"  [(ifelse-value current-land-use = "maize" [maize-N-moderate] current-land-use = "vegetable" [veg-N-moderate] current-land-use = "coffee" [coffee-N-moderate] current-land-use = "pasture" [0]) * field-size * global-fertiliser-price]
    fertiliser = "testing high"      [(ifelse-value current-land-use = "maize" [maize-N-high] current-land-use = "vegetable" [veg-N-high] current-land-use = "coffee" [coffee-N-high] current-land-use = "pasture" [0]) * field-size * global-fertiliser-price]
    fertiliser = "testing very high" [(ifelse-value current-land-use = "maize" [maize-N-veryhigh] current-land-use = "vegetable" [veg-N-veryhigh] current-land-use = "coffee" [coffee-N-veryhigh] current-land-use = "pasture" [0]) * field-size * global-fertiliser-price]
                                     [0]
    )] of fields with [member? "testing" fertiliser]

  report fert-cost
end

to-report calc-cost-of-fertiliser [fields-of-interest]
  let fert-cost sum [(ifelse-value
    fertiliser = "low"       [0]
    fertiliser = "moderate"  [(ifelse-value current-land-use = "maize" [maize-N-moderate] current-land-use = "vegetable" [veg-N-moderate] current-land-use = "coffee" [coffee-N-moderate] current-land-use = "pasture" [0]) * field-size * global-fertiliser-price]
    fertiliser = "high"      [(ifelse-value current-land-use = "maize" [maize-N-high] current-land-use = "vegetable" [veg-N-high] current-land-use = "coffee" [coffee-N-high] current-land-use = "pasture" [0]) * field-size * global-fertiliser-price]
    fertiliser = "very high" [(ifelse-value current-land-use = "maize" [maize-N-veryhigh] current-land-use = "vegetable" [veg-N-veryhigh] current-land-use = "coffee" [coffee-N-veryhigh] current-land-use = "pasture" [0]) * field-size * global-fertiliser-price]
                             [0]
  )] of fields-of-interest
  report fert-cost
end

; generic reporters
to-report take [n xs]
  report sublist xs 0 min list n (length xs)
end

to-report frequency [an-item a-list]
    report length (filter [ i -> i = an-item] a-list)
end

to-report get-experiences [delta-32 delta-21]
   report (
     list
      (delta-32)
      (delta-21)
      0 )
end

to-report calc-ew [outcome-t3 outcome-t2 outcome-t1]
  let my-forgetfullness forgetfullness

  ; get experiences
  let experiences get-experiences 0 0

  ; calculate
  let forgetfullness-adjusted-experiences (map * my-forgetfullness experiences)
  let ew sum forgetfullness-adjusted-experiences
  report ew
end

to check-67
  setup

  ask households with [who = 67][
      print (word
        "step: " ticks
        " | AO t-1: " precision income-outcome-t1start 4
        " | AO t-2: " precision income-outcome-t2 4
        " | AO t-3: " precision income-outcome-t3 4

        " | d AO 2-1: " precision (income-outcome-t2 - income-outcome-t1start) 4
        " | d AO 3-2: " precision (income-outcome-t3 - income-outcome-t2) 4

        " | AL t-1end: " precision income-threshold-t1end 4
        " | AL t-1start: " precision income-threshold-t1start 4
        " | AL t-2: " precision income-threshold-t2 4
        " | AL t-3: " precision income-threshold-t3 4

        " | ew: " precision ew-income 4
    )]

  repeat 21 [
    go
    ask households with [who = 67][
      print (word
        "step: " ticks
        " | AO t-1: " precision income-outcome-t1start 4
        " | AO t-2: " precision income-outcome-t2 4
        " | AO t-3: " precision income-outcome-t3 4

        " | d AO 2-1: " precision (income-outcome-t2 - income-outcome-t1start) 4
        " | d AO 3-2: " precision (income-outcome-t3 - income-outcome-t2) 4

        " | AL t-1end: " precision income-threshold-t1end 4
        " | AL t-1start: " precision income-threshold-t1start 4
        " | AL t-2: " precision income-threshold-t2 4
        " | AL t-3: " precision income-threshold-t3 4
        " | AL mean: " precision income-threshold-mean 4

        " | ew: " precision ew-income 4
    )]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Aesthetic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to hide-fields
  ask fields [set hidden? true]
  ask myfields [set hidden? true]
end

to hide-cattle
  ask cattle [set hidden? true]
  ask mycows [set hidden? true]
  ask mycalves [set hidden? true]
end

to hide-cities
  ask cities [set hidden? true]
  ask myfarms [set hidden? true]
end

to hide-households
  ask households [set hidden? true]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to test-lu
  if (chance-maize + chance-pasture + chance-veg + chance-coffee > 1) [
    error ("The sum of the chances of different land uses cannot be > 1")
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Initialising households (done once) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; test with importing and exporting agents
to clean-slate
  clear-all
  file-close-all ; Close any files open from last run
  reset-ticks
end

to generate-households
ask patches [set farmstead -1]
  ask n-of Number-of-households patches [
    sprout-households 1
    set farmstead 1
  ]

  ask households [
    set color white
    set pcolor grey
    set shape "house"
    ;set household-size random-poisson hhsize-lambda
  ]
end

; procedure to write some turtle properties to a file
to write-hhs-to-csv
  ; we use the `of` primitive to make a list of lists and then
  ; use the csv extension to write that list of lists to a file;
  csv:to-file "Netlogo_output/households.csv" [ (list who xcor ycor size color heading shape income-expectation) ] of households ;  why do I use income expectation here, and not income-outcome-t1end? Check in R
end

to write-pophhs-to-csv
  let popfile (word "Netlogo_Output/households" Population-nr ".csv")
  csv:to-file popfile [(list name xcor ycor size color heading shape household-size children elders adults TLU oxen cows calves farmland number_fields labourcap region woreda network-size
                             income-outcome-t1end food-outcome-t1end leisure-outcome-t1end
                             income-threshold-t1start food-threshold-t1start leisure-threshold-t1start
                             income-relativeimportance food-relativeimportance leisure-relativeimportance)] of households
end
@#$#@#$#@
GRAPHICS-WINDOW
285
24
788
528
-1
-1
15.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
49
64
113
97
NIL
Setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
49
105
112
138
NIL
Go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1362
45
1590
211
Income t1
Income distribution (ETB/year)
Number of households
-100.0
100000.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true ";; set-plot-y-range 0 10\nset-plot-x-range -100000 100000\nset-histogram-num-bars 30\nset-plot-pen-color 0 \nplot 0\nhistogram [income-outcome-t1end] of households" "histogram [income-outcome-t1start] of households"

PLOT
1362
220
1591
383
Maize consumption t1
Maize consumption distribution (kg per household per week)
NIL
0.0
50.0
0.0
10.0
true
false
"" ""
PENS
"default" 5.0 1 -16777216 true "histogram [food-outcome-t1end] of households" "histogram [food-outcome-t1start] of households"

BUTTON
120
105
180
138
Go once
Go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
125
1173
246
1233
Number-of-households
100.0
1
0
Number

TEXTBOX
25
202
175
222
Inputs
16
0.0
1

TEXTBOX
28
33
178
53
Commands
16
0.0
1

TEXTBOX
1365
17
1515
37
Monitoring
16
0.0
1

PLOT
1362
567
1592
736
Cattle ownership
Cattle ownership (#)
Frequency (# hh)
0.0
10.0
0.0
10.0
true
false
"" "histogram [count mycow-neighbors] of households"
PENS
"default" 1.0 1 -16777216 true "histogram [count link-neighbors] of households" "histogram [count mycow-neighbors] of households"

INPUTBOX
15
1093
94
1153
remember-t3
0.5
1
0
Number

INPUTBOX
99
1092
182
1152
remember-t2
0.75
1
0
Number

INPUTBOX
188
1092
267
1152
remember-t1
0.9
1
0
Number

INPUTBOX
15
1155
94
1215
memory-time
3.0
1
0
Number

INPUTBOX
250
955
331
1015
optimism
0.96
1
0
Number

INPUTBOX
15
525
97
585
chance-maize
0.65
1
0
Number

INPUTBOX
101
525
182
585
chance-pasture
0.2
1
0
Number

BUTTON
15
150
80
183
Hide fields
hide-fields
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1982
237
2267
402
Maize production
Years
kg per HH
0.0
10.0
0.0
10.0
true
true
"plot 0" ""
PENS
"Least" 1.0 0 -2674135 true "plot 0" "if ticks > 0 [plot min [maizeproduction] of households]"
"Mean" 1.0 0 -9276814 true "plot 0" "if ticks > 0 [plot mean [maizeproduction] of households]"
"Median" 1.0 0 -4539718 true "plot 0" "if ticks > 0 [plot median [maizeproduction] of households]"
"25% quartile" 1.0 0 -1664597 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([maizeproduction] of households)]"
"75% quartile" 1.0 0 -8862290 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([maizeproduction] of households)]"

PLOT
1982
412
2267
572
Milk production
Years
Liter per HH
0.0
10.0
0.0
10.0
true
true
"plot 0" ""
PENS
"Least" 1.0 0 -2674135 true "plot 0" "if ticks > 0 [plot min [milkproduction] of households]"
"Mean" 1.0 0 -9276814 true "plot 0" "if ticks > 0 [plot mean [milkproduction] of households]"
"Median" 1.0 0 -4539718 true "plot 0" "if ticks > 0 [plot median [milkproduction] of households]"
"25% quartile" 1.0 0 -1264960 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([milkproduction] of households)]"
"75% quartile" 1.0 0 -6759204 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([milkproduction] of households)]"

PLOT
1364
752
1594
902
Household size
Household members (#)
Frequency (#hh)
0.0
20.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "histogram [household-size] of households" "histogram [household-size] of households"

PLOT
2307
222
2592
387
Maize profit
Years
Profit (ETB)
0.0
10.0
0.0
10.0
true
true
"plot 0" ""
PENS
"Least" 1.0 0 -2674135 true "plot 0" "if ticks > 0 [plot min [maizeprofit] of households]"
"25% quartile" 1.0 0 -1264960 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([maizeprofit] of households)]"
"Mean" 1.0 0 -9276814 true "plot 0" "if ticks > 0 [plot mean [maizeprofit] of households]"
"Median" 1.0 0 -5987164 true "plot 0" "if ticks > 0 [plot median [maizeprofit] of households]"
"75% quartile" 1.0 0 -6759204 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([maizeprofit] of households)]"

PLOT
1982
582
2267
742
Butter production
Years
Kg per HH
0.0
10.0
0.0
2.0
true
true
"plot 0" ""
PENS
"Least" 1.0 0 -2674135 true "plot 0" "if ticks > 0 [plot min [butterproduction] of households]"
"25% quartile" 1.0 0 -1264960 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([butterproduction] of households)]"
"Mean" 1.0 0 -9276814 true "plot 0" "if ticks > 0 [plot mean [butterproduction] of households]"
"Median" 1.0 0 -5987164 true "plot 0" "if ticks > 0 [plot median [butterproduction] of households]"
"75% quartile" 1.0 0 -6759204 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([butterproduction] of households)]"

MONITOR
2187
697
2302
742
Butter producers (#)
count households with [butterproduction > 0]
0
1
11

MONITOR
2187
527
2287
572
Milk producers (#)
count households with [milkproduction > 0]
17
1
11

PLOT
2307
567
2592
727
Butter sales
Years
Profit (ETB)
0.0
10.0
0.0
10.0
true
true
"plot 0" ""
PENS
"Least" 1.0 0 -2674135 true "plot 0" "if ticks > 0 [plot min [buttersales] of households]"
"25% quartile" 1.0 0 -1264960 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([buttersales] of households)]"
"Mean" 1.0 0 -11053225 true "plot 0" "if ticks > 0 [plot mean [buttersales] of households]"
"Median" 1.0 0 -5987164 true "plot 0" "if ticks > 0 [plot median [buttersales] of households]"
"75% quartile" 1.0 0 -6759204 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([buttersales] of households)]"

MONITOR
2187
357
2297
402
Maize producers (#)
count households with [maizeproduction > 0]
17
1
11

MONITOR
1492
637
1579
682
Oxen owners
count households with [oxen > 0]
17
1
11

MONITOR
1494
592
1581
637
Cattle owners
count households with [herd > 0]
17
1
11

PLOT
2307
397
2592
552
Cattle sales
Years
Profit (ETB)
0.0
10.0
0.0
10.0
true
true
"plot 0" ""
PENS
"Least" 1.0 0 -2674135 true "plot 0" "if ticks > 0 [plot min [bullsales + oxensales] of households]"
"25% quartile" 1.0 0 -1264960 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([bullsales + oxensales] of households)]"
"Mean" 1.0 0 -11053225 true "plot 0" "if ticks > 0 [plot mean [bullsales + oxensales] of households]"
"Median" 1.0 0 -7500403 true "plot 0" "if ticks > 0 [plot median [bullsales + oxensales] of households]"
"75% quartile" 1.0 0 -6759204 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([bullsales + oxensales] of households)]"

MONITOR
2027
437
2084
482
Mean
mean [milkproduction] of households
0
1
11

MONITOR
2017
607
2074
652
Mean
mean [butterproduction] of households
0
1
11

MONITOR
2342
587
2399
632
Mean
mean [buttersales] of households
0
1
11

MONITOR
2347
422
2404
467
Mean
mean [oxensales + bullsales] of households
0
1
11

MONITOR
2357
247
2414
292
Mean
mean [maizeprofit] of households
0
1
11

MONITOR
2027
262
2084
307
Mean
mean [maizeproduction] of households
0
1
11

MONITOR
2512
337
2602
382
Maize sellers (#)
count households with [maizeprofit > 0]
17
1
11

MONITOR
2512
682
2607
727
Butter sellers (#)
count households with [butterprofit > 0]
17
1
11

MONITOR
2512
512
2614
557
Cattle sellers (#)
count households with [oxensales + bullsales > 0]
17
1
11

INPUTBOX
15
590
95
650
chance-veg
0.07
1
0
Number

BUTTON
85
150
145
183
Hide cattle
hide-cattle
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
150
150
215
183
Hide cities
hide-cities
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1982
67
2267
232
Vegetable prodution
Years
kg per HH
0.0
10.0
0.0
10.0
true
true
"plot 0" ""
PENS
"Least" 1.0 0 -2674135 true "plot 0" "if ticks > 0 [plot min [vegproduction] of households]"
"25% quartile" 1.0 0 -1264960 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([vegproduction] of households)]"
"Mean" 1.0 0 -7500403 true "plot 0" "if ticks > 0 [plot mean [vegproduction] of households]"
"Median" 1.0 0 -4539718 true "plot 0" "if ticks > 0 [plot median [vegproduction] of households]"
"75% quartile" 1.0 0 -6759204 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([vegproduction] of households)]"

MONITOR
2017
87
2074
132
Mean
mean [vegproduction] of households
0
1
11

PLOT
2307
52
2597
217
Vegetable profit
Years
Profit (ETB)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Least" 1.0 0 -2674135 true "plot 0" "if ticks > 0 [plot min [vegprofit] of households]"
"25% quartile" 1.0 0 -1264960 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([vegprofit] of households)]"
"Mean" 1.0 0 -9276814 true "plot 0" "if ticks > 0 [plot mean [vegprofit] of households]"
"Median" 1.0 0 -4539718 true "plot 0" "if ticks > 0 [plot median [vegprofit] of households]"
"75% quartile" 1.0 0 -6759204 true "plot 0" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([vegprofit] of households)]"

MONITOR
2347
77
2404
122
Mean
mean [vegprofit] of households
0
1
11

MONITOR
2192
172
2292
217
Veg producers (#)
count households with [vegproduction > 0]
17
1
11

MONITOR
2517
172
2609
217
Veg sellers (#)
count households with [vegprofit > 0]
0
1
11

MONITOR
1387
67
1442
112
Mean
mean [income-outcome-t1end] of households
0
1
11

MONITOR
1387
242
1444
287
Mean
mean [food-outcome-t1end] of households
0
1
11

PLOT
2632
577
2897
742
Mean income
Year
ETB per HH
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Min" 1.0 0 -2674135 true "" "if ticks > 0 [plot min [income-outcome-t1end] of households]"
"25% quartile" 1.0 0 -1264960 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([income-outcome-t1end] of households)]"
"Mean" 1.0 0 -11053225 true "" "if ticks > 0 [plot mean [income-outcome-t1end] of households]"
"Median" 1.0 0 -4539718 true "" "if ticks > 0 [plot median [income-outcome-t1end] of households]"
"75% quartile" 1.0 0 -6759204 true "" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([income-outcome-t1end] of households)]"

PLOT
2632
227
2897
392
Mean maize consumption
Year
kg per (HH*week)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Least" 1.0 0 -2674135 true "" "if ticks > 0 [plot min [food-outcome-t1end] of households]"
"25% quartile" 1.0 0 -1264960 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([food-outcome-t1end] of households)]"
"Mean" 1.0 0 -11053225 true "" "if ticks > 0 [plot mean [food-outcome-t1end] of households]"
"Median" 1.0 0 -5987164 true "" "if ticks > 0 [plot median [food-outcome-t1end] of households]"
"75% quartile" 1.0 0 -6759204 true "" "if ticks > 0 [plot max [food-outcome-t1end] of households]"

TEXTBOX
2637
162
2787
180
Aspirational dimensions
12
0.0
1

MONITOR
2817
687
2897
732
Negative HH's
count households with [income-outcome-t1end < 0.001]
0
1
11

MONITOR
2677
597
2734
642
Mean
mean [income-outcome-t1end] of households
0
1
11

MONITOR
2662
247
2719
292
Mean
mean [food-outcome-t1end] of households
0
1
11

TEXTBOX
2637
202
2787
220
Aspirational outcomes
11
0.0
1

TEXTBOX
2912
202
3062
220
Aspirational thresholds
11
0.0
1

PLOT
2912
227
3177
392
Mean food threshold
Years
kg consumed (hh*week)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Least" 1.0 0 -2674135 true "" "if ticks > 0 [plot min [food-threshold-mean] of households]"
"25% quartile" 1.0 0 -1264960 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([food-threshold-mean] of households)]"
"Mean" 1.0 0 -11053225 true "" "if ticks > 0 [plot mean [food-threshold-mean] of households]"
"Median" 1.0 0 -4539718 true "" "if ticks > 0 [plot median [food-threshold-mean] of households]"
"75% quartile" 1.0 0 -6759204 true "" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([food-threshold-mean] of households)]"

PLOT
2912
577
3177
742
Mean income threshold
Years
Birr (HH*year)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Least" 1.0 0 -2674135 true "" "if ticks > 0 [plot min [income-threshold-mean] of households]"
"25% quartile" 1.0 0 -1264960 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([income-threshold-mean] of households)]"
"Mean" 1.0 0 -11053225 true "" "if ticks > 0 [plot mean [income-threshold-mean] of households]"
"Median" 1.0 0 -4539718 true "" "if ticks > 0 [plot median [income-threshold-mean] of households]"
"75% quartile" 1.0 0 -6759204 true "" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([income-threshold-mean] of households)]"

PLOT
2567
1107
3112
1267
Income of two random households
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Outcome HH1" 1.0 0 -6565750 true "" "if ticks > 0 [plot mean [income-outcome-t1end] of households with [who = 1]]"
"Aspiration HH1" 1.0 0 -10899396 true "" "if ticks > 0 [plot mean [income-threshold-t1start] of households with [who = 1]]"
"Outcome HH2" 1.0 0 -8275240 true "" "if ticks > 0 [plot mean [income-outcome-t1end] of households with [who = 2]]"
"Aspiration HH2" 1.0 0 -13791810 true "" "if ticks > 0 [plot mean [income-threshold-t1start] of households with [who = 2]]"

PLOT
2630
750
3175
910
Maize consumption of two random households
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Outcome HH1" 1.0 0 -6565750 true "" "if ticks > 0 [plot mean [food-outcome-t1end] of households with [who = 1]]"
"Aspiration HH1" 1.0 0 -10899396 true "" "if ticks > 0 [plot mean [food-threshold-t1start] of households with [who = 1]]"
"Outcome HH2" 1.0 0 -8275240 true "" "if ticks > 0 [plot mean [food-outcome-t1end] of households with [who = 2]]"
"Aspiration HH2" 1.0 0 -13791810 true "" "if ticks > 0 [plot mean [food-threshold-t1start] of households with [who = 1]]"

MONITOR
3102
337
3172
382
Mean gap
mean [food-threshold-mean - food-outcome-t1end] of households
0
1
11

MONITOR
3107
687
3172
732
Mean gap
mean [income-threshold-mean - income-outcome-t1end] of households
0
1
11

MONITOR
3032
1202
3099
1247
Gap HH1
mean [income-threshold-mean - income-outcome-t1end] of households with [who = 1]
0
1
11

MONITOR
3032
842
3099
887
Gap HH1
mean [food-threshold-mean - food-outcome-t1end] of households with [who = 1]
0
1
11

MONITOR
3022
307
3079
352
Mean
mean [food-threshold-mean] of households
0
1
11

MONITOR
3027
677
3084
722
Mean
mean [income-threshold-mean] of households
0
1
11

MONITOR
2802
147
2894
192
Satisficed HH's
count households with [satisficed = \"yes\"]
0
1
11

PLOT
2912
47
3177
197
Satisficed HH's
Year
Count
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"satisficed" 1.0 0 -16777216 true "" "plot count households with [satisficed = \"yes\"]"
"could not find" 1.0 0 -2674135 true "" "plot count households with [color = red]"

MONITOR
560
625
775
670
...could not find a satisficing strategy
count households with [color = red]
0
1
11

MONITOR
560
675
775
720
...found a satisificing strategy
count households with [color = 67]
0
1
11

MONITOR
560
575
777
620
 ...searched for a satisficing strategy
count households with [color = 67] + count households with [color = red]
0
1
11

TEXTBOX
590
550
740
568
Households that...
12
0.0
1

MONITOR
1907
1097
2027
1142
wealth of bottom 10%
sum sublist sort [income-outcome-t1start] of households 0 ceiling (Number-of-households * 0.10) / sum [income-outcome-t1start] of households
4
1
11

MONITOR
1907
1147
2027
1192
wealth of bottom 25%
sum sublist sort [income-outcome-t1start] of households 0 ceiling (Number-of-households * 0.25) / sum [income-outcome-t1start] of households
4
1
11

MONITOR
1907
1197
2027
1242
wealth of bottom 50%
sum sublist sort [income-outcome-t1start] of households 0 ceiling (Number-of-households * 0.50) / sum [income-outcome-t1start] of households
4
1
11

MONITOR
2057
1197
2177
1242
wealth of top 50%
sum sublist sort [income-outcome-t1start] of households ceiling (Number-of-households * 0.50) Number-of-households / sum [income-outcome-t1start] of households
4
1
11

MONITOR
2057
1147
2174
1192
wealth of top 25%
sum sublist sort [income-outcome-t1start] of households round (Number-of-households * 0.75) Number-of-households / sum [income-outcome-t1start] of households
4
1
11

MONITOR
2057
1097
2174
1142
wealth of top 10%
sum sublist sort [income-outcome-t1start] of households round (Number-of-households * 0.90) Number-of-households / sum [income-outcome-t1start] of households
4
1
11

MONITOR
2202
1097
2272
1142
GINI index
100 - gini
0
1
11

TEXTBOX
2287
1107
2437
1133
Range [0, 100] goes from very equal to extremely inequal
11
0.0
1

INPUTBOX
100
590
180
650
chance-coffee
0.07
1
0
Number

PLOT
285
540
545
725
Land use
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"maize" 1.0 0 -1184463 true "plot count fields with [current-land-use = \"maize\"]" "plot count fields with [current-land-use = \"maize\"]"
"pasture" 1.0 0 -10899396 true "plot count fields with [current-land-use = \"pasture\"]" "plot count fields with [current-land-use = \"pasture\"]"
"vegetable" 1.0 0 -955883 true "plot count fields with [current-land-use = \"vegetable\"]" "plot count fields with [current-land-use = \"vegetable\"]"
"coffee" 1.0 0 -6459832 true "plot count fields with [current-land-use = \"coffee\"]" "plot count fields with [current-land-use = \"coffee\"]"
"fallow" 1.0 0 -7500403 true "plot count fields with [current-land-use = \"fallow\"]" "plot count fields with [current-land-use = \"fallow\"]"

PLOT
290
745
545
895
Non-maize fields
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"pastures" 1.0 0 -10899396 true "" "plot count fields with [current-land-use = \"pasture\"]"
"vegetable" 1.0 0 -955883 true "" "plot count fields with [current-land-use = \"vegetable\"]"
"coffee" 1.0 0 -6459832 true "" "plot count fields with [current-land-use = \"coffee\"]"
"fallow" 1.0 0 -7500403 true "" "plot count fields with [current-land-use = \"fallow\"]"

PLOT
1917
917
2202
1077
Coffee production
Years
kg per HH
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Least" 1.0 0 -2674135 true "" "if ticks > 0 [plot min [coffeeproduction] of households]"
"25% quartile" 1.0 0 -1264960 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([coffeeproduction] of households)]"
"Mean" 1.0 0 -11053225 true "" "if ticks > 0 [plot mean [coffeeproduction] of households]"
"Median" 1.0 0 -4539718 true "" "if ticks > 0 [plot median [coffeeproduction] of households]"
"75% quartile" 1.0 0 -6759204 true "" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([coffeeproduction] of households)]"

MONITOR
1952
937
2009
982
Mean
mean [coffeeproduction] of households
0
1
11

MONITOR
2122
1032
2237
1077
Coffee producers (#)
count households with [coffeeland > 0]
0
1
11

PLOT
2247
917
2532
1077
Coffee profit
Years
Profit (ETB)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Least" 1.0 0 -2674135 true "" "if ticks > 0 [plot min [coffeeprofit] of households]"
"25% quartile" 1.0 0 -1264960 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([coffeeprofit] of households)]"
"Mean" 1.0 0 -11053225 true "" "if ticks > 0 [plot mean [coffeeprofit] of households]"
"Median" 1.0 0 -5987164 true "" "if ticks > 0 [plot median [coffeeprofit] of households]"
"75% quartile" 1.0 0 -6759204 true "" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([coffeeprofit] of households)]"

MONITOR
2452
1032
2552
1077
Coffee sellers (#)
count households with [coffeeprofit > 0]
0
1
11

MONITOR
2297
947
2354
992
Mean
mean [coffeeprofit] of households
0
1
11

PLOT
1364
927
1594
1077
Savings
Savings (ETB)
Frequency (#hh)
-10000.0
10000.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true ";; set-plot-y-range 0 10\nset-plot-x-range -1000000 1000000\nset-histogram-num-bars 30\nset-plot-pen-color 0 \nplot 0\nhistogram [savings] of households" "histogram [savings] of households"

MONITOR
1389
947
1454
992
Mean
mean [savings] of households
0
1
11

MONITOR
1519
947
1584
992
Median
median [savings] of households
0
1
11

PLOT
1362
392
1587
557
Leisure t1
person days per hh per week
NIL
-10.0
50.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "set-histogram-num-bars 30\nset-plot-pen-color 0 " "histogram [leisure-outcome-t1end] of households"

PLOT
2632
402
2897
567
Mean leisure 
Year
person days (HH*week)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Least" 1.0 0 -2674135 true "" "if ticks > 0 [plot min [leisure-outcome-t1end] of households]"
"25% quartile" 1.0 0 -1264960 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([leisure-outcome-t1end] of households)]"
"Mean" 1.0 0 -11053225 true "" "if ticks > 0 [plot mean [leisure-outcome-t1end] of households]"
"Median" 1.0 0 -5987164 true "" "if ticks > 0 [plot median [leisure-outcome-t1end] of households]"
"75% quartile" 1.0 0 -6759204 true "" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([leisure-outcome-t1end] of households)]"

PLOT
2912
402
3177
567
Mean leisure threshold
Years
person days (HH*week)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Least" 1.0 0 -2674135 true "" "if ticks > 0 [plot min [leisure-threshold-mean] of households]"
"25% quartile" 1.0 0 -1264960 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([leisure-threshold-mean] of households)]"
"Mean" 1.0 0 -11053225 true "" "if ticks > 0 [plot mean [leisure-threshold-mean] of households]"
"Median" 1.0 0 -5987164 true "" "if ticks > 0 [plot median [leisure-threshold-mean] of households]"
"75% quartile" 1.0 0 -6759204 true "" "if ticks > 0 [plot item round (Number-of-households * 0.75) sort([leisure-threshold-mean] of households)]"

MONITOR
2662
422
2719
467
Mean
mean [leisure-outcome-t1end] of households
1
1
11

PLOT
2567
927
3112
1092
Leisure of two random households
Year
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Outcome HH1" 1.0 0 -5509967 true "" "if ticks > 0 [plot mean [leisure-outcome-t1end] of households with [who = 1]]"
"Aspiration HH1" 1.0 0 -14439633 true "" "if ticks > 0 [plot mean [leisure-threshold-t1start] of households with [who = 1]]"
"Outcome HH2" 1.0 0 -6759204 true "" "if ticks > 0 [plot mean [leisure-outcome-t1end] of households with [who = 2]]"
"Threshold HH2" 1.0 0 -13403783 true "" "if ticks > 0 [plot mean [leisure-threshold-t1start] of households with [who = 2]]"

MONITOR
3017
497
3074
542
Mean
mean [leisure-threshold-mean] of households
1
1
11

MONITOR
2947
1027
3024
1072
threshold-t1
mean [leisure-threshold-t1start] of households with [who = 1]
2
1
11

MONITOR
2867
1027
2944
1072
outcome-t1
mean [leisure-outcome-t1end] of households with [who = 1]
2
1
11

MONITOR
3027
1027
3104
1072
threshold-m
mean [leisure-threshold-mean] of households with [who = 1]
2
1
11

SWITCH
158
274
265
307
fix-seed?
fix-seed?
0
1
-1000

BUTTON
200
475
275
510
generate hhs
generate-households
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
200
437
275
470
clean slate
clean-slate
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
200
515
275
548
hhs to csv
write-hhs-to-csv
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
200
590
270
623
hhs from csv
read-households-from-csv
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
200
555
270
588
let hhs die
ask households [die]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
560
745
795
895
dissatisficed hh that...
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"... found" 1.0 0 -13840069 true "" "plot count households with [color = green - 2]"
"... didn't" 1.0 0 -2674135 true "" "plot count households with [color = red]"
"... searched" 1.0 0 -7500403 true "" "plot count households with [color = red] + count households with [color = green - 2]"

CHOOSER
849
47
989
92
Strategy-order
Strategy-order
"random" "defined-by-similarity"
0

INPUTBOX
120
955
215
1015
b-no
0.92
1
0
Number

SWITCH
24
275
148
308
write-file?
write-file?
0
1
-1000

TEXTBOX
20
430
200
471
memory time does not (yet) have any impact on how the model runs
11
0.0
1

TEXTBOX
20
485
180
520
initial ATs = AOs + AOs * initial-aspiration-factor
11
0.0
1

MONITOR
1387
412
1444
457
mean
mean [leisure-outcome-t1start] of households
0
1
11

TEXTBOX
200
375
350
416
pessimism: < 1\nneutral: = 1\noptimism: > 1\n
11
0.0
1

MONITOR
3102
512
3169
557
Mean gap
mean [leisure-threshold-mean - leisure-outcome-t1end] of households
1
1
11

PLOT
2307
742
2592
902
Profit from cattle and butter
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"plot 0" ""
PENS
"Least" 1.0 0 -2674135 true "" "if ticks > 0 [plot min [bullsales + oxensales + buttersales] of households]"
"25% quartile" 1.0 0 -1664597 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([bullsales + oxensales + buttersales] of households)]"
"Mean" 1.0 0 -11053225 true "" "if ticks > 0 [plot mean [bullsales + oxensales + buttersales] of households]"
"Median" 1.0 0 -7500403 true "" "if ticks > 0 [plot median [bullsales + oxensales + buttersales] of households]"
"75% quartile" 1.0 0 -6759204 true "" "if ticks > 0 [plot item round (Number-of-households * 0.25) sort([bullsales + oxensales + buttersales] of households)]"

MONITOR
2267
762
2324
807
Mean
mean [bullsales + oxensales + buttersales] of households
0
1
11

BUTTON
185
105
252
138
Go 24x
repeat 24 [go]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1617
37
1732
82
chosen
word [chosen-strategy] of households with [who = test-hh]
17
1
11

MONITOR
1722
107
1807
152
Herd
word [count in-mycow-neighbors] of households with [who = test-hh]
17
1
11

MONITOR
1727
252
1812
297
All costs
word [round(costs-of-production)] of households with [who = test-hh]
0
1
11

MONITOR
1727
307
1812
352
Cow cost
word [cowcost] of households with [who = test-hh]
17
1
11

MONITOR
1727
357
1812
402
Feed cost
word [round(supplementaryfeedcost)] of households with [who = test-hh]
0
1
11

TEXTBOX
1627
22
1777
40
Test household
11
0.0
1

TEXTBOX
1622
92
1772
110
Expected
11
0.0
1

TEXTBOX
1732
92
1882
110
Actual
11
0.0
1

MONITOR
1617
307
1702
352
Cow cost
word [round(test-cowcost)] of households with [who = test-hh]
0
1
11

MONITOR
1617
357
1702
402
Feed cost
word [round(test-feedcost)] of households with [who = test-hh]
0
1
11

MONITOR
1615
107
1700
152
Herd
word [test-herd] of households with [who = test-hh]
0
1
11

MONITOR
1617
407
1702
452
Maize cost
[round(test-maizecost)] of households with [who = test-hh]
0
1
11

MONITOR
1620
457
1700
502
Veg cost
[round(test-vegcost)] of households with [who = test-hh]
0
1
11

MONITOR
1622
507
1699
552
Coffee cost
[round(test-coffeecost)] of households with [who = test-hh]
0
1
11

MONITOR
1727
407
1812
452
Maize cost
[round(maizecost)] of households with [who = test-hh]
0
1
11

MONITOR
1727
457
1812
502
Veg cost
[round(vegcost)] of households with [who = test-hh]
0
1
11

MONITOR
1727
507
1804
552
Coffee cost
[round(coffeecost)] of households with [who = test-hh]
0
1
11

MONITOR
1617
252
1702
297
All costs
[round(test-maizecost + test-vegcost + test-cowcost + test-feedcost + test-coffeecost)] of households with [who = test-hh]
0
1
11

MONITOR
1617
557
1689
602
Maize sales
[round(test-maizesales)] of households with [who = test-hh]
0
1
11

MONITOR
1727
557
1799
602
Maize sales
[round(maizesales)] of households with [who = test-hh]
0
1
11

MONITOR
1617
607
1687
652
Veg sales
[round test-vegsales] of households with [who = test-hh]
0
1
11

MONITOR
1727
607
1802
652
Veg sales
[round vegsales] of households with [who = test-hh]
0
1
11

MONITOR
1612
657
1689
702
Butter sales
[round test-buttersales] of households with [who = test-hh]
0
1
11

MONITOR
1727
657
1804
702
Butter sales
[round buttersales] of households with [who = test-hh]
0
1
11

MONITOR
1617
707
1687
752
Bull sales
[round test-bullsales] of households with [who = test-hh]
0
1
11

MONITOR
1727
707
1802
752
Bull sales
[round bullsales ] of households with [who = test-hh]
0
1
11

MONITOR
1607
757
1689
802
Coffee sales
[round test-coffeesales] of households with [who = test-hh]
0
1
11

MONITOR
1727
757
1809
802
Coffee sales
[round coffeesales] of households with [who = test-hh]
0
1
11

INPUTBOX
1742
22
1792
82
test-hh
5.0
1
0
Number

MONITOR
1617
812
1689
857
Oxen sales
[round test-oxensales] of households with [who = test-hh]
17
1
11

MONITOR
1727
812
1802
857
Oxen sales
[round oxensales] of households with [who = test-hh]
17
1
11

MONITOR
1827
357
1889
402
Adopted?
word [adopted-feed?] of households with [who = test-hh]
17
1
11

MONITOR
1727
202
1784
247
All sales
word [round(earnings-from-sales)] of households with [who = test-hh]
17
1
11

MONITOR
1622
202
1679
247
All sales
[round(test-maizesales + test-vegsales + test-buttersales + test-bullsales + test-oxensales + test-coffeesales)] of households with [who = test-hh]
17
1
11

MONITOR
1727
157
1784
202
Income
[round income-outcome-t1end] of households with [who = test-hh]
17
1
11

MONITOR
1622
157
1679
202
Income
[round income-expectation] of households with [who = test-hh]
17
1
11

MONITOR
1417
592
1489
637
Cattle total
count cattle
0
1
11

MONITOR
1612
862
1682
907
Cow sales
[round test-cowsales] of households with [who = test-hh]
0
1
11

MONITOR
1727
862
1794
907
Cow sales
[round oxensales] of households with [who = test-hh]
17
1
11

MONITOR
1817
107
1892
152
savings
[round(savings)] of households with [who = test-hh]
17
1
11

MONITOR
1807
157
1874
202
maizeland
[precision maizeland 2] of households with [who = test-hh]
17
1
11

MONITOR
1822
262
1879
307
Pasture
[precision privatepasture 2] of households with [who = test-hh]
17
1
11

MONITOR
1807
207
1899
252
maizepurchase
[round(maizepurchase)] of households with [who = test-hh]
17
1
11

MONITOR
1900
107
1975
152
coffeeland
[precision coffeeland 2] of households with [who = test-hh]
17
1
11

MONITOR
1882
157
1974
202
vegetableland
[precision vegetableland 2] of households with [who = test-hh]
17
1
11

MONITOR
1892
262
1954
307
Feedprod
[round feedproduction] of households with [who = test-hh]
17
1
11

MONITOR
1892
312
1954
357
Capacity
[floor (((feedproduction * 1000) / 365 )  / 3.15)] of households with [who = test-hh]
17
1
11

MONITOR
1802
17
2337
62
Options
[known-strategies] of households with [who = test-hh]
17
1
11

MONITOR
1842
427
1899
472
hh-size
[household-size] of households with [who = test-hh]
17
1
11

MONITOR
1847
487
1939
532
food threshold
[round food-threshold-t1end] of households with [who = test-hh]
0
1
11

MONITOR
1847
537
1937
582
maizeproduction
[round (maizeproduction / 56)] of households with [who = test-hh]
0
1
11

SWITCH
1020
50
1110
83
Income?
Income?
1
1
-1000

SWITCH
1020
100
1110
133
Food?
Food?
1
1
-1000

SWITCH
1018
155
1108
188
Leisure?
Leisure?
1
1
-1000

BUTTON
220
150
275
183
Hide hhs
hide-households
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
130
30
275
90
test-strategy
change to vegetable
1
0
String

MONITOR
445
10
552
55
Adopted strategy
count households with [member? test-strategy known-strategies]
0
1
11

MONITOR
310
10
407
55
Chose strategy
count households with [chosen-strategy = test-strategy]
0
1
11

CHOOSER
850
104
1002
149
Search-method
Search-method
"random draw" "sequential loop" "optimised selection"
0

MONITOR
1827
612
1884
657
veg-left
[round veg-left] of households with [who = test-hh]
0
1
11

MONITOR
1887
612
1944
657
vegprod
[round vegproduction] of households with [who = test-hh]
0
1
11

CHOOSER
849
164
987
209
Utility-calculation
Utility-calculation
"rank-sum" "weighted-sum" "cobb-douglas+" "cobb-douglas*" "cobb-douglasln"
1

SLIDER
160
815
270
848
Price-variability
Price-variability
0
100
1.0
1
1
NIL
HORIZONTAL

SLIDER
160
855
270
888
Yield-variability
Yield-variability
0
100
1.0
1
1
NIL
HORIZONTAL

SWITCH
160
775
267
808
Variability?
Variability?
0
1
-1000

SWITCH
20
775
137
808
Time-trend?
Time-trend?
0
1
-1000

TEXTBOX
155
720
280
765
Do yields and/or prices vary over time? Select the standard deviation in %
10
0.0
1

TEXTBOX
20
706
135
761
Do yields and/or prices increase or decrease over time? Give the mean effect in %
10
0.0
1

SLIDER
20
855
145
888
Yield-trend
Yield-trend
-30
40
0.0
1
1
NIL
HORIZONTAL

SLIDER
20
820
145
853
Price-trend
Price-trend
-100
100
0.0
1
1
NIL
HORIZONTAL

TEXTBOX
20
675
170
693
Temporal variability? 
14
0.0
1

SWITCH
155
670
275
703
Draw-random?
Draw-random?
0
1
-1000

SWITCH
23
232
148
265
Different-hhs?
Different-hhs?
0
1
-1000

TEXTBOX
30
901
180
941
Aspiration adaptation inputs
14
0.0
1

TEXTBOX
180
905
330
941
If the Draw-random? switch is on, b-no and optimism are also drawn randomly
10
0.0
1

INPUTBOX
395
955
475
1015
Population-nr
1.0
1
0
Number

INPUTBOX
395
1020
475
1080
Repetition-nr
1.0
1
0
Number

TEXTBOX
400
910
505
945
Behaviourspace inputs
14
0.0
1

SWITCH
155
234
264
267
Fix-chars?
Fix-chars?
0
1
-1000

TEXTBOX
850
25
1075
50
Alternative decision-making models
14
0.0
1

CHOOSER
850
225
988
270
Threshold-type
Threshold-type
"static" "dynamic" "infinite"
1

CHOOSER
850
285
990
330
Threshold-adjustment
Threshold-adjustment
"internal only" "internal and external" "external only"
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

house two story
false
0
Polygon -7500403 true true 2 180 227 180 152 150 32 150
Rectangle -7500403 true true 270 75 285 255
Rectangle -7500403 true true 75 135 270 255
Rectangle -16777216 true false 124 195 187 256
Rectangle -16777216 true false 210 195 255 240
Rectangle -16777216 true false 90 150 135 180
Rectangle -16777216 true false 210 150 255 180
Line -16777216 false 270 135 270 255
Rectangle -7500403 true true 15 180 75 255
Polygon -7500403 true true 60 135 285 135 240 90 105 90
Line -16777216 false 75 135 75 180
Rectangle -16777216 true false 30 195 93 240
Line -16777216 false 60 135 285 135
Line -16777216 false 255 105 285 135
Line -16777216 false 0 180 75 180
Line -7500403 true 60 195 60 240
Line -7500403 true 154 195 154 255

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="explore-feed-milk" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20"/>
    <metric>[feedproduction] of households</metric>
    <metric>[maizeproduction] of households</metric>
    <metric>[coffeeproduction] of households</metric>
    <metric>[feed-quantity] of cattle</metric>
    <metric>[chosen-strategy] of households</metric>
    <metric>[supplementary-feed] of cattle</metric>
    <metric>[milkyield] of cattle</metric>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hhsize-lambda">
      <value value="5.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-lambda">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maize-alpha">
      <value value="1.1123473"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-mu">
      <value value="5538"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-alpha">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="optimism">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maize-lambda">
      <value value="0.1128911"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>ask households with [count in-mycow-neighbors &gt; 0][show known-strategies show feedproduction show count in-mycow-neighbors show chosen-strategy]</metric>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hhsize-lambda">
      <value value="5.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-lambda">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maize-alpha">
      <value value="1.1123473"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income-mu">
      <value value="5538"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-alpha">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="optimism">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maize-lambda">
      <value value="0.1128911"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Aspiration b's and formulas" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="b-no" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="b-rate" first="0" step="2" last="0.1"/>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-lambda">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="b-log" first="0" step="0.0025" last="0.05"/>
    <enumeratedValueSet variable="cow-alpha">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aspiration-adaptation">
      <value value="&quot;no-experience&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="optimism">
      <value value="1.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hhsize-lambda">
      <value value="5.1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="b-lin" first="0" step="5.0E-7" last="1.0E-5"/>
  </experiment>
  <experiment name="Aspiration b's no experience" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20"/>
    <metric>[income-threshold-mean] of households</metric>
    <metric>[income-threshold-t1end] of households</metric>
    <metric>[income-threshold-t1start] of households</metric>
    <metric>[income-threshold-t2] of households</metric>
    <metric>[income-threshold-t3] of households</metric>
    <metric>[food-threshold-mean] of households</metric>
    <metric>[food-threshold-t1end] of households</metric>
    <metric>[food-threshold-t1start] of households</metric>
    <metric>[food-threshold-t2] of households</metric>
    <metric>[food-threshold-t3] of households</metric>
    <metric>[leisure-threshold-mean] of households</metric>
    <metric>[leisure-threshold-t1end] of households</metric>
    <metric>[leisure-threshold-t1start] of households</metric>
    <metric>[leisure-threshold-t2] of households</metric>
    <metric>[leisure-threshold-t3] of households</metric>
    <metric>[income-outcome-t3] of households</metric>
    <metric>[income-outcome-t2] of households</metric>
    <metric>[income-outcome-t1start] of households</metric>
    <metric>[income-outcome-t1end] of households</metric>
    <metric>[food-outcome-t3] of households</metric>
    <metric>[food-outcome-t2] of households</metric>
    <metric>[food-outcome-t1start] of households</metric>
    <metric>[food-outcome-t1end] of households</metric>
    <metric>[leisure-outcome-t3] of households</metric>
    <metric>[leisure-outcome-t2] of households</metric>
    <metric>[leisure-outcome-t1start] of households</metric>
    <metric>[leisure-outcome-t1end] of households</metric>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="b-no" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-lambda">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-log">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-alpha">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aspiration-adaptation">
      <value value="&quot;no-experience&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="optimism">
      <value value="1.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hhsize-lambda">
      <value value="5.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-lin">
      <value value="2.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Aspiration b's change rate bno0.2" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15"/>
    <metric>[income-threshold-mean] of households</metric>
    <metric>[income-threshold-t1end] of households</metric>
    <metric>[income-threshold-t1start] of households</metric>
    <metric>[income-threshold-t2] of households</metric>
    <metric>[income-threshold-t3] of households</metric>
    <metric>[food-threshold-mean] of households</metric>
    <metric>[food-threshold-t1end] of households</metric>
    <metric>[food-threshold-t1start] of households</metric>
    <metric>[food-threshold-t2] of households</metric>
    <metric>[food-threshold-t3] of households</metric>
    <metric>[leisure-threshold-mean] of households</metric>
    <metric>[leisure-threshold-t1end] of households</metric>
    <metric>[leisure-threshold-t1start] of households</metric>
    <metric>[leisure-threshold-t2] of households</metric>
    <metric>[leisure-threshold-t3] of households</metric>
    <metric>[income-outcome-t3] of households</metric>
    <metric>[income-outcome-t2] of households</metric>
    <metric>[income-outcome-t1start] of households</metric>
    <metric>[income-outcome-t1end] of households</metric>
    <metric>[food-outcome-t3] of households</metric>
    <metric>[food-outcome-t2] of households</metric>
    <metric>[food-outcome-t1start] of households</metric>
    <metric>[food-outcome-t1end] of households</metric>
    <metric>[leisure-outcome-t3] of households</metric>
    <metric>[leisure-outcome-t2] of households</metric>
    <metric>[leisure-outcome-t1start] of households</metric>
    <metric>[leisure-outcome-t1end] of households</metric>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-no">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="b-rate" first="0" step="0.1" last="2"/>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-lambda">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-log">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cow-alpha">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aspiration-adaptation">
      <value value="&quot;change-rate-experience&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="optimism">
      <value value="1.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hhsize-lambda">
      <value value="5.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-lin">
      <value value="2.0E-5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Aspiration b's change-rate" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="b-no" first="0.1" step="0.01" last="0.2"/>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="b-rate" first="0" step="0.01" last="0.1"/>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-log">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aspiration-adaptation">
      <value value="&quot;change-rate-experience&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="optimism" first="0.9" step="0.05" last="1.1"/>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-lin">
      <value value="2.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-aspiration-factor">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Aspiration b's no-exp calibrate" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="b-no" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-log">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aspiration-adaptation">
      <value value="&quot;no-experience&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="optimism" first="0.8" step="0.05" last="1.5"/>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-lin">
      <value value="2.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Aspiration b's no-exp calibrate 2" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-no">
      <value value="0.125"/>
      <value value="0.175"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-log">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aspiration-adaptation">
      <value value="&quot;no-experience&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="optimism" first="0.8" step="0.1" last="1.5"/>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-lin">
      <value value="2.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="rndsatopt-all asp dim combis" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-strategy">
      <value value="&quot;change to vegetable&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-no">
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-rate">
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-hh">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-log">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aspiration-adaptation">
      <value value="&quot;no-experience&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Make-decisions">
      <value value="&quot;random draw&quot;"/>
      <value value="&quot;satisficing&quot;"/>
      <value value="&quot;optimising&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
      <value value="&quot;cobb-douglas+&quot;"/>
      <value value="&quot;cobb-douglas*&quot;"/>
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="optimism">
      <value value="1.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-lin">
      <value value="2.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="rndsatopt-uprod dim combis" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-strategy">
      <value value="&quot;change to vegetable&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-no">
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-rate">
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-hh">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-log">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aspiration-adaptation">
      <value value="&quot;no-experience&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Make-decisions">
      <value value="&quot;random draw&quot;"/>
      <value value="&quot;satisficing&quot;"/>
      <value value="&quot;optimising&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="optimism">
      <value value="1.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-lin">
      <value value="2.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="dims,dec,util,var" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-strategy">
      <value value="&quot;change to vegetable&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-no">
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-rate">
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-hh">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-log">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aspiration-adaptation">
      <value value="&quot;no-experience&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Make-decisions">
      <value value="&quot;random draw&quot;"/>
      <value value="&quot;satisficing&quot;"/>
      <value value="&quot;optimising&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
      <value value="&quot;cobb-douglas+&quot;"/>
      <value value="&quot;cobb-douglas*&quot;"/>
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="optimism">
      <value value="1.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-lin">
      <value value="2.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-variability">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yield-variability">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="dims,dec,util,var no reps" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-strategy">
      <value value="&quot;change to vegetable&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-no">
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-maize">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-rate">
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-pasture">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-coffee">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="test-hh">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-veg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-log">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aspiration-adaptation">
      <value value="&quot;no-experience&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Make-decisions">
      <value value="&quot;random draw&quot;"/>
      <value value="&quot;satisficing&quot;"/>
      <value value="&quot;optimising&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
      <value value="&quot;cobb-douglas+&quot;"/>
      <value value="&quot;cobb-douglas*&quot;"/>
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="optimism">
      <value value="1.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b-lin">
      <value value="2.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-variability">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yield-variability">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SA dims,dec,util,b,opt" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Aspiration-adaptation">
      <value value="&quot;no-experience&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t1">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Make-decisions">
      <value value="&quot;random draw&quot;"/>
      <value value="&quot;satisficing&quot;"/>
      <value value="&quot;optimising&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
      <value value="&quot;cobb-douglas+&quot;"/>
      <value value="&quot;cobb-douglas*&quot;"/>
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t2">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-time">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="remember-t3">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-of-households">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
  </experiment>
  <experiment name="*SA random dynamic" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;random draw&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Internal-external">
      <value value="&quot;internal only&quot;"/>
      <value value="&quot;external only&quot;"/>
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="*SA random static" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;random draw&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;static&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Internal-external">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SA satisfice dynamic" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Make-decisions">
      <value value="&quot;satisficing&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Internal-external">
      <value value="&quot;internal only&quot;"/>
      <value value="&quot;internal and external&quot;"/>
      <value value="&quot;external only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
      <value value="&quot;cobb-douglas+&quot;"/>
      <value value="&quot;cobb-douglas*&quot;"/>
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="*SA satisfice static" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;static&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Internal-external">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
      <value value="&quot;rank-sum&quot;"/>
      <value value="&quot;cobb-douglas+&quot;"/>
      <value value="&quot;cobb-douglas*&quot;"/>
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="*SA optimise dynamic internal" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Internal-external">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
      <value value="&quot;rank-sum&quot;"/>
      <value value="&quot;cobb-douglas+&quot;"/>
      <value value="&quot;cobb-douglas*&quot;"/>
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="*SA optimise static" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;static&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Internal-external">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
      <value value="&quot;rank-sum&quot;"/>
      <value value="&quot;cobb-douglas+&quot;"/>
      <value value="&quot;cobb-douglas*&quot;"/>
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="*SA satisfice dynamic both" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Internal-external">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
      <value value="&quot;rank-sum&quot;"/>
      <value value="&quot;cobb-douglas+&quot;"/>
      <value value="&quot;cobb-douglas*&quot;"/>
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SA optimise infinite" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Make-decisions">
      <value value="&quot;optimising&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;infinite&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Internal-external">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
      <value value="&quot;CB+&quot;"/>
      <value value="&quot;CB*&quot;"/>
      <value value="&quot;CBln&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SA random infinite" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Make-decisions">
      <value value="&quot;random draw&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;infinite&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Internal-external">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="*SA satisfice dynamic external" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Internal-external">
      <value value="&quot;external only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
      <value value="&quot;rank-sum&quot;"/>
      <value value="&quot;cobb-douglas+&quot;"/>
      <value value="&quot;cobb-douglas*&quot;"/>
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="*SA optimise dynamic external" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Internal-external">
      <value value="&quot;external only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
      <value value="&quot;rank-sum&quot;"/>
      <value value="&quot;cobb-douglas+&quot;"/>
      <value value="&quot;cobb-douglas*&quot;"/>
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="*SA optimise dynamic both" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Internal-external">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
      <value value="&quot;rank-sum&quot;"/>
      <value value="&quot;cobb-douglas+&quot;"/>
      <value value="&quot;cobb-douglas*&quot;"/>
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="*SA satisfice dynamic internal" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Internal-external">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
      <value value="&quot;rank-sum&quot;"/>
      <value value="&quot;cobb-douglas+&quot;"/>
      <value value="&quot;cobb-douglas*&quot;"/>
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice static WS" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;static&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice static RS" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;static&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;rank-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice static CD+" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;static&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglas+&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice static CD*" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;static&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglas*&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice static CDlog" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;static&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice dynamic internal WS" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice dynamic internal RS" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;rank-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice dynamic internal CD+" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglas+&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice dynamic internal CD*" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglas*&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice dynamic internal CDlog" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice dynamic external WS" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;external only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice dynamic external RS" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;external only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;rank-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice dynamic external CD+" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;external only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglas+&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice dynamic external CD*" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;external only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglas*&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice dynamic external CDlog" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;external only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice dynamic both WS" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice dynamic both RS" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;rank-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice dynamic both CD+" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglas+&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice dynamic both CD*" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglas*&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA satisfice dynamic both CDlog" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise static WS" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;static&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise static RS" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;static&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;rank-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise static CD+" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;static&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglas+&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise static CD*" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;static&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglas*&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise static CDlog" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;static&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise dynamic internal WS" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise dynamic internal RS" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;rank-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise dynamic internal CD+" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglas+&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise dynamic internal CD*" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglas*&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise dynamic internal CDlog" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise dynamic external WS" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;external only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise dynamic external RS" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;external only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;rank-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise dynamic external CD+" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;external only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglas+&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise dynamic external CD*" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;external only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglas*&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise dynamic external CDlog" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;external only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise dynamic both WS" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise dynamic both RS" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;rank-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise dynamic both CD+" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglas+&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise dynamic both CD*" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglas*&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA optimise dynamic both CDlog" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;optimised selection&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;cobb-douglasln&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA random dynamic internal" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;random draw&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA random dynamic external" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;random draw&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;external only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="**SA random dynamic both" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;random draw&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="***SA satisfice dynamic both WS" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="***SA satisfice dynamic both WS def TFF" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="***SA satisfice dynamic both WS def TTF" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="***SA satisfice dynamic both WS def TFT" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="***SA satisfice dynamic both WS def TTT" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="***SA satisfice dynamic both WS def FTT" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="***SA satisfice dynamic both WS def FFT" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="***SA satisfice dynamic both WS def FTF" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="***SA satisfice dynamic both WS def FFF" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;defined-by-similarity&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="***SA satisfice dynamic both WS ran TFF" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="***SA satisfice dynamic both WS ran TTF" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="***SA satisfice dynamic both WS ran TFT" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="***SA satisfice dynamic both WS ran TTT" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="***SA satisfice dynamic both WS ran FTT" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="***SA satisfice dynamic both WS ran FFT" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="***SA satisfice dynamic both WS ran FTF" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="***SA satisfice dynamic both WS ran FFF" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="24"/>
    <enumeratedValueSet variable="Search-method">
      <value value="&quot;sequential loop&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Income?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Leisure?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy-order">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-type">
      <value value="&quot;dynamic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Threshold-adjustment">
      <value value="&quot;internal and external&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Utility-calculation">
      <value value="&quot;weighted-sum&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Population-nr" first="1" step="1" last="10"/>
    <steppedValueSet variable="Repetition-nr" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="fix-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-file?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Variability?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Draw-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Fix-chars?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
