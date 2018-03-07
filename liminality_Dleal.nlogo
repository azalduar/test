extensions
[
 nw
]

globals
[
 CAPall               ;; list with eacg ego's capacity for spreading
 count-agent          ;; count the number of agents that have been chosen as the seed innovator. (max count-agent = N)
 ego                  ;; the current seed innovator
 N                    ;; Number of agents
 T                    ;; Number of ties
 density              ;; Network density
 isolates
 mean-degree
 avg-path-length
 local-clustering     ;; local clustering coeff
 GSI                  ;; ln (gross friendship segreggation index)
 E-I                  ;; E-I segregation index

                      ;; in-group capacity for spreading variables
 in-CAPall_race
 in-CAPall_sex
 in-CAPall_grade
 in-CAPall_age
 in-CAPall_ses
 in-CAPall_racei
 in-CAPall_latino
 in-CAPall_white
 in-CAPall_black
 in-CAPall_native
 in-CAPall_asian
 in-CAPall_other
 in-CAPall_multi
                     ;; out-group capacity for spreading variables
 out-CAPall_race
 out-CAPall_sex
 out-CAPall_grade
 out-CAPall_age
 out-CAPall_ses
 out-CAPall_racei
 out-CAPall_latino
 out-CAPall_white
 out-CAPall_black
 out-CAPall_native
 out-CAPall_asian
 out-CAPall_other
 out-CAPall_multi
                    ;; capacity for spreading of agents with highest scores in relevant measures
 Y-CAPall_random
 Y-CAPall_best
 Y-CAPall_betw
 Y-CAPall_deg
 Y-CAPall_clos
 Y-CAPall_eigen
 Y-CAPall_clust
 Y-CAPall_rhet
 Y-CAPall_rhetdeg
 Y-CAPall_rbroker
                    ;; imprecision function
 IM-betw
 IM-rand
 IM-deg
 IM-clos
 IM-eigen
 IM-clust
 IM-rhet
 IM-rhetdeg
 IM-rbroker
]

turtles-own
[

 Ai              ;; Ai = 1 = adopter; Ai = 0 = non-adopter
 CAPi            ;; capacity for spreading of agent i
 Mi_raw          ;; agent i's raw capacity for spreading
 Ti              ;; adoption threshold of agent i

                 ;; agents' variables as reported in Add Health:
 my-orig-id
 my-grade
 my-race
 my-sex
 my-betw
 my-clos
 my-eigen
 my-degr
 my-clust
 my-bmi
 my-white
 my-black
 my-native
 my-asian
 my-other
 my-selfesteem
 my-usborn
 my-accepted
 my-income
 my-latino
 my-multir
 my-racei
 my-health
 my-age
 my-class
 my-bmi-status
 my-rhet
 my-rhet-deg
 my-traits      ;; list fo all relevant variables of agent i
 my-rbrokerage  ;; racial brokerage score of agent i
]

to go
 reset-variables
 generate-thresholds
 social-diffusion
 report-all-centrality-scores
 compute-imprecision-function
 tick
end



to generate-thresholds

 ask turtles
 [
  set Ti random-normal normal-mean normal-sd  ;; make threshols follow a normal distribution with mean = normal-mean and sd = normal-sd

  if Ti < normal-mean - (3 * normal-sd)       ;; make sure there are no Ti values that are more than  3 SDs beyond the mean
  [set Ti (normal-mean - (3 * normal-sd))]

  if Ti <= 0                                  ;; Ti cannot be less than 0 (0% of neighbors)
  [set Ti 0]

  if  Ti > normal-mean + (3 * normal-sd)      ;; make sure there are no Ti values that are less than  3 SDs below the mean
  [set Ti (normal-mean + (3 * normal-sd))]

  if Ti >= 1                                  ;; Ti cannot be more than 1 (100% of neighbors)
  [set Ti 1]
 ]
end



to social-diffusion

 set ego 0
 while [ego < N]                       ;; loop through all agents
 [
  ask turtles
  [
   set Ai 0                            ;; initially, make all agent non-adopters
   set shape "circle 2"
   set color cyan
  ]

  ask turtle ego                       ;; make agent i the seed innovator/adopter
  [
   set Ai 1
   set shape "circle"
   set color yellow

   ifelse individual-contagion?                                                  ;; if individual-contagion? is true:
   [
    repeat Reps                                                                  ;; repeat Reps number of times
    [
     ask other turtles                                                           ;; ask agents != from the seed innovator:
     [
      if ((count link-neighbors with [Ai = 1]) / count link-neighbors) >= Ti     ;; if the proportion of neighbors that have adopted is >= Ti
      [
       set Ai 1                                                                  ;; adopt the innovation too
       set shape "circle"
      ]
     ]
    ]
   ]
                                                                                 ;; if individual-contagion? is false:
   [
    ask link-neighbors                                                           ;; ask agent i (i.e. the seed innovator) to make its neighbors adopters too
    [
     set Ai 1
     set shape "circle"
     set color yellow
    ]

    repeat Reps                                                                  ;; repeat Reps number of times
    [
     ask other turtles with [color = cyan]                                       ;; ask agents != outside the seed neighborhood
     [
      if ((count link-neighbors with [Ai = 1]) / count link-neighbors) >= Ti     ;; if the proportion of neighbors that have adopted is >= Ti
      [
       set Ai 1                                                                  ;; adopt the innovation too
       set shape "circle"
      ]
     ]
    ]
   ]

   evaluate-my-diffusion-potential                                               ;; ask agent i to execute this procedure

   set count-agent count-agent + 1
   set ego ego + 1
  ]
 ]
end


to evaluate-my-diffusion-potential

 ;;;;;;;;;;;;;;;;;;;;;
 ; OVERALL DIFFUSION ;
 ;;;;;;;;;;;;;;;;;;;;;

 set Mi_raw (count other turtles with [Ai = 1 and color = cyan]) ;; retrieve the number of agents infected by agent i. Call this quantity Mi_raw
 set CAPi   (Mi_raw / count other turtles with [color = cyan])   ;; calculate the proportion of the population infected by agent i (Mi_raw/n). Call this quantity the spreading capacity of agent i (CAPi).
 set CAPall lput (CAPi) CAPall                                   ;; create a list with all CAPi

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; BOUNDARY-SPECIFIC DIFFUSION: IN-GROUP ;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 let in-CAPi_race   (((count other turtles with [Ai = 1 and color = cyan and my-race = [my-race] of turtle ego]) / count other turtles with [my-race = [my-race] of turtle ego and color = cyan]))          ;; agent i's proportion of same-race adopters
 let in-CAPi_sex    (((count other turtles with [Ai = 1 and color = cyan and my-sex = [my-sex] of turtle ego]) / count other turtles with [my-sex = [my-sex] of turtle ego and color = cyan]))              ;; agent i's proportion of same-sex adopters
 let in-CAPi_grade  (((count other turtles with [Ai = 1 and color = cyan and my-grade = [my-grade] of turtle ego]) / count other turtles with [my-grade = [my-grade] of turtle ego and color = cyan]))      ;; agent i's proportion of same-grade adopters
 let in-CAPi_age    (((count other turtles with [Ai = 1 and color = cyan and my-age = [my-age] of turtle ego]) / count other turtles with [my-age = [my-age] of turtle ego and color = cyan]))              ;; agent i's proportion of same-age adopters
 let in-CAPi_ses    (((count other turtles with [Ai = 1 and color = cyan and my-class = [my-class] of turtle ego]) / count other turtles with [my-class = [my-class] of turtle ego and color = cyan]))      ;; agent i's proportion of same-ses adopters
 let in-CAPi_racei  (((count other turtles with [Ai = 1 and color = cyan and my-racei = [my-racei] of turtle ego]) / count other turtles with [my-racei = [my-racei] of turtle ego and color = cyan]))      ;; agent i's proportion of same-raceinterv adopters
 let in-CAPi_latino (((count other turtles with [Ai = 1 and color = cyan and my-latino = [my-latino] of turtle ego]) / count other turtles with [my-latino = [my-latino] of turtle ego and color = cyan]))  ;; agent i's proportion of same-ethnicity adopters

 set in-CAPall_race   lput (in-CAPi_race)   in-CAPall_race   ;; create a list with all in-CAPall_race
 set in-CAPall_sex    lput (in-CAPi_sex)    in-CAPall_sex    ;; create a list with all in-CAPall_sex
 set in-CAPall_grade  lput (in-CAPi_grade)  in-CAPall_grade  ;; create a list with all in-CAPall_grade
 set in-CAPall_age    lput (in-CAPi_age)    in-CAPall_age    ;; create a list with all in-CAPall_age
 set in-CAPall_ses    lput (in-CAPi_ses)    in-CAPall_ses    ;; create a list with all in-CAPall_ses
 set in-CAPall_racei  lput (in-CAPi_racei)  in-CAPall_racei  ;; create a list with all in-CAPall_racei
 set in-CAPall_latino lput (in-CAPi_latino) in-CAPall_latino ;; create a list with all in-CAPall_latino

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; BOUNDARY-SPECIFIC DIFFUSION: OUT-GROUP ;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 let out-CAPi_race   (((count other turtles with [Ai = 1 and color = cyan and my-race != [my-race] of turtle ego]) / count other turtles with [my-race != [my-race] of turtle ego and color = cyan]))         ;; agent i's proportion of diff-race adopters
 let out-CAPi_sex    (((count other turtles with [Ai = 1 and color = cyan and my-sex != [my-sex] of turtle ego]) / count other turtles with [my-sex != [my-sex] of turtle ego and color = cyan]))             ;; agent i's proportion of diff-sex adopters
 let out-CAPi_grade  (((count other turtles with [Ai = 1 and color = cyan and my-grade != [my-grade] of turtle ego]) / count other turtles with [my-grade != [my-grade] of turtle ego and color = cyan]))     ;; agent i's proportion of diff-grade adopters
 let out-CAPi_age    (((count other turtles with [Ai = 1 and color = cyan and my-age != [my-age] of turtle ego]) / count other turtles with [my-age != [my-age] of turtle ego and color = cyan]))             ;; agent i's proportion of diff-age adopters
 let out-CAPi_ses    (((count other turtles with [Ai = 1 and color = cyan and my-class != [my-class] of turtle ego]) / count other turtles with [my-class != [my-class] of turtle ego and color = cyan]))     ;; agent i's proportion of diff-ses adopters
 let out-CAPi_racei  (((count other turtles with [Ai = 1 and color = cyan and my-racei != [my-racei] of turtle ego]) / count other turtles with [my-racei != [my-racei] of turtle ego and color = cyan]))     ;; agent i's proportion of diff-raceinterv adopters
 let out-CAPi_latino (((count other turtles with [Ai = 1 and color = cyan and my-latino != [my-latino] of turtle ego]) / count other turtles with [my-latino != [my-latino] of turtle ego and color = cyan])) ;; agent i's proportion of diff-ethnicity adopters

 set out-CAPall_race   lput (out-CAPi_race)   out-CAPall_race   ;; create a list with all out-CAPall_race
 set out-CAPall_sex    lput (out-CAPi_sex)    out-CAPall_sex    ;; create a list with all out-CAPall_race
 set out-CAPall_grade  lput (out-CAPi_grade)  out-CAPall_grade  ;; create a list with all out-CAPall_race
 set out-CAPall_age    lput (out-CAPi_age)    out-CAPall_age    ;; create a list with all out-CAPall_race
 set out-CAPall_ses    lput (out-CAPi_ses)    out-CAPall_ses    ;; create a list with all out-CAPall_race
 set out-CAPall_racei  lput (out-CAPi_racei)  out-CAPall_racei  ;; create a list with all out-CAPall_race
 set out-CAPall_latino lput (out-CAPi_latino) out-CAPall_latino ;; create a list with all out-CAPall_race

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; RACE-SPECIFIC DIFFUSION ;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;

 let ego-race [my-race] of turtle ego

 let in-CAPi_withinrace  (((count other turtles with [Ai = 1 and color = cyan and my-race = ego-race])  / count other turtles with [color = cyan and my-race = ego-race]))   ;; proportion of same-race adopters within ego's race
 let in-CAPi_outsiderace (((count other turtles with [Ai = 1 and color = cyan and my-race != ego-race]) / count other turtles with [color = cyan and my-race != ego-race]))  ;; proportion of diff-race non-adopters outsise ego's race

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; RACE-SPECIFIC DIFFUSION: WITHIN RACE ;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 if ego-race = 1
 [set in-CAPall_white  lput (in-CAPi_withinrace) in-CAPall_white]  ;; create a list of all white egos' capacity to diffuse within their whites alters
 if ego-race = 2
 [set in-CAPall_black  lput (in-CAPi_withinrace) in-CAPall_black]  ;; create a list of all blacks egos' capacity to diffuse within their blacks alters
 if ego-race = 3
 [set in-CAPall_native lput (in-CAPi_withinrace) in-CAPall_native] ;; create a list of all native egos' capacity to diffuse within their native alters
 if ego-race = 4
 [set in-CAPall_asian  lput (in-CAPi_withinrace) in-CAPall_asian]  ;; create a list of all asian egos' capacity to diffuse within their asian alters
 if ego-race = 5
 [set in-CAPall_other  lput (in-CAPi_withinrace) in-CAPall_other]  ;; create a list of all other egos' capacity to diffuse within their other alters
 if ego-race = 6
 [set in-CAPall_multi  lput (in-CAPi_withinrace) in-CAPall_multi]  ;; create a list of all muliracial egos' capacity to diffuse within their multiracial alters

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; RACE-SPECIFIC DIFFUSION: OUTSIDE RACE ;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 if ego-race = 1
 [set out-CAPall_white  lput (in-CAPi_outsiderace) out-CAPall_white]  ;; create a list of all white egos' capacity to diffuse outside to non-whites alters
 if ego-race = 2
 [set out-CAPall_black  lput (in-CAPi_outsiderace) out-CAPall_black]  ;; create a list of all blacks egos' capacity to diffuse outside to non-black alters
 if ego-race = 3
 [set out-CAPall_native lput (in-CAPi_outsiderace) out-CAPall_native] ;; create a list of all native egos' capacity to diffuse outside to non-native alters
 if ego-race = 4
 [set out-CAPall_asian  lput (in-CAPi_outsiderace) out-CAPall_asian]  ;; create a list of all asian egos' capacity to diffuse outsise to non-asian alters
 if ego-race = 5
 [set out-CAPall_other  lput (in-CAPi_outsiderace) out-CAPall_other]  ;; create a list of all other egos' capacity to diffuse outside to non-other alters
 if ego-race = 6
 [set out-CAPall_multi  lput (in-CAPi_outsiderace) out-CAPall_multi]  ;; create a list of all multiracial egos' capacity to diffuse outside to non-multiracial alters
end


to report-all-centrality-scores

 ask turtles                      ;; each agent calculates relevant centrality scores (e.g. betweenness centrality)
 [
  let local-race my-race
  set my-betw     (nw:betweenness-centrality)
  set my-eigen    (nw:eigenvector-centrality)
  set my-clos     (nw:closeness-centrality)
  set my-degr     (count link-neighbors)
  set my-clust    (nw:clustering-coefficient)
  set my-rhet     ((count link-neighbors with [my-race != local-race]) / count link-neighbors)
  set my-rhet-deg (((count link-neighbors with [my-race != local-race]) / count link-neighbors)) * count link-neighbors
 ]
end


to compute-imprecision-function

  let #-target-values round ((length CAPall) * intervention-size)  ;; set #-target-values as the number of items (i.e. CAP scores) that will be included in a given Y-CAPall set,

  set Y-CAPall_random n-of #-target-values CAPall                  ;; make a list with X (i.e. #-target-agents) random CAPall scores

  set Y-CAPall_best sort CAPall                                    ;; make a list with the X hightest values in CAPall
  set Y-CAPall_best reverse Y-CAPall_best
  set Y-CAPall_best sublist Y-CAPall_best 0 #-target-values


  foreach sort-on [(- my-betw)] turtles                            ;; make a list with the CAPi values of the agents with X hightest betweenness scores
  [?1 ->
   ask ?1
   [set Y-CAPall_betw lput ([CAPi] of self) Y-CAPall_betw]
  ]
  set Y-CAPall_betw  sublist Y-CAPall_betw 0 #-target-values


  foreach sort-on [(- my-degr)] turtles                            ;; make a list with the CAPi values of the agents with X hightest degree
  [ ?1 ->
   ask ?1
   [set Y-CAPall_deg lput ([CAPi] of self) Y-CAPall_deg]
  ]
  set Y-CAPall_deg     sublist Y-CAPall_deg 0 #-target-values

  foreach sort-on [(- my-clos)] turtles                            ;; make a list with the CAPi values of the agents with X hightest closeness scores
  [ ?1 ->
   ask ?1
   [set Y-CAPall_clos lput ([CAPi] of self) Y-CAPall_clos]
  ]
  set Y-CAPall_clos    sublist Y-CAPall_clos 0 #-target-values

  foreach sort-on [(- my-eigen)] turtles                           ;; make a list with the CAPi values of the agents with X hightest eigenvector centrality scores
  [ ?1 ->
   ask ?1
   [set Y-CAPall_eigen lput ([CAPi] of self) Y-CAPall_eigen]
  ]
  set Y-CAPall_eigen   sublist Y-CAPall_eigen 0 #-target-values


  foreach sort-on [(- my-clust)] turtles                           ;; make a list with the CAPi values of the agents with X hightest local clustering scores
  [ ?1 ->
   ask ?1
   [set Y-CAPall_clust lput ([CAPi] of self) Y-CAPall_clust]
  ]
  set Y-CAPall_clust   sublist Y-CAPall_clust 0 #-target-values

  foreach sort-on [(- my-rhet)] turtles                            ;; make a list with the CAPi values of the agents with the X most racially diverse egonetworks
  [ ?1 ->
   ask ?1
   [set Y-CAPall_rhet lput ([CAPi] of self) Y-CAPall_rhet]
  ]
  set Y-CAPall_rhet    sublist Y-CAPall_rhet 0 #-target-values


  foreach sort-on [(- my-rhet-deg)] turtles                        ;; make a list with the CAPi values of the agents with the X most racially diverse egonetworks * degree
  [ ?1 ->
   ask ?1
   [set Y-CAPall_rhetdeg lput ([CAPi] of self) Y-CAPall_rhetdeg]
  ]
  set Y-CAPall_rhetdeg sublist Y-CAPall_rhetdeg 0 #-target-values


  foreach sort-on [(- my-rbrokerage)] turtles                      ;; make a list with the CAPi values of the agents with X highest racial brokerage scores
  [ ?1 ->
   ask ?1
   [set Y-CAPall_rbroker lput ([CAPi] of self) Y-CAPall_rbroker]
  ]
  set Y-CAPall_rbroker sublist Y-CAPall_rbroker 0 #-target-values

                                                                        ;; compute the imprecision (IM) functions for each centrality score

  set IM-rand    (1 - ((mean Y-CAPall_random) / (mean Y-CAPall_best)))
  set IM-betw    (1 - ((mean Y-CAPall_betw) / (mean Y-CAPall_best)))
  set IM-deg     (1 - ((mean Y-CAPall_deg) / (mean Y-CAPall_best)))
  set IM-clos    (1 - ((mean Y-CAPall_clos) / (mean Y-CAPall_best)))
  set IM-eigen   (1 - ((mean Y-CAPall_eigen) / (mean Y-CAPall_best)))
  set IM-clust   (1 - ((mean Y-CAPall_clust) / (mean Y-CAPall_best)))
  set IM-rhet    (1 - ((mean Y-CAPall_rhet) / (mean Y-CAPall_best)))
  set IM-rhetdeg (1 - ((mean Y-CAPall_rhetdeg) / (mean Y-CAPall_best)))
  set IM-rbroker (1 - ((mean Y-CAPall_rbroker) / (mean Y-CAPall_best)))
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CODE TO SETUP MODEL, NOT RELEVENAT FOR THE ABM ITSELF  ;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to import-data

 if school = "sunshine"[file-open "sunshine_node_attributes_data_to_NetLogo.txt"]
 if school = "rnd-sunshine"[file-open "sunshine_node_attributes_data_to_NetLogo.txt"]

 while [not file-at-end?]
 [
  let agent-features read-from-string (word"[" file-read-line "]")
  create-ordered-turtles 1
  [
   set my-traits []
   set my-orig-id item 0 agent-features
   set my-bmi item 1 agent-features
   set my-sex item 2 agent-features
   set my-grade item 3 agent-features
   set my-age item 4 agent-features
   set my-income item 5 agent-features
   set my-latino item 6 agent-features
   set my-white item 7 agent-features
   set my-black item 8 agent-features
   set my-native item 9 agent-features
   set my-asian item 10 agent-features
   set my-other item 11 agent-features
   set my-multir item 12 agent-features
   set my-racei item 13 agent-features
   set my-usborn item 14 agent-features
   set my-health item 15 agent-features
   set my-selfesteem item 16 agent-features
   set my-accepted item 17 agent-features
   set my-race item 18 agent-features
   set my-class item 19 agent-features
   set my-bmi-status item 20 agent-features
   set my-rbrokerage item 23 agent-features
                                                          ;; put all the traits in one list
   set my-traits lput (item 1 agent-features) my-traits
   set my-traits lput (item 2 agent-features) my-traits
   set my-traits lput (item 3 agent-features) my-traits
   set my-traits lput (item 4 agent-features) my-traits
   set my-traits lput (item 5 agent-features) my-traits
   set my-traits lput (item 6 agent-features) my-traits
   set my-traits lput (item 14 agent-features) my-traits
   set my-traits lput (item 15 agent-features) my-traits
   set my-traits lput (item 16 agent-features) my-traits
   set my-traits lput (item 17 agent-features) my-traits
   set my-traits lput (item 18 agent-features) my-traits
   set my-traits lput (item 19 agent-features) my-traits
   set my-traits lput (item 20 agent-features) my-traits
   set my-traits lput (item 23 agent-features) my-traits

   set color cyan
   set size 2
  ]
 ]
 file-close


 if school = "sunshine"[file-open "sunshine_network_data_to_NetLogo.txt"]
 if school = "rnd-sunshine"[file-open "random_sunshine_network_data_to_NetLogo.txt"]
 while [not file-at-end?]
 [
  let edges read-from-string (word"[" file-read-line "]")
  let ego-ids item 0 edges
  let alters-ids item 1 edges
  ask turtle item 0 edges
  [
   create-link-with turtle item 1 edges
  ]
 ]
 file-close

 ask turtles with [count link-neighbors = 0] [die]
end

to c-net-stats

 set N count turtles
 set T count links
 set isolates count turtles with [count link-neighbors = 0]
 set mean-degree (sum [count my-links] of turtles) / N
 ;set local-clustering mean [nw:clustering-coefficient] of turtles
 ;set avg-path-length nw:mean-path-length
 set density T / ((N * (N - 1)) / 2)
end


to export-model

  export-world "model-data.csv"
end

to do-plots
 ifelse do-plots?
 [


  update-plots
  repeat 150
  [
   layout-spring turtles links 0.2 1 1.5
  ]
 ]
 [
  no-display
 ]
end


to reset-variables

 set CAPall []
 set count-agent 0
 set ego 9999

 set IM-rand    9999
 set IM-betw    9999
 set IM-deg     9999
 set IM-clos    9999
 set IM-eigen   9999
 set IM-clust   9999
 set IM-rhet    9999
 set IM-rhetdeg 9999
 set IM-rbroker 9999

 set in-CAPall_race   []
 set in-CAPall_sex    []
 set in-CAPall_grade  []
 set in-CAPall_age    []
 set in-CAPall_ses    []
 set in-CAPall_racei  []
 set in-CAPall_latino []

 set out-CAPall_race   []
 set out-CAPall_sex    []
 set out-CAPall_grade  []
 set out-CAPall_age    []
 set out-CAPall_ses    []
 set out-CAPall_racei  []
 set out-CAPall_latino []

 set in-CAPall_white  []
 set in-CAPall_black  []
 set in-CAPall_native []
 set in-CAPall_asian  []
 set in-CAPall_other  []
 set in-CAPall_multi  []

 set out-CAPall_white  []
 set out-CAPall_black  []
 set out-CAPall_native []
 set out-CAPall_asian  []
 set out-CAPall_other  []
 set out-CAPall_multi  []

 set Y-CAPall_best    []
 set Y-CAPall_random  []
 set Y-CAPall_betw    []
 set Y-CAPall_deg     []
 set Y-CAPall_clos    []
 set Y-CAPall_eigen   []
 set Y-CAPall_clust   []
 set Y-CAPall_rhet    []
 set Y-CAPall_rhetdeg []
 set Y-CAPall_rbroker []

 ask turtles
 [set Ti 9999]
end

to segregation-measures ;; Moody's (2001) gross friendship segregation and Krackhardt and Stern's (1988) E-I index

 let AA 0
 let BB 0
 let CC 0
 let DD 0


 let i 0                                     ;; declare the variable i as a local variable
 let j 0                                     ;; declare the variable j as a local variable
 let ego1 0                                  ;; declare the variable ego as a local variable
 let alter 0                                 ;; declare the variable alter as a local variable
 while [i < count turtles]                   ;; while i is less than group-size.
 [
  set j 0                                    ;; set the local variable j to 0
  while [j < count turtles]                  ;; while j is less than group-size, these nested while loops allow each agent (ego) to compare to all other agents (alter)
  [
   set ego1 [who] of turtle i                ;; the variable i represents ego's ID number. Ego's ID number starts at 0
   set alter [who] of turtle j               ;; the variable j represents alter's ID number. Alter's ID number starts at 0
   if ego1 != alter and j > i
   [
    ask turtle ego1
    [
     if ([my-race] of turtle alter = [my-race] of turtle ego1) and (link-neighbor? turtle alter)
     [set AA AA + 1]
     if ([my-race] of turtle alter != [my-race] of turtle ego1) and (link-neighbor? turtle alter)
     [set BB BB + 1]
     if ([my-race] of turtle alter = [my-race] of turtle ego1) and (not link-neighbor? turtle alter)
     [set CC CC + 1]
     if ([my-race] of turtle alter != [my-race] of turtle ego1) and (not link-neighbor? turtle alter)
     [set DD DD + 1]
    ]
   ]
   set j j + 1                               ;; go to the next alter
  ]
  set i i + 1                                ;; go to the next ego
 ]

 set E-I -1 * ((BB - AA) / count links)      ;; compute E-I index (see Bojanowski & Corten 2014)

 ifelse BB != 0 and CC != 0                  ;; compute GSI index [see Bojanowski & Corten 2014)
 [
  set GSI ln ((AA * DD) / (BB * CC))
 ]
 [
  set GSI "infinity"
 ]

;; "Alpha is substantively
;; interpretable as the odds ratio of a friendship between members
;; of a same-race dyad relative to friendship in a cross-race dyad. When
;; alpha = 1, then the odds of a same-race friendship equal the odds of a crossrace
;; friendship, and the setting is perfectly integrated. As alpha increases, the
;; relative odds of a same-race friendship increase by a factor of alpha. Since a
;; is scaled from 0 to indfinity, I use ln (a), which ranges from - infinity to infinity" Moody (2001: 692)



;; E-I index (Krackhardt and Stern 1988). We mutiply the original measure by -1 so that this measure takes the value -1 if all ties
;; in the network are between groups, and + 1 if all ties are within groups.
 end



;; Liminality and the Difussion of Innovations among Adolescents
;; Code for the dissertation: Three Essays on Network Dynamics and Liminality
;; Code Written by Diego F. Leal, University of Massachusetts (www.diegoleal.info)
;; Last updated: August 2017
@#$#@#$#@
GRAPHICS-WINDOW
11
66
316
372
-1
-1
3.062
1
10
1
1
1
0
0
0
1
-48
48
-48
48
0
0
1
ticks
30.0

CHOOSER
329
247
474
292
school
school
"rnd-sunshine"
0

SWITCH
329
181
475
214
do-plots?
do-plots?
0
1
-1000

BUTTON
335
111
398
144
NIL
setup
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
861
112
950
157
avg degree
mean-degree
3
1
11

MONITOR
863
155
949
200
Nodes
N
17
1
11

SWITCH
475
181
623
214
individual-contagion?
individual-contagion?
1
1
-1000

SLIDER
328
147
475
180
normal-mean
normal-mean
0
1
0.25
0.01
1
NIL
HORIZONTAL

SLIDER
477
146
622
179
normal-sd
normal-sd
0
0.025
0.02
0.0001
1
NIL
HORIZONTAL

PLOT
642
111
855
268
Degree Distribution
Degree
Frequency
0.0
15.0
0.0
25.0
true
false
"histogram [count my-links] of turtles" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [count link-neighbors] of turtles"

PLOT
642
268
854
421
Thresholds Distributiuon
Threshold
Frequency
0.1
0.35
0.0
130.0
true
false
"histogram [Ti] of turtles" ""
PENS
"default" 0.01 1 -16777216 true "" "histogram [Ti] of turtles"

BUTTON
425
112
488
145
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
40
399
299
452
FULL BLUE CIRCLE = NON-INITIAL ADOPTER\nFULL YELLOW CIRCLE = INITIAL ADOPTER\nEMPTY BLUE CIRCLE = NON-ADOPTER
13
0.0
1

MONITOR
864
242
951
287
graph density
density
3
1
11

MONITOR
400
339
531
384
% simulation complete
(count-agent * 100) / N
3
1
11

BUTTON
506
111
611
144
NIL
export-model
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
864
197
950
242
total dyads
count links
17
1
11

MONITOR
418
525
502
570
IM Betweeness
IM-betw
3
1
11

MONITOR
66
524
132
569
IM Random
IM-rand
3
1
11

MONITOR
131
524
192
569
IM Degree
IM-deg
3
1
11

MONITOR
263
525
337
570
IM Closeness
IM-clos
3
1
11

MONITOR
336
525
419
570
IM Eigenvector
IM-eigen
3
1
11

MONITOR
191
525
265
570
IM Clustering
IM-clust
3
1
11

MONITOR
502
525
650
570
IM Race Heterog. * Degree
IM-rhetdeg
3
1
11

SLIDER
329
215
475
248
intervention-size
intervention-size
0
1
0.05
0.01
1
NIL
HORIZONTAL

MONITOR
773
526
898
571
IM Interacial-brokerage
IM-rbroker
3
1
11

MONITOR
804
428
953
473
E-I Index of Segregation
E-I
3
1
11

MONITOR
644
428
792
473
Gross Segregation Index
GSI
3
1
11

SLIDER
477
215
618
248
Reps
Reps
1
1452
10.0
1
1
NIL
HORIZONTAL

MONITOR
400
386
530
431
mean overall adopoption
mean CAPall
3
1
11

MONITOR
863
287
950
332
NIL
isolates
17
1
11

MONITOR
863
332
951
377
avg path length
avg-path-length
3
1
11

MONITOR
862
377
951
422
local clust. coeff
local-clustering
3
1
11

MONITOR
649
525
772
570
IM Race Heterogeneity
IM-rhet
3
1
11

SLIDER
477
251
618
284
sims
sims
0
25
19.0
1
1
NIL
HORIZONTAL

TEXTBOX
115
495
819
525
Imprecision Functions (IM) Based on Different Different Simulated Strategies\n
20
0.0
1

TEXTBOX
39
373
261
398
Sociogram Color-Code:
20
0.0
1

TEXTBOX
661
68
930
96
Network Descriptive Statistics
20
0.0
1

TEXTBOX
380
58
615
114
Simulation Parameters and Initial Conditions
20
0.0
1

TEXTBOX
25
595
599
684
Code for the dissertation: Three Essays on Network Dynamics and Liminality\nCode Written by Diego F. Leal, University of Massachusetts (www.diegoleal.info)\nLast updated: August 2017\n\n
15
0.0
1

TEXTBOX
605
596
972
680
Note: The original version of this model is based on empirical data from Add Health. Since access to those data is restricted to protected the identity of the participants, the network used in this model is a size- and density-conditioned random graph based on the so-called Sunshine High included in the Add Health data set. To see results based on the empirical data, please contact Diego F. Leal\n
11
0.0
1

TEXTBOX
145
13
948
69
Ethnoracial Liminality and the Diffusion of Innovations among Adolescents
23
55.0
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
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment_1-25" repetitions="25" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1"/>
    <metric>(count-agent * 100) / N</metric>
    <metric>mean CAPall</metric>
    <metric>IM-rand</metric>
    <metric>IM-deg</metric>
    <metric>IM-betw</metric>
    <metric>IM-rhet</metric>
    <metric>IM-rhetdeg</metric>
    <metric>IM-rbroker</metric>
    <metric>mean in-CAPall_race</metric>
    <metric>mean out-CAPall_race</metric>
    <metric>mean in-CAPall_sex</metric>
    <metric>mean out-CAPall_sex</metric>
    <metric>mean in-CAPall_grade</metric>
    <metric>mean out-CAPall_grade</metric>
    <metric>mean in-CAPall_age</metric>
    <metric>mean out-CAPall_age</metric>
    <metric>mean in-CAPall_ses</metric>
    <metric>mean out-CAPall_ses</metric>
    <metric>mean in-CAPall_racei</metric>
    <metric>mean out-CAPall_racei</metric>
    <metric>mean in-CAPall_latino</metric>
    <metric>mean out-CAPall_latino</metric>
    <metric>mean in-CAPall_white</metric>
    <metric>mean out-CAPall_white</metric>
    <metric>mean in-CAPall_black</metric>
    <metric>mean out-CAPall_black</metric>
    <metric>mean in-CAPall_native</metric>
    <metric>mean out-CAPall_native</metric>
    <metric>mean in-CAPall_asian</metric>
    <metric>mean out-CAPall_asian</metric>
    <metric>mean in-CAPall_other</metric>
    <metric>mean out-CAPall_other</metric>
    <metric>mean in-CAPall_multi</metric>
    <metric>mean out-CAPall_multi</metric>
    <metric>mean in-CAPall_white</metric>
    <metric>mean out-CAPall_white</metric>
    <enumeratedValueSet variable="do-plots?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-mean">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individual-contagion?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="intervention-size" first="0.05" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="school">
      <value value="&quot;sunshine&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reps">
      <value value="10"/>
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
0
@#$#@#$#@
