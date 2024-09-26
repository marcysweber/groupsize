;;;;;;;;;;;;;;;;;;;;;;;
; This is the "base model" code, where we will build the simplest working primate group model
; I expect to then copy this code into new files, where the additional submodels can be added on piecemeal
; Contributors to this file are Marcy Ekanayake-Weber, Deanna Cunha, and Chris O'Connor-Coates
;
; this file also now includes simple group recognition. Paige Olsen will be working
; in this file to create group recognition.
;;;;;;;;;;;;;;;;;;;;;;
extensions [nw profiler matrix]

globals [
  ;; Landscape parameters
  ;regrowth-rate ; this is a slider now

  ;; patchsets and other mutable landscape vars
  empty-patches ;; this is used while making the clumps
  resource-patches ;; this is never actually used
  energy-left ;; this is needed to stop the landscape generation

  current-resources

  starting-pop-primates

  decidedtofight

  ;; vision
  ;other-primate-detection-radius ; this is a slider now
  ;resource-detection-radius ; this is a slider now
  cap-nearest-primates

  louvain-output ;; list of the most recent communities recognized by the nw algorithm
  louvain-output-prev ;; list generated the last time
  louvain-output-2prev ;; list generated the time previous to the last time
  louvain-output-3prev

  countdown-to-longer-processes
  total-starved

  scenario


]


undirected-link-breed [contacts contact]
undirected-link-breed [associations association]
undirected-link-breed [group-members group-member] ;;link between primagent and group
undirected-link-breed [groupdyads groupdyad] ;;link among all group members, only to calc interindiv distances

contacts-own [
 freq
]

breed [primates primate]
breed [groups group] ;;groups will actually be a kind of turtle "under the hood"


primates-own[
  sex
  age ;; in years
  ageclass ;; explanation
  stored-energy

  rhp ;; RHP is the resource holding potential - basically, how "strong" an individual is in a fight
  nearest-primates
  visible-primates
  visible-resources ;; patches,
  patch-picked

  grouped?
  my-louvain-output-commun ;; list of previous louvain communities, fput with the new one


  daily-distance-traveled ;; distance traveled, reset every 12 ticks
  distance-traveled ;; TOTAL distance traveled by primate
  list-daily-distance ;; a list of the daily distance travelled, recorded each time it is reset
  mean-daily-distance
  dist-this-turn

  total-energy-gained ;; total energy gained over time
  metabolism
  count-prox
  percent-t-in-prox

  freq-conflict

  exp-score ;; current value of estimated fighting ability, 0.0-1.0
  exp-delta-list ;; list recording every time the exp-score was changed by a win or loss
  running-exp-avg ;; average of the exp-delta-list

  wns
  lsses
  fights
  attacks
  avoids

  ;;variables for counter of primates for time spent moving or eating
  eating?
  moving?
  timemove
  timeeat
  daily-intake
  monthly-intake
  energy-intake-rate-list-daily
  energy-intake-rate-list-monthly
  energy-gained-weekly
  distance-traveled-weekly

]


groups-own[
  hierarchy
  creation-time
]

patches-own [
  penergy ;; current amount of energy that a patch has.
  quality
  extraction-rate ;; amount of energy that one can extract from a patch in 1 turn.
]

to setup
  ca
  resize-world -140 140 -140 140

  set starting-pop-primates round(abundance / energy-per-capita)

  ;; define scenario
  ifelse clump-size > 500 [set scenario "clump"][set scenario "uni"]
  ifelse qual-mean > 87 [set scenario (word scenario "-largepatch")][set scenario (word scenario "-smallpatch")]
  ifelse extraction-rate-mean < 7.5 [set scenario (word scenario "-slowextr")][set scenario (word scenario "-fastextr")]
  ifelse patch-regrowth-interval < 1750 [set scenario (word scenario "-shortregrowint")][set scenario (word scenario "-longregrowint")]
  type scenario type behaviorspace-run-number

  ;;set_life_history_params
  vision_settings

  ;; set up the resource landscape
  set energy-left abundance
  ;show energy-left type " energy left"
  set empty-patches patches
  ;set regrowth-rate qual-mean ;; this is arbitrary - could eventually be changed to a more accureate regrowth curve

  create_resources

  set resource-patches (patches with [quality != 0])
  set current-resources resource-patches
  set empty-patches (patches with [quality = 0])
  ;; now we have patch-sets of resource patches and patches without resources
  ;; note that a resource patch's penergy could be 0, but the QUALITY cannot be

  ;; make an initial population of primates
  create_starting_pop
  set total-starved (list)

  ;;reset time spent moving and eating (added by Namita)
  ask primates [
  set timemove 0
  set timeeat 0
  ]

  reset-ticks
end


to vision_settings
  ;set other-primate-detection-radius 5
  ;set resource-detection-radius 5

  ;;set the maximum possible amount of other primates that a primate can pay attentino to
  ifelse tgt-neighbor = 0 [set cap-nearest-primates 1][set cap-nearest-primates tgt-neighbor]
end

to create_resources ;; picks a new center-point and then generates a clump around it
  if (energy-left <= 0) [ stop ] ;; make sure that we don't try to make a new clump if there's no more energy we want on the landscape

  let center-point no-patches ;; reset the center patch of the new clump
  let this-clump-set no-patches ;; an )empty) agentset for the patches of the new clump

  let candidate-patches no-patches

  let this-clump-size clump_size ;; picks how big this clump should be



  ifelse is-patch-set? empty-patches [
    set center-point pick_center-point ]
    [
  ]


  ifelse is-patch? center-point [ ;; if the center-point was actually picked
    set this-clump-set (patch-set center-point this-clump-set)
    if resource-tests-on? [show "Creating the new centerpoint... \n"]


    if resource-tests-on? [type this-clump-set type " this clump set center point \n"]

    ask center-point [set candidate-patches patches in-radius clump-radius this-clump-size]

    if this-clump-size > (count candidate-patches) [
      set this-clump-size  (count candidate-patches) ]

    ask n-of this-clump-size candidate-patches [ create_patch_attributes ]
  ] [
    stop
  ]


  ifelse (energy-left > 0) [
      create_resources
    ][
    if resource-tests-on? [type "All of the energy was used up."]
      stop
    ]


end

to-report clump-radius [this-clump-size]

    let clump-rad (sqrt(this-clump-size / pi)) ;;use the formula for the area of the circle to get what the radius of the patch should be
    set clump-rad ceiling clump-rad;; round up as a buffer

  report clump-rad

end

to-report pick_center-point


  report one-of patches
end

to create_patch_attributes ;; this sets the relevant attributes of a new resource patch
  set quality patch_qual
  set penergy quality

  set extraction-rate patch_extr

  color_patches


  set energy-left (energy-left - quality)
  if resource-tests-on? [type energy-left type " energy left \n"]


end

to color_patches
  (ifelse
    (penergy < 1) [set pcolor brown]
    (penergy < 67) [set pcolor 57]
    (penergy < 110) [set pcolor 55]
    (penergy > 110) [set pcolor 53]
  )
end

to-report clump_size ;; gives the number of patches of the clump currently being created, as an integer pulled from a normal distribution
  report clump-size ;; simplified after weird MEE results
end

to-report patch_qual ;; gives the amount of maximum energy for the patch currently being created, also as an integer from a normal distribution
  report abs round (random-normal qual-mean qual-sd)
end

to-report patch_extr
  let rate 1
  let from-dist abs round (random-normal extraction-rate-mean extraction-rate-sd)
  if from-dist > 0 [set rate from-dist]
  report rate

end

to create_starting_pop ;; creates a beginning population of primate agents, of varialbe age and sex, and places them randomly on the landscape
  ;let spawn-points (patch-set patch 0 0 patch -70 70 patch 70 70 patch 70 -70 patch -70 -70)
  let spawn-points up-to-n-of 5 resource-patches

  create-primates starting-pop-primates [
    set size 5 ; size of turtles to 5
    ;set color brown ; color brown

    ;move-to one-of resource-patches ; sets coordinates to random resource patch
    move-to one-of spawn-points
    set stored-energy 200
    set grouped? false
    set my-louvain-output-commun (list )
    set list-daily-distance (list )

    set metabolism 0

    set rhp one-of [1 2 3 4 5 6 7 8]
    set exp-score 0.50
    set exp-delta-list (list 0.50)
    set running-exp-avg 0.50

    ;; sex
    primate-sex

    ;; age and ageclass
    primate-age

    set count-prox 0

    set energy-intake-rate-list-daily []
    set energy-intake-rate-list-monthly []

 primate-color

  ]

end

to primate-age
  ;; age and ageclass
  set age 10
end

to primate-sex
  set sex "F"

end

to primate-color
set color white
end




to go ;; put main schedule procedures for readability
  tick
  run_half_hourly_processses

  ;;; something is going wrong with regrowing
  ;; once daily processes
  if ticks mod 24 = 0 [run_daily_processes]

  ;; group recognition processes
  if ticks mod (24 * 7) = 0
  [if group-recog-module? [
    search_for_groups
    make_csv_weekly_group_level_data
    ask primates [
      set energy-gained-weekly 0
      set distance-traveled-weekly 0
    ]


    ]
  ]

  ;; run the longer processes
  ;; previously set at 2160
  if ticks mod (30 * 24) = 0 [run_monthly_processes]

  if (ticks mod round patch-regrowth-interval) = 0 [run_regrowth]
end




to run_half_hourly_processses
  set current-resources current-resources with [penergy > 0]

  ask primates [



  (ifelse penergy > 0 [
      eat ; if there is any penergy the primate eats
      ]
    [; otherwise the primate  moves

        find_nearest_primates

        find_visible-resources
        move
    ])
   timespenteatmove

  ]

  decay_exp


end

to run_daily_processes
  reset-distance-records

  ask primates [

    make_contacts
    (ifelse daily-intake = 0 [set energy-intake-rate-list-daily lput 0 energy-intake-rate-list-daily]
       [set energy-intake-rate-list-daily lput (daily-intake / 24) energy-intake-rate-list-daily])
    set daily-intake 0
  ]
    ;metabolize_starve
    if group-recog-module? [move_groups]
  ask links [hide-link]
end


to run_monthly_processes
  if go-tests-on? [show "running the longer processes"]

  ask primates [
    (ifelse monthly-intake = 0 [set energy-intake-rate-list-monthly lput 0 energy-intake-rate-list-monthly]
      [set energy-intake-rate-list-monthly lput (monthly-intake / (3 * 30 * 24)) energy-intake-rate-list-monthly])
    set monthly-intake 0
  ]

end

to run_regrowth

  let patches-to-regrow resource-patches with [penergy < quality]
  ask patches-to-regrow [  ;if there are any empty patches let them regrow
    if penergy < 0 [set penergy 0]
    regrow
    if go-tests-on? [type "this patch regrew! \n"]

  ]
  set current-resources (patch-set current-resources patches-to-regrow) ;;add newly regrown patches back to current-resources
end


to find_nearest_primates

  ifelse any? group-member-neighbors [
    set grouped? true
  ][
    set grouped? false
  ]

  set visible-primates other primates with [distance myself <= other-primate-detection-radius]

  if grouped? = true [
    let visible-and-group visible-primates
    ask visible-and-group [
      ;; if this visible primate is NOT in the same group, then set visible-and-group other visible-and-group
      if grouped? = true and not group-member-neighbor? one-of [group-member-neighbors] of myself [
        set visible-and-group other visible-and-group
      ]
    ]
    set visible-primates visible-and-group
  ]

  ifelse (count visible-primates > cap-nearest-primates) [
    set nearest-primates min-n-of cap-nearest-primates visible-primates [distance myself]
  ][
    set nearest-primates visible-primates
  ]

;; filter nearest-priamtes to only include group members, if the agent is grouped



  set-count-prox
end

to make_contacts ;this is a very expensive procedure
  if group-recog-module? [
    let nearby-primates other primates with [distance myself <= other-primate-detection-radius]
    if any? nearby-primates [
      ask nearby-primates [
        create-contact-with myself
        ;ask contact-with myself [set freq freq + 1]
      ]
    ]
  ]
end

to set-count-prox

  if any? other primates-here [ set count-prox count-prox + 1 ]

  set percent-t-in-prox 100 * (count-prox / ticks)

end


to find_visible-resources
  set visible-resources current-resources in-radius resource-detection-radius ;; find all resource patches, into an agent-set
  ;set visible-resources (sort-on [(- penergy)] visible-resources) ;; sort by penergy descending; now this is a list
  ;if any? visible-resources [set patch-picked max-one-of visible-resources [penergy]]
end



;;;; TURTLE PROCEDURES

;;;; FOOD AND FEEDING-RELATED
to eat
  if any? other primates-here [decide_to_fight]

  ifelse penergy > extraction-rate [
   set penergy (penergy - extraction-rate) ; this is patch energy minus the extraction rate energy used

    set stored-energy (stored-energy + extraction-rate) ; this is the stored energy of the primate plus the patch energy
    set total-energy-gained total-energy-gained + extraction-rate
    set monthly-intake monthly-intake + extraction-rate
    set daily-intake daily-intake + extraction-rate
    set energy-gained-weekly energy-gained-weekly + extraction-rate

  ][
    set stored-energy (stored-energy + penergy) ; this is the stored energy of the primate plus the patch energy
   set total-energy-gained total-energy-gained + penergy
    set monthly-intake monthly-intake + penergy
    set daily-intake daily-intake + penergy
    set energy-gained-weekly energy-gained-weekly + penergy

    set penergy 0
  ]
 color_patches

   ;; boolean to see whether moving or eating (added by Namita)
  set eating? true
    set moving? false
end

to metabolize_starve
  ;; subtract energy used from existing energy
  ask primates [set stored-energy stored-energy - metabolism]

  ;;recognize who has starved to death
  let starved primates with [stored-energy <= 0]

  ;; deal with the starved primates
  ask starved [

    ;; print statement test
    if go-tests-on? [show "A primate starved to death!"]

    ;; add to a total of starved which can be looked at
    set total-starved fput who total-starved ;; something is wrong with fput and the default of total-starved

    ;die
  ]
end


;;;; MOVEMENT-RELATED
to move ;; this is something already asked of primates, so don't ask primates again within this func!
  ;; movement will be a compromise between resources, movement cohesion, avoidance

  set dist-this-turn 1
  set visible-resources visible-resources with [penergy > 0]

  (ifelse not any? visible-resources [ ;; if there are no resources around, travel or wander
     (ifelse any? visible-primates [
            travel ;; travel if there are other turtles to stay with
           ][
            wander ;; wander if there are neither resources nor other agents
           ])
   ][ ;; if there are resources around, check if greg rules are met
    let num-neighbor count visible-primates with [distance myself <= tgt-dist]
      (ifelse num-neighbor >= tgt-neighbor [
           forage ;; if enough other agents nearby, then forage
           ][
           cohere ;; if not enough

      ])
   ])

  ;][set change-in-heading towards one-of neighbors with [not any? primates-here]]

  jump dist-this-turn
  set daily-distance-traveled daily-distance-traveled + dist-this-turn  ;; changed by Suchir
  set distance-traveled-weekly distance-traveled-weekly + dist-this-turn


  ;; boolean to see whether moving or eating (added by Namita)
  set moving? true
  set eating? false

end



to travel ;;if grouped, only travel with group members?
  if tgt-neighbor > 0 [
    set heading heading-of-nearest-primates
    set dist-this-turn max-move
  ]
  set heading heading + ((random movement-noise) - (movement-noise / 2))

end



to wander ;; this is mutually exclusive with forage, disperse - should it also include align, move-towards, etc?

  set heading heading + ((random movement-noise) - (movement-noise / 2)) ;; totally random <5 degree turns
  set dist-this-turn max-move
  ;show "wandered"

end

to forage ;; to be called if not starving, there are visible resources, and tgt_neighbor is met
  let new-heading heading


  (ifelse (is-patch? patch-picked) and (member? patch-picked visible-resources) and ([penergy] of patch-picked > 0) [
          set new-heading towards patch-picked
          (ifelse distance patch-picked > max-move [
             set dist-this-turn max-move
           ][
             set dist-this-turn distance patch-picked])
          ;show "still heading towards" show patch-picked
        ]
        [
             ;;;;need to pick the patch from visible-resources
            set patch-picked max-one-of visible-resources [penergy]
       set new-heading towards patch-picked
      (ifelse distance patch-picked > max-move [
             set dist-this-turn max-move
           ][
             set dist-this-turn distance patch-picked])
    ])

  set heading new-heading
  ;;if move-tests-on? [type "changing heading to " type first visible-resources type "\n"]

  ;show "foraged"
end

to cohere ;;if grouped, then only cohere with group members?
 (ifelse any? nearest-primates [

    let num-neighbor count nearest-primates with [distance myself <= tgt-dist]
    (ifelse num-neighbor < tgt-neighbor AND count(visible-primates) > num-neighbor AND tgt-neighbor > 0 [
      set heading heading-towards-nearest-primates
      set patch-picked no-patches
      set dist-this-turn max-move
    ][
      forage
      ])
  ][
    forage
  ])
;(ifelse any? nearest-primates [set movement-distance distance min-one-of nearest-primates [distance myself]][(ifelse distance patch-picked > 50 [set movement-distance 50][set movement-distance distance patch-picked])])
  ;show "heading towards " show nearest-primates


end




to-report heading-of-nearest-primates  ;; turtle procedure
  ;; from the flocking library model
  ;; We can't just average the heading variables here.
  ;; For example, the average of 1 and 359 should be 0,
  ;; not 180.  So we have to use trigonometry.


  let x-component sum [dx] of nearest-primates
  let y-component sum [dy] of nearest-primates
  ifelse x-component = 0 and y-component = 0
    [ report heading ]
    [ report atan x-component y-component ]
end

to-report heading-towards-nearest-primates  ;; turtle procedure
  ;; "towards myself" gives us the heading from the other turtle
  ;; to me, but we want the heading from me to the other turtle,
  ;; so we add 180

  let outside-nearest-primates nearest-primates with [not (patch-here = [patch-here] of myself)]

  let x-component 0
  let y-component 0

  if any? outside-nearest-primates [
    set x-component mean [sin (towards myself + 180)] of outside-nearest-primates
    set y-component mean [cos (towards myself + 180)] of outside-nearest-primates]

  ifelse x-component = 0 and y-component = 0
    [ report heading ]
    [ report atan x-component y-component ]
end

to-report heading-towards-visible-primates  ;; turtle procedure
  ;; "towards myself" gives us the heading from the other turtle
  ;; to me, but we want the heading from me to the other turtle,
  ;; so we add 180


  let x-component mean [sin (towards myself + 180)] of visible-primates
  let y-component mean [cos (towards myself + 180)] of visible-primates
  ifelse x-component = 0 and y-component = 0
    [ report heading ]
    [ report atan x-component y-component ]
end

to reset-distance-records
  ask primates [
      set distance-traveled distance-traveled + daily-distance-traveled
      set list-daily-distance fput daily-distance-traveled list-daily-distance
      set mean-daily-distance mean list-daily-distance
      set daily-distance-traveled 0
    ]

end



to move_groups

  ask groups with [count group-member-neighbors < 2] [
    ask group-member-neighbors [set grouped? false]
    die
  ]

  ask groups [
    let x-component 0
    let y-component 0

    if any? group-member-neighbors with [xcor != [xcor] of myself and ycor != [ycor] of myself] [
      set x-component mean [sin (towards myself + 180)] of (group-member-neighbors with [xcor != [xcor] of myself])
      set y-component mean [cos (towards myself + 180)] of (group-member-neighbors with [ycor != [ycor] of myself])
    ]

    ifelse x-component = 0 and y-component = 0 [
        fd 1
      ][
        set heading atan x-component y-component
        fd 5
      ]
  if any? other turtles-here [move-to one-of neighbors]
  ]


end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;contest competition module

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to-report cost-estimation [opponent]

  report (([exp-score] of opponent - exp-score) + 1) / 2 ;;; this puts the difference in scores back on a 0.0-1.0 scale
end


to-report benefit-estimation
  let bene 0

  ;(() / (qual-mean + (2 * qual-sd))) ;; the benefit assessment is the energy of patch in dispute as proportion of highest possible quality

  let current-portion (([penergy] of patch-here) / count primates-here)
  ;type current-portion type "amount in penergy i am projected to get \n"

  let winning-portion (([penergy] of patch-here) / (count primates-here - 1))
  ;type winning-portion type " amount in penergy i get if i kick opponent out \n"

  let potential-gain (winning-portion - current-portion)
  ;type potential-gain type " the difference, i.e., how much i gain \n"

  let alternative (mean [penergy] of neighbors)
 ;type alternative type " the other patches, by comparison \n"

  set bene (potential-gain / (potential-gain + alternative))
  ;type bene type " how does the potential gains compare to the surrounding patches? \n"

  ;;is the potential gain better than what you could forage from neighboring patches? if so, attack

  ;in the new version with co-occupation of patches, the benefit is how much energy i would get if i can kick off one other individual.
  ;so, dividing the remaianing energy on the patch by the current occupants


  report bene
end



to decide_to_fight ;; function to inform turtles on how to decide to fight

  let opponent one-of other primates-on patch-here ;; define i as one of the other primates on the next patch ahead (and not self as opponent)
  ;show opponent type "is my opponent \n\n"



  if opponent != nobody [;; if i (the opponent) is not "nobody", then do the following function (this should be the very first thing checked)

    set running-exp-avg mean exp-delta-list
    let bene benefit-estimation
    let cost-est 0.01 ;; estimate costs will be calculated below, depending on current assessment strategy
    let probr 1.0 ;; prob of making the right choice. default to 1, so that agents will make the optimal choice if something goes wrong

    ;; then we can do a die roll
    let roll random-float 1.0

    ;;;;;;;;;;;;;;;
    ;; ESTIMATE COSTS
        set cost-est cost-estimation opponent

    ;;;;;;;;;;;;;
    ;; USE COSTS TO DETERMINE BEST DECISION
  (ifelse bene > cost-est [
          set probr (bene / (bene + cost-est))

          (ifelse probr > roll [


            fight opponent
          ][
              set avoids avoids + 1
              ;;do nothing
          ])

        ]
        cost-est >= bene [
          ;;show "we decided not to fight \n" ;; if the fight does not occur, then self says, "we decided not to fight"
          ;let correct-choice avoid

          set probr (cost-est / (bene + cost-est))
          (ifelse probr > roll [
               ;;do nothing
               set avoids avoids + 1
           ][

               fight opponent
           ])
        ])





    ;;ask out-fightavoided-to i [set avoid-counter avoid-counter + 1]


  ]

end
;

to decay_exp
  ask primates [
    if running-exp-avg < 0 [
    set running-exp-avg 0
  ]
    if running-exp-avg > 1 [
      set running-exp-avg 1
    ]
  ]
  ask primates with [exp-score != running-exp-avg] [
    ifelse exp-score > running-exp-avg [
      ;; exp-score is higher than the running average
      set exp-score exp-score - 0.01
    ][
      ;; exp-score is lower than the running average
      set exp-score exp-score + 0.005
    ]
  ]
end



to fight [opponent] ;; function to have individuals (self and opponent) fight each other
    if opponent != nobody [ ;; if the opponent does not have a value of "nobody", then run the code

      set decidedtofight decidedtofight + 1
      set fights fights + 1

    ;;create a contact if group recog is on
    if group-recog-module? = true [
    ask opponent [
      create-contact-with myself
      ask contact-with myself [set freq freq + 1]
    ]]

      ask opponent [
        set fights ([fights] of opponent + 1) ;; have the opponent increase their count of fights by 1
      ]

      set fights (fights + 1) ;; have self increase their count of fights by 1

      ;;let j random-float 1.0 ;; j is a random decimal-number that is used below to determine if an individual wns a fight or not (fight influenced by rhp value)
      ;;let prob_self (rhp / (rhp + [rhp] of opponent)) ;; prob_self denotes the probability that an individual will win a fight (calculated using rhp values of the individuals involved and compared to j)
      ;;let prob_opp ([rhp] of opponent / ([rhp] of self + [rhp] of opponent)) ;; probability of opponent winning a fight, may not be necessary (not used in current version of code)

      ;;(ifelse j < prob_self [ ;; if j is less than the probability of self winning the fight (prob_self), then self wns the fight
    (ifelse [rhp] of opponent < rhp [
      ;; I WON
        update_winner_exp
        ask opponent [update_loser_exp]
      ]

      [;; I LOST
        update_loser_exp
        ask opponent [update_winner_exp]
      ])

    update_exp_list
    ask opponent [update_exp_list]
  ]
end

to update_winner_exp
  set wns wns + 1 ;; opponent recalculates their wns counter to increase it by 1
          ;;show "I win! \n"

          ifelse exp-score > 0.98 [
            set exp-score 1.0
          ][
            set exp-score exp-score + 0.01
          ]
end


to update_loser_exp
    right 180 ;; loser turns around
          fd 1 ;; loser runs away
set daily-distance-traveled daily-distance-traveled + 1

  set lsses (lsses + 1) ;; opponent recalculates their lsses counter to increase it by 1


        ;;;; also exp-score stuff here!!!!
          ifelse exp-score > 0.02 [
             set exp-score exp-score - 0.01
             ][
             set exp-score 0.01
             ]

end


to update_exp_list
      set exp-delta-list fput exp-score exp-delta-list

    ifelse length exp-delta-list > 4 [
      set running-exp-avg mean sublist exp-delta-list 0 5 ;;;;; the running average is only from the 10 most recent wins or losses
    ][
      set running-exp-avg mean exp-delta-list
    ]


end






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;group recognition module

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



to search_for_groups

  recognize_associations

if [length my-louvain-output-commun] of one-of primates >= 10 [
    check_existing_groups
    recognize_new_groups

  ]

  ;ask associations [die] ;; clear all of the associations

  ask links [hide-link]
end

to recognize_associations ;this is a very expensive procedure
  ;show "recognizing associations"
  nw:set-context primates contacts
  set louvain-output nw:louvain-communities

(foreach louvain-output [ [community ] -> ;; for all the values in the list, call them "community" and do the following
    ;let group-color one-of [red orange cyan sky violet magenta pink white]
    ask community [;; analogous to "ask turtles" - asking an agentset
      ;set color group-color
      ;create-associations-with other community ;is there any point to making this kind of link if we just immediately ask them to die?
      set my-louvain-output-commun fput community my-louvain-output-commun ;; add the current community to my list of communities
    ]
])

  ask contacts [die] ;; clear the nearest-primates links that we made in the last 500 ticks
end

to recognize_new_groups
  ;show "recognizing new groups"
  ;; search for individuals (not currently assigned to a group) who have overlapped louvain-communities
  ask primates [
    if not grouped? [
      let possible-group-members turtle-set self

      ;; find the community i'm in right now - associations
      let associates first my-louvain-output-commun

      ;; take the other primates from associations
      ask associates [
        ;; were they in the same community as me in louvain-output-prev?
        let recent-10-commun sublist my-louvain-output-commun 0 9
        let count-shared-commun length filter [ community -> member? myself community ] recent-10-commun ;; this filter could be a separate function/reporter

        if 8 <= count-shared-commun [ ;; if you are in any 2 of the previous communities
          set possible-group-members (turtle-set self possible-group-members)
        ]
      ]
      if any? other possible-group-members [
        ;; check if they are already in a group
        ifelse any? possible-group-members with [any? group-member-neighbors] [
          ;; if so, go ahead and add the possible-group-members to that group

          ask one-of possible-group-members with [any? group-member-neighbors] [

            let destination-group one-of group-member-neighbors ;get the group of that possible group member

            ask other possible-group-members [
              if grouped? [ emigrate_out self group-member-neighbors ] ;;emigrate before immigrate
              immigrate_in self destination-group
            ]
          ]
        ][
          ;; if all of the possible-group-members are currently groupless, make a new group
          make_new_group possible-group-members
        ]
      ]
    ]
  ]
end

to make_new_group [new-group-members]
  ;show "making a new group"
  ;; take a turtleset as input
  ask one-of new-group-members [
    hatch-groups 1 [
      create-group-members-with new-group-members [ set color red ]
      set size 10
      set color white
      set label "group"
      set shape "star"
    ]
  ]
  ask new-group-members [set grouped? true]
end


to check_existing_groups
  ;show "checking existing groups"
  ask groups [
    let in-group group-member-neighbors

    if count my-links < 2 [
      ask group-member-neighbors [set grouped? false]
      die
    ]

    ;; group_members links too long?
    ask my-group-members [
      if link-length > 80 [
        emigrate_out other-end myself
      ]
    ]

    ;;; A BETTER WAY of merging groups
    ;

    if any? other groups in-radius 20 [
      ;; is anyone in that nearby group associates with this group?
      let nearby-group one-of other groups in-radius 20
      let associated-out-group (turtle-set)

      ask [group-member-neighbors] of nearby-group [
        let recent-10-commun sublist my-louvain-output-commun 0 9
        if any? in-group with [8 <= length filter [ community -> member? self community ] recent-10-commun ] [
        set associated-out-group (turtle-set associated-out-group self)
        ]
      ]
      ;; if so, die, and everybody join the other group
      if count associated-out-group >= 2 [

        emigrate_out in-group self
        immigrate_in in-group nearby-group
        ]
        die
      ]

    ]



;  ]
end

to immigrate_in [ immigrants new-group ]
  if new-group = nobody [error "the new group was a nobody."]
  ask immigrants [
    if any? group-member-neighbors or grouped? = true [error "double group"]
    create-group-member-with new-group
    set grouped? true
    ;show "immigration"
  ]
end

to emigrate_out [ emigrants old-group ]
  ask emigrants [
    ;if not any? group-member-neighbors or grouped? = false [error "trying to emigrate when not in a group"]
    set grouped? false
    ask my-group-members [die]

    ;show "emigration"
  ]
end



;;;; patch procedures

to regrow

  set penergy (regrowth-rate * quality) + penergy ; this adds the regroth rate to the patches energy after it was eaten so that the patch can re-grow

  if penergy > quality [set penergy quality]

  color_patches
end



;; counter for primate with time spent moving and eating (added by Namita)
to timespenteatmove
  (ifelse eating? [
      set timeeat timeeat + 1
      ;;set label (word timeeat "," timemove )
  ] moving? [
      set timemove timemove + 1
      ;;set label (word timeeat "," timemove )
    ] []
  )
end





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; file output procedures;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to make_csv_weekly_group_level_data
  (if make_csv_weekly_group_level?  [
    let folder-path "/Users/me597/Documents/"
    set-current-directory folder-path
    let file-name (word behaviorspace-experiment-name behaviorspace-run-number ".csv")

    if ticks = 168 [
      file-open file-name

      file-type "run number, "
      file-type "ticks, "
      file-type "tgt-neighbor, "
      file-type "tgt-dist, "
      file-type "abundance, "
      file-type "energy-per-capita, "
      file-type "clump, "
      file-type "patch-size, "
      file-type "extraction, "
      file-type "patch-regrowth-interval, "
      file-type "max-move, "
      file-type "group-ID, "
      file-type "group-size, "
      file-type "group-mean-weekly-intake, "
      file-print "group-mean-weekly-distance-travelled"
      file-close
    ]

    if ticks > 3300 [
      file-open file-name
      ask groups with [count group-member-neighbors > 1] [
        let group-size count group-member-neighbors
        let group-mean-weekly-intake mean [energy-gained-weekly] of group-member-neighbors
        let group-mean-weekly-distance-traveled mean [distance-traveled-weekly] of group-member-neighbors

        file-type (word behaviorspace-run-number ", ")
        file-type (word ticks ", ")
        file-type (word tgt-neighbor ", ")
        file-type (word tgt-dist ", ")
        file-type (word abundance ", ")
        file-type (word energy-per-capita ", ")
        file-type (word clump-size ", ")
        file-type (word qual-mean ", ")
        file-type (word extraction-rate-mean ", ")
        file-type (word patch-regrowth-interval ", ")
        file-type (word max-move ", ")
        file-type (word who ", ")
        file-type (word group-size ", ")
        file-type (word group-mean-weekly-intake ", ")
        file-print group-mean-weekly-distance-traveled
      ]
      file-close
    ]
    ]
  )

end




to make_csv_group-level_data
  let folder-path "C:\\Users\\Marcy\\Desktop\\groupsizegrouplevel"

  set-current-directory folder-path
  let file-name (word behaviorspace-experiment-name behaviorspace-run-number ".csv")

  file-open file-name

  ;;column names

  file-type "run number, "
  file-type "tgt-neighbor, "
  file-type "tgt-dist, "
  file-type "abundance, "
  file-type "energy-per-capita, "
  file-type "clump, "
  file-type "patch-size, "
  file-type "extraction, "
  file-type "patch-regrowth-interval, "
  file-type "max-move, "
  file-type "group-ID, "
  file-type "group-size, "
  file-type "group-mean-intake, "
  file-type "group-inter-indiv-dist, "
  file-print "group-mean-distance-travelled"
  file-close


  file-open file-name
  ask groups with [count group-member-neighbors > 1] [
    let group-size count group-member-neighbors
    let group-mean-intake mean [total-energy-gained] of group-member-neighbors
    let group-mean-distance mean [distance-traveled] of group-member-neighbors

    ask groupdyads [die] ;;kill all existing groupdyads so that we only process one group at a time
    ask group-member-neighbors [
      create-groupdyads-with other [group-member-neighbors] of myself
    ]
    let group-mean-IID mean [link-length] of groupdyads
    ask groupdyads [die] ;;kill all existing groupdyads so that we only process one group at a time

    file-type (word behaviorspace-run-number ", ")
    file-type (word tgt-neighbor ", ")
    file-type (word tgt-dist ", ")
    file-type (word abundance ", ")
    file-type (word energy-per-capita ", ")
    file-type (word clump-size ", ")
    file-type (word qual-mean ", ")
    file-type (word extraction-rate-mean ", ")
    file-type (word patch-regrowth-interval ", ")
    file-type (word max-move ", ")
    file-type (word who ", ")
    file-type (word group-size ", ")
    file-type (word group-mean-intake ", ")
    file-type (word group-mean-IID ", ")
    file-print group-mean-distance

  ]
  file-close

end

to output_group_networks
  let folder-path "C:\\Users\\Marcy\\Desktop\\socialnetworkoutput"



  ask groups [
    let groupmembers sort group-member-neighbors
  ;; groupmembers is now a list of agents, e.g., (primate 0) (primate 2) (primate 28)

    set-current-directory folder-path
    make_vertices_list groupmembers

    set-current-directory folder-path
    make_csv_network_contact_uw groupmembers

    set-current-directory folder-path
    make_csv_network_contact_weighted groupmembers

    set-current-directory folder-path
    make_csv_network_patchdist groupmembers

  ]


end

to make_vertices_list [groupaslist]

  set-current-directory "vertices" ;; a folder for just the nodes/vertices files
  let file-name (word scenario behaviorspace-run-number who "vertices.csv")
  let idincre 0

  file-open file-name

  file-type "name, "
  file-print "id"

  foreach groupaslist [i ->
    file-type (word [who] of i ", ")
    file-print idincre
    set idincre idincre + 1
  ]


  file-close
end



to make_csv_network_contact_uw [groupaslist]
  ;; this is for network of contacts, unweighted
  ;; this should just be visual-contacts or no visual-contacts
  ;; this is in the group context

  set-current-directory "contactunweighted"

  let file-name (word scenario behaviorspace-run-number who "contact_uw.csv")
  file-open file-name

    ;;column names
  file-type "actor, recipient, "
  file-print "weight"

  foreach groupaslist [i ->
    foreach groupaslist [h ->
      ;;make a row, i as actor, h as recipient
      file-type (word [who] of i ", ")
      file-type (word [who] of h ", ")

      ;; is there a visual-contact link between i and h?
      let link-present [contact-neighbor? i] of h

      ifelse link-present [ ;;if there is a link, add 1
        file-print "1"
      ][
        file-print "0" ;;otherwise add 0
      ]
  ]]

  file-close

  ;;below was how we saved social network as a matrix
;  let groupmembers group-member-neighbors
;
;  nw:set-context groupmembers visual-contacts
;  nw:save-matrix file-name
  ;; this should work for creating binary matrix of visual contacts

end


to make_csv_network_contact_weighted [groupaslist]
  ;; this procedure is within the group context
  ;; this is for a network or how many ticks two agents were in each other's nearest-primates
  set-current-directory "contactweighted"

  let file-name (word scenario behaviorspace-run-number "group" who "contact_wght.csv")
  file-open file-name

  ;;column names
  file-type "actor, recipient, "
  file-print "weight"

  foreach groupaslist [i ->
    foreach groupaslist [h ->
      ;;make a row, i as actor, h as recipient
      file-type (word [who] of i ", ")
      file-type (word [who] of h ", ")

      ;; is there a visual-contact link between i and h?
      let link-present [contact-neighbor? i] of h

      ifelse link-present [ ;;if there is a link, add the freq to csv
        let the-link contact [who] of i [who] of h
        file-print [freq] of the-link
      ][
        file-print "0" ;;otherwise add 0
      ]
  ]]


  ;; below was how we saved social network as a matrix
;  let groupmembers sort group-member-neighbors
;  ;; groupmembers is now a list of agents, e.g., (primate 0) (primate 2) (primate 28)
;
;  file-type " , " ;;leave first cell blank for indentation
;
;  ;; write the column names as each primate in the group
;  foreach but-last groupmembers [ j ->
;    file-type (word j ", ")
;  ]
;  file-print last groupmembers
;
;  ;; now we need to go thru each row
;  foreach groupmembers [i -> ;; the row
;    file-type (word i ", ") ;; row label
;
;   foreach groupmembers [h ->
;     ;; is there a visual-contact link between i and h?
;      let link-present [visual-contact-neighbor? i] of h
;
;      ifelse link-present [
;        let the-link visual-contact [who] of i [who] of h
;        file-type (word [freq] of the-link ", ")
;
;      ][
;        file-type "0, "
;      ]
;
;    ]
;  file-print " "  ;; end row
;  ]



  file-close

end

to make_csv_network_patchdist [groupaslist]
  ;; this procedure is within the group context
  ;; this is for a matrix of the distances between all connected group members
  set-current-directory "patchdistance"

  let file-name (word scenario behaviorspace-run-number "group" who "patchdist.csv")
  file-open file-name

    ;;column names
  file-type "actor, recipient, "
  file-print "weight"

  foreach groupaslist [i ->
    foreach groupaslist [h ->
      ;;make a row, i as actor, h as recipient
      file-type (word [who] of i ", ")
      file-type (word [who] of h ", ")

      let this-distance 0
      ask h [set this-distance distance i]
      file-print this-distance
  ]]
  file-close


end

to-report mean-inter-indiv-dist
  (ifelse any? groups [
  ;make a new kind of link between all group members: groupdyads
  ask groups [
    ask group-member-neighbors [
      create-groupdyads-with other [group-member-neighbors] of myself
    ]
  ]

    report mean [link-length] of groupdyads][report 0])
  ;calculate the link-length of this new kind of link

end


to-report mean-group-size
  (ifelse any? groups [
  report mean [count my-links] of groups
  ][report 0])

end

to-report var-group-size

  (ifelse count groups > 1 [
  report variance [count my-links] of groups
  ][report 0])
end

to-report percent-grouped
  report count primates with [any? group-member-neighbors] / count primates
end


to-report foraging-efficiency-dist
  report mean [total-energy-gained / distance-traveled] of primates

end


to-report foraging-efficiency-time
  report mean [total-energy-gained / ticks] of primates

end

to-report mean-distance-traveled
  report mean [distance-traveled] of primates
end


;;reporters for Ch 3 hypothesis 2: variance over time
to-report var-energy-intake-daily
  report mean [variance energy-intake-rate-list-daily] of primates

end

to-report var-energy-intake-monthly
  report mean [variance energy-intake-rate-list-monthly] of primates
end

to-report mean-percent-time-moving
  report mean [timemove] of primates / ticks
end

to-report var-percent-time-moving
  report variance [timemove] of primates / ticks
end


;;reporters for Ch 3 hypotheis 3: variance among individuals

to-report var-energy-intake
  report variance [total-energy-gained / ticks] of primates
end

to-report n-interactions
  report decidedtofight + sum [avoids] of primates
end


to-report proportion-attacking
  let proportion 0

  let n-attacks decidedtofight
  let n-avoids sum [avoids] of primates

  if n-attacks > 0 or n-avoids > 0 [set proportion n-attacks / (n-attacks + n-avoids)]

  report proportion
end

to make-csv-indiv-energy-rhp
  let folder-path "C:\\Users\\Marcy\\Desktop\\"

  set-current-directory folder-path
  let file-name (word "ch3h3rhpenergy" scenario behaviorspace-run-number ".csv")

  file-open file-name

  ;;column names
  file-type "abundance, "
  file-type "clump, "
  file-type "patch-size, "
  file-type "extraction, "
  file-type "primate-ID, "
  file-type "rhp, "
  file-print "energy-intake"
  file-close


  file-open file-name

  ask primates [
    file-type (word abundance ", ")
    file-type (word clump-size ", ")
    file-type (word qual-mean ", ")
    file-type (word extraction-rate ", ")
    file-type (word who ", ")
    file-type (word rhp ", ")
    file-print total-energy-gained / ticks

  ]

  file-close


end






to run_profiling
profiler:reset
  profiler:start
  repeat 4000 [go]
  profiler:stop
  print profiler:report
  profiler:reset

end
@#$#@#$#@
GRAPHICS-WINDOW
588
10
1644
1067
-1
-1
3.73
1
10
1
1
1
0
1
1
1
-140
140
-140
140
1
1
1
ticks
30.0

SWITCH
29
594
155
627
go-tests-on?
go-tests-on?
1
1
-1000

BUTTON
53
10
108
46
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

SLIDER
12
301
184
334
abundance
abundance
1000
1200000
600000.0
1000
1
NIL
HORIZONTAL

SLIDER
13
342
185
375
clump-size
clump-size
0
1000
501.0
1
1
NIL
HORIZONTAL

SLIDER
13
421
185
454
qual-mean
qual-mean
0
200
60.0
1
1
NIL
HORIZONTAL

SLIDER
12
462
184
495
qual-sd
qual-sd
0
20
20.0
1
1
NIL
HORIZONTAL

BUTTON
35
88
135
121
go (forever)
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
20
643
181
676
resource-tests-on?
resource-tests-on?
1
1
-1000

BUTTON
29
51
140
84
go for one tick
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

SWITCH
30
680
177
713
move-tests-on?
move-tests-on?
1
1
-1000

PLOT
214
115
374
247
pop size
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count primates"
"pen-1" 1.0 0 -13791810 true "" "plot count groups"

PLOT
13
125
173
245
max and min stored energy
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -13840069 true "" "plot mean([stored-energy] of primates)"
"pen-1" 1.0 0 -817084 true "" "plot min([stored-energy] of primates)"

PLOT
234
677
394
797
distance traveled
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot [distance-traveled] of turtle 2"

SLIDER
194
11
366
44
tgt-dist
tgt-dist
1
50
20.0
1
1
NIL
HORIZONTAL

SLIDER
207
59
379
92
tgt-neighbor
tgt-neighbor
-1
9
7.0
1
1
NIL
HORIZONTAL

PLOT
210
387
370
507
groups
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count groups"

MONITOR
299
262
419
307
efficiency - cal/time
mean [total-energy-gained] of primates / ticks
3
1
11

MONITOR
218
335
333
380
efficiency - cal/distance
mean [total-energy-gained] of primates / mean [distance-traveled] of primates
3
1
11

SWITCH
20
513
193
546
group-recog-module?
group-recog-module?
0
1
-1000

SLIDER
21
555
193
588
extraction-rate-mean
extraction-rate-mean
1
20
5.0
1
1
NIL
HORIZONTAL

PLOT
205
507
405
657
group size
ticks
avg primates/group
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "plot 1" "if any? groups [plot mean [count group-member-neighbors] of groups]"
"pen-1" 1.0 0 -7500403 true "" "if any? groups [plot max [count group-member-neighbors] of groups]"

SLIDER
57
734
230
767
extraction-rate-sd
extraction-rate-sd
0
3
1.0
1
1
NIL
HORIZONTAL

SLIDER
55
865
275
898
other-primate-detection-radius
other-primate-detection-radius
5
100
70.0
1
1
NIL
HORIZONTAL

SLIDER
61
919
255
952
resource-detection-radius
resource-detection-radius
5
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
64
971
237
1004
regrowth-rate
regrowth-rate
.5
2
1.0
.1
1
NIL
HORIZONTAL

SLIDER
69
1023
259
1056
patch-regrowth-interval
patch-regrowth-interval
1000
3000
1500.0
100
1
NIL
HORIZONTAL

SLIDER
53
1064
226
1097
movement-noise
movement-noise
10
45
10.0
1
1
NIL
HORIZONTAL

SLIDER
71
827
244
860
max-move
max-move
0
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
21
382
194
415
energy-per-capita
energy-per-capita
0
10000
9000.0
250
1
NIL
HORIZONTAL

SWITCH
8
255
285
288
make_csv_weekly_group_level?
make_csv_weekly_group_level?
0
1
-1000

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
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="test of resource initialization" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1"/>
    <metric>sum [penergy] of patches</metric>
    <metric>map count clumps-list</metric>
    <metric>count resource-patches</metric>
    <metric>count empty-patches</metric>
    <enumeratedValueSet variable="clump-mean">
      <value value="10"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="abundance">
      <value value="1000"/>
      <value value="40000"/>
      <value value="80000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-distance-a">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-distance-lambda">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="1"/>
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="parameterscaletests -" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="abundance">
      <value value="383000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="66"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="240"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="parameterscaletests - uniform" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <metric>mean [mean-daily-distance] of primates</metric>
    <metric>min [mean-daily-distance] of primates</metric>
    <metric>max [mean-daily-distance] of primates</metric>
    <metric>mean [timemove / (timemove + timeeat)] of primates</metric>
    <metric>foraging-efficiency-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="parameterscaletests - clumped sp" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean [mean-daily-distance] of primates</metric>
    <metric>min [mean-daily-distance] of primates</metric>
    <metric>max [mean-daily-distance] of primates</metric>
    <metric>mean [timemove / (timemove + timeeat)] of primates</metric>
    <metric>foraging-efficiency-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="700000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="parameterscaletests - clumped lp" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean [mean-daily-distance] of primates</metric>
    <metric>min [mean-daily-distance] of primates</metric>
    <metric>max [mean-daily-distance] of primates</metric>
    <metric>mean [timemove / (timemove + timeeat)] of primates</metric>
    <metric>foraging-efficiency-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="400000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensitytests - uniform" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensitytests - clumped sp" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean [mean-daily-distance] of primates</metric>
    <metric>min [mean-daily-distance] of primates</metric>
    <metric>max [mean-daily-distance] of primates</metric>
    <metric>mean [timemove / (timemove + timeeat)] of primates</metric>
    <metric>foraging-efficiency-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="700000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensitytests - clumped lp" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean [mean-daily-distance] of primates</metric>
    <metric>min [mean-daily-distance] of primates</metric>
    <metric>max [mean-daily-distance] of primates</metric>
    <metric>mean [timemove / (timemove + timeeat)] of primates</metric>
    <metric>foraging-efficiency-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="400000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="extractiontests - uniform" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean [mean-daily-distance] of primates</metric>
    <metric>min [mean-daily-distance] of primates</metric>
    <metric>max [mean-daily-distance] of primates</metric>
    <metric>mean [timemove / (timemove + timeeat)] of primates</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>mean-group-size</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="extractiontests - clumped sp" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean [mean-daily-distance] of primates</metric>
    <metric>min [mean-daily-distance] of primates</metric>
    <metric>max [mean-daily-distance] of primates</metric>
    <metric>mean [timemove / (timemove + timeeat)] of primates</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="700000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="extractiontests - clumped lp" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean [mean-daily-distance] of primates</metric>
    <metric>min [mean-daily-distance] of primates</metric>
    <metric>max [mean-daily-distance] of primates</metric>
    <metric>mean [timemove / (timemove + timeeat)] of primates</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>mean-group-size</metric>
    <metric>mean-group-size</metric>
    <enumeratedValueSet variable="abundance">
      <value value="400000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensforefftests - LAUSPHE" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensforefftests - HAUSPLE" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="abundance">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensforefftests - LACSPLE" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensforefftests - HACSPLE" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="abundance">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensforefftests - LAULPLE" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensforefftests - HAULPLE" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="abundance">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensforefftests - LACLPLE" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensforefftests - HACLPLE" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="abundance">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensforefftests - LAUSPLE" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensforefftests - HAUSPHE" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="abundance">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensforefftests - LACSPHE" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensforefftests - HACSPHE" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="abundance">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensforefftests - LACLPHE" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensforefftests - HACLPHE" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="abundance">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensforefftests - LAUSPHE (1)" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study1 solitary exp" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [fights] of primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study1 low greg expv6" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [fights] of primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study1 high greg exp" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [fights] of primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensforefftests - 300 HE" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popdensforefftests - 400 LE" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count primates &lt;= 1</exitCondition>
    <metric>mean-group-size</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study1 solitary LA exp" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [freq-conflict] of primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study1 solitary HA exp" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [freq-conflict] of primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study1 low greg LA exp" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [freq-conflict] of primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study1 low greg HA exp" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [freq-conflict] of primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study1 high greg LA exp" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [freq-conflict] of primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study1 high greg HA exp" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [freq-conflict] of primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study2" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [fights] of primates</metric>
    <metric>mean-group-size</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study2v2" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [fights] of primates</metric>
    <metric>mean-group-size</metric>
    <enumeratedValueSet variable="abundance">
      <value value="100000"/>
      <value value="300000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study2v3" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [fights] of primates</metric>
    <metric>mean-group-size</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study2v4" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [fights] of primates</metric>
    <metric>mean-group-size</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study2v5" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [fights] of primates</metric>
    <metric>mean-group-size</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study2v6" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [fights] of primates</metric>
    <metric>mean-group-size</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study1 solitary expv6" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [fights] of primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study1 high greg expv6" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [fights] of primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study2v6-durationTest8000" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [fights] of primates</metric>
    <metric>mean-group-size</metric>
    <metric>timer</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study2v6-durationTest16000" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="16000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [fights] of primates</metric>
    <metric>mean-group-size</metric>
    <metric>timer</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study2v6-durationTest32000" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="32000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [fights] of primates</metric>
    <metric>mean-group-size</metric>
    <metric>timer</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study2v6-durationTest64000" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="64000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [fights] of primates</metric>
    <metric>mean-group-size</metric>
    <metric>timer</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study2v6-durationTest128000" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="128000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [fights] of primates</metric>
    <metric>mean-group-size</metric>
    <metric>timer</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study2v6-durationTest1024000" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1024000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [fights] of primates</metric>
    <metric>mean-group-size</metric>
    <metric>timer</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="study2withingroup" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="8000"/>
    <metric>foraging-efficiency-time</metric>
    <metric>mean [distance-traveled] of primates</metric>
    <metric>mean [fights] of primates</metric>
    <metric>mean-group-size</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="testofgregparams" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8000"/>
    <metric>mean [percent-t-in-prox] of primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="0.5"/>
      <value value="1"/>
      <value value="1.5"/>
      <value value="2"/>
      <value value="2.5"/>
      <value value="3"/>
      <value value="3.5"/>
      <value value="4"/>
      <value value="4.5"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="jan2022ch2study1-sol" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8000"/>
    <metric>mean [percent-t-in-prox] of primates</metric>
    <metric>mean [foraging-efficiency-time] of primates</metric>
    <metric>mean [distance-traveled] of primates / 8000</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="jan2022ch2study1-lowestgreg" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8000"/>
    <metric>mean [percent-t-in-prox] of primates</metric>
    <metric>mean [foraging-efficiency-time] of primates</metric>
    <metric>mean [distance-traveled] of primates / 8000</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="jan2022ch2study1-midlowgreg" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8000"/>
    <metric>mean [percent-t-in-prox] of primates</metric>
    <metric>mean [foraging-efficiency-time] of primates</metric>
    <metric>mean [distance-traveled] of primates / 8000</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="jan2022ch2study1-midgreg" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8000"/>
    <metric>mean [percent-t-in-prox] of primates</metric>
    <metric>mean [foraging-efficiency-time] of primates</metric>
    <metric>mean [distance-traveled] of primates / 8000</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="jan2022ch2study1-midhighgreg" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8000"/>
    <metric>mean [percent-t-in-prox] of primates</metric>
    <metric>mean [foraging-efficiency-time] of primates</metric>
    <metric>mean [distance-traveled] of primates / 8000</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="jan2022ch2study1-highestgreg" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8000"/>
    <metric>mean [percent-t-in-prox] of primates</metric>
    <metric>mean [foraging-efficiency-time] of primates</metric>
    <metric>mean [distance-traveled] of primates / 8000</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="jan2022ch2study2-midgreg" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8000"/>
    <metric>mean [percent-t-in-prox] of primates</metric>
    <metric>mean [foraging-efficiency-time] of primates</metric>
    <metric>mean [distance-traveled] of primates / 8000</metric>
    <metric>mean-group-size</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="jan2022ch2study2-midgreg (1)" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="8000"/>
    <metric>mean [percent-t-in-prox] of primates</metric>
    <metric>mean [foraging-efficiency-time] of primates</metric>
    <metric>mean [distance-traveled] of primates / 8000</metric>
    <metric>mean-group-size</metric>
    <enumeratedValueSet variable="abundance">
      <value value="300000"/>
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-mean">
      <value value="5"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate">
      <value value="1"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="debug greg var" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>var-energy-intake-quarterly</metric>
    <metric>var-energy-intake-biweek</metric>
    <enumeratedValueSet variable="abundance">
      <value value="333000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-freq">
      <value value="1000"/>
      <value value="1500"/>
      <value value="2000"/>
      <value value="2500"/>
      <value value="3000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="gregchh1p2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="abundance">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-pop-primates">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="0.5"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-freq">
      <value value="1500"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="pattern matching" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4300"/>
    <metric>mean [timemove] of primates</metric>
    <metric>mean [timeeat] of primates</metric>
    <metric>count primates</metric>
    <metric>mean [travel-distance] of primates</metric>
    <steppedValueSet variable="abundance" first="50000" step="200000" last="1000000"/>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="extraction-rate-mean" first="1" step="2" last="6"/>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-dist" first="0.5" step="20" last="50"/>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <steppedValueSet variable="qual-mean" first="5" step="50" last="200"/>
    <enumeratedValueSet variable="movement-noise">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-neighbor" first="0" step="2" last="7"/>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="clump-size" first="1" step="250" last="501"/>
    <steppedValueSet variable="patch-regrowth-freq" first="300" step="500" last="3000"/>
  </experiment>
  <experiment name="greg ch heat map" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4300"/>
    <metric>foraging efficiency</metric>
    <metric>variance in intake</metric>
    <enumeratedValueSet variable="abundance">
      <value value="500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="extraction-rate-mean" first="3" step="3" last="12"/>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-dist" first="0.5" step="5" last="50"/>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-neighbor" first="0" step="1" last="12"/>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="250"/>
    </enumeratedValueSet>
    <steppedValueSet variable="patch-regrowth-interval" first="300" step="300" last="3000"/>
  </experiment>
  <experiment name="4A" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4300"/>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>var-energy-intake-daily</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean [timemove] of primates / ticks</metric>
    <metric>count primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="8000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-dist" first="0.5" step="5" last="50"/>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-neighbor" first="0" step="1" last="11"/>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="4B, 7A" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4300"/>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>var-energy-intake-daily</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean [timemove] of primates / ticks</metric>
    <metric>count primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="8000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="extraction-rate-mean" first="4" step="0.7" last="11"/>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="500"/>
    </enumeratedValueSet>
    <steppedValueSet variable="patch-regrowth-interval" first="1000" step="181" last="3000"/>
  </experiment>
  <experiment name="4C" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4300"/>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>var-energy-intake-daily</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean [timemove] of primates / ticks</metric>
    <metric>count primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="8000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="extraction-rate-mean" first="4" step="0.7" last="11"/>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="qual-mean" first="25" step="11" last="150"/>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="4D" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4300"/>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>var-energy-intake-daily</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean [timemove] of primates / ticks</metric>
    <metric>count primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="8000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="extraction-rate-mean" first="4" step="0.7" last="11"/>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-neighbor" first="0" step="1" last="11"/>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="4E" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4300"/>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>var-energy-intake-daily</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean [timemove] of primates / ticks</metric>
    <metric>count primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="8000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-neighbor" first="0" step="1" last="11"/>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="500"/>
    </enumeratedValueSet>
    <steppedValueSet variable="patch-regrowth-interval" first="1000" step="181" last="3000"/>
  </experiment>
  <experiment name="4F, 7C" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4300"/>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>var-energy-intake-daily</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean [timemove] of primates / ticks</metric>
    <metric>count primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="8000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-dist" first="0.5" step="5" last="50"/>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="500"/>
    </enumeratedValueSet>
    <steppedValueSet variable="patch-regrowth-interval" first="1000" step="181" last="3000"/>
  </experiment>
  <experiment name="7B" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4300"/>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>var-energy-intake-daily</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean [timemove] of primates / ticks</metric>
    <metric>count primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="8000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="extraction-rate-mean" first="4" step="0.7" last="11"/>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-dist" first="0.5" step="5" last="50"/>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="5A" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4300"/>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>var-energy-intake-daily</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean [timemove] of primates / ticks</metric>
    <metric>count primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="4500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="9.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-dist" first="0.5" step="5" last="50"/>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-neighbor" first="0" step="1" last="11"/>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="2750"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="8B" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4300"/>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>var-energy-intake-daily</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean [timemove] of primates / ticks</metric>
    <metric>count primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="4500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="9.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-dist" first="0.5" step="5" last="50"/>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="500"/>
    </enumeratedValueSet>
    <steppedValueSet variable="patch-regrowth-interval" first="2500" step="45" last="3000"/>
  </experiment>
  <experiment name="5B" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4300"/>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>var-energy-intake-daily</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean [timemove] of primates / ticks</metric>
    <metric>count primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="9000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-dist" first="0.5" step="5" last="50"/>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-neighbor" first="0" step="1" last="11"/>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="5C" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4300"/>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>var-energy-intake-daily</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean [timemove] of primates / ticks</metric>
    <metric>count primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="9000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="qual-mean" first="25" step="11" last="150"/>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="clump-size" first="1" step="90" last="1000"/>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="8A" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4300"/>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>var-energy-intake-daily</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean [timemove] of primates / ticks</metric>
    <metric>count primates</metric>
    <enumeratedValueSet variable="abundance">
      <value value="500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="4500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="extraction-rate-mean" first="7" step="0.3" last="11"/>
    <enumeratedValueSet variable="qual-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-dist" first="0.5" step="5" last="50"/>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="2750"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="group size tgtneighbor x clump" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="600000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1750"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="7.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="6500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="22"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-neighbor" first="3" step="1" last="11"/>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="clump-size" first="1" step="100" last="1001"/>
  </experiment>
  <experiment name="group size tgtneighbor x tgtdist" repetitions="3" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="600000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="6500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-dist" first="1" step="4" last="41"/>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-neighbor" first="3" step="1" last="11"/>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="250"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="group size tgtneighbor x energypercapita" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="600000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="energy-per-capita" first="4000" step="500" last="9000"/>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-neighbor" first="3" step="1" last="11"/>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="250"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="group size tgtneighbor x clumpsize" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="600000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="6500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-neighbor" first="3" step="1" last="11"/>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="clump-size" first="1" step="50" last="501"/>
  </experiment>
  <experiment name="group size tgtneighbor x abundance" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <steppedValueSet variable="abundance" first="200000" step="100000" last="1200000"/>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="6500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-neighbor" first="3" step="1" last="11"/>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="250"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="group size energypercapita x abundance" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <steppedValueSet variable="abundance" first="200000" step="100000" last="1200000"/>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="energy-per-capita" first="4000" step="500" last="9000"/>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="250"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="group size clumpsize x energypercapita" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="600000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="energy-per-capita" first="4000" step="500" last="9000"/>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="clump-size" first="1" step="50" last="501"/>
  </experiment>
  <experiment name="group size clumpsize x abundance" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <steppedValueSet variable="abundance" first="200000" step="100000" last="1200000"/>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="6500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="clump-size" first="1" step="50" last="501"/>
  </experiment>
  <experiment name="group size regrowth x speed" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="600000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="patch-regrowth-interval" first="500" step="200" last="2500"/>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="6500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="max-move" first="10" step="2" last="30"/>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="250"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="group size patch qual x speed" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="600000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="6500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <steppedValueSet variable="qual-mean" first="25" step="12.5" last="150"/>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="max-move" first="10" step="2" last="30"/>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="250"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="group size clump x speed" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="600000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="6500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="max-move" first="10" step="2" last="30"/>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="clump-size" first="1" step="50" last="501"/>
  </experiment>
  <experiment name="group size regrowth x patch qual" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="600000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="patch-regrowth-interval" first="500" step="250" last="2500"/>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="6500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <steppedValueSet variable="qual-mean" first="25" step="12.5" last="150"/>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="250"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="group size regrowth x clump" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="600000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="patch-regrowth-interval" first="500" step="200" last="2500"/>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="6500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="clump-size" first="1" step="50" last="501"/>
  </experiment>
  <experiment name="group size patch qual x clump" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="600000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="6500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <steppedValueSet variable="qual-mean" first="25" step="12.5" last="150"/>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="clump-size" first="1" step="50" last="501"/>
  </experiment>
  <experiment name="group size p1-2 p1-3" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <steppedValueSet variable="abundance" first="200000" step="225000" last="1200000"/>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="energy-per-capita" first="4000" step="1250" last="9000"/>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-dist" first="1" step="10" last="41"/>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-neighbor" first="3" step="2" last="11"/>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="clump-size" first="1" step="125" last="501"/>
  </experiment>
  <experiment name="group size extraction rate mean x speed" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="600000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
    <steppedValueSet variable="extraction-rate-mean" first="2" step="1" last="8"/>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="6500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="max-move" first="10" step="2" last="30"/>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="250"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="group size extraction rate mean x clump size" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="600000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
    <steppedValueSet variable="extraction-rate-mean" first="2" step="1" last="8"/>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="6500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="clump-size" first="1" step="50" last="501"/>
  </experiment>
  <experiment name="group size extraction rate mean x energy per capita" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="600000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
    <steppedValueSet variable="extraction-rate-mean" first="2" step="1" last="8"/>
    <steppedValueSet variable="energy-per-capita" first="4000" step="500" last="9000"/>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="250"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="group size extraction rate mean x regrowth" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="600000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="patch-regrowth-interval" first="500" step="200" last="2500"/>
    <steppedValueSet variable="extraction-rate-mean" first="2" step="1" last="8"/>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="6500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-dist">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="250"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="group size target distance x clump size" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="600000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="6500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-dist" first="1" step="4" last="41"/>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="clump-size" first="1" step="50" last="501"/>
  </experiment>
  <experiment name="group size target distance x abundance" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <steppedValueSet variable="abundance" first="200000" step="100000" last="1200000"/>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="6500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-dist" first="1" step="4" last="41"/>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="250"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="group size target distance x extraction rate mean" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <postRun>make_csv_group-level_data</postRun>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <enumeratedValueSet variable="abundance">
      <value value="600000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1500"/>
    </enumeratedValueSet>
    <steppedValueSet variable="extraction-rate-mean" first="2" step="1" last="8"/>
    <enumeratedValueSet variable="energy-per-capita">
      <value value="6500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-dist" first="1" step="4" last="41"/>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tgt-neighbor">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="250"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="group size p1-2 clump 63" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4300"/>
    <metric>mean-group-size</metric>
    <metric>var-group-size</metric>
    <metric>percent-grouped</metric>
    <metric>foraging-efficiency-time</metric>
    <metric>foraging-efficiency-dist</metric>
    <metric>mean-distance-traveled</metric>
    <metric>mean-percent-time-moving</metric>
    <metric>var-energy-intake-monthly</metric>
    <metric>mean-inter-indiv-dist</metric>
    <steppedValueSet variable="abundance" first="200000" step="225000" last="1200000"/>
    <enumeratedValueSet variable="extraction-rate-sd">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-regrowth-interval">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extraction-rate-mean">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="energy-per-capita" first="4000" step="1250" last="9000"/>
    <enumeratedValueSet variable="qual-sd">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-dist" first="1" step="10" last="41"/>
    <enumeratedValueSet variable="other-primate-detection-radius">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qual-mean">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-noise">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="move-tests-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tgt-neighbor" first="3" step="2" last="11"/>
    <enumeratedValueSet variable="resource-detection-radius">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-move">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="regrowth-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-recog-module?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-size">
      <value value="63"/>
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
