library(dplyr)
library(tidyr)

rm(list = ls())

payoutpot=2265791546          

# Attacker faction name

attacker_faction="Monarch Retardation"

# FAS faction

FAS_faction="Fallen Angels Syndicate"

# Early hit cutoff point; enter in TCT

early_hit_point="2026-09-25 14:45:00"


# Current member roster

current_members=read.csv("C:\\Users\\Harry Huang\\Downloads\\members_report_20260926-1624.csv")

# Read in file from debrief with complete attack details here; must use double slashes, not single slashes
new_data=read.csv("C:\\Users\\Harry Huang\\Downloads\\Syn WarAttacks - MR Attacks (4).csv")





# Setting special outcomes to attacked

new_data$result[new_data$result=="Special"]<-"Attacked"


# Chain count

new_data <- new_data[order(new_data$ended_tct), ]
rownames(new_data) <- NULL

# Converting to date object

new_data$ended_tct <- as.POSIXct(new_data$ended_tct, format = "%d %b %H:%M:%S", tz = "UTC")
new_data$started_tct <- as.POSIXct(new_data$started_tct, format = "%d %b %H:%M:%S", tz = "UTC")


# Assigning enemy faction to any incoming war hits (to override stealthed hits)
new_data[new_data$war_modifier==2 & new_data$defender_faction==FAS_faction, ]$attacker_faction=attacker_faction

# Throwing out all incoming attacks not from the enemy faction 
new_data=new_data[new_data$attacker_faction==attacker_faction |  new_data$attacker_faction==FAS_faction, ]


# Filling in NAs in some columns with 1

new_data <- new_data %>%
  mutate(across(c(retaliation_modifier, group_modifier, overseas_modifier), ~ replace_na(.x, 1)))


# Whether or not to allow attacks in the following chain to count towards the hit quota

ChainQuota=FALSE
# 
# # Chain report
# chaindata=read.csv("")
# 
# # Trim white space
# chaindata$Name <- trimws(chaindata$Name)

# Hit quota is 1% of war hits

hitquota=round(0.01*length(which(new_data$war_modifier==2)))

outsidebonushits=which((new_data$chain_hit==5000 | new_data$chain_hit==2500 | new_data$chain_hit==1000 | new_data$chain_hit==500 | new_data$chain_hit==250 | new_data$chain_hit==100 | new_data$chain_hit==50 | new_data$chain_hit==25) & new_data$war_modifier==1)


# Creating dataframe to hold info by player

attacksbyplayer<-data.frame(current_members$Name) 

colnames(attacksbyplayer)[colnames(attacksbyplayer) == 'current_members.Name'] <- 'name'

attacksbyplayer$avgwarFF<-NA
attacksbyplayer$avgnonwarFF<-NA
attacksbyplayer$rawwarscore<-NA
attacksbyplayer$warhitcount<-NA
attacksbyplayer$nonwarhitcount<-NA
attacksbyplayer$totalhitcount<-NA
attacksbyplayer$warpercentshare<-0
attacksbyplayer$nonwarpercentshare<-0
attacksbyplayer$fullbleed<-NA
attacksbyplayer$discountedbleed<-NA
attacksbyplayer$outsidebonusfine<-rep(0, nrow(attacksbyplayer))
attacksbyplayer$bleed_pay<-NA
attacksbyplayer$non_war_hit_pay<-NA
attacksbyplayer$war_hit_pay<-NA
attacksbyplayer$hosp_charge<-0
  

# Bonus fines
for (i in outsidebonushits){
  if(new_data[i, ]$chain==25){
attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine=2500000+attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine
  }
  else if(new_data[i, ]$chain==50){
    attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine=5000000+attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine
  }
  else if(new_data[i, ]$chain==100){
    attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine=15000000+attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine
  }
  else if(new_data[i, ]$chain==250){
    attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine=25000000+attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine
  }
  else if(new_data[i, ]$chain==500){
    attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine=50000000+attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine
  }
  else if(new_data[i, ]$chain==1000){
    attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine=100000000+attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine
  }
  else if(new_data[i, ]$chain==2500){
    attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine=200000000+attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine
  }
  else if(new_data[i, ]$chain==5000){
    attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine=300000000+attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine
  }
}


# Removing incoming attacks and saving for bleed calculations

bleed_data=new_data

bleed_data=bleed_data[bleed_data$attacker_faction!=FAS_faction, ]

internal_hosp_data=new_data[new_data$defender_faction==FAS_faction & new_data$attacker_faction=FAS_faction, ]


new_data=new_data[new_data$attacker_faction==FAS_faction, ]


# Stripping out chain bonuses except for 10
exclude_vals <- c(5000, 2500, 1000, 500, 250, 100, 50, 25)

new_data <- new_data[!(new_data$chain_hit %in% exclude_vals), ]



# removing chain respect multiplier
new_data$respect=new_data$respect/pmax(1, (0.25*log10(new_data$chain_hit)+0.75))


# Redistributing assist respect

for (i  in which(new_data$group_modifier!=1)){

  #Calculating the number of assists
  assistnumber=log(6-4*new_data$group_modifier[i])/log(0.5)+1
  
  # Finding of the attacks with the same defender, which ones are the closest ones which are assists
  assists=which(new_data$defender==new_data$defender[i] & new_data$result %in% c("Assist", "Lost") )[order(abs(which(new_data$defender==new_data$defender[i] & new_data$result=="Assist")-i))][1:(assistnumber)]

  
  # Redistributing group attack bonus to assists
  new_data$respect[assists]=(new_data$respect[i]-(new_data$respect[i]/new_data$group_modifier[i]) )/assistnumber
  if(new_data$war_modifier[i]==2){
  new_data$war_modifier[assists]=2 
  }
  
  for (j in seq_len(assistnumber)){
    # Searching for attacks with the same attacker and defender and assigning those FF to the assist
    samepair <- which(new_data$attacker == new_data$attacker[assists[j]] &
                        new_data$defender == new_data$defender[assists[j]])
    # don't let the assist row match against itself
    samepair <- samepair[samepair != assists[j]]
    
    if (length(samepair) > 0 && !any(is.na(new_data$fair_fight[samepair]))) {
      new_data$fair_fight[assists[j]] <- mean(new_data$fair_fight[samepair])
    } else {
      # If no such matching pair exists, assign the FF of the main attack
      new_data$fair_fight[assists[j]] <- new_data$fair_fight[i]
    }
  }
  
  # Removing group attack bonus from respect gain
  new_data$respect[i]=new_data$respect[i]/new_data$group_modifier[i]
  
  rm(samepair)
}

attack_gaps_data=new_data[!new_data$result %in% c("Lost"), ]


attackgaps=array(-999, c(nrow(new_data), 4))

# calculating time between attack initiation and previous four attacks (which are not assists)
for(k in 1:4){
attackgaps[which(attack_gaps_data$result!="Assist")[-seq(1, k, by=1) ],k]=attack_gaps_data[attack_gaps_data$result!="Assist", ]$started_tct[(k+1):nrow(attack_gaps_data[attack_gaps_data$result!="Assist", ])]-
  attack_gaps_data[attack_gaps_data$result!="Assist", ]$ended_tct[1:(nrow(attack_gaps_data[attack_gaps_data$result!="Assist", ])-k)]
}

# finding chain saves
# attack starts within 1 min 45s chain timer or started within eight seconds of the end of a chain saving attack that started within 1m chain timer
# will not account for any chain saves past the fourth
 
new_data$chainsave=rep(FALSE, nrow(new_data))
new_data$chainsave[which((attackgaps[ ,1]>180) | (dplyr::lag(attackgaps[ , 1], 1)>240 & attackgaps[ ,2]<8) | (dplyr::lag(attackgaps[ , 1], 2)>240 & attackgaps[ ,3]<8) | (dplyr::lag(attackgaps[ , 1], 3)>240 & attackgaps[ ,4]<8))]=TRUE

# Preserving initial respect gain values for diagnostics
temp=new_data$respect


# Implementing early hit bonus

new_data$early_hit_bonus=0.25*( new_data$ended_tct<as.numeric(as.POSIXct(early_hit_point, tz="GMT")) )+1






for (i in 1:length(attacksbyplayer$name) ){

# Extracting attacks for each player
playerattacks=new_data[new_data$attacker==attacksbyplayer$name[i], ]
attacksbyplayer$rawwarscore[i]=sum(playerattacks[playerattacks$war_modifier==2, ]$respect)  
# Recalculate retals/hosps/chain saves war scores as if they were x3 FF
playerattacks$respect[ (playerattacks$chainsave==TRUE | playerattacks$retaliation_modifier==1.5 | playerattacks$result=="Hospitalized" | playerattacks$overseas_modifier==1.25) & playerattacks$war_modifier==2]=
  3*playerattacks$respect[ (playerattacks$chainsave==TRUE | playerattacks$retaliation_modifier==1.5 | playerattacks$result=="Hospitalized" | playerattacks$overseas_modifier==1.25) & playerattacks$war_modifier==2]/
  playerattacks$fair_fight[ (playerattacks$chainsave==TRUE | playerattacks$retaliation_modifier==1.5 | playerattacks$result=="Hospitalized" | playerattacks$overseas_modifier==1.25) & playerattacks$war_modifier==2]


# Extracting incoming attacks for each player
playerdefends=new_data[new_data$defender==attacksbyplayer$name[i], ]



# Calculating avg FF excluding retals/hosps/chain saves for spreadsheet
# Should probably think more about how to do this in a reasonable way...
attacksbyplayer$avgwarFF[i]=mean(playerattacks$fair_fight[(playerattacks$chainsave==FALSE | playerattacks$retaliation_modifier!=1.5 | playerattacks$result!="Hospitalized") & playerattacks$war_modifier==2])
attacksbyplayer$avgnonwarFF[i]=mean(playerattacks$fair_fight[(playerattacks$chainsave==FALSE | playerattacks$retaliation_modifier!=1.5 |playerattacks$result!="Hospitalized") & playerattacks$war_modifier==1])


# Setting fair_fight for retals/hosps/overseas_modifier/chain saves to 3 so that they get scaled accordingly
playerattacks$fair_fight[ (playerattacks$chainsave==TRUE | playerattacks$retaliation_modifier==1.5 | playerattacks$overseas_modifier== 1.25 | playerattacks$result=="Hospitalized") & playerattacks$war_modifier==2]=3
# Scaling up FF effect by mapping x1 to x3 multiplier to x1 to x6 multiplier 
attacksbyplayer$adjwarscore[i]=sum(playerattacks$respect[playerattacks$war_modifier==2 ]*2.5*(playerattacks$fair_fight[playerattacks$war_modifier==2]-0.6)*playerattacks[playerattacks$war_modifier==2, ]$early_hit_bonus)

if(is.finite(attacksbyplayer$adjwarscore[i])==FALSE){
  browser()
}

# Non war hits that are not chain saves that are above the hit quota or five, whichever is greater are not counted
# Non war hits that are not in a chain are not counted
# checking hits less than 10 chain

subset_rows <- playerattacks$chain < 10 & playerattacks$war_modifier == 1
subset_codes <- playerattacks$code[subset_rows]

if (length(subset_codes) > 0) {
  for (j in seq_along(subset_codes)) {

    code_j <- subset_codes[j]
    idx <- which(new_data$code == code_j)
    
    if (nrow(new_data)>idx + 10){ 
      if(new_data[idx + 10, ]$chain < 10) {
      playerattacks <- playerattacks[playerattacks$code != code_j, ]
    }
    } else if (nrow(new_data)<idx + 10){
      playerattacks <- playerattacks[playerattacks$code != code_j, ]
  }
  }
}


# counting war and non war hits
attacksbyplayer$warhitcount[i]=nrow(playerattacks[playerattacks$war_modifier==2, ])

attacksbyplayer$nonwarhitcount[i]=nrow(playerattacks[playerattacks$war_modifier==1 , ])


# Applying FF effect to non war hits, allowing chain saves and self-hits to be like x3 FF 

# We are capping non war hits that are not chain saves, not assists or losses and not own hits on FAS and not below chain hit 10
# We choose the best FF non war hits to retain

playerattacks$fair_fight[ playerattacks$chainsave==TRUE & playerattacks$war_modifier==1]=3
playerattacks$fair_fight[ playerattacks$attacker_faction==FAS_faction & playerattacks$defender_faction==FAS_faction]=3

capped_subset <- playerattacks[playerattacks$war_modifier==1 & playerattacks$chainsave==FALSE &
                                 playerattacks$result %in% c("Attacked","Mugged","Hospitalized") &
                                 !playerattacks$defender_faction==FAS_faction, ]

uncapped_subset <- playerattacks[playerattacks$war_modifier==1 &
                                    playerattacks$result %in% c("Attacked","Mugged","Hospitalized") &
                                    !playerattacks$defender_faction==FAS_faction & (playerattacks$chainsave==TRUE | playerattacks$chain_hit<10), ]


if( attacksbyplayer$nonwarhitcount[i]!=0 & nrow(capped_subset) <= max(hitquota, 5) ){
  
  attacksbyplayer$adjnonwarhitcount[i] <- sum(2.5*(capped_subset$fair_fight-0.6)*capped_subset$early_hit_bonus) +
    sum(2.5*(uncapped_subset$fair_fight-0.6)*uncapped_subset$early_hit_bonus)
  
} else if (attacksbyplayer$nonwarhitcount[i]!=0 & nrow(capped_subset) > max(hitquota, 5)){
  
  top_rows <- capped_subset[order(capped_subset$fair_fight, decreasing=TRUE)[1:max(hitquota, 5)], ]
  attacksbyplayer$adjnonwarhitcount[i] <- sum(2.5*(top_rows$fair_fight-0.6)*top_rows$early_hit_bonus) +
    sum(2.5*(uncapped_subset$fair_fight-0.6)*uncapped_subset$early_hit_bonus)
  
} else {
  attacksbyplayer$adjnonwarhitcount[i] <- 0
}





attacksbyplayer$mug[i]=nrow(playerattacks[playerattacks$result=="Mugged", ])

# Fining for mugs
if(attacksbyplayer$mug[i]==0){
  attacksbyplayer$mugfine[i]=0
}
else if(attacksbyplayer$mug[i]==1){
  attacksbyplayer$mugfine[i]=3000000
} else if(attacksbyplayer$mug[i]==2){
  attacksbyplayer$mugfine[i]=6000000
} else if(attacksbyplayer$mug[i]>2){
  attacksbyplayer$mugfine[i]=10000000
}

# Attacks on own faction are counted as x3 FF

attacksbyplayer$adjnonwarhitcount[i]=attacksbyplayer$adjnonwarhitcount[i]+
  6*nrow(playerattacks[playerattacks$defender_faction==FAS_faction & playerattacks$result=="Hospitalized", ] )

}





attacksbyplayer$totalhitcount=attacksbyplayer$nonwarhitcount+attacksbyplayer$warhitcount

# Removing anyone who failed the minimum quota from the payout
if(ChainQuota==FALSE){
  passedquota=attacksbyplayer$totalhitcount>=hitquota
} else if(ChainQuota==TRUE){
  idx <- match(attacksbyplayer$name, chaindata$Name)
  idx <- idx[!is.na(idx)]
  
  chaindata_ordered <- chaindata[idx, ]
  
  
  passedquota=(chaindata_ordered$Number.of.Hits-chaindata_ordered$War.Hits+attacksbyplayer$totalhitcount)>=hitquota

  attacksbyplayer$warpercentshare[!passedquota]=0
  attacksbyplayer$nonwarpercentshare[!passedquota]=0  
}

# Quota bonus applies either for hit count OR zero bleed (zero bleed is being checked by verifying that no hits have that person as the defender)
attacksbyplayer$quota_bonus= ( attacksbyplayer$totalhitcount>=(5*hitquota) ) | !(attacksbyplayer$name %in% unique(bleed_data[bleed_data$result %in% c("Attacked","Mugged","Hospitalized"),]$defender) )

attacksbyplayer$warpercentshare[passedquota] <-
  attacksbyplayer$adjwarscore[passedquota]/sum(attacksbyplayer$adjwarscore[passedquota])
attacksbyplayer$nonwarpercentshare[passedquota] <-
  attacksbyplayer$adjnonwarhitcount[passedquota]/sum(attacksbyplayer$adjnonwarhitcount[passedquota]) 


attacksbyplayer$passedquota=passedquota


# Bleed pot


bleed_data$prior_attacks=NA

# Assigning a Boolean for whether the defender was interrupted or was within 120 seconds of attack ending


for (i in 1:nrow(bleed_data) ){

  time=bleed_data$started_tct[i]
  defender=bleed_data$defender[i]

  prior_attacks=(nrow(bleed_data[time>bleed_data$started_tct & bleed_data$ended_tct>=(time-120) & bleed_data$attacker==defender, ])!=0) | bleed_data$result[i]=="Interrupted"
  bleed_data$prior_attacks[i]=prior_attacks
}



# Counting full bleeds and discounted bleeds

for (i in attacksbyplayer$name){
  attacksbyplayer$fullbleed[attacksbyplayer$name==i]=nrow(bleed_data[bleed_data$prior_attacks==FALSE & bleed_data$defender==i & bleed_data$result %in% c("Attacked", "Mugged", "Hospitalized") , ])
  attacksbyplayer$discountedbleed[attacksbyplayer$name==i]=nrow(bleed_data[ bleed_data$prior_attacks==TRUE & bleed_data$defender==i & bleed_data$result %in% c("Attacked", "Mugged", "Hospitalized") , ])
  
}

attacksbyplayer$totalbleed=attacksbyplayer$fullbleed+0.2*attacksbyplayer$discountedbleed


# The person at the 25% percentile will be paid exactly zero

zero_point=floor(nrow(attacksbyplayer)/4)

zero_person=attacksbyplayer[order(attacksbyplayer$totalbleed, decreasing=TRUE), ]$name[zero_point]




# Note that nrow(attacksbyplayer) is the member count


# The war pot's share of the hit pot, given that average war attacks are paid 50% more than the average non war attack
warpotshare=(sum(attacksbyplayer[attacksbyplayer$passedquota==TRUE, ]$warhitcount)*1.5)/    # numerator: 1.5x-weighted total war hits (quota-passers only)
  (sum(attacksbyplayer[attacksbyplayer$passedquota==TRUE, ]$nonwarhitcount)+1.5*sum(attacksbyplayer[attacksbyplayer$passedquota==TRUE, ]$warhitcount))  # denominator: non-war hits + 1.5x-weighted war hits (quota-passers only)

war_hit_count=sum(attacksbyplayer[attacksbyplayer$passedquota==TRUE, ]$warhitcount)  # total war hits by quota-passers; denominator for per-hit bleed pay
N=nrow(attacksbyplayer)  # total member count, used to scale flat per-person adjustments to a pot-wide total
sorted_players=attacksbyplayer[order(attacksbyplayer$totalbleed, decreasing=TRUE), ]  # members ranked from highest to lowest total bleed, used to walk the zero point down if needed

max_totalbleed=max(attacksbyplayer$totalbleed)  # highest total bleed in the faction; whoever holds this always has the most negative raw pay
min_totalbleed=min(attacksbyplayer$totalbleed)  # lowest total bleed in the faction
bleed_spread=max(max_totalbleed-min_totalbleed, 1)  # width of the bleed distribution; floored at 1 to avoid a zero-width edge case (e.g. everyone tied)

# Actual (floor-clamped) total bleed pot payout, given a candidate hit_pot_share h,
# the calibration player's totalbleed, and a flat downward shift M.
# avg_pay_per_hit restores the warpotshare factor from your original formula --
# the bleed pot is funded out of the war pot's share of the hit pot.
clamped_bleed_total=function(h, totalbleed_zero, M){  # h = candidate hit_pot_share; totalbleed_zero = bleed count of the calibration player; M = flat downward shift
  avg_pay_per_hit=h*warpotshare*payoutpot/war_hit_count  # $ per unit of totalbleed, scaled by hit pot share, war pot share, and total pot size
  base_bleed_pay=totalbleed_zero*avg_pay_per_hit  # the flat baseline bleed pay everyone starts from, calibrated so the zero-point player nets to 0 before M
  raw_pay=base_bleed_pay-attacksbyplayer$totalbleed*avg_pay_per_hit  # each player's bleed pay before the M shift and before flooring
  sum(pmax(raw_pay-M, -10000000))  # apply the flat M shift, floor each player's pay at -10m, then sum across all members -- matches your downstream pmax() usage
}

equilibrium_h=function(totalbleed_zero, M){  # solves for the h that is self-consistent with its own clamped bleed total
  
  h_upper=max(10, 20*war_hit_count/(warpotshare*bleed_spread))  # generous search bound for h, scaled up when war_hit_count is small or bleed_spread/warpotshare are small (both inflate avg_pay_per_hit)
  g=function(h) h-(1-clamped_bleed_total(h, totalbleed_zero, M)/payoutpot)  # root-finding function: zero when h equals 1 minus the clamped bleed total as a fraction of payoutpot
  uniroot(g, lower=-h_upper, upper=h_upper, extendInt="yes", maxiter=2000)$root  # numerically solve for h; extendInt widens the bracket automatically if needed, maxiter gives it room to converge
}

# Bleed pay downward adjustment
M=0  # initialize the flat downward shift to zero (no adjustment) before the loop begins

repeat{  # loop lets zero_point be pushed further down the sorted list if the -10m floor makes 0.6 hit_pot_share unreachable at the current zero_person
  
  totalbleed_zero=attacksbyplayer$totalbleed[attacksbyplayer$name==zero_person]  # look up the current calibration player's total bleed
  
  h0=equilibrium_h(totalbleed_zero, 0)  # unconstrained equilibrium hit_pot_share with no M adjustment applied, but still accounting for anyone already floored at -10m
  
  if(h0>=0.6){  # if the hit pot already clears 60% without any bleed pay cut
    hit_pot_share=h0  # keep the naturally higher share (never force it down to exactly 0.6)
    M=0  # no adjustment needed
    break  # exit the loop, done
  }
  
  h_max_achievable=1-(N*(-10000000))/payoutpot  # the ceiling on hit_pot_share if literally every member were floored at -10m bleed pay
  
  if(h_max_achievable<0.6){  # if even the most extreme possible adjustment still can't reach 60%
    zero_point=zero_point+1  # move the zero point one rank further down the sorted (highest-to-lowest bleed) list
    
    if(zero_point>=N){  # if we've run out of players to push the zero point past
      zero_point=N  # clamp at the last player in the list
      zero_person=sorted_players$name[zero_point]  # set zero_person to that last-ranked player
      M=1e15  # apply an effectively infinite shift so every player ends up floored
      hit_pot_share=equilibrium_h(attacksbyplayer$totalbleed[attacksbyplayer$name==zero_person], M)  # compute the best achievable share in this extreme case
      break  # exit the loop, this is the final fallback outcome
    }
    
    zero_person=sorted_players$name[zero_point]  # otherwise, update zero_person to the new (lower-bleed) candidate
    next  # restart the loop with the new zero_person/zero_point
  }
  
  M_bound=max(10*abs(payoutpot), 3*bleed_spread*2*warpotshare*payoutpot/max(war_hit_count,1))  # generous search bound for M, scaled by pot size, bleed spread, war pot share, and war hit count
  
  f=function(M) equilibrium_h(totalbleed_zero, M)-0.6  # root-finding function: zero when the equilibrium share hits exactly 0.6
  M=uniroot(f, lower=-M_bound, upper=M_bound, extendInt="yes", maxiter=2000)$root  # numerically solve for the M that forces hit_pot_share to exactly 0.6
  hit_pot_share=0.6  # lock the hit pot share at the 60% floor
  break  # exit the loop, done
  
}



warpot=payoutpot*hit_pot_share*warpotshare

nonwarpot=payoutpot*hit_pot_share-warpot
attacksbyplayer$war_hit_pay=warpot*attacksbyplayer$warpercentshare
attacksbyplayer$non_war_hit_pay=nonwarpot*attacksbyplayer$nonwarpercentshare


 
# Bleed penalty is equal to the average pay per war hit, not including quota misses
average_war_pay=( sum(attacksbyplayer[attacksbyplayer$passedquota==TRUE, ]$war_hit_pay)
                 /sum(attacksbyplayer[attacksbyplayer$passedquota==TRUE, ]$warhitcount) )


base_bleed_pay=attacksbyplayer$totalbleed[attacksbyplayer$name==zero_person]*average_war_pay


# Base bleed pay needs to have a floor at 0

attacksbyplayer$bleed_pay=pmax(base_bleed_pay-average_war_pay*attacksbyplayer$totalbleed, 0)


# Applying downward bleed adjustment if needed

attacksbyplayer$bleed_pay=pmax(attacksbyplayer$bleed_pay-M, -10000000)


attacksbyplayer$payout=attacksbyplayer$war_hit_pay+attacksbyplayer$non_war_hit_pay+attacksbyplayer$bleed_pay


# Quota bonus

attacksbyplayer[attacksbyplayer$quota_bonus, ]$payout=1.1*attacksbyplayer[attacksbyplayer$quota_bonus, ]$payout



# Calculating fees for own faction hospitalizations
for (i in attacksbyplayer$name){
  attacksbyplayer[attacksbyplayer$name==i, ]$hosp_charge=nrow(internal_hosp_data[internal_hosp_data$attacker_faction==FAS_faction & internal_hosp_data$result=="Hospitalized" & internal_hosp_data$defender==i, ])*
    6*nonwarpot/sum(attacksbyplayer$adjnonwarhitcount)
}

# Assigning fines and costs

attacksbyplayer$payout=attacksbyplayer$payout-attacksbyplayer$mugfine-attacksbyplayer$outsidebonusfine-attacksbyplayer$hosp_charge


write.csv(attacksbyplayer, paste0("War Payouts", Sys.Date(), ".csv"), row.names = FALSE)
