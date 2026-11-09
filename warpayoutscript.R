
library(dplyr)
library(tidyr)

rm(list = ls())

payoutpot=2928314948          

# Enemy faction name

enemy_faction="Phoenix X"

# FAS faction

FAS_faction="Fallen Angels Syndicate"

# Early hit cutoff point; enter in TCT

early_hit_point="2026-11-08 16:00:00"


# Current member roster

current_members=read.csv("members_report_PX.csv")

# Read in file from debrief with complete attack details here; must use double slashes, not single slashes
new_data=read.csv("Syn WarAttacks - PX Attacks.csv")





# Setting special outcomes to attacked

new_data$result[new_data$result=="Special"]<-"Attacked"


# Chain count

new_data <- new_data[order(new_data$ended_tct), ]
rownames(new_data) <- NULL

# Converting to date object

new_data$ended_tct <- as.POSIXct(new_data$ended_tct, format = "%d %b %H:%M:%S", tz = "UTC")
new_data$started_tct <- as.POSIXct(new_data$started_tct, format = "%d %b %H:%M:%S", tz = "UTC")


# Assigning enemy faction to any incoming war hits (to override stealthed hits)
new_data[new_data$war_modifier==2 & new_data$defender_faction==FAS_faction, ]$attacker_faction=enemy_faction



# Assigning war hit to any hits on enemy fac (to overcome assists)

new_data[new_data$defender_faction==enemy_faction, ]$war_modifier=2


# Assigning zero respect to all losses (in lieu of NA)

new_data$respect[new_data$result == "Lost"] <- 0

# Throwing out all incoming attacks not from the enemy faction 
new_data=new_data[new_data$attacker_faction==enemy_faction |  new_data$attacker_faction==FAS_faction, ]


# Filling in NAs in some columns with 1

new_data <- new_data %>%
  mutate(across(c(retaliation_modifier, group_modifier, overseas_modifier, warlord_modifier), ~ replace_na(.x, 1)))


# Whether or not to allow attacks in the following chain to count towards the hit quota

ChainQuota=FALSE
# 
# # Chain report
# chaindata=read.csv("")
# 
# # Trim white space
# chaindata$Name <- trimws(chaindata$Name)

# Hit quota is 1% of war hits

hitquota=round(0.01*
                 length(which(new_data$war_modifier==2 & (new_data$counted_as %in% c("hit") ) &
                                new_data$defender_faction==enemy_faction )) )

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
attacksbyplayer$chainbonushitcount<-0
attacksbyplayer$hosp_charge<-0


# Bonus fines
# BUGFIX: this loop was checking new_data[i,]$chain (the running chain counter at the
# time of the hit) instead of new_data[i,]$chain_hit (the milestone value: 25/50/100/...).
# outsidebonushits itself was already correctly built using chain_hit (see above), so the
# mismatch meant $chain almost never actually equalled 25/50/100/etc, and these fines were
# silently coming out to $0 for almost everyone. Changed every check below to $chain_hit.
for (i in outsidebonushits){
  if(new_data[i, ]$chain_hit==25){
    attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine=-2500000+attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine
  }
  else if(new_data[i, ]$chain_hit==50){
    attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine=-5000000+attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine
  }
  else if(new_data[i, ]$chain_hit==100){
    attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine=-15000000+attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine
  }
  else if(new_data[i, ]$chain_hit==250){
    attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine=-25000000+attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine
  }
  else if(new_data[i, ]$chain_hit==500){
    attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine=-50000000+attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine
  }
  else if(new_data[i, ]$chain_hit==1000){
    attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine=-100000000+attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine
  }
  else if(new_data[i, ]$chain_hit==2500){
    attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine=-200000000+attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine
  }
  else if(new_data[i, ]$chain_hit==5000){
    attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine=-300000000+attacksbyplayer[attacksbyplayer$name==new_data[i, ]$attacker, ]$outsidebonusfine
  }
}


# Removing incoming attacks and saving for bleed calculations

bleed_data=new_data


# BUGFIX: was "new_data$attacker_faction=FAS_faction" (single "=") which is a syntax error
# inside a data-frame filter (R parses it as an attempt to name a function argument,
# so the whole script fails to parse and nothing below this line can run). Changed to "==".
internal_hosp_data=new_data[new_data$defender_faction==FAS_faction & new_data$attacker_faction==FAS_faction, ]


new_data=new_data[new_data$attacker_faction==FAS_faction, ]



# Tossing out interrupts

for (i in 1:length(attacksbyplayer$name) ){
  
  playerattacks=new_data[new_data$attacker==attacksbyplayer$name[i], ]
  
  attacksbyplayer$interrupted[i]=nrow(playerattacks[playerattacks$war_modifier==2 & playerattacks$result=="Interrupted", ])
  
}


new_data=new_data[new_data$result!="Interrupted", ]




# Stripping out chain bonuses except for 10
# Preserve the qualifying chain bonus hits first so they can later be paid
# at the average adjusted war score of quota-passing players.
exclude_vals <- c(5000, 2500, 1000, 500, 250, 100, 50, 25)

chain_bonus_counts <- table(
  new_data$attacker[
    new_data$chain_hit %in% exclude_vals &
      new_data$attacker_faction == FAS_faction &
      new_data$result %in% c("Attacked", "Mugged", "Hospitalized")
  ]
)
attacksbyplayer$chainbonushitcount <- as.integer(chain_bonus_counts[attacksbyplayer$name])
attacksbyplayer$chainbonushitcount[is.na(attacksbyplayer$chainbonushitcount)] <- 0

# Remove chain bonus hits from the normal scoring/counting calculations so they
# are not counted twice.
new_data <- new_data[!(new_data$chain_hit %in% exclude_vals), ]



# removing chain respect multiplier
new_data$respect=if_else(!is.na(new_data$respect/pmax(1, (0.25*log10(new_data$chain_hit)+0.75)) ),
                         new_data$respect/pmax(1, (0.25*log10(new_data$chain_hit)+0.75) ), 
                         new_data$respect)


# Redistributing assist respect

for (i  in which(new_data$group_modifier!=1)){
  
  #Calculating the number of assists
  assistnumber=log(6-4*new_data$group_modifier[i])/log(0.5)+1
  
  # Finding of the attacks with the same defender, which ones are the closest ones which are assists
  # BUGFIX: previously ordered a "result=='Assist'"-only index vector but then used those
  # ranks to subset a longer "Assist"+"Lost" index vector -- the ranks didn't line up with
  # that vector's own positions once any "Lost" rows were present for this defender. Now we
  # build the candidate vector once and order that same vector by closeness to i.
  same_defender_idx <- which(new_data$defender==new_data$defender[i] & new_data$result %in% c("Assist", "Lost"))
  assists <- same_defender_idx[order(abs(same_defender_idx - i))][1:assistnumber]
  
  
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
  attacksbyplayer$avgwarFF[i]=mean(playerattacks$fair_fight[(playerattacks$chainsave==FALSE | playerattacks$retaliation_modifier!=1.5 | playerattacks$result %in% c("Attacked", "Mugged") ) & playerattacks$war_modifier==2])
  attacksbyplayer$avgnonwarFF[i]=mean(playerattacks$fair_fight[(playerattacks$chainsave==FALSE | playerattacks$retaliation_modifier!=1.5 |playerattacks$result %in% c("Attacked", "Mugged") ) & playerattacks$war_modifier==1])
  
  
  # Setting fair_fight for retals/hosps/overseas_modifier/chain saves to 3 so that they get scaled accordingly
  playerattacks$fair_fight[ (playerattacks$chainsave==TRUE | playerattacks$retaliation_modifier==1.5 | playerattacks$overseas_modifier== 1.25 | playerattacks$result=="Hospitalized") & playerattacks$war_modifier==2]=3
  # Scaling up FF effect by mapping x1 to x3 multiplier to x1 to x6 multiplier 
  attacksbyplayer$adjwarscore[i]=sum(playerattacks$respect[playerattacks$war_modifier==2 ]*2.5*(playerattacks$fair_fight[playerattacks$war_modifier==2]-0.6)*playerattacks[playerattacks$war_modifier==2, ]$early_hit_bonus)
  
  if(is.finite(attacksbyplayer$adjwarscore[i])==FALSE){
    browser()
  }
  
  
  # Non war hits that are not in a chain are not counted
  
  # checking hits less than 10 chain---was it part of a chain?
  
  subset_rows <- playerattacks$chain < 10 & playerattacks$war_modifier == 1
  subset_codes <- playerattacks$code[subset_rows]
  
  if (length(subset_codes) > 0) {
    for (j in seq_along(subset_codes)) {
      
      code_j <- subset_codes[j]
      idx <- which(new_data$code == code_j)
      
      # BUGFIX: previously nrow(new_data)==idx+10 exactly fell through both branches
      # (first needed strictly ">", second needed strictly "<") and was silently skipped.
      # Changed to ">=" (row idx+10 exists, so we can check it) / else (it doesn't exist,
      # so we can't confirm the hit started a chain, matching the prior "<" behaviour).
      if (nrow(new_data)>=idx + 10){ 
        if(new_data[idx + 10, ]$chain < 10) {
          playerattacks <- playerattacks[playerattacks$code != code_j, ]
        }
      } else {
        playerattacks <- playerattacks[playerattacks$code != code_j, ]
      }
    }
  }
  
  
  # counting war and non war hits
  attacksbyplayer$warhitcount[i]=nrow(playerattacks[playerattacks$war_modifier==2 & playerattacks$result %in% c( "Attacked", "Mugged", "Hospitalized"), ])
  
  attacksbyplayer$warassistcount[i]=nrow(playerattacks[playerattacks$war_modifier==2 & playerattacks$result=="Assist", ])
  
  attacksbyplayer$losses[i]=nrow(playerattacks[playerattacks$war_modifier==2 & playerattacks$result=="Lost", ])
  
  attacksbyplayer$nonwarhitcount[i]=nrow(playerattacks[playerattacks$war_modifier==1 & playerattacks$result %in% c( "Attacked", "Mugged", "Hospitalized"), ])
  
  
  
  
  # Applying FF effect to non war hits, allowing chain saves and self-hits to be like x3 FF 
  
  # We are capping non war hits that are not chain saves, not assists or losses and not own hits on FAS and not below chain hit 10
  # We choose the best FF non war hits to retain
  
  # BUGFIX: rule says retaliations/hospitalizations/overseas hits are also always x3 FF,
  # not just chain saves. Previously only chainsave forced FF=3 for non-war hits (the war-hit
  # block above already did this correctly). Added the same three conditions here.
  # This only affects the FF value in the remap -- it does not exempt these hits from the
  # non-war hit cap (only chain saves / chain-starters are exempt from the cap per the rules).
  playerattacks$fair_fight[ (playerattacks$chainsave==TRUE | playerattacks$retaliation_modifier==1.5 | playerattacks$overseas_modifier==1.25 | playerattacks$result=="Hospitalized") & playerattacks$war_modifier==1]=3
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
  } else if(attacksbyplayer$mug[i]==1){
    attacksbyplayer$mugfine[i]=-3000000
  } else if(attacksbyplayer$mug[i]==2){
    attacksbyplayer$mugfine[i]=-6000000
  } else if(attacksbyplayer$mug[i]>2){
    attacksbyplayer$mugfine[i]=-10000000
  }
  
  # Attacks on own faction are counted as x3 FF
  
  attacksbyplayer$adjnonwarhitcount[i]=attacksbyplayer$adjnonwarhitcount[i]+
    6*nrow(playerattacks[playerattacks$defender_faction==FAS_faction & playerattacks$result=="Hospitalized", ] )
  
}





# Chain bonus hits count as hits toward quota and the 5x quota bonus
attacksbyplayer$totalhitcount=attacksbyplayer$nonwarhitcount+attacksbyplayer$warhitcount+attacksbyplayer$warassistcount+attacksbyplayer$losses+attacksbyplayer$interrupted+attacksbyplayer$chainbonushitcount

# Removing anyone who failed the minimum quota from the payout
if(ChainQuota==FALSE){
  
  # Passing quota uses all types of hits
  passedquota=attacksbyplayer$totalhitcount>=hitquota
} else if(ChainQuota==TRUE){
  # SAFETY CHECK ADDED: chaindata is loaded from the commented-out lines near the top of
  # this script (the "# chaindata=read.csv(...)" block). If ChainQuota is set to TRUE without
  # uncommenting and filling in that block, this stops with a clear message instead of failing
  # later with a generic "object 'chaindata' not found" error.
  if(!exists("chaindata")){
    stop("ChainQuota is TRUE but chaindata was not loaded -- uncomment and fill in the chaindata read.csv() line near the top of the script.")
  }
  idx <- match(attacksbyplayer$name, chaindata$Name)
  idx <- idx[!is.na(idx)]
  
  chaindata_ordered <- chaindata[idx, ]
  
  
  passedquota=(chaindata_ordered$Number.of.Hits-chaindata_ordered$War.Hits+attacksbyplayer$totalhitcount)>=hitquota
  
  attacksbyplayer$warpercentshare[!passedquota]=0
  attacksbyplayer$nonwarpercentshare[!passedquota]=0  
}


# Quota bonus applies either for hit count OR zero bleed (zero bleed is being checked by verifying that no hits have that person as the defender)
attacksbyplayer$quota_bonus= ( attacksbyplayer$totalhitcount>=(5*hitquota) ) | !(attacksbyplayer$name %in% unique(bleed_data[bleed_data$result %in% c("Attacked","Mugged","Hospitalized"),]$defender) )

# Chain bonus hits are worth the average adjusted war score per ordinary war hit,
# calculated using quota-passing players only. Because the bonus score is added
# before war percentage shares are calculated, it is funded from the existing
# fixed war pot rather than increasing the total payout pot.
passed_war_hits <- sum(attacksbyplayer$warhitcount[passedquota])

if (passed_war_hits > 0) {
  average_adj_war_score <-
    sum(attacksbyplayer$adjwarscore[passedquota]) / passed_war_hits
} else {
  average_adj_war_score <- 0
}

attacksbyplayer$adjwarscore[passedquota] <-
  attacksbyplayer$adjwarscore[passedquota] +
  attacksbyplayer$chainbonushitcount[passedquota] * average_adj_war_score

# From this point on, treat each chain bonus exactly like an ordinary war hit.
# Quota was already calculated above with chainbonushitcount included, so this
# does not double-count bonuses for quota.
attacksbyplayer$warhitcount <-
  attacksbyplayer$warhitcount + attacksbyplayer$chainbonushitcount

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
  attacksbyplayer$fullbleed[attacksbyplayer$name==i]=nrow(bleed_data[bleed_data$prior_attacks==FALSE & bleed_data$defender==i & 
                                                                       bleed_data$result %in% c("Attacked", "Mugged", "Hospitalized") & !(bleed_data$attacker_faction %in% c("Fallen Angels Syndicate", "Fallen Angels Sanctuary") ), ])
  attacksbyplayer$discountedbleed[attacksbyplayer$name==i]=nrow(bleed_data[ bleed_data$prior_attacks==TRUE & bleed_data$defender==i & 
                                                                              bleed_data$result %in% c("Attacked", "Mugged", "Hospitalized") & !(bleed_data$attacker_faction %in% c("Fallen Angels Syndicate", "Fallen Angels Sanctuary") ), ])
  
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
# BUGFIX: denominator previously summed adjnonwarhitcount over ALL members, but the
# corresponding attacker-side credit (search "Attacks on own faction are counted as x3 FF")
# is paid out of nonwarpot at the per-hit rate implied by nonwarpercentshare, which only
# sums adjnonwarhitcount over passedquota members. Using the wrong (larger) denominator
# here meant the defender's charge didn't exactly match the value of the credit the
# attacker received whenever a quota-failing member had nonzero adjnonwarhitcount.
# Restricted the sum to passedquota members so charge and credit use the same per-hit value.
for (i in attacksbyplayer$name){
  attacksbyplayer[attacksbyplayer$name==i, ]$hosp_charge=-1*nrow(internal_hosp_data[internal_hosp_data$attacker_faction==FAS_faction & internal_hosp_data$result=="Hospitalized" & internal_hosp_data$defender==i, ])*
    6*nonwarpot/sum(attacksbyplayer$adjnonwarhitcount[attacksbyplayer$passedquota==TRUE])
}

# Assigning fines and costs

attacksbyplayer$payout=attacksbyplayer$payout+attacksbyplayer$mugfine+attacksbyplayer$outsidebonusfine+attacksbyplayer$hosp_charge


write.csv(attacksbyplayer, paste0("War Payouts", Sys.Date(), ".csv"), row.names = FALSE)
