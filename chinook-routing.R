inv.logit<-function(eta){1/(1+exp(-eta))}

###############################################################################
##      BAY DELTA SDM TEAM CHINOOK MODEL FUNCTIONS Oct 10, 2018              ##
##      Primary Author: James T. Peterson                                    ##
##      U.S. Geological Survey, Oregon CooperativeCooperative                ##
##      Fish and Wildlife Research Unit, Oregon State University             ##
##      Corvallis, Oregon 97331-3803, jt.peterson@oregonstate.edu            ##
##                                                                           ##
##     Although this software program has been used by                       ##
##     the U.S. Geological Survey (USGS), no warranty, expressed             ##
##     or implied, is made by the USGS or the U.S. Government as to          ##
##     the accuracy and functioning of the program and related program       ##
##     material nor shall the fact of distribution constitute any            ##
##     such warranty, and no responsibility is assumed by the USGS           ##
##     in connection therewith.                                              ##
###############################################################################
## Delta survival and routing function for Chinook Salmon                    ##
###############################################################################

# Inputs to routing and survival function
# DCC_open: Delta crosschannel gate open (1) or closed (0)
# hor_barr: Head of old river physical barrier in place (1) or not (0)
# bio_fence:Bioacoustic fence at Sutter/ Steamboat (1) or not (0)
# Q_free: average daily discharge at Freeport cms
# Q_vern: average daily discharge at Vernalis cms
# Q_stck: average daily discharge at Stockton cms
# Temp_vern: average daily temperature at Vernalis C
# Temp_pp: average daily temperature SJR at Prisoners Point C
# CVP_exp: average daily exports CVP cms
# SWP_exp: average daily exports SWP cms
# FL: Fork length (mm)

DeltaS<-function(DCC_open,hor_barr,bio_fence,Q_free,Q_vern,
                 Q_stck,Temp_vern,Temp_pp,CVP_exp,SWP_exp, 
                 No.fish.abv.Freeport, No.fish.abv.Vernalis, FL){
  # cutoffs for number of CVP pumps operating
  cutz<-c(37.5,60,95.6,NA,500)
  no.pump<-min(which(cutz>CVP_exp))
  
  # estimate size group 
  # Small: < 42 mm, Medium: 42-72 mm, Large: 72 - 110 mm, Very large: > 110 mm
  
  siz.grp<-ifelse(FL<42,1,ifelse(FL<73,2,ifelse(FL<111,3,4)))
  
  #### First estimate North Delta parameters
  
  #Entrained into sutter/steamboat
  psi_steam<- 0.36241455*inv.logit(2.014670488 + 2.458233791*((Q_free-610.1)/814.2))
  
  psi_steam<-psi_steam+ bio_fence*0.075
  
  # remain in Sacramento
  psi_sac1<- 1- psi_steam
  
  # entrained DCC
  psi_dcc<-inv.logit((-1.515076654 - 1.282849232*((Q_free-610.1)/814.2) + 0.030214424*DCC_open))*DCC_open + (1-DCC_open)*inv.logit(-10)
  
  # entrained georgiana slough
  psi_geo<-(1-psi_dcc)*(0.2669 + (1-0.2669)*inv.logit(-3.111 - 0.9443*DCC_open - 3.1743*((Q_free-610.1)/814.2)))
  
  # remain in sacramento
  psi_sac2<- 1- psi_dcc - psi_geo
  
  #size cutoffs 42,72,110, use min from study as smallest
  FL<-c(81,81,81,140)
  #Survival Sac Freeport to Sutter/Steamboat junction
  S_1<-inv.logit(3.243+ 0.3225*DCC_open + 1.1049*((Q_free-610.1)/814.2) + 0.152*(FL-155.1)/21.6)
  
  #Survival Sac Sutter/Steamboat junction to Georgiana
  S_2<-inv.logit(3.243 + 0.0673*DCC_open + 1.1049*((Q_free-610.1)/814.2) + 0.152*(FL-155.1)/21.6)
  
  #Survival Sutter/Steamboat Slough
  S_3<-inv.logit(1.2095 + 0.1508*DCC_open + 2.2758*((Q_free-610.1)/814.2) + 0.152*(FL-155.1)/21.6)
  
  #Survival Sac Georgiana Junction to Rio Vista
  S_4<-inv.logit(2.533 - 0.7343*DCC_open + 2.5756*((Q_free-610.1)/814.2) + 0.152*(FL-155.1)/21.6)
  
  #Survival Georgiana Slough
  S_5<-inv.logit(1.1175 - 0.0769*DCC_open + 2.1591*((Q_free-610.1)/814.2) + 0.152*(FL-155.1)/21.6)
  
  #Survival DCC to Moke
  S_6<-inv.logit(0.03667 - 0.2541*DCC_open + 1.1510*((Q_free-610.1)/814.2) + 0.152*(FL-155.1)/21.6)
  
  #Survival Sac  Rio Vista to Chipps Island
  S_7<-inv.logit(1.0934 - 0.4816*DCC_open + 0.0379*((Q_free-610.1)/814.2) + 0.152*(FL-155.1)/21.6)
  
  #### Next estimate South Delta parameters
  # Probability of remaining in SJR at HOR
  psi_sjr1<-inv.logit(-0.75908 + 1.72020*hor_barr + 0.00361*Q_vern + 0.02718*hor_barr*Q_vern)
  
  # Probability of entering old river
  psi_OR <-1-psi_sjr1
  
  #Probability of remaining in SJR at Turner Cut
  psi_sjr2<-inv.logit(5.83131 - 0.037708993*Q_stck)
  
  # probability of entering Turner cut
  psi_TC<-1-psi_sjr2
  
  #Probability of entrainment at CVP (Karp et al 2017) logit link
  psi_CVP<-inv.logit(-3.9435+ 2.9025*no.pump -0.3771*no.pump^2)
  
  #Probability of entrainment at SWP
  psi_SWP<-(1-psi_CVP)*inv.logit(-1.48969+ 0.016459209*SWP_exp)
  
  # Probability of remaining old river north
  psi_ORN<- 1- psi_CVP - psi_SWP
  
  #Survival Tributaries to HOR logit link
  S_prea<-inv.logit(5.77500 + 0.00706*Q_vern - 0.32810*Temp_vern+ 0.152*(FL-155.1)/21.6)
  
  #Survival HOR to Turner Cut logit link
  S_a<-inv.logit(-2.90330+ 0.01059*Q_vern+ 0.152*(FL-155.1)/21.6)
  
  #Survival SJR Turner Cut to Chipps
  S_bc<-inv.logit(13.41840 - 0.90070*Temp_pp+ 0.152*(FL-155.1)/21.6)
  
  #Survival down OR HOR to CVP
  S_d<-inv.logit(2.16030 -0.20500*Temp_vern+ 0.152*(FL-155.1)/21.6)
  
  #Survival ORN to Chipps Island (SJRGA)
  S_efc<-0.01
  
  #Survival through CVP (Karp et al 2017) logit link
  S_CVP<-inv.logit(-3.0771 + 1.8561*no.pump - 0.2284*no.pump^2)
  
  #Survival through SWP (Gingras 1997)
  S_SWP<-0.1325
  
  # North origin fish movement and survival to Chipps
  NorFish<-S_1*psi_steam*S_3*S_7 + S_1*psi_sac1*S_2*psi_sac2*S_4*S_7 + 
    S_1*psi_sac1*S_2*psi_dcc*S_6*(S_bc^1/2) + S_1*psi_sac1*S_2*psi_geo*S_5*(S_bc^1/2)
  
  Free.to.Chps<-round(No.fish.abv.Freeport*NorFish)[siz.grp]
  
  # At junction of Sutter/Steamboat with Sacramento
  Sut.in.Sut<-round(psi_steam*S_1*No.fish.abv.Freeport)[siz.grp]
  Sut.in.Sac<-round(psi_sac1*S_1*No.fish.abv.Freeport)[siz.grp]
  
  # At junction Delta Cross Channel/Georgiana slough
  Junc.in.DCC<-round(S_2*psi_dcc*Sut.in.Sac)[siz.grp]
  Junc.in.GeoSlo<-round(S_2*psi_geo*Sut.in.Sac)[siz.grp]
  Junc.in.Sac<-round(S_2*psi_sac2*Sut.in.Sac)[siz.grp]
  
  #South origin fish movement and Survival to Chipps
  SouFish<-S_prea*psi_sjr1*S_a*psi_sjr2*S_bc + S_prea*psi_sjr1*S_a*psi_TC*S_efc + 
    S_prea*psi_OR*S_d*psi_ORN*S_efc + S_prea*psi_OR*S_d*psi_CVP*S_CVP +
    S_prea*psi_OR*S_d*psi_SWP*S_SWP  
  
  
  Vern.to.Chps<-round(No.fish.abv.Vernalis*SouFish)[siz.grp]
  
  # Above Vernalis to head of old river (HOR)
  at.HOR.SJR<-round(No.fish.abv.Vernalis*S_prea*psi_sjr1)[siz.grp]
  at.HOR.OR<-round(No.fish.abv.Vernalis*S_prea*(1-psi_sjr1))[siz.grp]
  
  # HOR to Turner Cut
  at.TC.SJR<-round(at.HOR.SJR*S_a*psi_sjr2)[siz.grp]
  at.TC.TC<-round(at.HOR.SJR*S_a*(1-psi_sjr2))[siz.grp]
  
  # HOR to water projects for salvage or past to Old River North (ORN)
  CVP.salv<-round(at.HOR.OR*S_d*psi_CVP*S_CVP)[siz.grp]
  SWP.salv<-round(at.HOR.OR*S_d*psi_SWP*S_SWP)[siz.grp]
  to.ORN <- round(at.HOR.OR*S_d*psi_ORN)[siz.grp]
  
  c(
    Free.to.Chps=Free.to.Chps,
    Sut.in.Sut=Sut.in.Sut,
    Sut.in.Sac=Sut.in.Sac,
    Junc.in.DCC=Junc.in.DCC,
    Junc.in.GeoSlo=Junc.in.GeoSlo,
    Junc.in.Sac=Junc.in.Sac,
    at.HOR.SJR=at.HOR.SJR,
    at.HOR.OR=at.HOR.OR,
    at.TC.SJR=at.TC.SJR, 
    at.TC.TC=at.TC.TC,
    CVP.salv=CVP.salv,
    SWP.salv=SWP.salv,
    to.ORN=to.ORN,
    Vern.to.Chps=Vern.to.Chps
    )
}

# DCC_open<-1
# hor_barr<-0
# bio_fence<-0
# Q_free<-700
# CVP_exp<-99
# SWP_exp<- 100
# Q_vern<- 200
# Q_stck<- 10
# Temp_vern<-12
# Temp_pp<-15
# No.fish.abv.Freeport<-No.fish.abv.Vernalis<-10000
# FL = 100
# 
# 
# DeltaS(DCC_open,hor_barr,bio_fence,Q_free,Q_vern, Q_stck,Temp_vern,Temp_pp,CVP_exp,SWP_exp,
#        No.fish.abv.Freeport, No.fish.abv.Vernalis,FL)


# translation of output
#'Free.to.Chps: number fish surving from above Freeport to Chipps island
#'At junction of Sacramento and Sutter/ Steamboat:
#'    Sut.in.Sut: number fish surviving and entrained into Sutter/Steamboat
#'    Sut.in.Sac: number fish surviving and remaining in Sacramento
#'At junction of Sacramento and Delta Cross Channel/Georgiana slough:
#'   Junc.in.DCC: number fish surviving and entrained into Delta Cross Channel
#'   Junc.in.GeoSlo: number fish surviving and entrained into Georgiana slough  
#'   Junc.in.Sac : number fish surviving and remaining in Sacramento
#'Vern.to.Chps : number fish surving from above Vernalis to Chipps island
#'At junction of San Joaquin and Head of Old River:
#'   at.HOR.SJR: number fish surviving and remaining in San Joaquin
#'   at.HOR.OR: number fish surviving and entrained into old river
#'At junction of San Joaquin and Turner Cut:
#'   at.TC.SJR: number fish surviving and remaining in San Joaquin
#'   at.TC.TC:  number fish surviving and entrained into Turner Cut
#'At junction of old river and water projects:
#'   CVP.salv: number fish surviving, entrained, and salvaged CVP
#'   SWP.salv: number fish surviving, entrained, and salvaged CVP
#'   to.ORN: number fish surviving and remaining in Old River North
