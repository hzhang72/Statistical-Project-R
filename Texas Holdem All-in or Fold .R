#install.packages("holdem")
library(holdem)
library(help="holdem")
Iveybruin = function (numattable1, crds1, board1, round1, currentbet,
                      mychips1, pot1, roundbets, blinds1, chips1, ind1, dealer1,
                      tablesleft) {
  ## all in with any 9 pair or higher, or if lower card is 10 or higher,
  ## or if I have less than 3 times the big blind
  a1 = 0
  if ((crds1[1, 1] == crds1[2, 1]) && (crds1[1, 1] > 8.5)) a1 = mychips1
  if (crds1[2,1] > 9.5) a1 = mychips1
  if(mychips1 < 3*blinds1) a1 = mychips1
  a1
} ## end of Iveybruin 

nbhz = function (numattable1, crds1, board1, round1, currentbet,
                 mychips1, pot1, roundbets, blinds1, chips1, ind1, dealer1,
                 tablesleft) {
  ## all in with any pair of 4s or higher, if there is an ace, if lower card is J or higher,
  ## or if you have less than 4 times the big blind
  a1 = 0
  if ((crds1[1, 1] == crds1[2, 1]) && (crds1[1, 1] > 3.5)) a1 = mychips1
  if (crds1[2,1] == 14) a1 = mychips1
  if (crds1[2,1] > 10.5) a1 = mychips1
  if (chips1 < 4*blinds1) a1 = mychips1
  a1
} ## end of nbhz