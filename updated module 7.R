#loading packages
library(pryr)

#s3 dataset
s3data <- list(name = "Dragon Hunter", type = "Great Katana", skill = "Dragonwound Slash", phy_dmg = 152)
class(s3data) <- "weapon"
s3data

#s4 dataset
setClass("weapon",
         representation(
           name = "character",
           type = "character",
           skill = "character",
           phy_dmg = "numeric"
         ))
s4data <- new("weapon", name = "Dragon Hunter", type = "Great Katana", skill = "Dragonwound Slash", phy_dmg = 152)
s4data

#OO system
otype(s3data)
otype(s4data)

#base type
typeof(s3data$name)
typeof(s4data@phy_dmg)

#s3 and s4 generic functions
#s3
print_weapon_s3 <- function(plyr) {
  cat(plyr$name, "is a", plyr$type, "with the skill", plyr$skill, "and it does", 
      plyr$phy_dmg, "physical damage")}
print_weapon_s3(s3data)

#s4
setMethod("show", "weapon",
          function(object){
            cat(object@name, "is a", object@type, "with the skill", object@skill, "and it does", 
                object@phy_dmg, "physical damage")
          })
s4data

#2nd example
#s3
s3_2data <- list(name = "Ranni's Dark Moon", type = "Moon Sorceries", fp_cost = 57)
class(s3_2data) <- "sorcery"
s3_2data


#s4
setClass("sorcery",
         representation(
           name = "character",
           type = "character",
           fp_cost = "numeric"
         ))
s4_2data <- new("sorcery", name = "Ranni's Dark Moon", type = "Moon Sorceries", fp_cost = 57)
s4_2data