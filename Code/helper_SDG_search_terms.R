
## ######################################################################## ##
## This script collects all the search terms for SDGs at the target levels  ##
## ######################################################################## ##

#' **Last update on**:  2/28/2022
#' 
#' **New changes**: 
#' Compare to the earlier version, we made the following changes
#' 1. Instead of combining multiple term lists by `OR` for one particular target,  
#'    it is more intuitive and accurate to add each alternative term list to the search 
#'    term table or database directly. 
#'    
#' 2. Added `Look around` function to more accurately match SDG targets.
#' 
#' 
#' 
#' 
#' 


###
# *   --> blank
# ()  --> ""
# AND --> ,


library(dplyr)


## a vector (v) of terms to be concatenated by `AND`

func_AND_vector <- function(v){
  pat <- paste0("(?=.*(?:", v, "))", collapse="")
  return(pat)
}


## a vector (v) of terms to be concatenated by `OR`
func_OR_vector <- function(v){
  pat <- paste0(v, collapse = "|")
  # print(pat)
  return(pat)
}


## similar to `func_AND_vector`, and mainly used for the final `regular expressions` for a target
func_AND_plus <- function(v){
  pat <- func_AND_vector(v)
  target <- paste0("^", pat, ".+")
  # print(target)
  return(target)
}


## To exclude certain terms
func_to_exclude_terms <- function(which_sdg_term, terms_to_exclude) {
  terms_replacement <- paste0("(?!.*", paste(terms_to_exclude, collapse = ")(?!.*"), ")")
  
  if ( stringr::str_detect(which_sdg_term, "\\.\\+") )  {
    which_sdg_term <- gsub(pattern = "\\.\\+", replacement = terms_replacement, which_sdg_term) ## to exclude these keywords
  } else {
    which_sdg_term <- paste(which_sdg_term, terms_replacement)
  }
  return(which_sdg_term)
}


## Call the `look around function`
source('./Code/function_lookaround_nearby_n.R')




################################################################################################## #
# Indirect mention                                                                              ####
################################################################################################## #


## Auxiliary list ------

ag_ls = "agricultur|\\bcrop|food|farm|forestry|pastoral|pasture|\\bagro|\\bfisher|agronom|horticulture|cultivation|husbandry|grazing|\\bgrain|planting"

child_ls = paste("child|\\bteen|juvenile|youth|young|minor|under 5|under five|underfive|before fifth|adolescen",
                 "\\bgirl|\\bboy\\b|\\bboys\\b|\\bkid\\b|\\bkids\\b|\\bkiddo|\\bkiddy|\\bkiddies|offspring",
                 sep = "|")

death_ls = "mortal|death|\\bdead|\\bdie\\b|\\bdies\\b|\\bdied\\b|\\bdying|fatal|deceas|\\bdemis|\\bkill|lethal|perish|lose.*life|pass away"

disaster_ls = paste(
  "shock|disaster|catastrophe|hazard|hurricane|tornado|cyclone|flood|drought|\\bharm", 
  "wildfire|wild fire|firestorm|extreme weather|extreme temperature|extreme heat|heat.?wave|cold wave", 
  "extreme precipitation|lightning|thunderstorm|ice storm|blizzard|hailstorm|tropical storm|earthquake", 
  "volcanic eruption|limnic eruption|exploding lake|landslide|mudslide|debris flow|tsunami|sinkhole|subsidence", sep = "|")
## -> avoid "risk", which is too broad

emission_ls = paste("\\bemission|\\bCO2\\b|carbon|green.?house|\\bGHG\\b|G\\.H\\.G\\.|chlorofluorocarbon|\\bcfc\\b|c\\.f\\.c\\.|\\bCFCs\\b",
                    "water vapour|methane|\\bCH4\\b|nitrous oxide|\\bN2O\\b|\\bozone|\\bO3\\b|fluorinated gas", sep = "|")

female_ls = "woman|women|\\bgirl|female|\\blady\\b|\\bladies\\b"

increase_ls = paste("accelerat|ascend|\\badvanc|climb|better|boost|\\bboom|escalat|\\bgrow|\\bhike|\\bhiking|improv",
                    "increas|foster|increment|jump|\\blift|\\brais|\\brise.?\\b|rising|\\brose|risen|\\brocket|skyrocket|\\bsurg|\\bsoar|promot",
                    "strengthen|upsurg|upward", 
                    sep = "|")

illegal_ls = "illegal|criminal|illegitimate|illicit|unauthorized|unlawful|unlicensed|unconstitutional|prohibited|forbidden|banned"

finance_ls <- paste("financ|\\bfund.?\\b|\\bfunding\\b|\\bassist|budget|\\baid|invest|compensation|donor|donat",
                    "expenditure|\\bgrant\\b|\\bgrants\\b|\\bloan|official flow|\\bsubsidy|\\bsubsidies|\\bsubsidiz|\\bsubsidis")
## -> be careful when using invest*, might mistakenly match "investigation|investigate"


support_ls = paste("financ|\\bfund.?\\b|\\bfunding\\b|\\bassist|budget|help|\\baid|invest|boost|bolster|compensation|donor|donat",
                   "enhanc|expenditure|\\bgrant.?\\b|\\bloan|official flow|support|\\bsubsidy|\\bsubsidies|\\bsubsidiz|\\bsubsidis",
                   "\\bstimulat|strengthen|transfer|uphold", sep = "|")

urban_ls = "urbaniz|\\burban|\\bcity|\\bcities|human settlement|land consumption|land use|metropoli|town|municipal"


policy_ls = paste("action|activit|administ|agreement|approach|arrangement|blueprint|\\bdeal|\\bguide|govern", 
                  "method|manag|\\bpolicy|\\bpolicies|\\bplan\\b|\\bplans\\b|planning|proced|program|project.?\\b",
                  "practice|procedure|propos|protocol|\\brule.?\\b|regulat",
                  "\\blaw|\\btreat|\\bright|constitution|initiative|scheme|strateg", sep = "|")

poverty_ls = "poverty|\\bpoor|impover|underprivileg|necessitous|homeless|\\bhobo|low income|low-income|lower income|underclass"


reduce_ls = paste("alleviat|\\babate|\\bavert|abolish|eradicat|eliminat|\\beras|\\bease|\\bend.?\\b|\\bended\\b|rid of|phase out|\\bwipe out",
                  "\\breduc|\\bcut|\\bcurb|curtail|declin|decreas|diminish|\\bdrop|dwindl|halt|hinder|hamper|impede|\\binhibit.?\\b|\\binhibited\\b|\\binhibiting",
                  "lessen|mitigat|slash|shrink|\\bstop|\\btrim|weaken|\\bno\\b|\\bzero|prevent|prohibit|minimis|minimiz", sep = "|")


ls_hazardous_waste_chemicals = paste(
  "\\bPhenol|\\bEthane|\\bEthene|\\bCresol|\\bIndeno|\\bEndrin|\\bDibenz|\\bSilvex|\\bArsine|\\bPropen|\\bCumene|\\bXylene|\\bAldrin|\\bIsolan",
  "\\bOxamyl|\\bKepone|\\bThiram|\\bBarban|\\bBenzene|\\bEthanol|\\bDiethyl|\\bPropane|\\bMercury|\\bMethane|\\bOxirane|\\bDibenzo|\\bAniline",
  "\\bEthanal|\\bAcetone|\\bBrucine|\\bDinoseb|\\bIsodrin|\\bPhorate|\\bFamphur|\\bTirpate|\\bChloral|\\bLindane|\\bSafrole|\\bToluene|\\bBenomyl",
  "\\bPropham|\\bacetone|\\bbenzene|\\bPyridine|\\bThiourea|\\bPlumbane|\\bThallium|\\bEthanone|\\bPentanol|\\bNicotine|\\bFurfuran|\\bFurfural",
  "\\bMethanol|\\bAcrolein|\\bCyanogen|\\bDieldrin|\\bFluorine|\\bMethomyl|\\bAldicarb|\\bPhosgene|\\bAmitrole|\\bAuramine|\\bChrysene|\\bCreosote",
  "\\bDiallate|\\bCarbaryl|\\bPropoxur|\\bmethanol|\\bpyridine|\\bBenzamide|\\bManganese|\\bArgentate|\\bGuanidine|\\bAcetamide|\\bChlordane|\\bHydrazine",
  "\\bAziridine|\\bThiofanox|\\bEndothall|\\bParathion|\\bPhosphine|\\bToxaphene|\\bMetolcarb|\\bDimetilan|\\bPromecarb|\\bAzaserine|\\bBenzidine",
  "\\bMelphalan|\\bPronamide|\\bReserpine|\\bBromoform|\\bTriallate|\\bVinylamine|\\bEthanamine|\\bPiperidine|\\bStrychnine|\\bThiophenol|\\bDisulfoton",
  "\\bDimethoate|\\bEndosulfan|\\bHeptachlor|\\bSelenourea|\\bCarbofuran|\\bMethiocarb|\\bAcrylamide|\\bChloroform|\\bDaunomycin|\\bIsosafrole",
  "\\bPhenacetin|\\bResorcinol|\\bBendiocarb|\\bThiodicarb|\\bisobutanol|\\bAcetic acid|\\bBenzenamine|\\bStrychnidin|\\bNitric acid|\\bMethanamine",
  "\\bPyrrolidine|\\bNaphthalene|\\bThiophanate|\\bFormic acid|\\bCyclohexane|\\bEthyl ether|\\bEpinephrine|\\bMexacarbate|\\bCarbosulfan|\\bMitomycin C",
  "\\bParaldehyde|\\bTrypan blue|\\bCarbendazim|\\bethyl ether|\\bVanadic acid|\\bMethanethiol|\\bAcetaldehyde|\\bArsinic acid|\\bAcetonitrile",
  "\\bThiomethanol|\\bNitrobenzene|\\bAcrylic acid|\\bBenzenethiol|\\bBromoacetone|\\bDithiobiuret|\\bNitric oxide|\\bSodium azide|\\bZinc cyanide",
  "\\bFormparanate|\\bAcetophenone|\\bChlorambucil|\\bFluoranthene|\\bFormaldehyde|\\bLasiocarpine|\\bLead acetate|\\bMethoxychlor|\\bProsulfocarb",
  "\\bnitrobenzene|\\bGlucopyranose|\\bCarbamic acid|\\bHydroperoxide|\\bFulminic acid|\\bCarbonic acid|\\bSulfuric acid|\\bCyclohexanone|\\bDimethylamine",
  "\\bDipropylamine|\\bEthyl acetate|\\bAllyl alcohol|\\bDiethylarsine|\\bEthyleneimine|\\bEthyl cyanide|\\bThallic oxide|\\bPhysostigmine|\\bAcrylonitrile",
  "\\bChlornaphazin|\\bChlorobenzene|\\bMethyl iodide|\\bMalononitrile|\\bMethapyrilene|\\bThioacetamide|\\bTriethylamine|\\bchlorobenzene|\\bethyl acetate",
  "\\bethyl benzene|\\bcyclohexanone|\\bPropanoic acid|\\bSelenious acid|\\bPropanenitrile|\\bBenzenediamine|\\bEthylene oxide|\\bNitroglycerine",
  "\\bEthyl acrylate|\\bMethyl alcohol|\\bBarium cyanide|\\bCopper cyanide|\\bNickel cyanide|\\bPhenylthiourea|\\bSilver cyanide|\\bSodium cyanide",
  "\\bMethyl bromide|\\bVinyl chloride|\\bCrotonaldehyde|\\bDihydrosafrole|\\bCacodylic acid|\\bLead phosphate|\\bStreptozotocin|\\bToluenediamine",
  "\\bUracil mustard|\\bPhosphoric acid|\\bNaphthalenamine|\\bDiphosphoramide|\\bEthyl carbamate|\\bAcetyl chloride|\\bMethyl chloride|\\bZinc cyanide Zn",
  "\\bTetrahydrofuran|\\bCalcium cyanide|\\bBenzyl chloride|\\bEthanedinitrile|\\bFluoroacetamide|\\bNickel carbonyl|\\bTetraethyl lead|\\bBenzal chloride",
  "\\bChlorobenzilate|\\bEpichlorohydrin|\\bHexachlorophene|\\bLead subacetate|\\bEthanethioamide|\\btrifluoroethane|\\bBenzotrichloride|\\bCyanogen bromide",
  "\\bIsobutyl alcohol|\\bAmmonium picrate|\\bSulfur phosphide|\\bArsenic trioxide|\\bBeryllium powder|\\bCarbon disulfide|\\bHydrogen cyanide",
  "\\bHydrocyanic acid|\\bMethyl hydrazine|\\bMethyl parathion|\\bNitrogen dioxide|\\bOsmium tetroxide|\\bAldicarb sulfone|\\bCalcium chromate",
  "\\bCyclophosphamide|\\bDimethyl sulfate|\\bEthylenethiourea|\\bGlycidylaldehyde|\\bHexachloroethane|\\bHydrogen sulfide|\\bMaleic anhydride",
  "\\bMaleic hydrazide|\\bPropanedinitrile|\\bMethylthiouracil|\\bSelenium dioxide|\\bSelenium sulfide|\\bcarbon disulfide|\\bBenzeneethanamine",
  "\\bDiphosphoric acid|\\bFluoroacetic acid|\\bDiethylene glycol|\\bCarbamic chloride|\\bOsmium oxide OsO4|\\bCyanogen chloride|\\bMercury fulminate",
  "\\bHydrogen fluoride|\\bHydrofluoric acid|\\bMethacrylonitrile|\\bNickel cyanide Ni|\\bCopper cyanide Cu|\\bSilver cyanide Ag|\\bSodium cyanide Na",
  "\\bTetranitromethane|\\bArsenic pentoxide|\\bMethyl isocyanate|\\bNitrogen oxide NO|\\bPotassium cyanide|\\bPropargyl alcohol|\\bThiosemicarbazide",
  "\\bAmmonium vanadate|\\bPentachlorophenol|\\bMethylene bromide|\\bDibutyl phthalate|\\bDiethyl phthalate|\\bHexachlorobenzene|\\bPentachloroethane",
  "\\bMethyl chloroform|\\bTrichloroethylene|\\bHexachloropropene|\\bBendiocarb phenol|\\bCarbofuran phenol|\\btrichloroethylene|\\bCarbamothioic acid",
  "\\bNickel carbonyl Ni|\\bAluminum phosphide|\\bCarbon oxyfluoride|\\bCalcium cyanide Ca|\\bPhosphorus sulfide|\\bChloroacetaldehyde",
  "\\bNitrogen oxide NO2|\\bHydrogen phosphide|\\bVanadium pentoxide|\\bEthylene dibromide|\\bMethylene chloride|\\bDimethyl phthalate",
  "\\bEthyl methacrylate|\\bPentachlorobenzene|\\bPhthalic anhydride|\\bmethylene chloride|\\bMethyl ethyl ketone|\\bArsonous dichloride",
  "\\bMethyl methacrylate|\\bPotassium cyanide K|\\bCarbonic dichloride|\\bVanadium oxide V2O5|\\bDichloroethyl ether|\\bCarbonic difluoride",
  "\\bEthylene dichloride|\\bDiethylstilbesterol|\\bHexachlorobutadiene|\\bTetrachloroethylene|\\bmethyl ethyl ketone|\\bCarbamodithioic acid",
  "\\bPhosphorothioic acid|\\bBenzenebutanoic acid|\\bTetraphosphoric acid|\\bChromic acid H2 CrO4|\\bMethanesulfonic acid|\\bToluene diisocyanate",
  "\\bArsenic acid H3 AsO4|\\bArsenic oxide As2 O5|\\bArsenic oxide As2 O3|\\bDichloromethyl ether|\\bDichlorophenylarsine|\\bPropylene dichloride",
  "\\bHydrogen sulfide H2S|\\bCarbon tetrachloride|\\bcarbon tetrachloride|\\bCarbonochloridic acid|\\bThiodiphosphoric acid|\\bPhenylmercury acetate",
  "\\bThallium oxide Tl2 O3|\\bTrichloromethanethiol|\\bEthylidene dichloride|\\bPhosphorodithioic acid|\\bMethyl chlorocarbonate|\\bSelenium sulfide SeS2",
  "\\bMethyl isobutyl ketone|\\bDichloromethoxy ethane|\\bDiethylhexyl phthalate|\\bEthyl methanesulfonate|\\bOxiranecarboxyaldehyde",
  "\\bThallium chloride TlCl|\\btrichlorofluoromethane|\\bmethyl isobutyl ketone|\\bPhosphorofluoridic acid|\\bPentachloronitrobenzene",
  "\\bAcetic acid ethyl ester|\\bHydrazinecarbothioamide|\\bDichloroisopropyl ether|\\bDichlorodifluoromethane|\\bBenzenesulfonyl chloride",
  "\\bHexaethyl tetraphosphate|\\bPotassium silver cyanide|\\bTetraethyl pyrophosphate|\\bPhysostigmine salicylate|\\bFormetanate hydrochloride",
  "\\bChloromethyl methyl ether|\\bHexachlorocyclopentadiene|\\bchlorinated fluorocarbons|\\bCresols and cresylic acid|\\bDiisopropylfluorophosphate",
  "\\bDimethylcarbamoyl chloride|\\bTrichloromonofluoromethane|\\bThioimidodicarbonic diamide|\\bOctamethylpyrophosphoramide",
  "\\bMethyl ethyl ketone peroxide|\\bBenzenesulfonic acid chloride|\\bTetraethyldithiopyrophosphate|\\bEthylenebisdithiocarbamic acid",
  "\\bEthylene glycol monoethyl ether|\\bManganese dimethyldithiocarbamate", 
  "\\bDDD\\b|\\bDDT\\b",
  sep = "|")


developing_country_name_ls = paste(
  "Aruba|Afghanistan|Angola|Anguilla|Albania|United Arab Emirates|Argentina|Armenia|American Samoa", 
  "French Southern Territories|Antigua and Barbuda|Azerbaijan|Burundi|Benin|Burkina Faso|Bangladesh|Bahrain|Bahamas",
  "Bosnia and Herzegovina|Belarus|Belize|Bolivia|Brazil|Barbados|Brunei Darussalam|Bhutan|Botswana|Central African Republic",
  "Chile|China|Côte d'Ivoire|Cameroon|Congo|Cook Islands|Colombia|Comoros|Cabo Verde|Costa Rica|Cuba|Curaçao|Cayman Islands",
  "Cyprus|Cyprus|Djibouti|Dominica|Dominican Republic|Algeria|Ecuador|Egypt|Eritrea|Western Sahara|Ethiopia|Fiji|Micronesia",
  "Gabon|Georgia|Ghana|Guinea|Gambia|Guinea-Bissau|Equatorial Guinea|Grenada|Guatemala|Guam|Guyana|Heard Island and McDonald Islands",
  "Honduras|Haiti|Indonesia|India|Iran|Iraq|Jamaica|Jordan|Kazakhstan|Kenya|Kyrgyzstan|Cambodia|Kiribati|Saint Kitts and Nevis|Kuwait|\\bLao",
  "Lebanon|Liberia|Libya|Saint Lucia|Sri Lanka|Lesotho|Macao|Saint Martin|Morocco|Moldova|Madagascar|Maldives|Mexico|Marshall Islands",
  "North Macedonia|Mali|Myanmar|Montenegro|Mongolia|Northern Mariana Islands|Mozambique|Mauritania|Montserrat|Mauritius|Malawi|Malaysia",
  "Namibia|New Caledonia|Niger|Norfolk Island|Nigeria|Nicaragua|Niue|Nepal|Nauru|Oman|Pakistan|Panama|Pitcairn|Peru|Philippines|Palau",
  "Papua New Guinea|Puerto Rico|Paraguay|Palestine|French Polynesia|Qatar|Russia|Rwanda|Sudan|Senegal|Singapore|South Georgia and the South Sandwich Islands",
  "Saint Helena|Solomon Islands|Sierra Leone|El Salvador|Somalia|Serbia|South Sudan|Sao Tome and Principe|Suriname|Eswatini|Sint Maarten|Seychelles|Syrian",
  "Turks and Caicos Islands|Chad|Togo|Thailand|Tajikistan|Turkmenistan|Timor-Leste|Tonga|Trinidad and Tobago|Tunisia|Turkey|Tanzania|Uganda|Ukraine|Uruguay",
  "Uzbekistan|Saint Vincent and the Grenadines|Venezuela|Virgin Islands|Viet Nam|Vanuatu|Wallis and Futuna|Samoa|Yemen|South Africa|Zambia|Zimbabwe|Somaliland",
  "Kosovo|Ashmore|Cartier|Siachen Glacier|North Korea", 
  # func_AND_vector(c('Democratic', 'Korea')), ## this might cause errors
  sep = "|")


## "ARE" might match the common word "are" in a sentence - thus removed. 
## "MAR" might match "Mar" (e.g., March 1st, Mar 1st)
## "PRE" might match "per year"
developing_country_iso3_ls = paste(
  "ABW|AFG|AGO|AIA|ALB|ARG|ARM|ASM|ATF|ATG|AZE|BDI|BEN|BFA|BGD|BHR|BHS|BIH|BLR|BLZ|BOL|BRA|BRB|BRN|BTN|BWA",
  "CAF|CHL|CHN|CIV|CMR|COD|COG|COK|COL|COM|CPV|CRI|CUB|CUW|CYM|CYP|CYP|DJI|DMA|DOM|DZA|ECU|EGY|ERI|ESH|ETH|FJI|FSM|GAB|GEO|GHA|GIN|GMB|GNB|GNQ|GRD|GTM|GUM|GUY",
  "HMD|HND|HTI|IDN|IND|IRN|IRQ|JAM|JOR|KAZ|KEN|KGZ|KHM|KIR|KNA|KOR|KWT|LAO|LBN|LBR|LBY|LCA|LKA|LSO|MAF|MDA|MDG|MDV|MEX|MHL|MKD|MLI|MMR|MNE|MNG|MNP",
  "MOZ|MRT|MSR|MUS|MWI|MYS|NAM|NCL|NER|NFK|NGA|NIC|NIU|NPL|NRU|OMN|PAK|PAN|PCN|PHL|PLW|PNG|PRI|PRK|PRY|PSE|PYF|QAT|RUS|RWA|SDN|SEN|SGP|SGS|SHN|SLB|SLE|SLV",
  "SOM|SRB|SSD|STP|SUR|SWZ|SXM|SYC|SYR|TCA|TCD|TGO|THA|TJK|TKM|TLS|TON|TTO|TUN|TUR|TZA|UGA|UKR|URY|UZB|VCT|VEN|VIR|VNM|VUT|WLF|WSM|YEM|ZAF|ZMB|ZWE", 
  sep = "|")
developing_country_iso3_ls <- gsub('\\|', '\\\\b|\\\\b', developing_country_iso3_ls)
developing_country_iso3_ls <- paste0('\\b', developing_country_iso3_ls, '\\b')
developing_country_iso3_ls


### The 2-letter country code can match many relevant texts, so we would rather not use this.  
# developing_country_iso2_ls = paste(
#   "AW|AF|AO|AI|AL|AE|AR|AM|AS|TF|AG|AZ|BI|BJ|BF|BD|BH|BS|BA|BY|BZ|BO|BR|BB|BN|BT|BW|CF|CL|CN|CI|CM|CD|CG|CK|CO|KM|CV|CR|CU|CW|KY|CY|CY|DJ|DM|DO|DZ|EC|EG|ER|EH",
#   "ET|FJ|FM|GA|GE|GH|GN|GM|GW|GQ|GD|GT|GU|GY|HK|HM|HN|HT|ID|IN|IR|IQ|JM|JO|KZ|KE|KG|KH|KI|KN|KR|KW|LA|LB|LR|LY|LC|LK|LS|MO|MF|MA|MD|MG|MV|MX|MH|MK|ML|MM|ME|MN",
#   "MP|MZ|MR|MS|MU|MW|MY|NA|NC|NE|NF|NG|NI|NU|NP|NR|OM|PK|PA|PN|PE|PH|PW|PG|PR|KP|PY|PS|PF|QA|RU|RW|SD|SN|SG|GS|SH|SB|SL|SV|SO|RS|SS|ST|SR|SZ|SX|SC|SY|TC|TD|TG",
#   "TH|TJ|TM|TL|TO|TT|TN|TR|TZ|UG|UA|UY|UZ|VC|VE|VI|VN|VU|WF|WS|YE|ZA|ZM|ZW", 
#   sep = "|")
# developing_country_iso2_ls <- gsub('\\|', '\\\\b|\\\\b', developing_country_iso2_ls)
# developing_country_iso2_ls <- paste0('\\b', developing_country_iso2_ls, '\\b')
# developing_country_iso2_ls


developing_country_group_ls = 
  c("developing|least develop|least.?developed|less develop|underdevel|\\bpoor|low.?income|low income|small island|africa|\\bimpover|\\bpover", 
    "countr|\\bnation|\\bstate.?\\b")
developing_country_group_ls = func_AND_vector(developing_country_group_ls)

developing_country_ls = func_OR_vector(v = c(developing_country_name_ls, 
                                             developing_country_iso3_ls,
                                             # developing_country_iso2_ls,
                                             'global south|Third World'
                                             # developing_country_group_ls
))
developing_country_ls2 <- developing_country_group_ls





## 1. No Poverty -----------------------------------------------------------------------------------

SDG1_1 = c(poverty_ls, 
           reduce_ls, 
           "extreme|\\bmost\\b|excess|exceeding|\\bhigh\\b|\\bhuge\\b|great|\\bover\\b|remarkabl|striking|\\bsevere|serious")

SDG1_2 = c(poverty_ls, 
           reduce_ls)  

SDG1_3 = c(paste(poverty_ls, "vulnerab", sep = "|"), 
           "assure|\\bcare.?\\b|\\bcover|preserve|protect|safeguard|secure|shelter|shield|support")

SDG1_4 = c("access to|\\brights to", 
           "resource.?\\b|basic service|property|inheritance|ownership|\\bland|financial service|\\bestate|\\bhome\\b|\\bhouse\\b|\\bhouses\\b|\\bhousing\\b|\\bfarms")
SDG1_4 <- func_AND_plus(SDG1_4)
SDG1_4 <- gsub("\\.\\+", "(?!.*financial statement)(?!.*financial press)(?!.*financial result)(?!.*financial instrument)(?!.*Access Bank)(?!.*voting right)", SDG1_4) ## to exclude 'financial statement'



SDG1_5 = c(paste(poverty_ls, "vulnerab", sep = "|"), 
           disaster_ls, 
           reduce_ls)

SDG1_a_x = c(poverty_ls, 
             "econom|financ|develop", 
             paste(support_ls, "empower|relie|program|\\bpolicy|\\bpolicies", sep = "|"))
SDG1_a_y = c('government', 
             'expenditure|\\bspend',
             "essential service|education|health|social protection") 

SDG1_b_x = c(poverty_ls, 
             reduce_ls, 
             paste(policy_ls, support_ls, sep = '|'))

SDG1_b_y = c('government', 
             'expenditure|\\bspend', 
             'women|\\bpoor|vulnerable')



### further ------------- ###
SDG1_1 <- func_AND_plus(SDG1_1)
SDG1_2 <- func_AND_plus(SDG1_2)
SDG1_3 <- func_AND_plus(SDG1_3)

SDG1_5 <- func_AND_plus(SDG1_5)

SDG1_a_x <- func_AND_plus(SDG1_a_x)
SDG1_a_y <- func_AND_plus(SDG1_a_y)


SDG1_b_x <- func_AND_plus(SDG1_b_x)
SDG1_b_y <- func_AND_plus(SDG1_b_y)



# SDG1_a   <- func_OR_vector(c(SDG1_a_x, SDG1_a_y))
# SDG1_b   <- func_OR_vector(c(SDG1_b_x, SDG1_b_y))









## 2. Zero Hunger ----------------------------------------------------------------------------------

SDG2_1_x = c(reduce_ls, "hunger|undernourish|undernutrion|starv|famine|malnourish|malnutrition") 
SDG2_1_y = c(reduce_ls, "food|nutrition", "insecurity|desert|deprivation|deficien")
SDG2_1_z = c("food|nutrition", "access|safe|secur|nutritious|sufficient|\\bample|plentiful|abundant")


temp <- "malnutrition|malnourish|undernourish|undernutrition|under nourished|stunting|wasting|overweight|underweight|\\bpolio|paralysis|tephromyelitis|nutrition|anaem|anem"
SDG2_2_x = c(temp,
             paste(child_ls, "infant|neonate|newborn|baby|babies|older", sep = '|'))
SDG2_2_y = c(temp,
             "pregnan|lactat", 
             "women|woman")


SDG2_3_x = c(paste("\\bdouble", increase_ls, sep = "|"), 
             ag_ls, 
             "productiv|production|income")
SDG2_3_y = c("access to", 
             ag_ls,
             "\\bland|land tenure right|land right|land own|resource|\\binput|knowledge|financial service|market|opportunit|employ")
SDG2_3_z = c("\\breform", 
             ag_ls,
             "\\bland")

SDG2_4_x = c(ag_ls, 
             "sustain|resilien|productiv|organic|ecological")
SDG2_4_y = c("adapt", 
             paste(disaster_ls, "climate change|global warming|\\bwarm", sep = '|'))
SDG2_4_z = c("adapt", 
             'temperat|\\bsea level', 
             "\\bris")
SDG2_4_w = c("\\bland|\\bsoil\\b", 
             "quality|fertil")


SDG2_5_x = c("manag|maintain|diversif|diversity|conserv|secur|cultivat|farm|domesticat", 
             "seed|plant bank|animal|genetic|wild species")
SDG2_5_y = c("manag|maintain|diversif|diversity|conserv|secur|cultivat|farm|domesticat",
             "local|\\btraditional\\b|indigen|aboriginal|native|endemic", "breed|bred|knowledge")
SDG2_5_z = c("genetically modified|\\bGMO\\b", "food")


SDG2_a_x = c(paste(support_ls, "cooperat|collaborat|joint effort|\\bflow\\b|\\bflows\\b", '|'), 
             "rural|agricult",
             "infrastructure|research|investig|exten|\\badvanc|technolog") 
SDG2_a_y = c("\\bplant|\\bsoy|livestock|animal|cattle|\\bcow|\\bpig|sheep|\\bhog|horse|oxen|\\bherd|swine", "gene bank")


SDG2_b_x = c(ag_ls, 
             "export|\\btrade\\b|supply chain|value chain|market|business|commerce", 
             paste("\\bsubsidy|\\bsubsidies|\\bsubsidiz|\\bsubsidis|restrict|distort|allowance", support_ls, sep = '|'))
SDG2_b_y = "Doha Development Round|Doha Round|\\bDDR\\b|D\\.D\\.R\\."

SDG2_c   = c(ag_ls, "market|price", "volatil|anomal|change|unstable|unsettled|elastic|elusive")


### further ------------- ###
SDG2_1_x <- func_AND_plus(SDG2_1_x)
SDG2_1_y <- func_AND_plus(SDG2_1_y)
SDG2_1_z <- func_AND_plus(SDG2_1_z)


SDG2_2_x <- func_AND_plus(SDG2_2_x)
SDG2_2_y <- func_AND_plus(SDG2_2_y)


SDG2_3_x <- func_AND_plus(SDG2_3_x)
SDG2_3_y <- func_AND_plus(SDG2_3_y)
SDG2_3_z <- func_AND_plus(SDG2_3_z)


SDG2_4_x <- func_AND_plus(SDG2_4_x)
SDG2_4_y <- func_AND_plus(SDG2_4_y)
SDG2_4_z <- func_AND_plus(SDG2_4_z)
SDG2_4_w <- func_AND_plus(SDG2_4_w)


SDG2_5_x <- func_AND_plus(SDG2_5_x)
SDG2_5_y <- func_AND_plus(SDG2_5_y)
SDG2_5_z <- func_AND_plus(SDG2_5_z)


SDG2_a_x <- func_AND_plus(SDG2_a_x)
SDG2_a_y <- func_AND_plus(SDG2_a_y)


SDG2_b_x <- func_AND_plus(SDG2_b_x)


SDG2_c   <- func_AND_plus(SDG2_c)


### may no need to put them together ------------ #

# SDG2_1   <- func_OR_vector(c(SDG2_1_x, SDG2_1_y, SDG2_1_z))
# SDG2_2   <- func_OR_vector(c(SDG2_2_x, SDG2_2_y))
# SDG2_3   <- func_OR_vector(c(SDG2_3_x, SDG2_3_y, SDG2_3_z))
# SDG2_4   <- func_OR_vector(c(SDG2_4_x, SDG2_4_y, SDG2_4_z, SDG2_4_w))
# SDG2_5   <- func_OR_vector(c(SDG2_5_x, SDG2_5_y, SDG2_5_z))
# SDG2_a   <- func_OR_vector(c(SDG2_a_x, SDG2_a_y))
# SDG2_b   <- func_OR_vector(c(SDG2_b_x, SDG2_b_y))










## 3. Good Health and Well-being  ------------------------------------------------------------------


SDG3_1 = c(paste("matern|antenatal|birth|gestational|parturient", func_AND_plus(c("post","natal|partum")), sep = "|"), 
           paste(death_ls, "healthcare|health care|health-care|complica|depress", sep = "|"),
           reduce_ls)

SDG3_2 = c(paste(child_ls, "newborn|foetal|premature|preterm|pediatric|perinatal|infant|neonat", sep = '|'),
           paste(death_ls, "syndrome|wellness|wellbeing|well being|well-being", sep = '|'),
           reduce_ls)


temp <- paste(
  "epidemic|pandemic|outbreak|communicable disease|\\binfect|contagious|endemic|\\bSARS\\b|acute respiratory syndrome|zika|Zikv", 
  "dengue|schistosomiasis|ebola|measles|cholera|Chagas|\\bAIDS\\b|\\bHIV\\b|Acquired immunodeficiency syndrome|Human immunodeficiency virus", 
  "Yellow fever|Middle East respiratory syndrome|MERS-CoV|Antiretroviral|tuberculos|malaria|'tropical disease'|trypanosom|hepatit|Lyme disease", 
  "COVID|2019-nCoV|SARS-CoV-2|SARS-CoV2|HCoV-2019|\\bHCOV\\b|NCOVID-19|coronavirus|corona virus",
  "water borne|water-borne",
  "sexual transmi", sep = "|"
)
SDG3_3 = c(temp, reduce_ls)



SDG3_4_x1 = c(
  paste("non.?communicable",
        "\\bNCD\\b|cardiovascular|heart attack|myocard infarct|cerebrovascular accident|\\bCVA\\b", 
        "cancer|carcinoma|lung emphysema|neoplasm|tumor|tumour",
        "diabet|copd|coad|asthma|diarrhea|diarrhoea|dysentery|\\bobes|suicid|stroke", sep = '|'),
  death_ls,
  reduce_ls);

SDG3_4_x2 = c(
  "chronic", 
  "bronchitis|disease|hepatitis|pulmonary|respiratory|\\bstress", 
  reduce_ls);

SDG3_4_y = c(
  "\\bmental|mood|psychological|psychiatric|psychosis|psychotic|cogniti|emotion", 
  "disorder|disease|\\bhealth\\b|wellness|illness|\\bsick|disabilit", 
  reduce_ls)
SDG3_4_z = c('life expectancy', increase_ls)



SDG3_5 = c("substance|drug|alcohol|drinking|drunk|ethanol|liquor|liqueur|booze|\\bwine|\\bbeer|Narcotic", 
           "\\babus|misuse|misconduct|obsessive|addict|\\bharm|hazardous|disorder",
           paste('\\btreatment', reduce_ls, sep = "|"))

SDG3_6 = c("\\broad|\\blane|avenue|street|\\bdrive|\\bcar\\b|automobile|motor|vehicle|highway|expressway|freeway|\\brail|congest|collision|collid|\\bjam|traffic|transport|transit|travel", 
           paste(death_ls, 
                 "fatal|injur|accident|adversit|bottleneck|damag|wound|catastroph|calamit|casualt|crash|disaster|\\bharm|hazard|emergenc|traged", 
                 sep = "|"))

SDG3_7_x1 = c(
  "family planning|contracept|condom|diaphragm|Birth Control|intrauterine device|\\bIUD\\b|conception control",
  "access|availab|modern|information|informing|\\binform\\b|\\binforms\\b|\\binformed\\b|educat")

SDG3_7_x2 = c(
  "\\bsex|reproductive", 
  "\\bcare\\b",
  "access|availab|modern|information|informing|\\binform\\b|\\binforms\\b|\\binformed\\b|educat")

SDG3_7_x3 = c(
  "unintended|unwanted", 
  "pregnanc|motherhood",
  "access|availab|modern|information|informing|\\binform\\b|\\binforms\\b|\\binformed\\b|educat")

SDG3_7_y = c(child_ls, "birth|pregnan")


SDG3_8_x = c("health|Medical|\\bhearing|\\bDental|\\bVision\\b",
             "coverage|\\bcare\\b|service|benefit",
             "universal|access|availab|affordable|\\bpublic|essential")
SDG3_8_y = c("health|medicine|vaccin|pharmac", 
             "household",
             "expenditure|expense|income|financ")
SDG3_8_y <- func_AND_plus(SDG3_8_y) 
SDG3_8_y <- gsub("\\.\\+", "(?!.*financial statement)", SDG3_8_y) ## to exclude 'financial statement'


SDG3_9_x = c("hazard|unsafe|unintentional|inadequate|contamin|pollut", 
             "chemical|\\bair\\b|\\bwater\\b|\\bsoil\\b|sanita|hygien|poison",
             paste(death_ls, "illness|\\bsick", sep = "|"))
SDG3_9_y = c("\\black", 
             "sanita|hygien|poison", 
             paste(death_ls, "illness|\\bsick", sep = "|"))


SDG3_a = c("tobacco|nicotine|cigar|\\bvap|smok" , 
           paste("control|regulat|administer|\\bban\\b|\\bbans\\b|\\bbanned\\b|govern|manag|cessation|\\bquit", reduce_ls, sep = "|"))
SDG3_a   <- func_AND_plus(SDG3_a)
SDG3_a   <- gsub("\\.\\+", "(?!.*government)(?!.*Governance)(?!.*Management)(?!.*manager)", SDG3_a) ## to exclude these


SDG3_b_x = c(
  paste("vaccin|medicin|medication|antibiotic|pharmac", "health facilit", sep = '|'),
  "research|develop",
  developing_country_ls,
  support_ls)
SDG3_b_y = c(
  paste("vaccin|medicin|medication|antibiotic|pharmac", "health facilit", sep = '|'),
  "access|\\bavailab|afford|TRIPS Agreement|Trade-Related Aspects of Intellectual Property Rights")
SDG3_b_x <- func_AND_plus(SDG3_b_x)
SDG3_b_y <- func_AND_plus(SDG3_b_y)
SDG3_b_x <- gsub("\\.\\+", "(?!.*development expense)(?!.*investigat)(?!.*Invest Ltd)", SDG3_b_x) ## to exclude these
SDG3_b_y <- gsub("\\.\\+", "(?!.*available data)(?!.*extension)", SDG3_b_y) ## to exclude these


## ------------------------------------------------------------------------------------- ##
## For this case, we used two options to find match 
SDG3_c_x = c("\\bhealth\\b|well.?being", 
             "financ[a-z]*|\\bfund.?\\b|\\bfunding\\b|\\bassistance|\\baid|invest|donor|donat|\\bgrant.?\\b|official flow")
temp <- SDG3_c_x
term_ls  <- c('statement', 'granted', 'investigat', 'financial assumption', 'finance corporation', 
              'finance condition', 'First Aid', 'Animal Health', 'funded')

### Option 1: use `func_AND_plus` function
# SDG3_c_x <- func_AND_plus(SDG3_c_x)
# SDG3_c_x <- func_to_exclude_terms(SDG3_c_x, terms_to_exclude = term_ls) ## to exclude these

### Option 2: to use look around function ----
SDG3_c_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, exclude = term_ls)
## ------------------------------------------------------------------------------------- ##


SDG3_c_y = c("\\bhealth\\b",
             "workforce|\\bworker|employ|\\blabor|\\blabour",
             "recruit|develop|\\btrain|retention")
SDG3_c_y <- func_AND_plus(SDG3_c_y)  


SDG3_d_x = c("health", 
             "early warning|manag|risk reduction|emergency|\\burgen",
             increase_ls)
SDG3_d_y = c("International Health Regulations|\\bIHR\\b|health risk",
             "capacity")
SDG3_d_z1 = c("blood", 
              "\\binfect", 
              "resist")
SDG3_d_z2 = c("antibiotic|antimicr", 
              "resist")

SDG3_general_x = c('health|good|quality', 
                   paste("\\blife\\b|\\blives\\b", child_ls, female_ls, sep = "|"))
SDG3_general_y = "well.?being"

SDG3_general_x <- func_AND_plus(SDG3_general_x)




SDG3_1 <- func_AND_plus(SDG3_1) 
SDG3_2 <- func_AND_plus(SDG3_2) 
SDG3_3 <- func_AND_plus(SDG3_3) 

SDG3_4_x1 <- func_AND_plus(SDG3_4_x1)
SDG3_4_x2 <- func_AND_plus(SDG3_4_x2)
SDG3_4_y  <- func_AND_plus(SDG3_4_y)
SDG3_4_z  <- func_AND_plus(SDG3_4_z)


SDG3_5 <- func_AND_plus(SDG3_5)
SDG3_6 <- func_AND_plus(SDG3_6)

SDG3_7_x1 <- func_AND_plus(SDG3_7_x1)
SDG3_7_x2 <- func_AND_plus(SDG3_7_x2)
SDG3_7_x3 <- func_AND_plus(SDG3_7_x3)
SDG3_7_y  <- func_AND_plus(SDG3_7_y)


SDG3_8_x <- func_AND_plus(SDG3_8_x) 
SDG3_8_y <- SDG3_8_y

SDG3_9_x <- func_AND_plus(SDG3_9_x) 
SDG3_9_y <- func_AND_plus(SDG3_9_y)


SDG3_d_x  <- func_AND_plus(SDG3_d_x)
SDG3_d_y  <- func_AND_plus(SDG3_d_y)
SDG3_d_z1 <- func_AND_plus(SDG3_d_z1)
SDG3_d_z2 <- func_AND_plus(SDG3_d_z2)




### may no need to put them together ------------ #

# SDG3_4   <- func_OR_vector(c(SDG3_4_x1, SDG3_4_x2, SDG3_4_y,  SDG3_4_z))
# SDG3_7   <- func_OR_vector(c(SDG3_7_x1, SDG3_7_x2, SDG3_7_x3, SDG3_7_y))
# SDG3_8   <- func_OR_vector(c(SDG3_8_x, SDG3_8_y))
# SDG3_9   <- func_OR_vector(c(SDG3_9_x, SDG3_9_y))
# SDG3_b   <- func_OR_vector(c(SDG3_b_x, SDG3_b_y))
# SDG3_c   <- func_OR_vector(c(SDG3_c_x, SDG3_c_y))
# SDG3_d   <- func_OR_vector(c(SDG3_d_x, SDG3_d_y, SDG3_d_z1, SDG3_d_z2))
# SDG3_general <- func_OR_vector(c(SDG3_general_x, SDG3_general_y))







## 4. Quality Education ----------------------------------------------------------------------------- 


SDG4_1_x1 = c(child_ls, 
              "primary|secondary|middle", 
              "educat|school")
SDG4_1_x2 = c(child_ls, 
              "\\bgrade", 
              "\\b2\\b|\\b2nd\\b|second|\\btwo|\\b3\\b|third|\\b3rd\\b|three")
SDG4_1_x3 = c(child_ls, 
              "reading|\\bmath|educat|school",
              "proficiency|competence|skill")
SDG4_1_y = c(child_ls,
             "inclusi|quality|equitab|\\bfree\\b|access|affordab",
             "educat|school")

SDG4_2_x = c(
  paste(child_ls, "primary entry age|pre.?primary", sep = "|"),
  "develop|\\bcare.?\\b|educat|health|learn|wellbeing|well being|well-being|psycho") 
SDG4_2_x <- func_AND_plus(SDG4_2_x) 
SDG4_2_x <- gsub("\\.\\+", "(?!.*young)", SDG4_2_x) ## to exclude these

SDG4_2_y <- "pre.?school"


temp     <- "access|availab|equitab|equal|\\bfree\\b|affordable|quality|particip|\\bjoin\\b|\\bjoins|\\bjoined|\\bjoining|attend|lifelong|adult|non.?traditional"
SDG4_3_x <- c("technical|vocational|tertiary", 
              "educat|\\bcollege.?\\b|universit|\\btraining|learn|high school",
              temp)
SDG4_3_y <- c("\\bGED\\b|G\\.E\\.D\\.|General Educational Development", 
              temp)
SDG4_3_x <- func_AND_plus(SDG4_3_x) 
SDG4_3_y <- func_AND_plus(SDG4_3_y) 
SDG4_3_x <- func_to_exclude_terms(which_sdg_term = SDG4_3_x, terms_to_exclude = c('Access Bank', 'Information is available', 'job fair')) ## to exclude these
SDG4_3_y <- func_to_exclude_terms(which_sdg_term = SDG4_3_y, terms_to_exclude = c('Access Bank', 'Information is available', 'job fair')) ## to exclude these


SDG4_4_x = c("technical|vocational|information and communications|\\bICT\\b",
             "skill|competenc|technique|expert|certificate|diploma",
             "\\bemployment|\\bjob|entrepreneur")
SDG4_4_y = c("financial literacy",
             "\\bemployment|\\bjob|entrepreneur")


SDG4_5 = c(
  "education|vocational|\\btraining|school",
  "disparit|parit|equal access|equit|equalit|inclusi|exclus|inequalit|discrimin",
  paste("disabilit|disabled|handicap|wounded|indigen|aboriginal|vulnerable|\\bgender|female|women|rural", 
        # urban_ls,              ## -> can be too broad --> removed
        # "bottom|top|conflict", ## -> can be too broad
        "underrepresented|wealth|low.?income",
        sep = '|'))

SDG4_6 = "literacy|numeracy|literate|reading skill|writing skill"
SDG4_6 = paste0('^(?=.*(?:', SDG4_6, '))',  "(?!.*financial literacy)")


SDG4_7_x = c("\\bknowledge|skill|educat|curricula|teach|student assessment",
             "sustainable development|sustainable lifestyle|human right|gender equality|peace|non.?violence|cultural diversity")
SDG4_7_y = c("global citizenship|international|Universal",  
             "education")

SDG4_a = c("educat|learn|school", 
           "facilit|infrastructure|environment|basic service|electricity|internet|computer|drinking water|sanitation|hand.?washing",
           "\\bbuild|\\bupgrade|\\boffer|\\bprovide")

SDG4_b = c(paste("scholarship|fellowship|official development assistance", 
                 "financ|\\bfund.?\\b|\\bfunding\\b|\\baid|invest|\\bgrant.?\\b|official flow", 
                 sep = "|"),  
           paste("knowledge|skill|educat|vocational training|information technology|communications technology",
                 "information and communications technology",
                 "\\btechnical\\b|engineer|scien|mathemat|technolo|\\bSTEM\\b|S\\.T\\.E\\.M\\.", sep = "|"),
           developing_country_ls) 
SDG4_b <- func_AND_plus(SDG4_b)
SDG4_b <- gsub("\\.\\+", "(?!.*Corporate Governance)", SDG4_b) ## to exclude these



SDG4_c = c('teacher|educator|faculty|instructor|lecturer|professor|supervisor|mentor.?\\b|tutor|trainer|adviser|advisor|\\bcoach\\b|\\bcoaches\\b|caregiver', 
           "supply|attrition|qualif|\\btrain|capab")


#### other related but not included in the above 
SDG4_general = c("school|educat|learning",
                 "inclusive|attend|enroll|basic|\\bequity|\\bequitab|safe|lifelong|lifetime|continuing")




SDG4_1_x1 <- func_AND_plus(SDG4_1_x1)
SDG4_1_x2 <- func_AND_plus(SDG4_1_x2)
SDG4_1_x3 <- func_AND_plus(SDG4_1_x3)
SDG4_1_y <- func_AND_plus(SDG4_1_y)


SDG4_4_x <- func_AND_plus(SDG4_4_x)
SDG4_4_y <- func_AND_plus(SDG4_4_y)

SDG4_5 <- func_AND_plus(SDG4_5)  
SDG4_6 <- SDG4_6

SDG4_7_x <- func_AND_plus(SDG4_7_x)
SDG4_7_y <- func_AND_plus(SDG4_7_y)

SDG4_a <- func_AND_plus(SDG4_a)
SDG4_b <- (SDG4_b)
SDG4_c <- func_AND_plus(SDG4_c)
SDG4_general <- func_AND_plus(SDG4_general)



# SDG4_1   <- func_OR_vector(c(SDG4_1_x1, SDG4_1_x2, SDG4_1_x3, SDG4_1_y))
# SDG4_2   <- func_OR_vector(c(SDG4_2_x, SDG4_2_y))
# SDG4_3   <- func_OR_vector(c(SDG4_3_x, SDG4_3_y))
# SDG4_4   <- func_OR_vector(c(SDG4_4_x, SDG4_4_y))
# SDG4_7   <- func_OR_vector(c(SDG4_7_x, SDG4_7_y))







## -5. Gender Equality -----------------------------------------------------------------------------


SDG5_1_x = c("discrimin|bias|stereotype|equality|parity|justice|disparities|segregat|\\banti|hostile|prejudice|marginali",
             paste(female_ls, "\\bsex|\\bgender", sep = "|"))
SDG5_1_y = "misogyn|feminis"


SDG5_2_x = c("violence|assault|attack|aggression|offens|\\babuse|traffick|exploit|\\bforc|misconduct",
             paste(female_ls, "domestic|\\bsex|prostitution", sep = "|"))
SDG5_2_y = "prostitut|\\brape|harass"


SDG5_3_x = c(paste(child_ls, "\\bharm|\\bearly|forced|before age 15|before age 18", sep = "|"),
             "marr")
SDG5_3_y = c(paste(female_ls, '\\bgender', sep = "|"),
             'genital', 
             "mutilat|\\bcut")


SDG5_4 = c("unpaid|shared responsibility|allocation|equity|equitab|fairness", 
           paste("\\bcare.?\\b|domestic work|household work|family work", female_ls, sep = "|"))


SDG5_5 = c(paste("leadership|leader|\\bseat|decision.?making|national parliament|government|autonomy",
                 "managerial position|political participation|politician|manager|in middle management|in senior management|underrepresentation|entrepreneurship|\\bpower|glass ceiling", sep = "|"),
           female_ls)


SDG5_6_x = c("access|availab|\\bright|decision|\\blaw|regulation|information|educat", 
             "sexual|\\breproduct|\\bcontracept|family planning|divorc|parenting plan|\\babort")
SDG5_6_y = c("\\bSEX\\b|health.?care", 
             "information|educat", 
             "\\bfull\\b|\\bequal\\b")


SDG5_a_x = c("access|availab|\\bright|\\breform|equal|\\blegal|\\blaw",
             "resource|\\bfund|ownership|control|\\bland|property|financial service|finance|inheritance|owner|budget|\\bquota\\b",
             paste(female_ls, "\\bgender", sep = "|"))
SDG5_a_y = "agrarian feminism"


SDG5_b = c("tech|information|empower|mobile|telephone|\\bICT\\b",
           female_ls)	

SDG5_c = c(paste(policy_ls, "legislation", sep = "|"),
           paste("\\bgender|\\bsex", female_ls, sep = "|"),
           "equal|parity|equit|fairness|promot|empower")

SDG5_general = c(paste("\\bgender|\\bsex", female_ls, sep = "|"),
                 "equal")



SDG5_1_x <- func_AND_plus(SDG5_1_x) 

SDG5_2_x <- func_AND_plus(SDG5_2_x) 

SDG5_3_x <- func_AND_plus(SDG5_3_x)
SDG5_3_y <- func_AND_plus(SDG5_3_y)

SDG5_4 <- func_AND_plus(SDG5_4) 
SDG5_5 <- func_AND_plus(SDG5_5) 

SDG5_6_x <- func_AND_plus(SDG5_6_x)
SDG5_6_y <- func_AND_plus(SDG5_6_y)

SDG5_a_x <- func_AND_plus(SDG5_a_x) 

SDG5_b <- func_AND_plus(SDG5_b) 
SDG5_c <- func_AND_plus(SDG5_c) 

SDG5_general <- func_AND_plus(SDG5_general) 



# SDG5_1   <- func_OR_vector(c(SDG5_1_x, SDG5_1_y))
# SDG5_2   <- func_OR_vector(c(SDG5_2_x, SDG5_2_y))
# SDG5_3   <- func_OR_vector(c(SDG5_3_x, SDG5_3_y))
# SDG5_6   <- func_OR_vector(c(SDG5_6_x, SDG5_6_y))
# SDG5_a   <- func_OR_vector(c(SDG5_a_x, SDG5_a_y))






## 6. Clean Water and Sanitation --------------------------------------------------------------------

SDG6_1 = c("access|availab|afford|safe|secure|\\bclean|manag|equitab", 
           "drinking water|drinkable water")

SDG6_2_x = c("access|availab|safe|manag|adequate|equitab|equal|facilit|service",
             paste("sanita|hygien|soap|cleanliness", "hand wash|wash.*hand", sep = '|'))
SDG6_2_y = c("access|availab|safe|\\bopen|hygien|adequate|equal|health|\\bclean|manag",
             "bathroom|defecation|toilet|restroom|lavatory|latrine|water closet|sewerage")


SDG6_3_x = c("\\bwater\\b|aquatic|aquifer",
             "pollut|black|contamination",
             reduce_ls)
SDG6_3_y = c('\\bwater\\b|aquatic', 'recycl|\\bre.?us')
SDG6_3_z = c('\\bwater\\b|aquatic', 'quality', paste('good', increase_ls, sep = "|"))
SDG6_3_w = c("dump|releas|recycl|\\bre.?us|untreated|\\btreated\\b|sustainab",
             "hazard|chemical|toxicology|ecotoxicology|wastewater|sewer")

SDG6_4_x = c("\\bwater\\b|freshwater", 
             "\\befficien|sustainab|secur|availab|suppl|conservat")
SDG6_4_y = c("\\bwater\\b|freshwater",
             "footprint|green|blue|grey|scarcity|shortage|\\bstress|dearth|deficit|\\black|shortfall|insufficiency|deficienc",
             paste('address', reduce_ls, sep = "|"))


SDG6_5 = c("water resource|watershed",
           paste("manag|transboundary|transnational|international|interbasin|cooperat|collaborat|joint effort|co.?operation|coping", policy_ls, sep = "|"))


SDG6_6 = c("\\bwater\\b|mountain|forest|wetland|river|aquifer|lake",
           "ecosystem|eco-system")


SDG6_a_x = c("\\bwater\\b|sanita",
             paste("development assistance|harvesting|desalina|\\befficien|waste|\\btreatment|recycl|\\bre.?us|technolog|purification|infrastructure", 
                   support_ls, sep = "|"),
             developing_country_ls)

SDG6_a_y = c("\\bwater\\b|sanita",
             "international", "\\bcoop|\\bco-op|collab",
             developing_country_ls)

SDG6_a_z = c("\\bwater\\b|sanita",
             "capacity", "build",
             developing_country_ls)

SDG6_b =  c("\\bwater\\b|sanita", 
            paste(support_ls, increase_ls, sep = "|"),
            "participat|administr|manag|\\bpolicy|\\bpolicies|\\blocal|regional|resident|indigen|aboriginal|\\btraditional\\b|communit|native|endemic")



SDG6_1   <- func_AND_plus(SDG6_1) 

SDG6_2_x <- func_AND_plus(SDG6_2_x) 
SDG6_2_y <- func_AND_plus(SDG6_2_y)


SDG6_3_x <- func_AND_plus(SDG6_3_x) 
SDG6_3_y <- func_AND_plus(SDG6_3_y) 
SDG6_3_z <- func_AND_plus(SDG6_3_z) 
SDG6_3_w <- func_AND_plus(SDG6_3_w) 

SDG6_4_x <- func_AND_plus(SDG6_4_x) 
SDG6_4_y <- func_AND_plus(SDG6_4_y) 

SDG6_5 <- func_AND_plus(SDG6_5) 
SDG6_6 <- func_AND_plus(SDG6_6) 

SDG6_a_x <- func_AND_plus(SDG6_a_x)
SDG6_a_y <- func_AND_plus(SDG6_a_y)
SDG6_a_z <- func_AND_plus(SDG6_a_z)


SDG6_b <- func_AND_plus(SDG6_b) 




# SDG6_2   <- func_OR_vector(c(SDG6_2_x, SDG6_2_y))
# SDG6_3   <- func_OR_vector(c(SDG6_3_x, SDG6_3_y, SDG6_3_z, SDG6_3_w))
# SDG6_4   <- func_OR_vector(c(SDG6_4_x, SDG6_4_y))
# SDG6_a   <- func_OR_vector(c(SDG6_a_x, SDG6_a_y, SDG6_a_z))






## 7. Affordable and Clean Energy ----------------------------------------------------------------

renewable_ls_x = c(paste("\\badvanc|alcohol|modern|\\bclean|\\brenewable|alternat|ethanol|biorefinery|\\bwind\\b|solar|biomass|nuclear|\\bsun\\b|\\btide\\b|tidal",
                         "\\bwave.?\\b|\\bwood.?\\b|thermal|geothermal|algae|Hydro|hybrid|hydrogen|synthetic|green|sustainable",
                         "enviroment.?friendly|eco.?friendly", sep = "|"),
                   "energy|\\bfuel.?\\b|\\bpower\\b")

renewable_ls_y = paste("\\brenewables|biofuel|bioenergy|biodiesel|biogas|bioethanol|hydropower", 
                       "hydroelectric|electrofuel|\\befuel|\\be-fuel|photovoltaic|wind farm|offshore wind|wind turbine|solar panel", sep = "|")
renewable_ls_x <- func_AND_vector(renewable_ls_x) ## not suitable to use `func_AND_plus` here
renewable_ls   <- func_OR_vector(c(renewable_ls_y, renewable_ls_x))



SDG7_1 = c(paste('energy|\\bfuel|\\bpower\\b', "electricity", sep = "|"), 
           "reliable|affordab|modern",
           "access|availab")


SDG7_2_w = c(renewable_ls, 
             "\\bshare|consum|transiti|shift")
### Option 1: use `func_AND_plus` function
# SDG7_2_w <- func_AND_plus(SDG7_2_w) 

### Option 2: to use look around function ----
temp <- SDG7_2_w
SDG7_2_w <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)



SDG7_2_x = c(renewable_ls, 
             "\\bmeet|fulfill",
             "\\demand|requirement")
SDG7_2_y = paste("energy transition", "smart grid|smart meter", sep = "|")



SDG7_2_z = c("fossil.?fuel|\\bcoal\\b|\\bcoals\\b|petrol|natural gas|methane|crude oil|gasoline|kerosene|non.?renewable", 
             reduce_ls)
### Option 1: use `func_AND_plus` function
# SDG7_2_z <- func_AND_plus(SDG7_2_z) 

### Option 2: to use look around function ----
temp <- SDG7_2_z
SDG7_2_z <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)


SDG7_3 = c("energy|\\belectric|\\bfuel|\\bpower\\b|\\butilit",
           "\\befficien|intens|pollut|footprint|\\bper GDP")

SDG7_a_x = c(renewable_ls, 
             "cooperat|collaborat|joint effort",
             paste("research|\\btech", finance_ls, sep = "|"))
SDG7_a_x <- func_AND_plus(SDG7_a_x)
SDG7_a_x <- gsub("\\.\\+", "(?!.*statement)(?!.*granted)(?!.*investigat)(?!.*financial assumption)(?!.*finance corporation)", SDG7_a_x) ## to exclude these keywords

SDG7_a_y = c('research|\\btechnolog|infrastructure|hybrid system', 
             'energy|\\belectric|\\bfuel',
             paste(increase_ls, support_ls, "access|availab", sep = "|"))

SDG7_b = c(renewable_ls, 
           developing_country_ls,
           support_ls)


####
SDG7_1   <- func_AND_plus(SDG7_1)


SDG7_2_x <- func_AND_plus(SDG7_2_x) 



SDG7_3 <- func_AND_plus(SDG7_3)

SDG7_a_x <- SDG7_a_x
SDG7_a_y <- func_AND_plus(SDG7_a_y)


SDG7_b <- func_AND_plus(SDG7_b)



# SDG7_2   <- func_OR_vector(c(SDG7_2_x, SDG7_2_y, SDG7_2_z, SDG7_2_w))
# SDG7_a   <- func_OR_vector(c(SDG7_a_x, SDG7_a_y))














## 8. Decent Work and Economic Growth --------------------------------------------------------------

economic_ls = paste("econom|\\bGDP\\b|gross domestic product",
                    "fiscal|monetary", 
                    sep = "|")
## taking away these synonyms due to excessive matches: (financ|profit|revenue|income|\\bgain|proceed|yield|dividend|\\bearning|\\bmonies|budget|return|\\bvalu) 

SDG8_1 = c(economic_ls, 
           "per capita",
           paste("Sustain|develop|prosper", increase_ls, sep = "|")) ## ,"nation|countr|state"

SDG8_2 = c(economic_ls, 
           paste("\\bstimulat", increase_ls, sep = "|"),
           "Productiv|\\befficien|technolo|innovation|moderniz|modernis|diversif|per employ|per work|per person")
SDG8_2 <- func_AND_plus(SDG8_2)
SDG8_2 <- gsub("\\.\\+", "(?!.*financial statement)(?!.*Financial Review)(?!.*financial condition)", SDG8_2) ## to exclude


SDG8_3_x = c("development",
             '\\bpolicy|\\bpolicies', 
             paste(economic_ls, "employ", sep = "|"),
             increase_ls)

SDG8_3_y = c("\\bemploy|\\bjob|\\bwork", 
             "decent|quality|stable|stabili|inclusiv",
             paste("\\bcreate|\\bcreati|support", increase_ls, sep = "|"))
# SDG8_3_y  <- func_AND_plus(SDG8_3_y)
### Option 3: to use look around + AND function ----
temp <- SDG8_3_y
SDG8_3_y <- lookaround_nearby_n_plus1AND(word_ls1 = temp[1], word_ls2 = temp[2], n = 5, third_AND_string = temp[3])


SDG8_3_z1 = c("productive activit|entrepreneurship",
              paste("support", increase_ls, sep = "|")) 

SDG8_3_z2 = c("micro-|small-|medium-|starting|startup|kickstart|inclusiv", 
              "enterprise|entrepren|\\bcompany\\b|\\bcompanies|\\bfirm.?\\b|\\bcorporate\\b|\\bcorporation|business",
              increase_ls)
# SDG8_3_z2 <- func_AND_plus(SDG8_3_z2)
### Option 3: to use look around + AND function ----
temp <- SDG8_3_z2
SDG8_3_z2 <- lookaround_nearby_n_plus1AND(word_ls1 = temp[1], word_ls2 = temp[2], n = 3, third_AND_string = temp[3])


SDG8_3_z3 = c("inclusive", 
              "economic growth")

SDG8_3_z4 = c("informal employment", 
              reduce_ls)



SDG8_4_x1 = c("sustain|\\befficien", 
              "resource",
              "consumption|production")

SDG8_4_x2 = "resource.?efficien*|efficient resource"

SDG8_4_y = c("consumption|footprint", 
             "per GDP|per capita|\\befficien",
             "material.?\\b")
SDG8_4_z = c("environmental degradation|environmental impact|environmental issue|environmental risk",
             paste(reduce_ls, "decoupl|informed", sep = "|"))


SDG8_5_x1 = c(paste("\\bfull\\b|\\bdecent|productiv", "inclusi", sep = "|"),
              "\\bemployment|\\bhiring|\\bemployee|\\bjob|\\bwork\\b|\\bworker|\\bstaff")
### Option 1: use `func_AND_plus` function
# SDG8_5_x1 <- func_AND_plus(SDG8_5_x1)
### Option 2: to use look around function ----
temp <- SDG8_5_x1
SDG8_5_x1 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6)


SDG8_5_x2 = c("discrimin|informal|precarious|inequal|inequit|disparit",
              "\\bemployment|\\bhiring|\\bemployee|\\bjob|\\bworker|\\bwork\\b|\\bworking",
              reduce_ls)

SDG8_5_x3 = c("equal|\\bequitab", 
              "\\bwage|\\bpay|\\bsalary|\\bsalaries|\\bearning|\\bstipend")

SDG8_5_x4 = c("equal|\\bequitab", 
              "opportunit",
              "people|person|\\bemployee|\\bstaff|\\bworker|\\bwork\\b|\\bworking|woman|women|\\bgender|\\bsex|ethni|\\brace") 

SDG8_5_x5 = c("\\bwage|\\bpay|\\bsalary|\\bsalaries|\\bearning|\\bstipend", 
              "\\bgap\\b|difference|inequalit|disparit",
              reduce_ls)

SDG8_5_x6 = c("unemployment",
              reduce_ls)


SDG8_6 = c(child_ls, 
           "educat|employ|\\btrain")

SDG8_7_x = c(paste("forced", child_ls, sep = "|"),
             "\\blabour|\\blabor|\\bwork|soldier|\\bslave*|traffick")
SDG8_7_y = "modern slave|human traffick"


SDG8_8_x1 = "\\blabour right|\\blabor right" 
SDG8_8_x2 = c("human right",
              "employ")
SDG8_8_x2 <- func_AND_plus(SDG8_8_x2)
SDG8_8_x2 <- gsub("\\.\\+", "(?!.*voting right)(?!.*Rights issue)", SDG8_8_x2) ## to exclude

SDG8_8_x3 = c("\\bsafe|\\bsecure", 
              "\\bwork|\\bworker|\\bworking|\\bjob.?\\b",
              "environment\\b")
SDG8_8_x3 <- func_AND_plus(SDG8_8_x3)

SDG8_8_x4 <- "occupational health|occupational safe|zero incident|safety management|safety and health management"

SDG8_8_x5 = c(paste(death_ls, "injur|\\bharm|danger|\\bincident", sep = "|"), 
              "\\bjob.?\\b|\\bwork|\\bemploy|occupation",
              reduce_ls)
SDG8_8_x5 <- func_AND_plus(SDG8_8_x5)
SDG8_8_x5 <- gsub("\\.\\+", "(?!.*alcohol)(?!.*Drinking)(?!.*No\\.)", SDG8_8_x5) ## to exclude



SDG8_9_x = c("touris", 
             paste("sustainab|community.?based|\\bemployment", func_AND_plus(c('\\bcreat', '\\bjob')), sep = "|"))
SDG8_9_y = func_AND_plus(c("\\btour",  "\\bpolicy|\\bpolicies|\\bGDP\\b|Gross Domestic Product"))
SDG8_9_z = 'ecotourism'  

SDG8_10 = c("access to|inclusi", 
            "financial service|financial institution|\\bbanks|\\banking|bank branch|insurance|automated teller machine|\\bATMs\\b")
SDG8_10 <- func_AND_plus(SDG8_10)
SDG8_10 <- gsub("\\.\\+", "(?!.*financial statement)(?!.*Access Bank)", SDG8_10) ## to exclude 'financial statement'


SDG8_a = c(support_ls, 
           '\\btrade\\b')
temp <- SDG8_a
ex_words <- c('receivable')
### Option 1: use `func_AND_plus` function
# SDG8_a <- func_AND_plus(SDG8_a)
# SDG8_a <- gsub("\\.\\+", "(?!.*receivable)", SDG8_a) ## to exclude 

### Option 2: to use look around function ----
SDG8_a <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, exclude = ex_words)





SDG8_b = c(child_ls, 
           'employ', 
           policy_ls)




SDG8_1 <- func_AND_plus(SDG8_1)
SDG8_2 <- (SDG8_2)

SDG8_3_x  <- func_AND_plus(SDG8_3_x)
SDG8_3_z1 <- func_AND_plus(SDG8_3_z1)
SDG8_3_z3 <- func_AND_plus(SDG8_3_z3)
SDG8_3_z4 <- func_AND_plus(SDG8_3_z4)

SDG8_4_x1 <- func_AND_plus(SDG8_4_x1)
SDG8_4_y  <- func_AND_plus(SDG8_4_y)
SDG8_4_z  <- func_AND_plus(SDG8_4_z)


SDG8_5_x2 <- func_AND_plus(SDG8_5_x2)
SDG8_5_x3 <- func_AND_plus(SDG8_5_x3)
SDG8_5_x4 <- func_AND_plus(SDG8_5_x4)
SDG8_5_x5 <- func_AND_plus(SDG8_5_x5)
SDG8_5_x6 <- func_AND_plus(SDG8_5_x6)

SDG8_6   <- func_AND_plus(SDG8_6)

SDG8_7_x <- func_AND_plus(SDG8_7_x)

SDG8_9_x <- func_AND_plus(SDG8_9_x)

SDG8_10  <- SDG8_10
SDG8_b   <- func_AND_plus(SDG8_b)




# SDG8_3   <- func_OR_vector(c(SDG8_3_x,  SDG8_3_y, SDG8_3_z1, SDG8_3_z2, SDG8_3_z3))
# SDG8_4   <- func_OR_vector(c(SDG8_4_x,  SDG8_4_y, SDG8_4_z))
# SDG8_5   <- func_OR_vector(c(SDG8_5_x1, SDG8_5_x2, SDG8_5_y1, SDG8_5_y2, SDG8_5_z))
# SDG8_7   <- func_OR_vector(c(SDG8_7_x,  SDG8_7_y))
# SDG8_8   <- func_OR_vector(c(SDG8_8_x1, SDG8_8_x2, SDG8_8_x3, SDG8_8_x4, SDG8_8_x5))
# SDG8_9   <- func_OR_vector(c(SDG8_9_x,  SDG8_9_y, SDG8_9_z))







## 9. Industry, Innovation, and Infrastructure ------------------------------------------

infrastructure_ls = paste(
  "infrastructure|\\bhouse\\b|\\bhouses\\b|\\bhousing\\b|architecture|construction|freight|transport|\\broad|\\brail|\\bport.?\\b|power plant|\\bdams",
  "bridge|airport|aviation|sewer|broadband|cellular infrastructure|internet|telecommunication|electricity|power grid|electrical grid|\\bpark.?\\b",
  "tunnel|water supply|Canal|Hospital|Irrigation scheme|Levee|Lighthouse",
  "Pipeline|transit|Public space|Sewage treatment|Sewerage|Sluice|Solid waste|Utilities|\\bWeir|waterway|harbor|\\bdock|\\bdike", sep = "|")

###

SDG9_1   = c("quality|reliab|sustainab|resilien|afford|equitab|inclus",
             infrastructure_ls)
SDG9_1   <- func_AND_plus(SDG9_1)




SDG9_2_x = c("inclusiv|sustainab|cradle to cradle|circular",
             "\\bindustr")
temp <- SDG9_2_x
### Option 1: use `func_AND_plus` function
# SDG9_2_x <- func_AND_plus(SDG9_2_x)

### Option 2: to use look around function ----
SDG9_2_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)



SDG9_2_y = c('manufactur', 
             '\\bvalue', 
             'per capita|\\bGDP\\b|gross domestic product')
SDG9_2_z = c('manufactur', 
             'employ')

SDG9_2_y <- func_AND_plus(SDG9_2_y)
SDG9_2_z <- func_AND_plus(SDG9_2_z)


SDG9_3_x = c("\\bindustr|enterprise|entrepreneur",
             "access|availab|\\breach",
             "\\bfinanc|fiscal|\\bcredit|\\bloan|value chain|\\bfund.?\\b|\\bfunding\\b|micro.?credit|micro.?financ")
SDG9_3_y = c("small.?scale industr|micro.?enterprise", 
             "proportion|\\bpercent")
SDG9_3_z = c("\\bsmall|starting|startup|kickstart", 
             "enterprise|entrepren|\\bcompany\\b|\\bcompanies|\\bfirm.?\\b|\\bcorporate\\b|\\bcorporation|business",
             "proportion|\\bpercent")

SDG9_3_x <- func_AND_plus(SDG9_3_x)
SDG9_3_x <- gsub("\\.\\+", "(?!.*Access Bank)", SDG9_3_x) ## to exclude
SDG9_3_y <- func_AND_plus(SDG9_3_y)
SDG9_3_z <- func_AND_plus(SDG9_3_z)


SDG9_4_x = c("upgrad|retrofit",
             paste(infrastructure_ls, "\\bindustr", sep = '|'))
SDG9_4_y = c("resource", 
             "\\buse\\b|\\buses\\b",
             "\\befficien")
SDG9_4_z = c("\\bindustr",  
             emission_ls,
             paste(reduce_ls, "manage|\\btreat", sep = '|'))
SDG9_4_w = c("\\bclean|environmentally sound|green", 
             "technolog|\\bindustr")

SDG9_4_x <- func_AND_plus(SDG9_4_x)
SDG9_4_y <- func_AND_plus(SDG9_4_y)
SDG9_4_z <- func_AND_plus(SDG9_4_z)
SDG9_4_w <- func_AND_plus(SDG9_4_w)


SDG9_5_x = c("\\btech|innov|research", 
             paste("\\bindustr|manufactur|\\bspending|\\bGDP\\b|gross domestic product|\\bproduct|\\bservice.?\\b", finance_ls, sep = "|"),
             paste(increase_ls, "upgrad|\\bencourag|prioritiz|\\bfurther\\b|\\bfortify|\\bfortifie|\\bcontinue|\\binculcat", sep = "|"))
SDG9_5_y = c("\\bresearch\\b|Researcher|\\bR&D\\b|\\bR & D\\b|\\bR-D\\b|\\bRTD\\b|\\bR\\+D\\b", 
             increase_ls)

SDG9_5_x <- func_AND_plus(SDG9_5_x)
SDG9_5_x <- gsub("\\.\\+", "(?!.*incremental cost)", SDG9_5_x) ## to exclude 
SDG9_5_y <- func_AND_plus(SDG9_5_y)


SDG9_a <- c("resilient|sustainable", 
            infrastructure_ls, 
            support_ls,
            developing_country_ls)

SDG9_a <- func_AND_plus(SDG9_a)


SDG9_b = c("\\btechnolog|innovation|research|high.?tech|\\bR&D\\b|\\bR & D\\b|\\bR-D\\b|industrial diversification",
           paste(increase_ls, support_ls, sep = "|"),
           paste("domestic", developing_country_ls, sep = "|"))
SDG9_b <- func_AND_plus(SDG9_b)
SDG9_b <- gsub("\\.\\+", "(?!.*Award)(?!.*Financial Report)(?!.*Annual Report)", SDG9_b) ## to exclude 





SDG9_c_x = c("access|availab", 
             "internet|mobile network|wireless|Phone service|broadband|cellular network|\\bICT\\b|\\b5G\\b|\\b4G\\b")
SDG9_c_y = c("access|availab", 
             'information|communication', 
             '\\btech')
SDG9_c_x <- func_AND_plus(SDG9_c_x)
SDG9_c_y <- func_AND_plus(SDG9_c_y)
SDG9_c_x <- gsub("\\.\\+", "(?!.*Access Bank)", SDG9_c_x) ## to exclude
SDG9_c_y <- gsub("\\.\\+", "(?!.*Access Bank)", SDG9_c_y) ## to exclude




# SDG9_2   <- func_OR_vector(c(SDG9_2_x, SDG9_2_y, SDG9_2_z))
# SDG9_3   <- func_OR_vector(c(SDG9_3_x, SDG9_3_y, SDG9_3_z))
# SDG9_4   <- func_OR_vector(c(SDG9_4_x, SDG9_4_y, SDG9_4_z, SDG9_4_w))
# SDG9_5   <- func_OR_vector(c(SDG9_5_x, SDG9_5_y))
# SDG9_c   <- func_OR_vector(c(SDG9_c_x, SDG9_c_y))






## 10. Reduce inequality within and among countries ------------------------------------------------

SDG10_1 = c("income|livelihood|expenditure|earning|\\bpay\\b|\\bwage",
            "household|per capita|population",
            paste("empower", increase_ls, sep = "|"))

SDG10_2 = c("inclusi",
            paste("empower|\\breform", increase_ls, sep = "|"),
            "social|economic|socioeconomic|political|\\bage\\b|\\bsex|disab|\\brace|racial|ethnic|origin|religi|low.?income|demographic")

SDG10_3_x = c("inequalit|discrimin|harass|homophobia|racism|sexism|prejudice",
              reduce_ls)
SDG10_3_y = c("equal opportunity|fairness|justice|inclusi",
              paste("\\bensure|promot|empower|support|\\bstimulat|strengthen", increase_ls, sep = "|"))
## -> excluded "equity" as it can also refer to "shares in a company which do not pay a fixed amount of interest"
## -> removed  "human right" as this target did not mention this aspect. 

SDG10_4_x = c("equal|inequalit",
              "\\bpolicy|\\bpolicies|fiscal|economic|financ|\\bwage|income|social|socio.?economic|\\blabor|\\blabour")
SDG10_4_y = "\\bGINI\\b|social protection"
SDG10_4_x <- func_AND_plus(SDG10_4_x)
# SDG10_4_x <- gsub("\\.\\+", "(?!.*\\bequal to)(?!.*equalization)", SDG10_4_x)  ## to exclude 'financial statement'    # NOT WORK
# SDG10_4_x <- gsub("\\.\\+", "(?!.*equal to)(?!.*equalization)", SDG10_4_x)     ## to exclude 'financial statement'    # WORK
SDG10_4_x <- gsub("\\.\\+", "(?!.*\\\\bequal to)(?!.*equalization)", SDG10_4_x)  ## to exclude 'financial statement'    # WORK (use this, note that must add more \\ to the replacement)

# SDG10_4_x <- gsub("\\^", "^(?!.*\\bequal to)(?!.*equalization)", SDG10_4_x) ## to exclude 'financial statement'   # NOT WORK
# SDG10_4_x <- func_AND_vector(SDG10_4_x)
# SDG10_4_x <- paste0("^(?!.*\\bequal to)(?!.*equalization)", SDG10_4_x) ## to exclude 'financial statement'        # WORK (this also work, but not the same as others)



SDG10_5 = c("regulat|monitor|administer|govern.?\\b|governed|governing|sound",
            "financial market|financial institution")
SDG10_5 <- func_AND_plus(SDG10_5)
SDG10_5 <- gsub("\\.\\+", "(?!.*regulatory)(?!.*regulator)(?!.*General government)(?!.*Corporate Governance)(?!.*Financial Institutions Act)(?!.*Pension Fund)", SDG10_5) ## to exclude



SDG10_6 = c("representation|\\bvoic|\\bright|\\bvot|effective|credib|accountable|legitimate|justif|lawful",
            "econom|financ|\\bfund.?\\b|\\bfunding\\b",
            "institution|organization|organisation|association|\\bunion|\\bsystem",
            developing_country_ls)
SDG10_6 <- func_AND_plus(SDG10_6)
SDG10_6 <- gsub("\\.\\+", "(?!.*financial statement)", SDG10_6) ## to exclude 'financial statement'


SDG10_7_x = c("migrat|mobili",
              "orderly|safe|regular|responsible|\\bpolicy|\\bpolicies")
SDG10_7_y = "refugee"


SDG10_a = c(paste(reduce_ls, "\\bspecial|differential|\\bfree\\b", sep = '|'),
            'tariff|duty|\\btax\\b|\\bWTO\\b',
            developing_country_ls)


SDG10_b_x = c("financ|\\bfunding|\\bassistance|budget|\\baids|invest|donor|donat|official flow",
              "develop|resource|capabilit|national plan|program",
              developing_country_ls)
SDG10_b_y = c("north-south divide|financial flow|resource flow|foreign direct investment|\\bFDI\\b|\\bODA\\b",
              developing_country_ls)
SDG10_b_x <- func_AND_plus(SDG10_b_x)
SDG10_b_x <- gsub("\\.\\+", "(?!.*financial statement)(?!.*financial group)", SDG10_b_x) ## to exclude



SDG10_c_x = c("remittance", 
              "\\bcost\\b|\\bcosts\\b",
              reduce_ls)
SDG10_c_y = c("remittance", 
              "migrant|emigrant|immigrant|corridor")

SDG10_general = c("\\bequal|inequalit|unequal",
                  "\\bSocial|societal|societ|\\bSocio|environm|health")


####
SDG10_1 <- func_AND_plus(SDG10_1)
SDG10_2 <- func_AND_plus(SDG10_2)

SDG10_3_x <- func_AND_plus(SDG10_3_x)
SDG10_3_y <- func_AND_plus(SDG10_3_y)


SDG10_5 <- (SDG10_5)

SDG10_6 <- SDG10_6

SDG10_7_x <- func_AND_plus(SDG10_7_x)

SDG10_a   <- func_AND_plus(SDG10_a)

SDG10_b_y <- func_AND_plus(SDG10_b_y)

SDG10_c_x <- func_AND_plus(SDG10_c_x)
SDG10_c_y <- func_AND_plus(SDG10_c_y)


SDG10_general <- func_AND_plus(SDG10_general)




# SDG10_3   <- func_OR_vector(c(SDG10_3_x, SDG10_3_y))
# SDG10_4   <- func_OR_vector(c(SDG10_4_y, SDG10_4_x))
# SDG10_7   <- func_OR_vector(c(SDG10_7_x, SDG10_7_y))
# SDG10_b   <- func_OR_vector(c(SDG10_b_x, SDG10_b_y))
# SDG10_c   <- func_OR_vector(c(SDG10_c_x, SDG10_c_y))







## -11. Sustainable Cities and Communities --------------------------------------------------------

waste_ls = "waste|rubbish|garbage|junk|debris|trash|litter|sewage|sludge|scrap metal|scrap lumber|solvent|product residue|kiln dust|slag|fly ash"


SDG11_1_x = c("access|availab|affordable|adequate|formal|safe|insufficient|deficient|\\black|scarce|scant|unsatisfactory",
              paste("\\bhouse\\b|\\bhouses\\b|\\bhousing\\b|\\bhome\\b|apartment|condo|dwelling|residence|accommodation", 
                    "basic service|\\bwater\\b|sanitation|hygiene|electricity|slum|Shanty|\\bliving", sep = "|"))
SDG11_1_y = c(urban_ls,
              "slum")
SDG11_1_z = "formal settlement|upgrade slum"


SDG11_1_x <- func_AND_plus(SDG11_1_x)
SDG11_1_x <- gsub("\\.\\+", "(?!.*Access Bank)", SDG11_1_x) ## to exclude 'financial statement'


SDG11_2 = c("access|availab|afford|safe|improv",
            "transport|\\broad|congestion|shipment|transit|\\bbus\\b|\\bbuses\\b|\\bbusses\\b|shuttle")
SDG11_2 <- func_AND_plus(SDG11_2)
SDG11_2 <- gsub("\\.\\+", "(?!.*suppl)(?!.*equipment)(?!.*Access Bank)", SDG11_2) ## to exclude 


SDG11_3_x = c(urban_ls,
              "planning|management",
              "participat|democrat|integrated")
SDG11_3_y = c(paste("buildings", urban_ls, sep = "|"),
              "inclusi|sustainab|resilien|zero.?carbon|zero.?energy|low.?carbon|low.?energy")
temp <- SDG11_3_y
### Option 1: use `func_AND_plus` function
# SDG11_3_y <- func_AND_plus(SDG11_3_y)

### Option 2: to use look around function ----
SDG11_3_y <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6)


SDG11_4 = c(paste("protect|safeguard|preserv|conserv|assure|retain", support_ls, sep = "|"),
            paste("heritage|archaeological site|geological site|monumental sculpture|monumental painting",
                  "monuments|Architectural|cultural landscape|Rare breed|historic building|historic site",
                  "historic place|\\btradition.?\\b|\\bcustom.?\\b|aesthetic belief|spiritual belief", 
                  "artistic expression|\\blanguage|performing art|social practice|\\britual.?\\b|festive event",
                  "traditional craftsmanship", sep = "|"))


SDG11_5 = c(reduce_ls,
            paste(death_ls, "economic loss|\\bmissing|affected|damag|\\bdisrupt", sep = '|'),
            disaster_ls)

SDG11_6 = c(paste("contaminat|environment|\\bair.?\\b|\\bsmog|\\bhaze|pm2\\.5|pm10|pm 2\\.5|pm 10|particulate matter", 
                  waste_ls, 
                  "\\bwater\\b|spill|\\bsoil\\b|footprint|pollution|pollutant|recycl|circular|toxin|toxic", sep = "|"),
            urban_ls)

SDG11_7_x = c("safe|secure|inclusi|accessible|available|reachable|green|\\bpublic|\\bnatur",
              "\\bspace.?\\b|bikeway")
## -> “area” is removed to avoid noise
SDG11_7_y = c(urban_ls,
              'victim',
              "physical|sexual",
              "harass")

SDG11_a_x = c("econom|social|socio|environment|socio.?economic|ecolog",
              "\\blink|balanc|\\bbind|bridg|connect|network", 
              paste("rural", urban_ls, sep = "|"))
SDG11_a_y = c(urban_ls, 
              paste("development plan|fiscal", policy_ls, sep = '|'),
              "\\blocal|region|territorial")

SDG11_b = c(paste(urban_ls, "local government", sep = "|"),
            policy_ls,
            paste("inclusi|resilien|resource efficien|climate change|risk reduction", disaster_ls, sep = "|"))

SDG11_c = c(support_ls,
            "resilient building|sustainable building",
            developing_country_ls)


####
SDG11_1_y <- func_AND_plus(SDG11_1_y)


SDG11_2   <- SDG11_2

SDG11_3_x <- func_AND_plus(SDG11_3_x)


SDG11_4 <- func_AND_plus(SDG11_4)
SDG11_5 <- func_AND_plus(SDG11_5)
SDG11_6 <- func_AND_plus(SDG11_6)


SDG11_7_x <- func_AND_plus(SDG11_7_x)
SDG11_7_y <- func_AND_plus(SDG11_7_y)


SDG11_a_x <- func_AND_plus(SDG11_a_x)
SDG11_a_y <- func_AND_plus(SDG11_a_y)


SDG11_b <- func_AND_plus(SDG11_b)
SDG11_c <- func_AND_plus(SDG11_c)



# SDG11_1   <- func_OR_vector(c(SDG11_1_x, SDG11_1_y, SDG11_1_z))
# SDG11_3   <- func_OR_vector(c(SDG11_3_x, SDG11_3_y))
# SDG11_7   <- func_OR_vector(c(SDG11_7_x, SDG11_7_y))
# SDG11_a   <- func_OR_vector(c(SDG11_a_x, SDG11_a_y))









## 12. Responsible Consumption and Production ---------------------------------------------------

company_ls <- "\\bcompany\\b|\\bcompanies|\\bfirm.?\\b|\\bcorporate\\b|\\bcorporation|business|enterprise"


SDG12_1_x1 = c("sustainab|green|\\bclean|Responsible|Eco.?Friendly/Environmentally.?Friendly|Recyclable",
               "consum|\\bproduc|manufactur")
temp <- SDG12_1_x1

### Option 1: use `func_AND_plus` function
# SDG12_1_x1   <- func_AND_plus(SDG12_1_x1)

### Option 2: to use look around function ----
SDG12_1_x1 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 10)


SDG12_1_x2 = "\\bSCP\\b|\\b10YFP\\b"

SDG12_2_x = c("sustainab|\\befficien", 
              "resource|material.?\\b", 
              "manag|\\busing\\b|\\buse\\b|\\buses\\b")
SDG12_2_y = c("material.?\\b|carbon|\\bwater\\b", 
              "footprint|\\bflow\\b|\\bflows\\b|consum",
              reduce_ls)
SDG12_2_z = c("manag|conserv|protocol",
              "natural resource|natural capital")

SDG12_3 = c(paste(reduce_ls, "halve|\\bhalf", sep = "|"), 
            "food|harvest|supply chain",
            "wast|\\bloss|spoil")

SDG12_4_x = c(paste("manag|\\btreat|agreement|responsi|recycl|\\bre.?us|life cycle|safe handl|per capita", 
                    "environmentally sound|Responsible|Eco.?Friendly/Environmentally.?Friendly",
                    reduce_ls, sep = '|'),
              paste(waste_ls, 
                    "chemical|contaminat|pesticide|pollut|\\bpoison|hazard|toxin|toxic", 
                    "virulent|radioactive|Chlorinated aliphatic hydrocarbons|\\bWood preserv|Multisource leachate|Petroleum refin|Explosives manufactur",
                    ls_hazardous_waste_chemicals,
                    sep = "|"))
SDG12_4_y = "environmental permit"


SDG12_5_x = c(paste("recycl|\\bre.?us|circular", reduce_ls, sep = "|"), 
              "waste")
SDG12_5_y = c("recycl|\\bre.?us|circular", 
              "material.?\\b|resource")
SDG12_5_z = "cradle to cradle|Life cycle assessment|Circular economy|refurbishment"



## ------------------------------------------------------------------------------------ ##
SDG12_6_x = c("sustainab",
              "practic|action|reporting|\\bpublish|information",
              company_ls)
temp <- SDG12_6_x

w_ex <- c('Annual Report', 'Corporate Information', 'Initiatives for Sustainable Growth',
          'Sustainability Meeting', 'Corporate Governance Sustainability', 'Company Overview')

### Option 1
# SDG12_6_x <- func_AND_plus(SDG12_6_x)
# SDG12_6_x <- gsub("\\.\\+", 
#                   "(?!.*Annual Report)(?!.*Corporate Information)(?!.*Initiatives for Sustainable Growth)(?!.*Sustainability Meeting)(?!.*Corporate Governance Sustainability)(?!.*Company Overview)", 
#                   SDG12_6_x) ## to exclude 
### Option 2
# t2 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, exclude = w_ex)

### Option 4: to use look around + AND function + exclude ----
SDG12_6_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, third_AND_string = temp[3], exclude = w_ex)
## ------------------------------------------------------------------------------------ ##


SDG12_6_y = c("\\bsoci",
              "responsibilit",
              company_ls, 
              increase_ls)
SDG12_6_z <- c("commitment", 
               "social responsibilit|environmental stewardship")
SDG12_6_y <- func_AND_plus(SDG12_6_y)
SDG12_6_z <- func_AND_plus(SDG12_6_z)

SDG12_6_y <- gsub("\\.\\+", 
                  "(?!.*Social Responsibility Committee)", 
                  SDG12_6_y) ## to exclude 





SDG12_7 = c("sustainab",
            "buying|procure|obtain|harvest|collect|gather|consum|label",
            paste(policy_ls, "implemen", sep = '|'))


SDG12_8 = c("sustainable development|harmony with nature",
            "information|awareness|educat|curricul")

SDG12_a = c("sustainab|green|\\bclean",
            "consum|\\bproduc|manufactur|\\buse|\\busing\\b",
            "scientific|\\bscience|tech|\\brenewable",
            developing_country_ls)

SDG12_b = c("sustainab|green",
            "touris",
            "monitor|manag|accounting|\\btrack|regulat|administer|govern")

SDG12_c = c("fossil|\\bcoal\\b|\\bcoals\\b|petrol|natural gas|crude oil|gasoline|kerosene|non.?renewable",
            "\\bsubsidy|\\bsubsidies|\\bsubsidiz|\\bsubsidis|expenditure|expens")



####

SDG12_2_x <- func_AND_plus(SDG12_2_x)
SDG12_2_y <- func_AND_plus(SDG12_2_y)
SDG12_2_z <- func_AND_plus(SDG12_2_z)


SDG12_3   <- func_AND_plus(SDG12_3)

SDG12_4_x <- func_AND_plus(SDG12_4_x)


SDG12_5_x <- func_AND_plus(SDG12_5_x)
SDG12_5_y <- func_AND_plus(SDG12_5_y)


SDG12_7 <- func_AND_plus(SDG12_7)
SDG12_8 <- func_AND_plus(SDG12_8)
SDG12_a <- func_AND_plus(SDG12_a)
SDG12_b <- func_AND_plus(SDG12_b)
SDG12_c <- func_AND_plus(SDG12_c)


# SDG12_1   <- func_OR_vector(c(SDG12_1_x1, SDG12_1_x2))
# SDG12_2   <- func_OR_vector(c(SDG12_2_x, SDG12_2_y, SDG12_2_z))
# SDG12_4   <- func_OR_vector(c(SDG12_4_x, SDG12_4_y))
# SDG12_5   <- func_OR_vector(c(SDG12_5_x, SDG12_5_y, SDG12_5_z))
# SDG12_6   <- func_OR_vector(c(SDG12_6_x, SDG12_6_y, SDG12_6_z))











## 13. Climate Action -----------------------------------------------------------------------------

climate_ls = paste("\\bclimat|global change|global environment change|\\bENSO\\b|\\bEl Ni|Southern Oscillation",
                   "extreme weather|warming|\\bwarmer|warmest", 
                   "temperature|hotter|hottest|heat.?up|heat.?wave",
                   "heavy rain|cloudburst|aridity|drought|flood|\\bstorm|extreme temperature|extreme heat", 
                   "cold wave|extreme precipitation|heavy downpour|thunderstorm|ice storm|blizzard|hailstorm|tropical storm",
                   "Human activit|anthropogenic",
                   "\\bsea.?level|\\bsea.?ice|\\bmelt", 
                   sep = "|")

climate_good_ls = paste(
  "\\bCOP\\b|\\bCOP.?\\d+|\\bIPCC\\b|\\bUNFCC\\b|Paris Agreement|Paris Climate Agreement|Montreal Protocol",
  "1\\.5.?°C|2.?°C|1\\.5 degrees Celsius|2 degrees Fahrenheit|net.?zero|SDG 13|goal 13|target 13|indicator 13", 
  sep = "|")

####
SDG13_1 = c(paste(climate_ls, 'kyoto protocol', sep = "|"), 
            paste(disaster_ls, death_ls, "\\bmissing person|human loss", sep = "|"),
            paste("resilien|adapt|adjust|\\bcombat", reduce_ls, sep = '|'))


SDG13_2_x = paste(climate_good_ls, 'kyoto protocol|\\bunfccc|tipping point', sep = '|')


SDG13_2_y = c(
  paste("ocean acidification|deforestation|desertificati|animal farming|farming livestock|fertilizer",
        "air pollut|fossil.?fuel|fossil gas|\\bcoal\\b|\\bcoals\\b|petrol|natural gas|typhoons|hurricane", sep = "|"), 
  reduce_ls)
temp <- SDG13_2_y

### Option 1: use `func_AND_plus` function
# SDG13_2_y  <- func_AND_plus(SDG13_2_y)

### Option 2: to use look around function ----
SDG13_2_y <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)




SDG13_2_z = c(paste(climate_ls, climate_good_ls, sep = "|"), 
              paste("adapt|action|\\bact\\b|legislation|contribut|convention|prepar|fight|\\bcombat|tackle", 
                    "response to|cope.?with|coping with|deal.?with|dealing with|handl|measure",
                    policy_ls, sep = '|')) 
SDG13_2_w1 <- c(emission_ls, 
                paste("captur|sequest|\\blimit|remov|absorb", reduce_ls, sep = "|"))
SDG13_2_z  <- func_AND_plus(SDG13_2_z)
SDG13_2_w1 <- func_AND_plus(SDG13_2_w1)


SDG13_2_w2 <- paste("energy transition|decarboni", renewable_ls, sep = "|")
SDG13_2_w2 <- paste0(SDG13_2_w2, "(?!.*Power segment)(?!.*Renewables & Power)(?!.*wind up)(?!.*Clean Energy Fuels Corp)(?!.*\\\\bltd\\b)(?!.*Total Solar)") ## to exclude
###



SDG13_3 = c(paste(climate_ls, 'kyoto protocol', emission_ls, sep = "|"),
            paste("adapt|captur|contribut|convention", reduce_ls, policy_ls, sep = '|'),
            "educat|aware|engag|outreach|communicat|cultivat|information|informing|\\binform\\b|\\binforms\\b|\\binformed\\b|\\btrain|tutor|instruct|teach|learn")


SDG13_a_x = c(paste(climate_ls, climate_good_ls, 'kyoto protocol', sep = '|'),
              paste(support_ls, "dollar|capital|mobili|implement|operat|convention|mitigat|foster", sep = '|'))
SDG13_a_y = "United Nations Framework Convention on Climate Change|\\bunfccc\\b|U\\.N\\.F\\.C\\.C\\.C\\.|Green Climate Fund|\\bGCF\\b|G\\.C\\.F\\."


SDG13_b  = c(paste(climate_ls, climate_good_ls, 'kyoto protocol', sep = '|'),
             paste(policy_ls, "capacity|\\bscope", support_ls, sep = '|'),
             developing_country_ls)


SDG13_general = paste(climate_ls, climate_good_ls, 'kyoto protocol', emission_ls, sep = '|')


####
SDG13_1   <- func_AND_plus(SDG13_1)
SDG13_3   <- func_AND_plus(SDG13_3)
SDG13_a_x <- func_AND_plus(SDG13_a_x)
SDG13_b   <- func_AND_plus(SDG13_b)

SDG13_general <- SDG13_general


# SDG13_2   <- func_OR_vector(c(SDG13_2_x, SDG13_2_y, SDG13_2_z, SDG13_2_w1, SDG13_2_w2))
# SDG13_a   <- func_OR_vector(c(SDG13_a_x, SDG13_a_y))









## 14. Life Below Water -------------------------------------------------------------------------

ocean_ls = "marine|maritime|ocean|oceanography|\\bsea\\b|seawater|sublittoral|littoral|coast|tidal|aquatic|coral"


SDG14_1 = c(ocean_ls,
            "pollut|nutrient|eutroph|Kelp|\\balga|plastic|micro.?plastic|debris|run.?off|chemical|arsenic|contaminat|fertiliz|waste")

SDG14_2 = c(paste(ocean_ls, 'coral', sep = "|"),
            "sustainab|resilien|restor|manag|mitigat|health|productiv|habitat|bleach") 

SDG14_3 = c(ocean_ls,
            "acidi|\\bpH\\b")

SDG14_4_x = c("\\bfish|seafood|blue food|\\bcatch\\b|\\bcatches\\b|bycatch|harvest|overfish",
              paste(illegal_ls, 
                    "unreported|underreport|unregulated|destruct|destroy|diminish|exploit", 
                    "sustainab|restor|conserv|manag|mitig|regulat|monitor|restrict|enforc|prohibit|\\bquota\\b|monitor", 
                    sep = "|"))
SDG14_4_y = "overfish|maximum sustainable yield|\\bMSY\\b"

SDG14_5 = c(ocean_ls,
            "restor|conserv|protect|preserv|\\breduc|\\bplan\\b|\\bplans\\b|planning|mitig|restrict|enforc|monitor|prohibit|report")

SDG14_6_x = c("\\bfish|seafood|\\bcatch\\b|\\bcatches\\b|bycatch|harvest|aquaculture|blue food|mariculture",
              "\\bsubsidy|\\bsubsidies|\\bsubsidiz|\\bsubsidis")
SDG14_6_y = c("\\bfish|seafood|\\bcatch\\b|\\bcatches\\b|bycatch|harvest|aquaculture|blue food|mariculture",
              paste(illegal_ls, "unreported|unregulated|destructive|destroy|diminish|exploit|poach|traffick", sep = "|"),
              paste("instrument|\\blaw|enforc|restrict|prohibit|\\bquota\\b|regulat|monitor|prohibit", policy_ls, sep = '|'))

SDG14_7_x = c("econom|benefi|sustain",
              "\\bfish|aquacultur|seafood|\\bcatch\\b|\\bcatches\\b|bycatch|blue food|mariculture",
              developing_country_ls)
SDG14_7_y = c("econom|benefi|sustain",
              ocean_ls, "resource|touris",
              developing_country_ls)


temp <- "scient|\\bknowledge|research|technolog|budget|\\bspending|health"
SDG14_a_x1 = c(ocean_ls,
               temp,
               increase_ls)

SDG14_a_x2 = c(ocean_ls,
               temp,
               "mammal|species|\\bplant|animal|bird|biodivers")
SDG14_a_x3 = c(temp,
               "\\bfish|cetacean|\\bwhale.?\\b")
SDG14_a_x4 = "Intergovernmental Oceanographic Commission|Guidelines on the Transfer of Marine Technology"


SDG14_b = c(paste("artisan|tradition", "small.?scale", sep = '|'),
            "\\bfish|seafood|\\bcatch\\b|\\bcatches\\b|bycatch|harvest",
            "resource|market|\\blaw|legal|\\bpolicy|\\bpolicies|institutional|regulat|jurisdiction")

SDG14_c = c(paste(ocean_ls, "resource", sep = '|'),
            "restor|conserv|protect|sustain|manag|mitig|monitor",
            "\\blaw|legal|\\bpolicy|\\bpolicies|institutional|regulat|jurisdiction|instrument")

####
SDG14_1 <- func_AND_plus(SDG14_1)
SDG14_2 <- func_AND_plus(SDG14_2)
SDG14_3 <- func_AND_plus(SDG14_3)

SDG14_4_x <- func_AND_plus(SDG14_4_x)


SDG14_5 <- func_AND_plus(SDG14_5)

SDG14_6_x <- func_AND_plus(SDG14_6_x)
SDG14_6_y <- func_AND_plus(SDG14_6_y)


SDG14_7_x <- func_AND_plus(SDG14_7_x)
SDG14_7_y <- func_AND_plus(SDG14_7_y)


SDG14_a_x1 <- func_AND_plus(SDG14_a_x1)
SDG14_a_x2 <- func_AND_plus(SDG14_a_x2)
SDG14_a_x3 <- func_AND_plus(SDG14_a_x3)

SDG14_b <- func_AND_plus(SDG14_b)
SDG14_c <- func_AND_plus(SDG14_c)



# SDG14_4   <- func_OR_vector(c(SDG14_4_x, SDG14_4_y))
# SDG14_6   <- func_OR_vector(c(SDG14_6_x, SDG14_6_y))
# SDG14_7   <- func_OR_vector(c(SDG14_7_x, SDG14_7_y))
# SDG14_a   <- func_OR_vector(c(SDG14_a_x1, SDG14_a_x2, SDG14_a_x3, SDG14_a_x4))








## 15. Life On Land --------------------------------------------------------------------------------

ecosystem_ls = "ecosystem|eco-system|\\becolog|environment|\\bnatur|environs"


SDG15_1_x = c("terrestrial|inland|fresh.?water|\\bforest|woodland|wetland|marsh|mountain|dryland|rainforest|agroforest|tundra|biodivers|wildlife|wild animal|wild species",
              "conserv|restor|reserv|mainten|protect|preserv|safeguard|secur|sustain")
SDG15_1_y = c(ecosystem_ls,
              "service|\\bgood.?\\b|\\bproduct.?\\b|\\bsupply|\\bsupplies|contribution",
              "conserv|restor|reserv|maintain|protect|preserv|safeguard|secur|sustain")
SDG15_1_y <- func_AND_plus(SDG15_1_y)
SDG15_1_y <- gsub("\\.\\+", "(?!.*financial service)(?!.*nature reserve)", SDG15_1_y) ## to exclude


SDG15_2 = c("forest|rainforest|woodland|\\btree.?plant|plant.? tree|planting tree|deforest|afforest|reforest|silvicultur|mangrove|\\bREDD\\b|R\\.E\\.D\\.D\\.|Reducing Emissions from Deforestation and forest Degradation|silviculture|timber|shrub",
            paste("conserv|restor|sustain|manag|mitig|health|resilien|stewardship|certifi|audit|account", increase_ls, sep = "|"))
temp <- SDG15_2

### Option 1: use `func_AND_plus` function
# SDG15_2 <- func_AND_plus(SDG15_2)

### Option 2: to use look around function ----
SDG15_2 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)



SDG15_3 = c("degrad|desertif|contamin|pollut|denitrificat|de-nitrificat|health|nitro|drought|flood", 
            "\\bland|\\bsoil\\b|sediment", 
            paste("\\bcombat|halt|conserv|restor|sustain|manag", reduce_ls, sep = "|"))

SDG15_4_x = c("mountain|alpine",
              paste(ecosystem_ls, "biodiver|vegetation|protected", sep = '|'))
SDG15_4_y = "Mountain Green Cover Index"

SDG15_5_x = c("nature|natural", 
              "habitat|\\breserve.?\\b|\\bpreserve|\\bpark.?\\b")
SDG15_5_y = "Protected Area|protection area|conservation area|conservation park|biodivers|Red List"
SDG15_5_z = c("habitat|\\breserve.?\\b|sanctuar|threaten|endanger|extinct|diversity|richness|vulnerab|extirpate|environment|forest|grassland|Wetland|Mountain",
              "species|Wildlife|animal|\\bplant.?\\b|Flora|Fauna")
SDG15_5_z <- func_AND_plus(SDG15_5_z)
SDG15_5_z <- gsub("\\.\\+", "(?!.*habitat for humanity)", SDG15_5_z) ## to exclude


SDG15_6 = c("genetic resource|genetic|\\bgene\\b",
            "\\bfair|equitab|access|availab|equal|\\bshare.?\\b|sharing")

SDG15_7 = c(paste("poach|traffic", illegal_ls, sep = "|"),
            "species|flora|fauna|wildlife|logg|animal|mammal|\\bhunt|cultivat|\\bcatch\\b|\\bcatches\\b")


SDG15_8 = "\\binvasive|\\binvasion|\\balien|\\binvade|nonindigenous|non.?indigenous|non.?native"



## ------------------------------------------------------------------------------------ ##
SDG15_9 = c(paste("ecosystem", "biodiver", sep = '|'),
            "\\bnation|countr|region|local",
            paste("development|poverty reduction|accounting|reporting|Aichi|action plan|target|planning|strateg", policy_ls, sep = "|"))
temp <- SDG15_9
w_ex <- c('business environment', 'political environment', 'social environment')

### Option 1
# SDG15_9 <- func_AND_plus(SDG15_9)
# SDG15_9 <- gsub("\\.\\+", "(?!.*business environment)(?!.*political environment)(?!.*social environment)", SDG15_9) ## to exclude 'financial statement'

### Option 4: to use look around + AND function + exclude ----
SDG15_9 <- lookaround_nearby_n(word_ls1 = temp[2], word_ls2 = temp[3], n = 3, third_AND_string = temp[1], exclude = w_ex)
## ------------------------------------------------------------------------------------ ##



SDG15_a = c(paste(ecosystem_ls, "biodiver", sep = '|'),
            paste(finance_ls, "revenue|\\bvalue|incentiv|\\bspending", sep = '|'),
            "conserv|preserv|sustainable use|sustainably use")
SDG15_a <- func_AND_plus(SDG15_a)
SDG15_a <- gsub("\\.\\+", "(?!.*business environment)(?!.*political environment)(?!.*social environment)", SDG15_a) ## to exclude either of the three terms

SDG15_b = c("forest",
            paste(support_ls, "conserv|preserv|manag|mitig|sustain|revenue|\\bvalue|incentiv|\\bspending|expenditure", sep = '|'))

SDG15_c = c("poach|traffic|illicit|illegal",
            "species|flora|fauna|wildlife|logg|animal|mammal|\\bhunt|cultivat|\\bcatch\\b|\\bcatches\\b",
            "global support|\\blocal|communit")


SDG15_general_x = paste(func_AND_plus(c(ecosystem_ls, "resilien")),
                        func_AND_plus(c('human', 'wildlife', 'conflict')), 
                        func_AND_plus(c("community", "conserv")), 
                        "eco.?touris", 
                        sep = "|")
SDG15_general_y = c(ecosystem_ls,
                    "impact|conserv|restor|sustaina|manag|mitig|health|\\bwell\\b|resilien|stewardship|audit|account")
SDG15_general_y <- func_AND_plus(SDG15_general_y)

####

SDG15_1_x <- func_AND_plus(SDG15_1_x)
SDG15_3   <- func_AND_plus(SDG15_3)

SDG15_4_x <- func_AND_plus(SDG15_4_x)

SDG15_5_x <- func_AND_plus(SDG15_5_x)
SDG15_6 <- func_AND_plus(SDG15_6)
SDG15_7 <- func_AND_plus(SDG15_7)
SDG15_8 <- SDG15_8
SDG15_9 <- SDG15_9
SDG15_a <- SDG15_a

SDG15_b <- func_AND_plus(SDG15_b)
SDG15_c <- func_AND_plus(SDG15_c)






# SDG15_1   <- func_OR_vector(c(SDG15_1_x, SDG15_1_y))
# SDG15_4   <- func_OR_vector(c(SDG15_4_x, SDG15_4_y))
# SDG15_5   <- func_OR_vector(c(SDG15_5_x, SDG15_5_y, SDG15_5_z))
# SDG15_general   <- func_OR_vector(c(SDG15_general_x, SDG15_general_y))








## 16. Peace, Justice, and Strong Institutions ----------------------------------------------------

SDG16_1_x = c("violen|victim|homicid|murder|\\bkill|assault|assassination",
              reduce_ls)
SDG16_1_y = c("verbal|physical|domestic|psychological|child|\\bsex", 
              "\\babus|violen|assault|torture",
              reduce_ls) 
SDG16_1_z = "safe walking|peaceful societ"

SDG16_2 = c(child_ls,
            "\\babus|bully|kidnap|exploit|traffick|victim|violen|tortur|punish|aggress|\\brap|\\bporn",
            reduce_ls)


SDG16_3_x = c("access", 
              "\\bcode|\\bnorm|\\border|justice|authorit|\\brule.?\\b|\\blaw|\\blegal|legislation|litigation|conflict resolution|dispute resolution")
SDG16_3_y = c("report|regulat|resolution",
              "victim|violence|detain|prison|inmate|disput")
SDG16_3_z = "actual innocence|false confession|Unsentenced detain|Arbitrary detent|Enforced disappearance"

SDG16_3_x <- func_AND_plus(SDG16_3_x)
SDG16_3_y <- func_AND_plus(SDG16_3_y)
SDG16_3_x <- gsub("\\.\\+", "(?!.*Access Bank)", SDG16_3_x)   ## to exclude
SDG16_3_y <- gsub("\\.\\+", "(?!.*Annual Report)", SDG16_3_y) ## to exclude


SDG16_4_x <- c(paste(illegal_ls, "crime|cybercrime|irregular|prohib|smuggl|unwarranted|stolen|\\bsteal|theft|seized|traced|tracked|conflict|traffic|terrorist", sep = "|"),
               paste(finance_ls, "\\barm|\\bgun|weapon|ammunition|rifle|pistol|firearm|shotgun|\\basset", sep = '|'),
               paste("\\brecovery|\\breturn|\\bagainst\\b|\\bAnti.?\\b|\\bcombat", reduce_ls, sep = "|"))
SDG16_4_x <- func_AND_plus(SDG16_4_x)
SDG16_4_x <- gsub("\\.\\+", "(?!.*statement)(?!.*granted)(?!.*investigat)(?!.*financial assumption)(?!.*finance corporation)", SDG16_4_x) ## to exclude these keywords
SDG16_4_y <- "unauthorized acquisition|money laundering"


SDG16_5_x = c("corrupt|\\bbrib|Tax evasion|extortion", 
              paste("implement|identif|\\bpolicy|\\bpolicies|legislation|\\bagainst\\b|\\bAnti.?\\b|\\bcombat", reduce_ls, sep = "|"))
SDG16_5_y = c("contact|asked|\\bpay\\b|\\bpaid|extort", 
              "public official")
SDG16_5_z = c('miscarriage', 'justice')


SDG16_6_x = c(paste("institut|\\bcustom.?\\b|government|legislat|judicia|public service|organization.?\\b|association.?\\b",
                    "society|societies|peer group|social group|\\breligion|communit", 
                    sep = "|"),
              "effective|accountab|transparent|satisf")
SDG16_6_y = paste("government budget|government expenditure|judicial system|penal system|legal system|criminal tribunal",
                  func_AND_plus(c("peace|inclusi|\\bfair", "societ")), sep = '|')

temp      <-  "responsive|inclusive|participatory|representative|independen"
SDG16_7_x = c("legislature|institution|public service|judiciary",
              temp)
SDG16_7_y = c("decision.?mak", 
              temp)
SDG16_7_z = paste("democracy", 
                  func_AND_plus(c('separat', "\\bpower")), 
                  sep = "|")


SDG16_8 = c("institution|global govern|international organization", 
            "participat|\\bvote|\\bvoting|\\bvoice|suffrag|representation|election|electoral",
            developing_country_ls)


SDG16_9_x = func_AND_plus(c("legal", "identity"))
SDG16_9_y = func_AND_plus(c("birth", "regist|certifi"))


SDG16_10_x = c("public access|availab|attack|freedom|\\bkill|kidnap|murder|enforc|disappear|deten|detain|tortur",
               "journalis|media|\\bpress|unionis|human rights advocate")
SDG16_10_y = 'Aarhus Convention'


temp <- paste("\\bagainst\\b|\\bAnti.?\\b|\\bcombat", reduce_ls, sep = "|")
SDG16_a_x1 = c("violen|terroris|\\bcrime|criminal|genocid|murder|human traffick|refugee|extremis|insurgen|\\bwar|warfare",
               temp) 
SDG16_a_x2 = c("\\barm|\\bgun|weapon", 
               "conflict", 
               temp) 
SDG16_a_y1 = c("institution|cooperat|collaborat|joint effort|partnership|independen|human right|civil right|child right|democracy|treaty|\\bcivil",
               "\\bnation|countr|\\bstate.?\\b|domestic",
               increase_ls)

SDG16_a_y2 = c("\\bpeace", 
               "\\bkeep",
               increase_ls)
SDG16_a_z  = "building capacity|Paris Principle|Geneva Convention"


SDG16_b = c("\\blaw|legislat|\\bpolicy|\\bpolicies|\\bright|against|protect",
            "discrimin|harass")


SDG16_general = paste("ethnic conflict|exonerat|justice system",
                      func_AND_plus(c("environment", "\\blaw|govern|\\benact|legistlat")), sep = "|")

####
SDG16_1_x <- func_AND_plus(SDG16_1_x)
SDG16_1_y <- func_AND_plus(SDG16_1_y)


SDG16_2 <- func_AND_plus(SDG16_2)



SDG16_5_x <- func_AND_plus(SDG16_5_x)
SDG16_5_y <- func_AND_plus(SDG16_5_y)
SDG16_5_z <- func_AND_plus(SDG16_5_z)


SDG16_6_x <- func_AND_plus(SDG16_6_x)


SDG16_7_x <- func_AND_plus(SDG16_7_x)
SDG16_7_y <- func_AND_plus(SDG16_7_y)

SDG16_8   <- func_AND_plus(SDG16_8)


SDG16_10_x <- func_AND_plus(SDG16_10_x)

SDG16_a_x1 <- func_AND_plus(SDG16_a_x1)
SDG16_a_x2 <- func_AND_plus(SDG16_a_x2)
SDG16_a_y1 <- func_AND_plus(SDG16_a_y1)
SDG16_a_y2 <- func_AND_plus(SDG16_a_y2)

SDG16_b    <- func_AND_plus(SDG16_b)




# SDG16_1   <- func_OR_vector(c(SDG16_1_x, SDG16_1_y, SDG16_1_z))
# SDG16_3   <- func_OR_vector(c(SDG16_3_x, SDG16_3_y, SDG16_3_z))
# SDG16_4   <- func_OR_vector(c(SDG16_4_x, SDG16_4_y))
# SDG16_5   <- func_OR_vector(c(SDG16_5_x, SDG16_5_y, SDG16_5_z))
# SDG16_6   <- func_OR_vector(c(SDG16_6_x, SDG16_6_y))
# SDG16_7   <- func_OR_vector(c(SDG16_7_x, SDG16_7_y, SDG16_7_z))
# SDG16_9   <- func_OR_vector(c(SDG16_9_x, SDG16_9_y))
# SDG16_10  <- func_OR_vector(c(SDG16_10_x, SDG16_10_y))
# SDG16_a   <- func_OR_vector(c(SDG16_a_x1, SDG16_a_x2, SDG16_a_y1, SDG16_a_y2, SDG16_a_z))









## 17. Partnerships for the Goals -----------------------------------------------------------------

### Finance

SDG17_1 = c("domestic resource|domestic capacity|domestic budget",
            "revenue|budget|\\btax|expenditure")
SDG17_1 <- func_AND_plus(SDG17_1)
SDG17_1 <- gsub("\\.\\+", "(?!.*governmental charge)", SDG17_1) ## to exclude
SDG17_1_y <- c('government revenue', 
               '\\bGDP\\b')


SDG17_2 = paste("\\bODA\\b", 
                func_AND_plus(c("development assistance", developing_country_ls)), sep = "|")


SDG17_3_x = c("financial resource|\\bfund.?\\b|\\bfunding\\b|financial assist|\\baid|invest|\\bdonat|official flow", 
              "\\bto\b|\\bfor\\b",
              developing_country_ls)
SDG17_3_x <- func_AND_plus(SDG17_3_x)
SDG17_3_x <- gsub("\\.\\+", "(?!.*statement)(?!.*grant)(?!.*subsid)(?!.*budget)(?!.*investigat)(?!.*financial assumption)(?!.*finance corporation)", SDG17_3_x) ## to exclude these keywords

SDG17_3_y = c("foreign direct investment|\\bFDI\\b|official development assistance|South.?South cooperation|remittance",
              developing_country_ls)


SDG17_4 = c('debt',
            "financ|relief|restructuring|sustainab|service|distress",
            developing_country_ls)
SDG17_4 <- func_AND_plus(SDG17_4)
SDG17_4 <- gsub("\\.\\+", "(?!.*financial statement)", SDG17_4) ## to exclude 'financial statement'


SDG17_5 = c(finance_ls,
            'promot',
            developing_country_ls)



### Technology

SDG17_6 = c("access|availab|facilit",
            "\\bscience|\\btech|innovation|knowledge.?sharing|Internet|broadband",
            paste("North.?South|South.?South|triangular region", developing_country_ls, sep = "|"))

SDG17_7 = c("environment",
            "technolog",
            "development|transfer|dissemination|diffusion",
            developing_country_ls)

SDG17_8 = c("technolog|\\bscience|innovation|information|communication|internet",
            "capacity",
            developing_country_ls)

### Capacity-building
SDG17_9_x = c("capacity building|international support",
              developing_country_ls)

SDG17_9_y = c("financ|technical",
              "assistance", 
              developing_country_ls)
SDG17_9_y <- func_AND_plus(SDG17_9_y)
SDG17_9_y <- gsub("\\.\\+", "(?!.*financial statement)", SDG17_9_y) ## to exclude 'financial statement'

SDG17_9_z = c("North.?South|South.?South|triangular", 
              "cooperation|collaboration|partnership",
              developing_country_ls)


### Trade
SDG17_10_x = c("\\btrade|\\btrading|\\bWTO\\b",
               "universal|rules.?based|\\bopen\\b|non.?discrimin|equitab|equal|multilateral")
SDG17_10_y = paste("Doha Development Agenda", 
                   func_AND_plus(c('tariff', 'average')), 
                   sep = "|")
SDG17_10_x <- func_AND_plus(SDG17_10_x)
SDG17_10_x <- gsub("\\.\\+", "(?!.*equity method)(?!.*equity instrument)(?!.*liabilities and equity)(?!.*trading income)(?!.*open group)(?!.*Private Limited)", SDG17_10_x) ## to exclude



SDG17_11 = c(increase_ls,
             "export",
             developing_country_ls)


SDG17_12_x = c("market access", 
               developing_country_ls)
SDG17_12_y = c("duty|duties|\\bquota\\b|quotum", 
               "\\bfree\\b", 
               developing_country_ls)

### Systemic issues
SDG17_13_x = c("macroeconomic|macro-econom",
               "stability")
SDG17_13_y = c("macroeconomic|macro-econom",
               "\\bpolicy|\\bpolicies", 
               "coordination|coherence")

SDG17_14 = c(policy_ls,
             "coheren|coordinat|consistent|systematic",
             "sustainab|\\bSDG")

SDG17_15_x = c("respect",
               "leadership|initiative|\\blead|direction|\\bpolicy|\\bpolicies|framework|planning",
               "countr|\\bnation|\\bState\\b|government")
SDG17_15_y = c('\\bpolicy|\\bpolicies|framework|\\bplan\\b|\\bplans\\b|planning|objective|priorit|goal|target', 
               "country.?owned|country.?led|nation.?owned|nation.?led|\\bState.?owned|\\bState.?led|government.?owned|government.?led")

SDG17_15_x <- func_AND_plus(SDG17_15_x)
SDG17_15_y <- func_AND_plus(SDG17_15_y)
SDG17_15_x <- func_to_exclude_terms(which_sdg_term = SDG17_15_x, terms_to_exclude = c("with respect to"))



SDG17_16_x = c("\\bmulti", 
               "stakeholder|partner|cooperat|collaborat",
               "Sustainable|\\bSDG|goal")
SDG17_16_y = c("\\bshare.?\\b|\\bsharing|mobiliz|assembl|marshal", 
               paste("\\bknowledge|expertise|\\btech|automation", "financial resource", sep = '|'),
               "Sustainable|\\bSDG|goal")
SDG17_16_z = c("Global Partnership|joint effort", 
               "sustainable|\\bSDG|goal")

SDG17_16_y <- func_AND_plus(SDG17_16_y)

# terms_to_exclude  <- c('statement', 'granted', 'subsid', 'budget', 'investigat', 'financial assumption', 'finance corporation')
# terms_replacement <- paste0("(?!.*", paste(terms_to_exclude, collapse = ")(?!.*"), ")")
# SDG17_16_y <- gsub(pattern = "\\.\\+", replacement = terms_replacement, SDG17_16_y) ## to exclude these keywords

### --> OR
terms_to_exclude  <- c('statement', 'granted', 'subsid', 'budget', 'investigat', 'financial assumption', 'finance corporation')
SDG17_16_y <- func_to_exclude_terms(which_sdg_term = SDG17_16_y, terms_to_exclude = terms_to_exclude)



SDG17_17 = c("partnership", 
             "\\bpublic|private|civil societ|government")


SDG17_18 = c("\\bdata|statistic|indicator",
             "high.?quality|timely|reliable|availab",
             developing_country_ls)


SDG17_19_x = c("measur|monitor", 
               "sustainable develop|\\bSDG")
SDG17_19_y = func_AND_plus(c("statistic", "capacity|strengthen"))
SDG17_19_z = func_AND_plus(c("census", "population|\\bhousing"))
SDG17_19_w = func_AND_plus(c("regist", "birth|death"))



SDG17_general = c("sustainab|global",
                  "partner|cooperat|collaborat|joint effort|coordinat|stabil|stabl")



####
SDG17_1_y <- func_AND_plus(SDG17_1_y)

SDG17_2   <- SDG17_2

SDG17_3_x <- SDG17_3_x
SDG17_3_y <- func_AND_plus(SDG17_3_y)

SDG17_4 <- SDG17_4
SDG17_5 <- func_AND_plus(SDG17_5)
SDG17_6 <- func_AND_plus(SDG17_6)
SDG17_7 <- func_AND_plus(SDG17_7)
SDG17_8 <- func_AND_plus(SDG17_8)

SDG17_9_x <- func_AND_plus(SDG17_9_x)
SDG17_9_y <- SDG17_9_y
SDG17_9_z <- func_AND_plus(SDG17_9_z)

SDG17_11 <- func_AND_plus(SDG17_11)

SDG17_12_x <- func_AND_plus(SDG17_12_x)
SDG17_12_y <- func_AND_plus(SDG17_12_y)

SDG17_13_x <- func_AND_plus(SDG17_13_x)
SDG17_13_y <- func_AND_plus(SDG17_13_y)

SDG17_14   <- func_AND_plus(SDG17_14)


SDG17_16_x <- func_AND_plus(SDG17_16_x)
SDG17_16_y <- SDG17_16_y
SDG17_16_z <- func_AND_plus(SDG17_16_z)

SDG17_17   <- func_AND_plus(SDG17_17)
SDG17_18   <- func_AND_plus(SDG17_18)

SDG17_19_x <- func_AND_plus(SDG17_19_x)

SDG17_general <- func_AND_plus(SDG17_general)



# SDG17_1    <- func_OR_vector(c(SDG17_1,   SDG17_1_y))
# SDG17_3    <- func_OR_vector(c(SDG17_3_x, SDG17_3_y))
# SDG17_9    <- func_OR_vector(c(SDG17_9_x, SDG17_9_y, SDG17_9_z))
# SDG17_10   <- func_OR_vector(c(SDG17_10_x, SDG17_10_y))
# SDG17_12   <- func_OR_vector(c(SDG17_12_x, SDG17_12_y))
# SDG17_13   <- func_OR_vector(c(SDG17_13_x, SDG17_13_y))
# SDG17_15   <- func_OR_vector(c(SDG17_15_x, SDG17_15_y))
# SDG17_16   <- func_OR_vector(c(SDG17_16_x, SDG17_16_y, SDG17_16_z))
# SDG17_19   <- func_OR_vector(c(SDG17_19_x, SDG17_19_y, SDG17_19_z, SDG17_19_w))







################################################################################################## #
# Direct mention -----------------------------------------------------------------------------------
################################################################################################## #
## Terms for this type are fairly straightforward and easy to do ...

source('./Code/helper_UN_SDG_Target_list.R') ## --> ls_un; ls_un_id; goals_ls

## Goal level -----------------
# SDG1	 = (‘SDG 1\\b’|’SDG1\\b’|Goal 1\\b|Goal1\\b)
# SDG1_1 = (‘SDG 1.1\\b’|’SDG1.1\\b’)

# goal_ls1 <- data.frame(term = goals_ls)
# goal_ls2 <- data.frame(term = paste0('SDG ', seq(1, 17)))
# goal_ls3 <- ls_un %>% distinct(GoalName) %>% dplyr::rename(term = GoalName)
# goal_ls <- rbind(goal_ls1, goal_ls2, goal_ls3)


## 1.1 create a list of SDG id names, such as SDG 1, goal 17, ...
goal_ls_x <- paste0(
  '(sdg|goal)',
  # '.{0,2}',     ## `.` matches any character (e.g., a space or `s`); `{0,2}` matches the previous token between 0 and 2 times
  '[^0-9]{0,2}',  ## the above one can be problematic if `.` can be a number. Here we change it to match only non-numeric character
  '(?=',          ## Positive lookahead: e.g., in "SDG(?=17)", "17" immediately follows the "SDG".
  seq(1, 17), 
  '\\b',          ## \\b assert position at a word boundary
  ')'
)

## 1.2 get a list of SDG descriptions for each Goal
goal_ls_y <- ls_un %>% distinct(GoalName)
goal_ls_y <- goal_ls_y$GoalName

## 1.3 put 1 and 2 together
goal_keys <- data.frame(SDG_id = paste0('SDG', seq(1, 17), '_general'), 
                        goal_ls_x, 
                        goal_ls_y) %>%
  dplyr::mutate(SDG_keywords = paste(goal_ls_x, goal_ls_y, sep = "|")) %>%
  dplyr::select(SDG_id, SDG_keywords)



## Target level ------------------------------------------------------------------------------------

### get a list of target ids
targ_df  <- ls_un %>% distinct(target_id_un) 
targ_ids <- targ_df$target_id_un; targ_ids
targ_ids_ <- gsub('\\.', '\\\\.', targ_ids); targ_ids_ ## '.' can be matched with any character 

target_ls <- paste0(
  '(sdg|goal|target|indicator)',
  # '.{0,2}',        ## 
  '[^0-9]{0,2}',
  '(?=', targ_ids_,  ## 
  '[\\.]{0,1}',      ## there might be 0 or 1 period follows, e.g., if targ_ids = 17.1, then this can match '17.1' and '17.1.1' but not '17.11'
  ')'                ## 
)

target_keys <- data.frame(SDG_id = paste0('SDG', targ_ids), 
                          SDG_keywords = target_ls)%>%
  dplyr::mutate(SDG_id = gsub('\\.', '_', SDG_id))









#### ################################################################################### #
####                                                                                     #
####                                  Test                                            ####
####                                                                                     #
#### ################################################################################### #

# # pat <- '(sdg|goal|target|indicator)[^0-9]{0,2}(?=17[\\.]{0,1})|No Poverty'       ## 1. works
# pat <- '(sdg|goal|target|indicator)[^0-9]{0,2}(?=7\\.a[\\.]{0,1})|No Poverty'      ## 2. best
# # pat <- '(sdg|goal|target|indicator)(?:s|\\s){0,2}(?=7\\.a[\\.]{0,1})|No Poverty' ## 3. same as 2
# 
# data.frame(term = c(
#   'i love SDGs and sdg 7 and sdg 7 and goal 7',
#   'i love SDGs and sdg 7.1 and and goal 7.1, indicator7.12, target7.1.1, indicator 7.1.2',
#   'I like sdg17 and you',
#   'I like sdg-17 and you',
#   'I like sdgs 17 and you',
#   'I like goal 7 and you',
#   'I like goals 7 and you',
#   'I like goal 7.a and you',
#   'I like goals 7.a and you',
#   'I like goal 7.a.1 and you',
#   'I like goal 17.a.1 and you',
#   'I like goals 7.a.1 and you',
#   'I like sdg1 and 17 and you',
#   'I like 17 sdgs and you?',
#   'when will be no Poverty?'
# )) %>%
#   dplyr::mutate(
#     match = ifelse(
#       grepl(pattern = pat, x = term, ignore.case = T, perl = T), 1, 0),
#     n = str_count(string = term, regex(pattern = pat, ignore_case = T))) %>%
#   # arrange(desc(match)) %>%
#   as.data.frame()



### ref:  https://stackoverflow.com/questions/41802272/understanding-lookahead-in-r-regexp
###       https://tpristavec.github.io/regex/#26
###       https://users.cs.cf.ac.uk/Dave.Marshall/PERL/node79.html
