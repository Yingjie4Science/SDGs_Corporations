


###
# *   --> blank
# ()  --> ""
# AND --> ,



## make a vector of terms to concatenate with AND

func_AND_vector <- function(v){
  pat <- paste0("(?=.*(?:", v, "))", collapse="")
  return(pat)
}


## make a vector of terms to concatenate with OR
func_OR_vector <- function(v){
  pat <- paste0(v, collapse = "|")
  # print(pat)
  return(pat)
}


func_AND_plus <- function(target){
  target <- func_AND_vector(target)
  target <- paste0("^", target, ".+")
  # print(target)
  return(target)
}





################################################################################################## #
# Indirect mention ---------------------------------------------------------------------------------
################################################################################################## #


## Auxiliary list ------

ag_ls = "agricultur|food|farm|forestry|pastoral|pasture|agro|fisher|agronom|horticulture|cultivation|husbandry|grazing|planting"

child_ls = "child|^teen|juvenile|youth|young|minor|'under 5'|'under five'|adolescen|girl|boy"

death_ls = "mortal|death|dead|^die|dying|fatal|deceas|^demis|destruct|lethal"

disaster_ls = "shock|disaster|catastrophe|hazard|flood|drought|risk|exposure|harm|wildfire|wild fire|extreme weather"

emission_ls = "emission|co2|carbon|green house|greenhouse|GHG|G.H.G.|chlorofluorocarbon|cfc|c.f.c.|methane|CH4|nitrous oxide|^N2O|ozone|\\bO3\\b"

increase_ls = "accelerat|ascend|advanc|climb|better|boost|^boom|escalat|^grow|^hik|improv|increas|increment|jump|^lift|^rais|^ris|rocket|skyrocket|^surg|^soar|strengthen|upsurg|upward"

support_ls = "financ|fund|assist|help|^aid|invest|boost|bolster|^cash|compensation|donor|donat|enhanc|expenditure|grant|^loan|money|official flow|support|subsid|stimulate|strengthen|transfer|uphold"

urban_ls = "urbaniz|urban|city|cities|settlement|land consumption|land use|metropoli|town|municipal"

poverty_ls = "poverty|poor|impover|underprivileg|necessitous"

policy_ls = "action|activit|administ|agreement|approach|arrangement|blueprint|deal|guide|govern|method|manag|polic|plan|proced|program|project|practice|procedure|propos|protocol|rule|regulat|law|treat|right|constitution|initiative|scheme|strateg"

reduce_ls = "alleviat|abate|avert|abolish|eradicat|eliminat|^eras|^ease|^end|rid of|phase out|wipe out|reduc|^cut|^curb|curtail|declin|decreas|diminish|drop|dwindl|halt|hinder|hamper|impede|inhibit|lessen|lower|mitigat|slash|shrink|stop|trim|weaken|\\bno\\b|zero|prevent|prohibit"



developing_country_name_ls = paste(
  "Aruba|Afghanistan|Angola|Anguilla|Albania|United Arab Emirates|Argentina|Armenia|American Samoa", 
  "French Southern Territories|Antigua and Barbuda|Azerbaijan|Burundi|Benin|Burkina Faso|Bangladesh|Bahrain|Bahamas",
  "Bosnia and Herzegovina|Belarus|Belize|Bolivia|Brazil|Barbados|Brunei Darussalam|Bhutan|Botswana|Central African Republic",
  "Chile|China|Côte d'Ivoire|Cameroon|Congo|Cook Islands|Colombia|Comoros|Cabo Verde|Costa Rica|Cuba|Curaçao|Cayman Islands",
  "Cyprus|Cyprus|Djibouti|Dominica|Dominican Republic|Algeria|Ecuador|Egypt|Eritrea|Western Sahara|Ethiopia|Fiji|Micronesia",
  "Gabon|Georgia|Ghana|Guinea|Gambia|Guinea-Bissau|Equatorial Guinea|Grenada|Guatemala|Guam|Guyana|Hong Kong|Heard Island and McDonald Islands",
  "Honduras|Haiti|Indonesia|India|Iran|Iraq|Jamaica|Jordan|Kazakhstan|Kenya|Kyrgyzstan|Cambodia|Kiribati|Saint Kitts and Nevis|Kuwait|Lao",
  "Lebanon|Liberia|Libya|Saint Lucia|Sri Lanka|Lesotho|Macao|Saint Martin|Morocco|Moldova|Madagascar|Maldives|Mexico|Marshall Islands",
  "North Macedonia|Mali|Myanmar|Montenegro|Mongolia|Northern Mariana Islands|Mozambique|Mauritania|Montserrat|Mauritius|Malawi|Malaysia",
  "Namibia|New Caledonia|Niger|Norfolk Island| Nigeria|Nicaragua|Niue|Nepal|Nauru|Oman|Pakistan|Panama| Pitcairn|Peru|Philippines|Palau",
  "Papua New Guinea|Puerto Rico|Paraguay|Palestine|French Polynesia|Qatar|Russia|Rwanda|Sudan| Senegal|Singapore|South Georgia and the South Sandwich Islands",
  "Saint Helena|Solomon Islands|Sierra Leone|El Salvador|Somalia|Serbia|South Sudan|Sao Tome and Principe|Suriname|Eswatini|Sint Maarten|Seychelles|Syrian",
  "Turks and Caicos Islands|Chad|Togo|Thailand|Tajikistan|Turkmenistan|Timor-Leste|Tonga|Trinidad and Tobago|Tunisia|Turkey|Tanzania|Uganda|Ukraine|Uruguay",
  "Uzbekistan|Saint Vincent and the Grenadines|Venezuela|Virgin Islands|Viet Nam|Vanuatu|Wallis and Futuna|Samoa|Yemen|South Africa|Zambia|Zimbabwe|Somaliland",
  "Kosovo|Ashmore|Cartier|Siachen Glacier|North Korea", 
  func_AND_plus(c('Democratic','Korea')), 
  sep = "|")

developing_country_iso3_ls = paste(
  "ABW|AFG|AGO|AIA|ALB|ARE|ARG|ARM|ASM|ATF|ATG|AZE|BDI|BEN|BFA|BGD|BHR|BHS|BIH|BLR|BLZ|BOL|BRA|BRB|BRN|BTN|BWA",
  "CAF|CHL|CHN|CIV|CMR|COD|COG|COK|COL|COM|CPV|CRI|CUB|CUW|CYM|CYP|CYP|DJI|DMA|DOM|DZA|ECU|EGY|ERI|ESH|ETH|FJI|FSM|GAB|GEO|GHA|GIN|GMB|GNB|GNQ|GRD|GTM|GUM|GUY",
  "HKG|HMD|HND|HTI|IDN|IND|IRN|IRQ|JAM|JOR|KAZ|KEN|KGZ|KHM|KIR|KNA|KOR|KWT|LAO|LBN|LBR|LBY|LCA|LKA|LSO|MAC|MAF|MAR|MDA|MDG|MDV|MEX|MHL|MKD|MLI|MMR|MNE|MNG|MNP",
  "MOZ|MRT|MSR|MUS|MWI|MYS|NAM|NCL|NER|NFK|NGA|NIC|NIU|NPL|NRU|OMN|PAK|PAN|PCN|PER|PHL|PLW|PNG|PRI|PRK|PRY|PSE|PYF|QAT|RUS|RWA|SDN|SEN|SGP|SGS|SHN|SLB|SLE|SLV",
  "SOM|SRB|SSD|STP|SUR|SWZ|SXM|SYC|SYR|TCA|TCD|TGO|THA|TJK|TKM|TLS|TON|TTO|TUN|TUR|TZA|UGA|UKR|URY|UZB|VCT|VEN|VIR|VNM|VUT|WLF|WSM|YEM|ZAF|ZMB|ZWE", 
  sep = "|")
developing_country_iso3_ls <- gsub('\\|', '\\\\b|\\\\b', developing_country_iso3_ls)
developing_country_iso3_ls <- paste0('\\b', developing_country_iso3_ls, '\\b')
developing_country_iso3_ls

developing_country_iso2_ls = paste(
  "AW|AF|AO|AI|AL|AE|AR|AM|AS|TF|AG|AZ|BI|BJ|BF|BD|BH|BS|BA|BY|BZ|BO|BR|BB|BN|BT|BW|CF|CL|CN|CI|CM|CD|CG|CK|CO|KM|CV|CR|CU|CW|KY|CY|CY|DJ|DM|DO|DZ|EC|EG|ER|EH",
  "ET|FJ|FM|GA|GE|GH|GN|GM|GW|GQ|GD|GT|GU|GY|HK|HM|HN|HT|ID|IN|IR|IQ|JM|JO|KZ|KE|KG|KH|KI|KN|KR|KW|LA|LB|LR|LY|LC|LK|LS|MO|MF|MA|MD|MG|MV|MX|MH|MK|ML|MM|ME|MN",
  "MP|MZ|MR|MS|MU|MW|MY|NA|NC|NE|NF|NG|NI|NU|NP|NR|OM|PK|PA|PN|PE|PH|PW|PG|PR|KP|PY|PS|PF|QA|RU|RW|SD|SN|SG|GS|SH|SB|SL|SV|SO|RS|SS|ST|SR|SZ|SX|SC|SY|TC|TD|TG",
  "TH|TJ|TM|TL|TO|TT|TN|TR|TZ|UG|UA|UY|UZ|VC|VE|VI|VN|VU|WF|WS|YE|ZA|ZM|ZW", 
  sep = "|")
developing_country_iso2_ls <- gsub('\\|', '\\\\b|\\\\b', developing_country_iso2_ls)
developing_country_iso2_ls <- paste0('\\b', developing_country_iso2_ls, '\\b')
developing_country_iso2_ls


developing_country_ls = c("developing|least develop|less develop|underdevel|poor|small island|africa|impover|pover", 
                          "countr|nation|state")
developing_country_ls = func_AND_plus(developing_country_ls)
developing_country_ls = func_OR_vector(v = c(developing_country_ls, 
                                             developing_country_iso3_ls,
                                             developing_country_iso2_ls,
                                             'global south|Third World'))





## 1. No Poverty -----------------------------------------------------------------------------------
  
SDG1_1 = c(poverty_ls, 
           reduce_ls, 
           "extreme|most|excess|exceeding|high|huge|great|over|remarkabl|striking|severe|serious")

SDG1_2 = c(poverty_ls, 
           reduce_ls)  

SDG1_3 = c(paste(poverty_ls, "vulnerab", sep = "|"), 
           "assure|care|cover|preserve|protect|safeguard|secure|shelter|shield|support")

SDG1_4 = c("access|availab|rights", 
           "resource|basic service|property|ownership|land|financ|holding|estate|home|house|farm")

SDG1_5 = c(paste(poverty_ls, "vulnerab", sep = "|"), 
           disaster_ls, 
           reduce_ls)

SDG1_a = c(poverty_ls, 
           "econom|financ|develop", 
           paste(support_ls, "empower|relie", sep = "|"))

SDG1_b = c(poverty_ls, 
           reduce_ls, 
           policy_ls)


### further ------------- ###
SDG1_1 <- func_AND_vector(SDG1_1)
SDG1_1 <- paste0("^", SDG1_1, ".+"); SDG1_1
SDG1_2 <- func_AND_plus(SDG1_2)
SDG1_3 <- func_AND_plus(SDG1_3)
SDG1_4 <- func_AND_plus(SDG1_4)
SDG1_5 <- func_AND_plus(SDG1_5)
SDG1_a <- func_AND_plus(SDG1_a)
SDG1_b <- func_AND_plus(SDG1_b)




## 2. Zero Hunger ----------------------------------------------------------------------------------

SDG2_1_x = c(reduce_ls, "hunger|undernourish|starv|famine|malnourish") 
SDG2_1_y = c(reduce_ls, "food", "insecurity|desert")
SDG2_1_z = c("food", "access|availab|safe|secur|nutritious|sufficient|ample|plentiful|abundant")

SDG2_2 = c("malnutrition|malnourish|undernourish|undernutrition|under nourished|stunting|wasting|overweight|^polio|paralysis|tephromyelitis|nutrition|anaem|anem",
           paste(child_ls, "infant|pregnan|lactat|women|woman|older", sep = '|'))

SDG2_3_x = c(ag_ls, 
             "productiv|production|income")
SDG2_3_y = c("access|availab", 
             "land|'land tenure right'|'land right'|'land own'|resource|input|knowledge|'financial service'|market|opportunit|employ")
SDG2_3_z = c("reform", 
             "land")

SDG2_4_x = c(ag_ls, 
             "sustain|resilien|productiv|organic|ecological")
SDG2_4_y = c("adapt", 
             paste(disaster_ls, "climate change|global warming|warm", func_AND_plus(c('temperat', '^ris')), sep = '|'))
SDG2_4_z = c("adapt", 
             'sea level', "ris")
SDG2_4_w = c("land|soil", "quality|fertil")

SDG2_5_x = c("manag|diversif|diversity|conserv|secur|cultivat|farm|domesticat", 
             "seed|'plant bank'|animal|genetic|wild species")
SDG2_5_y = c("manag|diversif|diversity|conserv|secur|cultivat|farm|domesticat",
             "local|traditional", "breed|bred|knowledge")
SDG2_5_z = c("genetically modified|gmo", "food")


SDG2_a_x = c(paste(support_ls, "cooperat|collaborat|joint effort|flow", '|'), 
           "rural|agricult",
           "infrastructure|research|investig|exten|advanc|technolog") 
SDG2_a_y = c("plant|soy|livestock|animal|cattle|cow|pig|sheep|hog|horse|oxen|herd|swine", "gene bank")


SDG2_b_x = c(ag_ls, 
             "export|trade|supply chain|value chain|market|business|commerce", 
             paste("subsid|restrict|allowance", support_ls, sep = '|'))
SDG2_b_y = "Doha Development Round|Doha Round|DDR|D.D.R."

SDG2_c = c(ag_ls, "market|price", "volatil|anomal|change|unstable|unsettled|elastic|elusive")


### further ------------- ###
SDG2_1_x <- func_AND_plus(SDG2_1_x)
SDG2_1_y <- func_AND_plus(SDG2_1_y)
SDG2_1_z <- func_AND_plus(SDG2_1_z)
SDG2_1   <- func_OR_vector(c(SDG2_1_x, SDG2_1_y, SDG2_1_z))

SDG2_2 <- func_AND_plus(SDG2_2)

SDG2_3_x <- func_AND_plus(SDG2_3_x)
SDG2_3_y <- func_AND_plus(SDG2_3_y)
SDG2_3_z <- func_AND_plus(SDG2_3_z)
SDG2_3   <- func_OR_vector(c(SDG2_3_x, SDG2_3_y, SDG2_3_z))

SDG2_4_x <- func_AND_plus(SDG2_4_x)
SDG2_4_y <- func_AND_plus(SDG2_4_y)
SDG2_4_z <- func_AND_plus(SDG2_4_z)
SDG2_4_w <- func_AND_plus(SDG2_4_w)
SDG2_4   <- func_OR_vector(c(SDG2_4_x, SDG2_4_y, SDG2_4_z, SDG2_4_w))


SDG2_5_x <- func_AND_plus(SDG2_5_x)
SDG2_5_y <- func_AND_plus(SDG2_5_y)
SDG2_5_z <- func_AND_plus(SDG2_5_z)
SDG2_5   <- func_OR_vector(c(SDG2_5_x, SDG2_5_y, SDG2_5_z))


SDG2_a_x <- func_AND_plus(SDG2_a_x)
SDG2_a_y <- func_AND_plus(SDG2_a_y)
SDG2_a   <- func_OR_vector(c(SDG2_a_x, SDG2_a_y))


SDG2_b_x <- func_AND_plus(SDG2_b_x)
SDG2_b_y <- func_AND_plus(SDG2_b_y)
SDG2_b   <- func_OR_vector(c(SDG2_b_x, SDG2_b_y))

SDG2_c <- func_AND_plus(SDG2_c)




## 3. Good Health and Well-being  ------------------------------------------------------------------


SDG3_1 = c(paste0("matern|antenatal|birth|", func_AND_plus(c("post","natal|partum"))), 
           paste0("death_ls|health|healthcare|complica|depress|^care", func_AND_plus(c("health", "^care"))))

SDG3_2 = c(paste(child_ls, "newborn|'before fifth'|premature|infant", func_AND_plus(c("^neo", "^natal")), sep = '|'),
           paste("death_ls|syndrome|wellness|well", func_AND_plus(c("well", "being")), sep = '|'))

SDG3_3 = func_OR_vector(v = c(
  "epidemic|pandemic|outbreak|communicable|infect|contagious|endemic|COVID|^sars|'acute respiratory syndrome'|zika", 
  "dengue|schistosomiasis|ebola|measles|cholera|^AIDS|^HIV|Acquired immunodeficiency syndrome|Human immunodeficiency virus", 
  "Yellow fever|Middle East respiratory syndrome|MERS-CoV|Antiretroviral|tuberculos|malaria|'tropical disease'|hepatit|Lyme disease", 
  func_AND_plus(c("water", "borne")),
  func_AND_plus(c("sexual", "transmi"))))

SDG3_4_x = c(
  paste("non-communicable",
        "^NCD|cardiovascular|cancer|diabet|chronic respiratory|asthma|suicid|diarrhea|diarrhoea|dysentery|^obes", 
        death_ls,
        sep = '|'),
  # paste(death_ls, "disorder|disease|illness|sick", sep = "|"),
  reduce_ls);
SDG3_4_y = c(
  "mental|psychological|psychiatric", 
  paste(death_ls, "disorder|disease|health|well|illness|sick|disabilit", sep = "|"), 
  reduce_ls)
SDG3_4_z = c('life expectancy', increase_ls)

SDG3_5 = c("substance|drug|alcohol|drink|ethanol|liquor|liqueur|booze|wine|beer|Narcotic", 
           "abus|misuse|misconduct|obsessive|addict|harm|disorder")

SDG3_6 = c("road|traffic|congest|collision|crash|jam|transport|transit|travel", 
           paste(death_ls, 
                 "injur|accident|adversit|bottleneck|damag|catastroph|calamit|casualt|disaster|emergenc|traged", 
                 sep = "|"))

SDG3_7_x = c(
  paste("^sex|reproductive|family planning|contracept|condom|diaphragm|Birth Control|intrauterine device|^IUD|conception control", 
        func_AND_plus(c("health", "care")), sep = "|"), 
  "access|availab|modern|inform|educat")
SDG3_7_y = c(child_ls, "birth|mother|pregnan")



SDG3_8 = c("health|medicine|vaccin", 
           paste("coverage|expenditure|expense|income|financ|care|service|essential|access|availab|Affordable|public", policy_ls, sep = "|"))

SDG3_9_x = c("hazard|unsafe|unintentional|contamin|pollut", 
             "chemical|air|water|soil|sanita|hygien|poison",
             paste(death_ls, "illness|sick|health|well being|well-being|wellbeing", sep = "|"))
SDG3_9_y = c("lack", 
             "sanita|hygien*|poison", 
             paste(death_ls, "illness|sick|health|well being|well-being|wellbeing", sep = "|"))


SDG3_a = c("tobacco|nicotine|cigar|^vap|smok" , 
           "control|regulat|reduc|manag|^use")

SDG3_b = c(
  paste("vaccin|medicin|medication|treatment|antibiotic|pharmac|drug|communicable|disease",
        func_AND_plus(c("basic","health|well being|well-being")),
        "health facilit", sep = '|'),
  "research|develop|access|availab|cover|afford|available|quality|assist|TRIPS Agreement|Trade-Related Aspects of Intellectual Property Rights")

          
SDG3_c = c("health|well being|well-being", 
           "financ|recruit|develop|train|retention|workforce|worker|employ|labor|labour")

SDG3_d_x = c("health", "early warning|manag|risk|emergency|urgen")
SDG3_d_y = c("International Health Regulations|IHR")
SDG3_d_z = c(
  paste(func_AND_plus(c("blood", "infect")), 
        "antibiotic|antimicr", sep = '|'), 
  "resist")
  
          
          
SDG3_1 <- func_AND_plus(SDG3_1) 
SDG3_2 <- func_AND_plus(SDG3_2) 
SDG3_3 <- SDG3_3

SDG3_4_x <- func_AND_plus(SDG3_4_x)
SDG3_4_y <- func_AND_plus(SDG3_4_y)
SDG3_4_z <- func_AND_plus(SDG3_4_z)
SDG3_4   <- func_OR_vector(c(SDG3_4_x, SDG3_4_y, SDG3_4_z))

SDG3_5 <- func_AND_plus(SDG3_5)
SDG3_6 <- func_AND_plus(SDG3_6)

SDG3_7_x <- func_AND_plus(SDG3_7_x)
SDG3_7_y <- func_AND_plus(SDG3_7_y)
SDG3_7   <- func_OR_vector(c(SDG3_7_x, SDG3_7_y))

SDG3_8 <- func_AND_plus(SDG3_8) 

SDG3_9_x <- func_AND_plus(SDG3_9_x) 
SDG3_9_y <- func_AND_plus(SDG3_9_y)
SDG3_9   <- func_OR_vector(c(SDG3_9_x, SDG3_9_y))


SDG3_a <- func_AND_plus(SDG3_a)
SDG3_b <- func_AND_plus(SDG3_b)  
SDG3_c <- func_AND_plus(SDG3_c)  

SDG3_d_x <- func_AND_plus(SDG3_d_x)
SDG3_d_y <- func_AND_plus(SDG3_d_y)
SDG3_d_z <- func_AND_plus(SDG3_d_z)
SDG3_d   <- func_OR_vector(c(SDG3_d_x, SDG3_d_y, SDG3_d_z))

                  
          









## 4. Quality Education ----------------------------------------------------------------------------- 
          
          
SDG4_1 = c(
  paste(child_ls, 
        func_AND_plus(c("grade", "2|2nd|second|two|3|third|3rd|three")), 
        "primary|secondary|middle", sep = "|"), 
  "read|math|educat|school")


SDG4_2 = c(
  paste(child_ls, 
        "^pre|early", 
        func_AND_plus(c("before", "primary|school")),sep = "|"),
  paste("develop|^care|educat|health|learn|^well", 
        func_AND_plus(c("psycho", "^soci")), sep = "|")) 


SDG4_3 = c("technical|vocational|tertiary|third|3rd|educat|colleg|universit|train|learn|'high school'|GED|G.E.D.|'General Educational Development'",
           "access|availab|equit|equal|free|affordable|quality|particip|join|attend|lifelong|adult|nontraditional")

SDG4_4 = c("learn|educat|skill|technique|technical|expert",
           "employ|job|entrepreneur|lifelong|continuing|vocational|adult|youth")

SDG4_5 = c(
  "education|vocational|train|school|empower",
  paste("disabilit|disabled|handicap|wounded|indigen|vulnerable|gender|female|women|rural", urban_ls, 
        "bottom|top|conflict|disparit|equal|access|availab|equit|inclu|exclu|inequality|discriminat|special|gap|'poverty trap'|underrepresented",
        func_AND_plus(c("drop",  "out")), sep = '|'))

SDG4_6 = "literacy|numeracy|literate"

SDG4_7_x = c("^knowledge|skill|educat|curricula|teach|student|cultur",
             "sustainable development|sustainable lifestyle|'human right'|'gender equality'|peace|non-violence|'global citizenship'|international|'cultural diversity'|polic|reform")
SDG4_7_y = "Universal education"

SDG4_a = c("educat|learn|school", 
           "facilit|infrastructure|environment|'basic service'")

SDG4_b = c(paste("scholarship|fellowship|flow", support_ls, sep = "|"),  
           "knowledge|skill|educat|vocational|train|inform|'communications technology|\\btechnical\\b|engineer|scien|mathemat|technolo|STEM|S.T.E.M.",
           developing_country_ls) 


SDG4_c = c('teacher|educator|faculty|instructor|lecturer|professor|supervisor|mentor|tutor|trainer|adviser|coach|caregiver', 
           "supply|attrition|qualif|train|capab|capacity")


#### other related but not included in the above 
SDG4_general = c("school|educat",
                 "attend|enroll|basic|equit|safe|lifelong|lifetime|continuing")

          


SDG4_1 <- func_AND_plus(SDG4_1)
SDG4_2 <- func_AND_plus(SDG4_2)  
SDG4_3 <- func_AND_plus(SDG4_3)  
SDG4_4 <- func_AND_plus(SDG4_4)  
SDG4_5 <- func_AND_plus(SDG4_5)  
SDG4_6 <- SDG4_6

SDG4_7_x <- func_AND_plus(SDG4_7_x)
SDG4_7   <- func_OR_vector(c(SDG4_7_x, SDG4_7_y))

SDG4_a <- func_AND_plus(SDG4_a)
SDG4_b <- func_AND_plus(SDG4_b)
SDG4_c <- func_AND_plus(SDG4_c)
SDG4_general <- func_AND_plus(SDG4_general)
                    
                    




## -5. Gender Equality -----------------------------------------------------------------------------

female_ls = "woman|women|girl|female|lady|ladies"

###
  
SDG5_1_x = c("discriminat|equality|justice|disparities|segregat|^anti|marginali|infanticid|gap|'wage gap'|unemploy|empower|livelihood",
           paste(female_ls, "sex|gender|employment", sep = "|"))
SDG5_1_y = "misogyn|feminis"


SDG5_2_x = c("violence|assault|attack|offens|abuse|traffick|exploit|forc",
           paste(female_ls, "domestic|sexual", sep = "|"))
SDG5_2_y = "prostitut|^rape|harass"


SDG5_3_x = c(paste(child_ls, "harm|early|forced|'before age 15'|'before age 18'", sep = "|"),
           "marr")
SDG5_3_y = c(female_ls,  'genital', "mutilat|^cut")


SDG5_4 = c("unpaid|'shared responsibility'|allocation|equity", 
           paste("care|'domestic work'|household|famil", female_ls, sep = "|"))


SDG5_5 = c("leadership|leader|seat|decision-making|'national parliament'|government|autonomy|'managerial position'|political participation|politician|manager|underrepresentation|entrepreneurship|power",
           female_ls)


SDG5_6 = c("access|availab|right|decision|law|regulation|information|educat", 
           "sexual|reproductive|contraceptive|'family planning'|divorc")


SDG5_a_x = c("access|availab|right|reform|equal|legal|law",
           "resource|fund|ownership|control|land|property|'financial service'|finance|inheritance|owner|right|budget|quota",
           paste(female_ls, "gender", sep = "|"))
SDG5_a_y = "agrarian feminism"


SDG5_b = c("tech|information|empower|mobile|telephone|\\bICT\\b",
           female_ls)	

SDG5_c = c("policy_ls|legislation|promot|empower", 
           paste("gender|sex|equality", female_ls, sep = "|"))



SDG5_1_x <- func_AND_plus(SDG5_1_x) 
SDG5_1   <- func_OR_vector(c(SDG5_1_x, SDG5_1_y))

SDG5_2_x <- func_AND_plus(SDG5_2_x) 
SDG5_2   <- func_OR_vector(c(SDG5_2_x, SDG5_2_y))

SDG5_3_x <- func_AND_plus(SDG5_3_x)
SDG5_3_y <- func_AND_plus(SDG5_3_y)
SDG5_3   <- func_OR_vector(c(SDG5_3_x, SDG5_3_y))

SDG5_4 <- func_AND_plus(SDG5_4) 
SDG5_5 <- func_AND_plus(SDG5_5) 
SDG5_6 <- func_AND_plus(SDG5_6) 

SDG5_a_x <- func_AND_plus(SDG5_a_x) 
SDG5_a   <- func_OR_vector(c(SDG5_a_x, SDG5_a_y))

SDG5_b <- func_AND_plus(SDG5_b) 
SDG5_c <- func_AND_plus(SDG5_c) 






## 6. Clean Water and Sanitation --------------------------------------------------------------------

SDG6_1 = c("access|availab|afford|safe|secure|clean|manag|equit", 
           "drinking water|drinkable water")

SDG6_2_x = c("access|availab|safe|manag|adequate|equit|equal|facilit|service",
             paste("sanita|hygien|soap|cleanliness", func_AND_plus(c("hand", "wash")), sep = '|'))
SDG6_2_y = c("access|availab|safe|open|hygien|adequate|equal|health|clean|manag",
             "bathroom|defecation|toilet|restroom|lavatory|latrine|water closet")


SDG6_3_x = c("water|aquatic",
             "quality|pollut|black|recycl|reus")
SDG6_3_y = c("dump|releas|recycl|reus|untreated|treated|sustainab",
             "hazardous|chemical|toxicology|ecotoxicology|material|wastewater|sewer")

SDG6_4 = c("water|freshwater", 
           "efficien|sustainab|footprint|green|blue|grey|scarcity|shortage|stress|dearth|deficit|lack|shortfall|insufficiency|deficiency|secur|availab|suppl|conservat")


SDG6_5 = c("water resource|watershed",
           paste("manag|transboundary|transnational|cooperat|collaborat|joint effort|co-operation", policy_ls, sep = "|"))


SDG6_6 = c("water|mountain|forest|wetland|river|aquifer|lake",
           func_AND_plus(c("eco", "system")))


SDG6_a = c("water|sanita",
           paste(func_AND_plus(c("international", "coop|co-op|collab")),
                 func_AND_plus(c("capacity", "build")), 
                 "'development assistance'|harvesting|desalina|efficien|waste|treatment|recycl|reus|technolog|purification|infrastructure", 
                 support_ls, sep = "|"),
           developing_country_ls)

SDG6_b =  c("water|sanita", 
            support_ls,
            "local|indigen|\\btraditional\\b")



SDG6_1   <- func_AND_plus(SDG6_1) 

SDG6_2_x <- func_AND_plus(SDG6_2_x) 
SDG6_2_y <- func_AND_plus(SDG6_2_y)
SDG6_2   <- func_OR_vector(c(SDG6_2_x, SDG6_2_y))

SDG6_3_x <- func_AND_plus(SDG6_3_x) 
SDG6_3_y <- func_AND_plus(SDG6_3_y) 
SDG6_3   <- func_OR_vector(c(SDG6_3_x, SDG6_3_y))

SDG6_4 <- func_AND_plus(SDG6_4)
SDG6_5 <- func_AND_plus(SDG6_5) 
SDG6_6 <- func_AND_plus(SDG6_6) 
SDG6_a <- func_AND_plus(SDG6_a) 
SDG6_b <- func_AND_plus(SDG6_b) 






## 7. Affordable and Clean Energy ----------------------------------------------------------------

renewable_ls_x = c(paste("advance|modern|clean|renewable|alternat|wind|solar|biomass|nuclear|sun|tide|tidal|wave|wood|thermal|algae|Hydro|hybrid|green",
                         func_AND_plus(c("enviro|eco", "friendly")), sep = "|"),
                   "energy|fuel|power")

renewable_ls_y = paste("renewable|biofuel|bioenergy|biodiesel|biogas|bioethanol|biorefinery|hydrogen|hydropower", 
                       "hydroelectric|ethanol|photovoltaic|wind farm|offshore wind|wind turbine|solar panel", sep = "|")
renewable_ls_x <- func_AND_plus(renewable_ls_x) 
renewable_ls   <- func_OR_vector(c(renewable_ls_x, renewable_ls_y))



SDG7_1 = c(paste(renewable_ls, "electricity", sep = "|"), 
           "reliable|affordab|access|availab|modern")

SDG7_2_x = c(renewable_ls, 
             "share|consum|transiti|shift")
SDG7_2_y = paste("energy transition", func_AND_plus(c("smart", "grid|meter")), sep = "|")
SDG7_2_z = c("coal|fossil fuel|fossil-fuel", 
             reduce_ls)


SDG7_3 = c("energy|electricity|fuel|power|utilit",
           "efficien|intens|footprint|\\bper\\b")

SDG7_a_x = c(renewable_ls, 
             paste("cooperat|collaborat|joint effort|develop|research|tech", support_ls, sep = "|"))
SDG7_a_y = c('research|^tech|infrastructure|hybrid system', 
             'energy',
             paste("access|availab", support_ls, sep = "|"))

SDG7_b = c(renewable_ls, 
           paste(developing_country_ls, support_ls, "enhanc", sep = "|"))


####
SDG7_1 <- func_AND_plus(SDG7_1)

SDG7_2_x <- func_AND_plus(SDG7_2_x) 
SDG7_2_z <- func_AND_plus(SDG7_2_z) 
SDG7_2   <- func_OR_vector(c(SDG7_2_x, SDG7_2_y, SDG7_2_z))

SDG7_3 <- func_AND_plus(SDG7_3)

SDG7_a_x <- func_AND_plus(SDG7_a_x)
SDG7_a_y <- func_AND_plus(SDG7_a_y)
SDG7_a   <- func_OR_vector(c(SDG7_a_x, SDG7_a_y))

SDG7_b <- func_AND_plus(SDG7_b)



## 8. Decent Work and Economic Growth --------------------------------------------------------------

economic_ls = "econom|profit|revenue|^GDP|gross|income|gain|proceed|yield|dividend|earning|fiscal|financ|monetary|budget|return"


###

SDG8_1 = c(economic_ls, 
           paste("develop|prosperity", increase_ls, sep = "|")) ## ,"nation|countr|state"

SDG8_2 = c(economic_ls, 
           'productiv', 
           "technolo|innovation|modernization")

SDG8_3_x = c('polic', 
             paste(economic_ls, "development|employ", sep = "|"))
SDG8_3_y = c("employ|job|work", 
             "decent|quality|creation|stable")
SDG8_3_z = c("microenterprise|micro-enterprise|entrepreneur|inclusive",
             'growth') 
SDG8_3_w = c("small|medium|starting", 
             "enterprise|entrepreneur",
             "growth")


SDG8_4 = c("sustainab|efficien|environment|footprint", 
           "resource|material|consumption|production|^GDP|growth")

SDG8_5_x = c("full|decent|productive|informal|precarious|protect",
           "employ|job|work")
SDG8_5_y = "unemploy|equal pay"


SDG8_6 = c(child_ls, 
           "educat|employ|train")

SDG8_7_x = c(paste("forced", child_ls, sep = "|"),
           "labour|labor|soldier")
SDG8_7_y = "modern slavery|human trafficking"


SDG8_8_x = c("labour|labor|employ", 
             'right')
SDG8_8_y = c("safe|secure", 
             "work|job",
             "environment|workplace|condition")
SDG8_8_z = c(paste(death_ls, "precarious|injur|harm", sep = "|"), 
             "job|work|employ")


SDG8_9_x = c("touris", 
             paste("sustainab|community-based|employment", func_AND_plus(c('creat', 'job')), sep = "|"))
SDG8_9_y = func_AND_plus(c("tour",  "poli"))
SDG8_9_z = 'ecotourism'  

SDG8_10 = c("access|availab|inclusi", 
            "financ|bank|insurance")

SDG8_a = c(support_ls, 
           'trade')

SDG8_b = c(child_ls, 
           'employ', 
           policy_ls)




SDG8_1 <- func_AND_plus(SDG8_1)
SDG8_2 <- func_AND_plus(SDG8_2)

SDG8_3_x <- func_AND_plus(SDG8_3_x)
SDG8_3_y <- func_AND_plus(SDG8_3_y)
SDG8_3_z <- func_AND_plus(SDG8_3_z)
SDG8_3_w <- func_AND_plus(SDG8_3_w)
SDG8_3   <- func_OR_vector(c(SDG8_3_x, SDG8_3_y, SDG8_3_z, SDG8_3_w))

SDG8_4 <- func_AND_plus(SDG8_4)

SDG8_5_x <- func_AND_plus(SDG8_5_x)
SDG8_5   <- func_OR_vector(c(SDG8_5_x, SDG8_5_y))

SDG8_6 <- func_AND_plus(SDG8_6)

SDG8_7_x <- func_AND_plus(SDG8_7_x)
SDG8_7   <- func_OR_vector(c(SDG8_7_x, SDG8_7_y))

SDG8_8_x <- func_AND_plus(SDG8_8_x)
SDG8_8_y <- func_AND_plus(SDG8_8_y)
SDG8_8_z <- func_AND_plus(SDG8_8_z)
SDG8_8   <- func_OR_vector(c(SDG8_8_x, SDG8_8_y, SDG8_8_z))

SDG8_9_x <- func_AND_plus(SDG8_9_x)
SDG8_9   <- func_OR_vector(c(SDG8_9_x, SDG8_9_y, SDG8_9_z))

SDG8_10 <- func_AND_plus(SDG8_10)
SDG8_a <- func_AND_plus(SDG8_a)
SDG8_b <- func_AND_plus(SDG8_b)







## -9. Industry, Innovation, and Infrastructure ----------------------------------------------------

infrastructure_ls = paste(
  "infrastructure|building|^hous|architecture|construction|freight|transport|^road|^rail|^port|power plant|^dams",
  "bridge|airport|aviation|sewer|broadband|internet|telecommunication|electricity|power grid|electrical grid|park",
  "tunnel|water supply|Canal|Hospital|Irrigation scheme|Levee|Lighthouse|",
  "Pipeline|transit|Public space|Sewage treatment|Sewerage|Sluice|Solid waste|Utilities|Weir|waterway|harbor|dock|dike", sep = "|")
  


###

SDG9_1 = c("quality|reliable|sustainab|resilien|afford|equitable|access|availab|inclus",
           infrastructure_ls)

SDG9_2 = c("inclusive|sustainab|share|'cradle to cradle'|circular",
           "industr")

SDG9_3 = c("industr|enterprise|entrepreneur",
           "financial service|affordable credit|loan|value chain|fund")

SDG9_4_x = c("upgrade|retrofit",
             paste(infrastructure_ls, "industr", sep = '|'))
SDG9_4_y = c("resource", 
             "efficiency")
SDG9_4_z = c("industr",  
             emission_ls,
             paste(reduce_ls, "management|treatment", sep = '|'))
SDG9_4_w = c("clean|environment|green", 
             "technolog")

SDG9_5_x = c("tech|innovation|research|development", 
             "industr|manufactur")

SDG9_a = c(infrastructure_ls, support_ls, developing_country_ls)

SDG9_b = c(paste("tech|innovation|research|development|diversif", increase_ls, sep = "|"),
           paste("domestic", developing_country_ls, sep = "|"))

SDG9_c = c("access|availab", 
           paste("information|internet|mobile network|Phone service|broadband|^ICT|\\b5G\\b|\\b4G\\b", 
                 func_AND_plus(c('communications', 'technology')), sep = '|'))


#### 
SDG9_1 <- func_AND_plus(SDG9_1)
SDG9_2 <- func_AND_plus(SDG9_2)
SDG9_3 <- func_AND_plus(SDG9_3)

SDG9_4_x <- func_AND_plus(SDG9_4_x)
SDG9_4_y <- func_AND_plus(SDG9_4_y)
SDG9_4_z <- func_AND_plus(SDG9_4_z)
SDG9_4_w <- func_AND_plus(SDG9_4_w)
SDG9_4   <- func_OR_vector(c(SDG9_4_x, SDG9_4_y, SDG9_4_z, SDG9_4_w))

SDG9_5_x <- func_AND_plus(SDG9_5_x)
SDG9_5   <- SDG9_5_x

SDG9_a <- func_AND_plus(SDG9_a)
SDG9_b <- func_AND_plus(SDG9_b)
SDG9_c <- func_AND_plus(SDG9_c)



## 10. Reduce inequality within and among countries ------------------------------------------------

SDG10_1 = c("income|livelihood|household expenditure|earning|pay",
            paste("empower", increase_ls, sep = "|"))

SDG10_2 = c("inclusi|empower|promot|reform",
            "social|economic|political|age|sex|disab|race|ethnicity|origin|religion|income|demographic")

SDG10_3_x = c("inequalit|discriminat|harass|homophobia|racism|sexism",
              reduce_ls)
SDG10_3_y = c("equal opportunity|^equity||human right|justice|inclusi",
              "ensure|promot|empower")

SDG10_4_x = c("equal|inequalit",
              "polic|fiscal|economic|financ|wage|income|social|socio-economic|labor|labour")
SDG10_4_y = "GINI|social protection"

SDG10_5 = c("regulat|monitor|sound",
            "financ",
            "market|institution")

SDG10_6 = c("represent|voice|right|vote|effective|credible|accountable|legitimate",
            "econom|financ|fund")


SDG10_7_x = c("migrat|mobility",
            "orderly|safe|regular|responsible|polic")
SDG10_7_y = "refugee"


SDG10_a = c(paste(reduce_ls, "special|differential|free", sep = '|'),
            'tariff|duty|wto|Trade|import|export',
            developing_country_ls)


SDG10_b_x = c(paste(support_ls, "flow", sep = '|'),
              "develop|resource|capabilit|national plan|program")
SDG10_b_y = "north-south divide|financial flow|resource flow|foreign direct investment|FDI|ODA"


SDG10_c = c("remittance", 
            "cost",
            reduce_ls)

SDG10_general = c("equal|inequalit",
                  "sustainab|environm|health")


####
SDG10_1 <- func_AND_plus(SDG10_1)
SDG10_2 <- func_AND_plus(SDG10_2)

SDG10_3_x <- func_AND_plus(SDG10_3_x)
SDG10_3_y <- func_AND_plus(SDG10_3_y)
SDG10_3   <- func_OR_vector(c(SDG10_3_x, SDG10_3_y))

SDG10_4_x <- func_AND_plus(SDG10_4_x)
SDG10_4   <- func_OR_vector(c(SDG10_4_x, SDG10_4_y))


SDG10_5 <- func_AND_plus(SDG10_5)
SDG10_6 <- func_AND_plus(SDG10_6)

SDG10_7_x <- func_AND_plus(SDG10_7_x)
SDG10_7   <- func_OR_vector(c(SDG10_7_x, SDG10_7_y))

SDG10_a <- func_AND_plus(SDG10_a)

SDG10_b_x <- func_AND_plus(SDG10_b_x)
SDG10_b   <- func_OR_vector(c(SDG10_b_x, SDG10_b_y))

SDG10_c <- func_AND_plus(SDG10_c)
SDG10_general <- func_AND_plus(SDG10_general)
# 
# 
# 
# 
## -11. Sustainable Cities and Communities --------------------------------------------------------



SDG11_1 = c("access|availab|afford",
            "housing|'basic service'|slum|Shanty|settlement|living")
SDG11_2 = c("access|availab|afford|improv",
            "transport|road|congestion|shipment|transit|bus|shuttle")
SDG11_3 = c(urban_ls,
            "inclusi|sustainab|smart|resilien|plan|manage|participat")

SDG11_4 = c("protect|safeguard|preserv|conserv|expenditure|fund|invest|assure|retain",
            "heritage")


SDG11_5 = c(reduce_ls,
            paste(death_ls, "loss|missing|affect|damag|disrupt", sep = '|'),
            disaster_ls)

SDG11_6 = c("contaminat|environment|air|smog|haze|pm2.5|pm10|pm 2.5|pm 10|particulate matter|waste|water|rubbish|garbage|junk|debris|spill|trash|litter|sewage|sludge|soil|footprint|pollution|pollutant|recycl|circular|toxin|toxic",
            urban_ls)

SDG11_7 = c("safe|secure|inclusi|accessible|available|reachable|green|public|natur",
            "space")

SDG11_a_x = c("link|balanc|bind|bridg|connect|network", 
              "urban|rural")
SDG11_a_y = c(paste("plan|develop", policy_ls, sep = '|'),
              "region|territorial")

SDG11_b = c(paste(urban_ls, "local government|Strategy", sep = "|"),
            paste("inclusi|efficien|climate", disaster_ls, sep = "|"))

SDG11_c = c(support_ls,
            "infrastructure|building|hous|architecture|construction|concrete|cement",
            developing_country_ls)

####
SDG11_1 <- func_AND_plus(SDG11_1)
SDG11_2 <- func_AND_plus(SDG11_2)
SDG11_3 <- func_AND_plus(SDG11_3)
SDG11_4 <- func_AND_plus(SDG11_4)
SDG11_5 <- func_AND_plus(SDG11_5)
SDG11_6 <- func_AND_plus(SDG11_6)
SDG11_7 <- func_AND_plus(SDG11_7)

SDG11_a_x <- func_AND_plus(SDG11_a_x)
SDG11_a_y <- func_AND_plus(SDG11_a_y)
SDG11_a   <- func_OR_vector(c(SDG11_a_x, SDG11_a_y))

SDG11_b <- func_AND_plus(SDG11_b)
SDG11_c <- func_AND_plus(SDG11_c)
# 
# 
# 
# 
## -12. Responsible Consumption and Production ---------------------------------------------------

SDG12_1 = c("sustainab|green|clean",
            "consum|produc|manufactur|^us")

SDG12_2_x = c("sustainab", 
              "resource|manag|^us")
SDG12_2_y = c("material", 
              "footprint|flow|consum")
SDG12_2_z = "virtual water|remanufactur"


SDG12_3 = c("reduc|halve|half",
            "food|harvest|'supply chain'",
            "waste|loss")

SDG12_4 = c(paste("manag|minim", reduce_ls, sep = '|'),
            "chemical|contaminat|waste|pesticide|pollut|hazard|toxin|toxic|release to|human health|environment")

SDG12_5_x = c("recycl|prevent|reduc|reus|circular|conserv",
              "waste|material|resource")
SDG12_5_y = "cradle to cradle|Life cycle|Circular economy"


SDG12_6_x = c("sustainab|green",
              "practic|action|report|information|informing|\\binform\\b")
SDG12_6_y = c("soci",
              "responsib")


SDG12_7 = c("sustainab|green",
            "procure|obtain|harvest|collect|gather|consum|label",
            paste(policy_ls, "implemen", sep = '|'))


SDG12_8 = c("sustainab|green",
            "inform|aware|educat|curricul|outreach|engag|community")

SDG12_a = c("sustainab|green|clean",
            "consum|produc|manufactur|^us",
            "scientific|science|technolog|renewable",
            developing_country_ls)

SDG12_b = c("sustainab|green",
            "touris",
            "monitor|account|track|regulat")

SDG12_c = c("fossil|coal|petrol",
            "subsid|expenditure|expens")



####
SDG12_1 <- func_AND_plus(SDG12_1)

SDG12_2_x <- func_AND_plus(SDG12_2_x)
SDG12_2_y <- func_AND_plus(SDG12_2_y)
SDG12_2   <- func_OR_vector(c(SDG12_2_x, SDG12_2_y, SDG12_2_z))

SDG12_3 <- func_AND_plus(SDG12_3)
SDG12_4 <- func_AND_plus(SDG12_4)

SDG12_5_x <- func_AND_plus(SDG12_5_x)
SDG12_5   <- func_OR_vector(c(SDG12_5_x, SDG12_5_y))

SDG12_6_x <- func_AND_plus(SDG12_6_x)
SDG12_6_y <- func_AND_plus(SDG12_6_y)
SDG12_6   <- func_OR_vector(c(SDG12_6_x, SDG12_6_y))

SDG12_7 <- func_AND_plus(SDG12_7)
SDG12_8 <- func_AND_plus(SDG12_8)
SDG12_a <- func_AND_plus(SDG12_a)
SDG12_b <- func_AND_plus(SDG12_b)
SDG12_c <- func_AND_plus(SDG12_c)





## -13. Climate Action ---------------------------------------------------------------------------

climate_ls = paste("climat|warming|'extreme weather'|temperature|heat|melt|SDG 13|goal 13|target 13|indicator 13", 
                   func_AND_plus(c("sea", "level", "ris")), 
                   sep = "|")


####
SDG13_1 = c(paste(climate_ls, 'kyoto protocol', sep = "|"), 
            disaster_ls,
            paste("resilien|adapt|adjust", reduce_ls, sep = '|'))


SDG13_2_x = c(paste(climate_ls, 'kyoto protocol', emission_ls, sep = '|'),
              paste("adapt|captur|contribut|convention", reduce_ls, policy_ls, sep = '|')) 
SDG13_2_y = renewable_ls


SDG13_3 = c(paste(climate_ls, 'kyoto protocol', emission_ls, sep = "|"),
            paste("adapt|captur|contribut|convention", reduce_ls, policy_ls, sep = '|'),
            "educat|aware|engag|outreach|communicat|cultivat|inform|train|tutor|instruct")


SDG13_a_x = c(paste(climate_ls, 'kyoto protocol|convention|mitigat', sep = '|'),
              paste(support_ls, "dollar|mobili|implement|operat", sep = '|'))
SDG13_a_y = "United Nations Framework Convention on Climate Change|unfccc|U.N.F.C.C.C.|Green Climate Fund|GCF|G.C.F."


SDG13_b = c(paste(climate_ls, 'kyoto protocol', sep = '|'),
            paste(policy_ls, "capacity|scope", sep = '|'),
            developing_country_ls)


SDG13_general = paste(climate_ls, 'kyoto protocol', emission_ls, sep = '|')


####
SDG13_1 <- func_AND_plus(SDG13_1)

SDG13_2_x <- func_AND_plus(SDG13_2_x)
SDG13_2   <- func_OR_vector(c(SDG13_2_x, SDG13_2_y))

SDG13_3 <- func_AND_plus(SDG13_3)

SDG13_a_x <- func_AND_plus(SDG13_a_x)
SDG13_a   <- func_OR_vector(c(SDG13_a_x, SDG13_a_y))

SDG13_b   <- func_AND_plus(SDG13_b)

SDG13_general <- SDG13_general
 



## -14. Life Below Water -------------------------------------------------------------------------


ocean_ls = "marine|maritime|ocean|sea|coast|tidal|aquatic|coral"


SDG14_1 = c(ocean_ls,
            "pollut|nutrient|eutroph|Kelp|alga|plastic|debris|runoff|chemical|fertiliz|waste")

SDG14_2 = c(paste(ocean_ls, 'coral', sep = "|"),
            "sustainab|resilien|restor|manag|mitigat|health|productiv|habitat|bleach") 

SDG14_3 = c(ocean_ls,
            "acidi|pH")

SDG14_4_x = c("fish|seafood|catch|harvest",
              "illegal|unreported|underreport|unregulated|destruct|destroy|diminish|exploit|sustainab|restor|conserv|manag|mitig|regulat|monitor|restrict|enforc|prohibit|\\bquota\\b|monitor")
SDG14_4_y = "overfish|maximum sustainable yield|MSY"

SDG14_5 = c(ocean_ls,
            "restor|conserv|protect|preserv|reduc|plan|mitig|restrict|enforc|monitor|prohibit|report")

SDG14_6_x = c("fish|seafood|catch|harvest",
              "subsid")
SDG14_6_y = c("fish|seafood|catch|harvest",
              "illegal|unreported|unregulated|destructive|destroy|diminish|exploit|poach|traffick",
              paste("instrument|law|enforc|restrict|prohibit|quota|regulat|monitor|prohibit", policy_ls, sep = '|'))

SDG14_7 = c("econom|benefi|sustain",
            paste(func_AND_plus(c(ocean_ls,"resource|touris")),
                  "fish|aquacultur|seafood|catch", sep = '|'),
            developing_country_ls)


SDG14_a_x = c(ocean_ls,
              "scient|^knowledge|research|technolog|budget|health")

SDG14_a_y = c("scient|^knowledge|research|technolog|budget|health",
              paste(func_AND_plus(c(ocean_ls, "mammal|species|plant|animal|bird|biodivers")),
                    "fish|cetacean|whal|invert", sep = '|'))
SDG14_a_z = "Intergovernmental Oceanographic Commission|Guidelines on the Transfer of Marine Technology"


SDG14_b = c(paste("artisan|tradition", func_AND_plus(c("small","scale")), sep = '|'),
            "fish|seafood|catch|harvest",
            "resource|market|law|legal|polic|institutional|regulat|jurisdiction")

SDG14_c = c(paste(ocean_ls, "resource", sep = '|'),
            "restor|conserv|protect|sustain|manag|mitig|monitor",
            "law|legal|polic|institutional|regulat|jurisdiction|instrument")

####
SDG14_1 <- func_AND_plus(SDG14_1)
SDG14_2 <- func_AND_plus(SDG14_2)
SDG14_3 <- func_AND_plus(SDG14_3)

SDG14_4_x <- func_AND_plus(SDG14_4_x)
SDG14_4   <- func_OR_vector(c(SDG14_4_x, SDG14_4_y))

SDG14_5 <- func_AND_plus(SDG14_5)

SDG14_6_x <- func_AND_plus(SDG14_6_x)
SDG14_6_y <- func_AND_plus(SDG14_6_y)
SDG14_6   <- func_OR_vector(c(SDG14_6_x, SDG14_6_y))

SDG14_7 <- func_AND_plus(SDG14_7)

SDG14_a_x <- func_AND_plus(SDG14_a_x)
SDG14_a_y <- func_AND_plus(SDG14_a_y)
SDG14_a   <- func_OR_vector(c(SDG14_a_x, SDG14_a_y, SDG14_a_z))

SDG14_b <- func_AND_plus(SDG14_b)
SDG14_c <- func_AND_plus(SDG14_c)












## 15. Life On Land --------------------------------------------------------------------------------

ecosystem_ls = "ecosystem|eco-system|ecolog|environment|natur|environs"


SDG15_1 = paste(func_AND_plus(c("terrestrial|land|inland|freshwater|forest|wetland|mountain|dryland|biodivers|wildlife|wild animal|wild species|protected|reserve",
                                "conserv|restor|mainten|protect|preserv|safeguard|secur|sustain")),
                func_AND_plus(c(ecosystem_ls, 
                                "service|good|product|suppl|output|benefit|contribut",
                                "conserv|restor|mainten|protect|preserv|safeguard|secur|sustain")), 
                sep = "|")

SDG15_2 = c("forest|rainforest|woodland|tree|deforest|afforest|reforest|REDD|R.E.D.D.|Reducing Emissions from Deforestation and forest Degradation|silviculture|timber|soil",
            "conserv|restor|sustain|manag|mitig|health|resilien|stewardship|certifi|audit|account")

SDG15_3 = c("degrad|desertif|contamin|pollut|denitrificat|de-nitrificat|health|nitro|drought|flood", 
            "land|soil|sediment", 
            paste("combat|halt|conserv|restor|sustain|manag", reduce_ls, sep = "|"))

SDG15_4 = c("mountain",
            paste(ecosystem_ls, "biodiver|green|vegetation", sep = '|'))

SDG15_5 = "habitat|biodivers|Red List|threaten|endanger|extinct|diversity|richness|vulnerab|extirpate"

SDG15_6 = c("genetic resource|genetic|\\bgene\\b",
            "fair|equit|access|availab|equal|shar")

SDG15_7 = c("poach|traffic|illicit|illegal",
            "species|flora|fauna|wildlife|logg|animal|mammal|hunt|cultivat|catch")


SDG15_8 = paste("invasive|invasion|alien|invade", func_AND_plus(c("non","native")), sep = '|')


SDG15_9 = c(paste(ecosystem_ls, "biodiver", sep = '|'),
            "plan|develop|povert|strateg|account|reporting|Aichi|action|target")


SDG15_a = c(paste(ecosystem_ls, "biodiver", sep = '|'),
            paste(support_ls, "revenue|value|incentiv", sep = '|'),
            "conserv|preserv|manag|sustain")

SDG15_b = c("forest",
            paste(support_ls, "conserv|preserv|manag|mitig|sustain|revenue|value|incentiv", sep = '|'))

SDG15_c = c("poach|traffic|illicit|illegal",
            "species|flora|fauna|wildlife|logg|animal|mammal|hunt|cultivat|catch",
            "global support|local|communit")



SDG15_general_x = paste(func_AND_plus(c(ecosystem_ls, "resilien")),
                        func_AND_plus(c('human', 'wildlife', 'conflict')), 
                        func_AND_plus(c("community", "conserv")), 
                        func_AND_plus(c("eco", "touris")), 
                        sep = "|")
SDG15_general_y = c(ecosystem_ls,
                    "impact|conserv|restor|sustaina|manag|mitig|health|well|resilien|stewardship|audit|account")

####

SDG15_1 <- SDG15_1
SDG15_2 <- func_AND_plus(SDG15_2)
SDG15_3 <- func_AND_plus(SDG15_3)
SDG15_4 <- func_AND_plus(SDG15_4)
SDG15_5 <- SDG15_5
SDG15_6 <- func_AND_plus(SDG15_6)
SDG15_7 <- func_AND_plus(SDG15_7)
SDG15_8 <- SDG15_8
SDG15_9 <- func_AND_plus(SDG15_9)
SDG15_a <- func_AND_plus(SDG15_a)
SDG15_b <- func_AND_plus(SDG15_b)
SDG15_c <- func_AND_plus(SDG15_c)


SDG15_general_y <- func_AND_plus(SDG15_general_y)
SDG15_general   <- func_OR_vector(c(SDG15_general_x, SDG15_general_y))




## -16. Peace, Justice, and Strong Institutions ----------------------------------------------------


SDG16_1_x = c(paste(death_ls, 
                    "violen|victim|homicid|murder|kill|assault|assassination", sep = "|"),
              reduce_ls)
SDG16_1_y = c("verbal|physical|domestic|psychological|child|sex", 
              "abuse|violen|assault|torture",
              reduce_ls) 
SDG16_1_z = "safe walking|peaceful society"
  

SDG16_2_x = c("abuse|exploitation|trafficking",
              reduce_ls)              
SDG16_2_y = c(child_ls,
              "abus|exploit|traffic|victim|violen|tortur|punish|aggress|^rap|^porn",
              reduce_ls)


SDG16_3_x = c("access|availabl", 
              "code|norm|order|justice|authorit|rul|law|legal|legislation|litigation|resolution|regulation")
SDG16_3_y = "victim|violence|detain|prison|inmate|disput|conflict resolution|actual innocence|false confession|sentenced|Arbitrary detention|Enforced disappearance"


SDG16_4_x = c("illicit|illegal|illegitimate|banned|criminal|irregular|prohib|smuggl|unauthorized|unlawful|unconstitutional|unlicensed|unlicenced|unwarranted|stolen|steal|theft|organiz|seiz|found|surrender|trace|tracing|track|conflict|traffic",
              paste(support_ls, "^arm|gun|asset|money|transfer|flow", sep = '|'))
SDG16_4_y = "criminal|crime|cybercrime"


SDG16_5 = paste("corrupt|brib|Tax evasion", 
                func_AND_plus(c("contact|asked|pay|paid|extort", "public official")), sep = "|")

SDG16_6_x = c("institut|government|legislature|judiciary|authority|ministry|public service",
              "effective|accountab|transparent|satisf")
SDG16_6_y = paste("approved budget|judicial system|criminal tribunal",
                  func_AND_plus(c("peace|inclusive|fair", "societ")), sep = '|')


SDG16_7_x = c(paste(func_AND_plus(c("decision", "mak")), 
                    "legislature|institution|public service|judiciary", sep = '|'), 
              "respon|inclu|participatory|represent|independen")
SDG16_7_y = paste(func_AND_plus(c('separat', "power")), 
                  "democracy", sep = "|")


SDG16_8 = c("institution|govern|vote|voting|suffrag",
            developing_country_ls)


SDG16_9 = paste(func_AND_plus(c("legal", "identity")),
                func_AND_plus(c("birth", "regist|certifi")), sep = "|")

SDG16_10_x = c("access|availab|open|information|freedom|kill|kidnap|enforc|disappear|deten|detain|tortur",
               "journalis|media|press|unionis|human rights advocate|public")
SDG16_10_y = 'Aarhus Convention'


SDG16_a_x = c(paste("violen|terroris|^crime|criminal|genocid|murder|human traffick|refugee|extremis|insurgen|^war|warfare",
                    func_AND_plus(c("^arm|^gun|weapon", "conflict")), sep = "|"), 
              reduce_ls) 
SDG16_a_y = c(paste("institution|cooperat|collaborat|joint effort|partnership|independen|human right|democracy|treaty|^civil",
                    func_AND_plus(c("^peace", "keep")), sep = "|"),
              increase_ls)
SDG16_a_z = "building capacity|Paris Principle*|Geneva Convention"


SDG16_b = c("^law|polic|right|against|protect",
            "discriminat|harass")


SDG16_general = paste("ethnic conflict|exonerat|justice system",
                      func_AND_plus(c("environment", "^law|govern")), sep = "|")

####
SDG16_1_x <- func_AND_plus(SDG16_1_x)
SDG16_1_y <- func_AND_plus(SDG16_1_y)
SDG16_1   <- func_OR_vector(c(SDG16_1_x, SDG16_1_y, SDG16_1_z))

SDG16_2_x <- func_AND_plus(SDG16_2_x)
SDG16_2_y <- func_AND_plus(SDG16_2_y)
SDG16_2   <- func_OR_vector(c(SDG16_2_x, SDG16_2_y))

SDG16_3_x <- func_AND_plus(SDG16_3_x)
SDG16_3   <- func_OR_vector(c(SDG16_3_x, SDG16_3_y))

SDG16_4_x <- func_AND_plus(SDG16_4_x)
SDG16_4   <- func_OR_vector(c(SDG16_4_x, SDG16_4_y))

SDG16_5 <- SDG16_5

SDG16_6_x <- func_AND_plus(SDG16_6_x)
SDG16_6   <- func_OR_vector(c(SDG16_6_x, SDG16_6_y))

SDG16_7_x <- func_AND_plus(SDG16_7_x)
SDG16_7   <- func_OR_vector(c(SDG16_7_x, SDG16_7_y))

SDG16_8 <- func_AND_plus(SDG16_8)

SDG16_9 <- SDG16_9

SDG16_10_x <- func_AND_plus(SDG16_10_x)
SDG16_10   <- func_OR_vector(c(SDG16_10_x, SDG16_10_y))

SDG16_a_x <- func_AND_plus(SDG16_a_x)
SDG16_a_y <- func_AND_plus(SDG16_a_y)
SDG16_a   <- func_OR_vector(c(SDG16_a_x, SDG16_a_y, SDG16_a_z))

SDG16_b <- func_AND_plus(SDG16_b)

SDG16_general <- SDG16_general







                         
## -17. Partnerships for the Goals -----------------------------------------------------------------


### Finance

SDG17_1 = c("domestic|government",
            "resource|capacity|revenue|budget|tax")

SDG17_2 = paste(func_AND_plus(c("development", "assist|aid")), "ODA", sep = "|")

temp <- SDG17_2


SDG17_3_x = c(paste(func_AND_plus(c("financ", "resource")), 
                    temp, sep = "|"), 
              developing_country_ls)
SDG17_3_y = "foreign direct invest'|FDI|'South-South cooperation'|remittance"


SDG17_4 = c('debt',
            "financing|relief|restructuring|sustainab|service|distress",
            developing_country_ls)

SDG17_5 = c("invest|fund|financ",
            developing_country_ls)



### Technology

SDG17_6 = c("access|availab|facilit",
            "science|^tech|innovation|knowledge-sharing|Internet|broadband",
            paste("North-South|South-South|triangular region", developing_country_ls, sep = "|"))

SDG17_7 = c("environment",
            "technolog",
            "development|transfer|dissemination|diffusion",
            developing_country_ls)

SDG17_8 = c("technology|science|innovation|information|communication|internet",
            "capacity",
            developing_country_ls)

### Capacity-building
SDG17_9 = c("capacity building|financ|technical assistance|international support",
            paste("North-South|South-South|triangular cooperation", developing_country_ls, sep = "|"))


### Trade
SDG17_10_x = c("trad|^WTO",
               "universal|rules-based|open|non‑discriminatory|equitable|equal")
SDG17_10_y = "Doha Development Agenda|Weighted tariff average"


SDG17_11 = c("export",
             developing_country_ls)


SDG17_12 = c(paste(func_AND_plus(c("duty|quota", "free")), 
                   "market access", sep = "|"),
             developing_country_ls)

### Systemic issues
SDG17_13 = c("macroeconomic",
             paste("stability", func_AND_plus(c("polic", "coordination|coherence")), sep = "|"))


SDG17_14 = c(policy_ls,
             "coheren",
             "sustainab", 
             "develop")


SDG17_15 = c("respect|leadership|country-owned",
             "polic|framework|plan")


SDG17_16 = paste("multi-stakeholder|partner|cooperat|collaborat|joint effort",
                 func_AND_plus(c("^shar|mobiliz|assembl|marshal", 
                                 paste("knowledge|expertise|skill|^tech|automation|capital|currency|asset|resource", 
                                       support_ls, sep = '|'))), 
                 func_AND_plus(c("stakeholder", "sustainable|^SDG")),
                 sep = "|")

SDG17_17 = c("partnership", 
             "public|private|civil|societ|government")


SDG17_18 = c("data|statistic",
             "high-quality|timely|reliable|availab",
             developing_country_ls)


SDG17_19 = paste(func_AND_plus(c("measur", 
                                 paste(func_AND_plus(c("sustain", "develop")), "SDG", sep = "|"))), 
                 func_AND_plus(c("statistic", "capacity")), 
                 func_AND_plus(c("census", "population|housing")),
                 func_AND_plus(c("regist", "birth|death")), 
                 sep = "|")


SDG17_general = c("sustainab|global",
                  "partner|cooperat|collaborat|joint effort|coordinate|stability")


####
SDG17_1 <- func_AND_plus(SDG17_1)
SDG17_2 <- SDG17_2
SDG17_3_x <- func_AND_plus(SDG17_3_x)
SDG17_3   <- func_OR_vector(c(SDG17_3_x, SDG17_3_y))
SDG17_4 <- func_AND_plus(SDG17_4)
SDG17_5 <- func_AND_plus(SDG17_5)
SDG17_6 <- func_AND_plus(SDG17_6)
SDG17_7 <- func_AND_plus(SDG17_7)
SDG17_8 <- func_AND_plus(SDG17_8)
SDG17_9 <- func_AND_plus(SDG17_9)
SDG17_10_x <- func_AND_plus(SDG17_10_x)
SDG17_10   <- func_OR_vector(c(SDG17_10_x, SDG17_10_y))
SDG17_11 <- func_AND_plus(SDG17_11)
SDG17_12 <- func_AND_plus(SDG17_12)
SDG17_13 <- func_AND_plus(SDG17_13)
SDG17_14 <- func_AND_plus(SDG17_14)
SDG17_15 <- func_AND_plus(SDG17_15)
SDG17_16 <- SDG17_16
SDG17_17 <- func_AND_plus(SDG17_17)
SDG17_18 <- func_AND_plus(SDG17_18)
SDG17_19 <- SDG17_19
SDG17_general <- func_AND_plus(SDG17_general)







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


###   OR 
goal_ls_x <- paste0(
  '(sdg|goal)',
  # '.{0,2}',     ## `.` matches any character (e.g., a space or `s`); `{0,2}` matches the previous token between 0 and 2 times
  '[^0-9]{0,2}',  ## the above one can be problematic if `.` can be a number. Here we change it to match only non-numeric character
  '(?=',        ## Positive lookahead: e.g., in "SDG(?=17)", "17" immediately follows the "SDG".
  seq(1, 17), 
  '\\b',        ## \b assert position at a word boundary
  ')'
)
goal_ls_y <- ls_un %>% distinct(GoalName)
goal_ls_y <- goal_ls_y$GoalName
goal_keys <- data.frame(SDG_id = paste0('SDG', seq(1, 17), '_general'), 
                        goal_ls_x, goal_ls_y) %>%
  dplyr::mutate(SDG_keywords = paste(goal_ls_x, goal_ls_y, sep = "|")) %>%
  dplyr::select(SDG_id, SDG_keywords)



## Target level ------------------------------------------------------------------------------------
targ_df <- ls_un %>% distinct(target_id_un) 
targ_ids <- targ_df$target_id_un; targ_ids
targ_ids_ <- gsub('\\.', '\\\\.', targ_ids); targ_ids_ ## '.' can be matched wiht any character 

target_ls <- paste0(
  '(sdg|goal|target|indicator)',
  # '.{0,2}',         ## 
  '[^0-9]{0,2}',
  '(?=', targ_ids_,  ## 
  '[\\.]{0,1}',     ## there might be 0 or 1 period follows, e.g., if targ_ids = 17.1, then this can match '17.1' and '17.1.1' but not '17.11'
  ')'               ## 
)

target_keys <- data.frame(SDG_id = paste0('SDG', targ_ids), 
                          SDG_keywords = target_ls)%>%
  dplyr::mutate(SDG_id = gsub('\\.', '_', SDG_id))










## - Test ------------------------------------------------------------------------------------------
# pat <- '(sdg|goal|target|indicator)[^0-9]{0,2}(?=17[\\.]{0,1})|No Poverty'
# pat <- '(sdg|goal|target|indicator)[^0-9]{0,2}(?=7\\.a[\\.]{0,1})|No Poverty'
# 
# test <- data.frame(term = c(
#   'i love SDGs and sdg 17 and sdg 17 and goal 17',
#   'i love SDGs and sdg 17.1 and and goal 17.1, indicator17.12， targe17.1.1, indicator 17.1.2', 
#   'I like sdg17 and you',
#   'I like sdg-17 and you',
#   'I like sdgs 17 and you',
#   'I like goals 7 and you',
#   'I like goals 7.a and you',
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
# test


### ref:  https://stackoverflow.com/questions/41802272/understanding-lookahead-in-r-regexp
###       https://tpristavec.github.io/regex/#26
###       https://users.cs.cf.ac.uk/Dave.Marshall/PERL/node79.html
