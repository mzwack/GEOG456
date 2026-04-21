# ============================================================
# PWHL GIS Map - Data Export Script
# Working directory: ~/Desktop/GEOG456/Final_project
# All player data hardcoded from QuantHockey 2025-26 season
# fastRhockey used ONLY for standings (with hardcoded fallback)
# ============================================================

library(fastRhockey)
library(jsonlite)

# ================================================================
# STANDINGS  — fetched from fastRhockey with hardcoded fallback
# ================================================================
message("Fetching standings...")
standings_raw <- tryCatch(pwhl_standings(season=2026), error=function(e){ message("  ",e$message); NULL })

# Hardcoded fallback standings (as of ~Apr 17 2026, update if needed)
STANDINGS <- list(
  BOS = list(wins=19, losses=7,  otl=1),
  MIN = list(wins=16, losses=9,  otl=3),
  MTL = list(wins=19, losses=7,  otl=2),
  NYR = list(wins=10, losses=15, otl=2),
  OTT = list(wins=12, losses=13, otl=3),
  TOR = list(wins=13, losses=7,  otl=0),
  SEA = list(wins=7,  losses=17, otl=3),
  VAN = list(wins=7,  losses=15, otl=4)
)

# Try to update from API if available
if (!is.null(standings_raw) && nrow(standings_raw) > 0) {
  name_map <- c(Boston="BOS", Minnesota="MIN", Montreal="MTL",
                "New York"="NYR", Ottawa="OTT", Toronto="TOR",
                Seattle="SEA", Vancouver="VAN")
  for (i in seq_len(nrow(standings_raw))) {
    r <- standings_raw[i,]
    tm_name <- trimws(as.character(r$team))
    tm <- unname(name_map[tm_name])
    if (!is.na(tm) && tm %in% names(STANDINGS)) {
      STANDINGS[[tm]] <- list(
        wins   = suppressWarnings(as.integer(r$wins)),
        losses = suppressWarnings(as.integer(r$losses)),
        otl    = suppressWarnings(as.integer(r$ot_losses))
      )
    }
  }
  message("  Standings updated from API")
}

# ================================================================
# COACHES
# ================================================================
COACHES <- list(
  BOS = list(gm="Danielle Marmer", head="Kris Sparre",    assistants=c("Jordan LaVallée-Smotherman","Stefanie McKeough")),
  MIN = list(gm="Melissa Caruso",  head="Ken Klee",        assistants=c("Allie Thunstrom","Samantha Holmes")),
  MTL = list(gm="Danièle Sauvageau", head="Kori Cheverie", assistants=c("Caroline Ouellette","Emmanuelle Blais")),
  NYR = list(gm="Pascal Daoust",   head="Greg Fargo",      assistants=c("Josh Sciba","Valerie Bois")),
  OTT = list(gm="Mike Hirshfeld",  head="Carla MacLeod",   assistants=c("Haley Irwin (Acting HC)","Meghann Treacy")),
  TOR = list(gm="Gina Kingsbury",  head="Troy Ryan",       assistants=c("Sami Jo Small","Vicky Sunohara")),
  SEA = list(gm="Meghan Turner",   head="Steve O'Rourke",  assistants=c("Christine Bumstead","Clayton Beddoes")),
  VAN = list(gm="Cara Gardner Morey", head="Brian Idalski",assistants=c("B.J. Adams","Myles Fitzgerald"))
)

# ================================================================
# HARDCODED ROSTERS
# Source: QuantHockey.com 2025-26 season
# Fields: name, jersey, pos, dob, hometown, country, province_state,
#         goals, games_played, is_goalie, save_pct
# ================================================================

P <- function(name, jersey, pos, dob, hometown, country, province_state,
              goals=NA, gp=NA, is_goalie=FALSE, save_pct=NA) {
  list(name=name, jersey=jersey, position=pos, dob=dob,
       hometown=hometown, country=country, province_state=province_state,
       goals=goals, games_played=gp, is_goalie=is_goalie, save_pct=save_pct)
}

ROSTERS <- list(
  
  # ── BOSTON FLEET ─────────────────────────────────────────────
  BOS = list(
    P("Aerin Frankel",     31,"G","1999-05-24","New York City, New York, USA",  "USA","New York",     NA,24,TRUE, 0.953),
    P("Abbey Levy",        39,"G","2000-04-02","Congers, New York, USA",        "USA","New York",     NA, 1,TRUE, 0.875),
    P("Amanda Thiele",     30,"G","2002-01-01","Madison, Wisconsin, USA",       "USA","Wisconsin",    NA, 1,TRUE, 0.889),
    P("Megan Keller",       5,"D","1996-05-01","Farmington, Michigan, USA",     "USA","Michigan",      5,16,FALSE,NA),
    P("Haley Winn",         8,"D","2003-07-14","Rochester, New York, USA",      "USA","New York",      1,16,FALSE,NA),
    P("Hadley Hartmetz",    6,"D","2001-03-25","Phoenixville, Pennsylvania, USA","USA","Pennsylvania",  0,13,FALSE,NA),
    P("Riley Brengman",    16,"D","2002-07-06","China Township, Michigan, USA", "USA","Michigan",      2,14,FALSE,NA),
    P("Daniela Pejšová",   55,"D","2002-08-14","Teplice, Czech Republic",       "CZE","",              1,14,FALSE,NA),
    P("Zoe Boyd",           3,"D","2000-08-09","Caledon East, Ontario, Canada", "CAN","Ontario",       0,12,FALSE,NA),
    P("Rylind MacKinnon",  53,"D","2000-03-05","Cranbrook, British Columbia, Canada","CAN","British Columbia",0,14,FALSE,NA),
    P("Mia Biotti",         7,"D","2002-07-24","Cambridge, Massachusetts, USA", "USA","Massachusetts",  0, 5,FALSE,NA),
    P("Olivia Zafuto",     73,"F","1997-01-25","Niagara Falls, New York, USA",  "USA","New York",       0, 3,FALSE,NA),
    P("Noemi Neubauerová", 81,"D","2003-01-01","Prague, Czech Republic",        "CZE","",              0, 2,FALSE,NA),
    P("Susanna Tapani",    77,"F","1993-03-02","Laitila, Finland",               "FIN","",              4,16,FALSE,NA),
    P("Alina Müller",      11,"F","1998-03-12","Winterthur, Switzerland",        "CHE","",              3,16,FALSE,NA),
    P("Jamie Lee Rattray", 47,"F","1992-09-30","Kanata, Ontario, Canada",       "CAN","Ontario",       3,16,FALSE,NA),
    P("Abby Newhook",      19,"F","2003-05-13","St. John's, Newfoundland and Labrador, Canada","CAN","Newfoundland and Labrador",5,15,FALSE,NA),
    P("Ella Huber",        26,"F","2002-12-18","Northfield, Illinois, USA",     "USA","Illinois",      2,16,FALSE,NA),
    P("Jill Saulnier",     44,"F","1992-03-07","Halifax, Nova Scotia, Canada",  "CAN","Nova Scotia",   1,16,FALSE,NA),
    P("Liz Schepers",      13,"F","1999-02-13","Mound, Minnesota, USA",         "USA","Minnesota",     2,15,FALSE,NA),
    P("Shay Maloney",      27,"F","1999-08-31","McHenry, Illinois, USA",        "USA","Illinois",      2,16,FALSE,NA),
    P("Sophie Shirley",     9,"F","1999-06-30","Saskatoon, Saskatchewan, Canada","CAN","Saskatchewan",  0,12,FALSE,NA),
    P("Hannah Brandt",     20,"F","1993-11-27","Vadnais Heights, Minnesota, USA","USA","Minnesota",     0,16,FALSE,NA),
    P("Laura Kluge",       25,"F","1996-11-06","Berlin, Germany",                "DEU","",              0,15,FALSE,NA),
    P("Olivia Mobley",     15,"F","2001-10-28","St. Louis Park, Minnesota, USA","USA","Minnesota",     3,10,FALSE,NA),
    P("Loren Gabel",       36,"F","1997-07-24","Kitchener, Ontario, Canada",    "CAN","Ontario",       0, 2,FALSE,NA)
  ),
  
  # ── MINNESOTA FROST ──────────────────────────────────────────
  MIN = list(
    P("Marlène Boissonnault", 1,"G","1998-05-12","Sherbrooke, Quebec, Canada",  "CAN","Quebec",        NA, 4,TRUE, 0.912),
    P("Nicole Hensley",      29,"G","1994-06-23","Littleton, Colorado, USA",    "USA","Colorado",      NA,12,TRUE, 0.915),
    P("Maddie Rooney",       35,"G","1997-07-07","Duluth, Minnesota, USA",      "USA","Minnesota",     NA,15,TRUE, 0.922),
    P("Lee Stecklein",        2,"D","1994-04-23","Roseville, Minnesota, USA",   "USA","Minnesota",      0,13,FALSE,NA),
    P("Kendall Cooper",       4,"D","2002-05-19","Oakville, Ontario, Canada",   "CAN","Ontario",        1,15,FALSE,NA),
    P("Sidney Morin",         5,"D","1995-06-06","Minnetonka, Minnesota, USA",  "USA","Minnesota",      0,15,FALSE,NA),
    P("Madison Bizal",        9,"D","2000-01-25","Elk River, Minnesota, USA",   "USA","Minnesota",      0, 6,FALSE,NA),
    P("Brooke Becker",       15,"D","2002-05-30","Orchard Park, New York, USA", "USA","New York",       0,14,FALSE,NA),
    P("Ava Rinker",          17,"D","2003-01-01","Steamboat Springs, Colorado, USA","USA","Colorado",   NA,NA,FALSE,NA),
    P("Mae Batherson",       21,"D","2000-12-05","Garmisch-Partenkirchen, Germany","CAN","Ontario",     2,15,FALSE,NA),
    P("Natalie Buchbinder",  22,"D","1999-01-22","Fairport, New York, USA",     "USA","New York",       0,15,FALSE,NA),
    P("Katy Knoll",           6,"F","2001-01-16","Amherst, New York, USA",      "USA","New York",       6,15,FALSE,NA),
    P("Claire Butorac",       7,"F","1999-09-24","Andover, Minnesota, USA",     "USA","Minnesota",      0,15,FALSE,NA),
    P("Samantha Cogan",       8,"F","1997-07-07","Ottawa, Ontario, Canada",     "CAN","Ontario",        2,23,FALSE,NA),
    P("Kelly Pannek",        12,"F","1995-12-29","Plymouth, Minnesota, USA",    "USA","Minnesota",      8,15,FALSE,NA),
    P("Grace Zumwinkle",     13,"F","1999-04-23","Excelsior, Minnesota, USA",   "USA","Minnesota",      6,15,FALSE,NA),
    P("Dominique Petrie",    14,"F","2001-02-21","Hermosa Beach, California, USA","USA","California",   2,10,FALSE,NA),
    P("Kaitlyn O'Donohoe",   16,"F","2001-09-20","Atlanta, Georgia, USA",       "USA","Georgia",        0, 4,FALSE,NA),
    P("Élizabeth Giguère",   18,"F","1997-05-08","Quebec City, Quebec, Canada", "CAN","Quebec",         3,24,FALSE,NA),
    P("Vanessa Upson",       24,"F","2003-11-18","Stoney Creek, Ontario, Canada","CAN","Ontario",       0,13,FALSE,NA),
    P("Kendall Coyne Schofield",26,"F","1992-05-25","Oak Lawn, Illinois, USA",  "USA","Illinois",      10,15,FALSE,NA),
    P("Taylor Heise",        27,"F","2000-03-17","Lake City, Minnesota, USA",   "USA","Minnesota",      3,15,FALSE,NA),
    P("Klára Hymlárová",     71,"F","1999-02-27","Opava, Czech Republic",       "CZE","",               1,15,FALSE,NA),
    P("Abby Hustler",        74,"F","2003-05-05","St. Louis, Prince Edward Island, Canada","CAN","Prince Edward Island",2,15,FALSE,NA),
    P("Britta Curl-Salemme", 77,"F","2000-03-20","Bismarck, North Dakota, USA", "USA","North Dakota",   7,15,FALSE,NA),
    P("Peyton Anderson",     91,"F","2001-04-18","Arvada, Colorado, USA",       "USA","Colorado",       0,15,FALSE,NA)
  ),
  
  # ── MONTRÉAL VICTOIRE ────────────────────────────────────────
  MTL = list(
    P("Megan Warrener",      1,"G","1997-03-24","Calgary, Alberta, Canada",     "CAN","Alberta",       NA, 2,TRUE, 0.893),
    P("Sandra Abstreiter",  30,"G","1998-07-23","Freising, Germany",             "DEU","",             NA, 3,TRUE, 0.887),
    P("Ann-Renée Desbiens", 35,"G","1994-04-10","La Malbaie, Quebec, Canada",   "CAN","Quebec",        NA,24,TRUE, 0.955),
    P("Kati Tabin",          9,"D","1997-04-21","Winnipeg, Manitoba, Canada",   "CAN","Manitoba",       1,18,FALSE,NA),
    P("Tamara Giaquinto",   12,"D","2002-03-29","Toronto, Ontario, Canada",     "CAN","Ontario",        0, 4,FALSE,NA),
    P("Jessica DiGirolamo", 22,"D","1999-02-13","Mississauga, Ontario, Canada", "CAN","Ontario",        1,18,FALSE,NA),
    P("Erin Ambrose",       23,"D","1994-04-30","Keswick, Ontario, Canada",     "CAN","Ontario",        0,14,FALSE,NA),
    P("Amanda Boulier",     44,"D","1993-03-30","Watertown, Connecticut, USA",  "USA","Connecticut",    0,18,FALSE,NA),
    P("Kelly Ann Nadeau",   51,"D","1998-03-30","Mont-Laurier, Quebec, Canada", "CAN","Quebec",         0, 6,FALSE,NA),
    P("Nicole Gosling",     61,"D","2002-04-21","London, Ontario, Canada",      "CAN","Ontario",        1,18,FALSE,NA),
    P("Maggie Flaherty",    91,"D","2000-06-02","Lakeville, Minnesota, USA",    "USA","Minnesota",      3,18,FALSE,NA),
    P("Nadia Mattivi",      93,"D","2001-01-01","Baselga di Pinè, Italy",        "ITA","",              0, 5,FALSE,NA),
    P("Maya Labad",          4,"F","2002-05-07","Mascouche, Quebec, Canada",    "CAN","Quebec",         1, 4,FALSE,NA),
    P("Samantha Isbell",     5,"F","1998-02-17","Thunder Bay, Ontario, Canada", "CAN","Ontario",        0, 8,FALSE,NA),
    P("Laura Stacey",        7,"F","1994-11-12","Kleinburg, Ontario, Canada",   "CAN","Ontario",        3,18,FALSE,NA),
    P("Abby Roque",         11,"F","1997-09-25","Sault Ste. Marie, Michigan, USA","USA","Michigan",      5,17,FALSE,NA),
    P("Alexandra Labelle",  13,"F","1996-02-27","Saint-Louis-de-Gonzague, Quebec, Canada","CAN","Quebec",0,16,FALSE,NA),
    P("Claire Vekich",      15,"F","2002-01-01","Coleraine, Minnesota, USA",     "USA","Minnesota",     NA,NA,FALSE,NA),
    P("Hayley Scamurra",    16,"F","1994-09-28","Williamsville, New York, USA", "USA","New York",       2,18,FALSE,NA),
    P("Dara Greig",         17,"F","2000-12-30","Camden, New Jersey, USA",      "CAN","Ontario",        3,18,FALSE,NA),
    P("Kaitlin Willoughby", 19,"F","1995-03-26","Saskatoon, Saskatchewan, Canada","CAN","Saskatchewan", 0,18,FALSE,NA),
    P("Maureen Murphy",     21,"F","1999-12-15","Buffalo, New York, USA",       "USA","New York",       4,18,FALSE,NA),
    P("Lina Ljungblom",     25,"F","2001-10-15","Skövde, Sweden",               "SWE","",               2, 9,FALSE,NA),
    P("Shiann Darkangelo",  27,"F","1993-11-28","Brighton, Michigan, USA",      "USA","Michigan",       3,18,FALSE,NA),
    P("Catherine Dubois",   28,"F","1995-07-29","Quebec City, Quebec, Canada",  "CAN","Quebec",         0,12,FALSE,NA),
    P("Marie-Philip Poulin",29,"F","1991-03-28","Beauceville, Quebec, Canada",  "CAN","Quebec",         8,17,FALSE,NA),
    P("Jade Downie-Landry", 77,"F","1995-10-03","Saint-Jean-sur-Richelieu, Quebec, Canada","CAN","Quebec",1,11,FALSE,NA),
    P("Skylar Irving",      88,"F","2002-01-21","Kingston, Massachusetts, USA", "USA","Massachusetts",  1,16,FALSE,NA),
    P("Natálie Mlýnková",   96,"F","2001-05-24","Zlín, Czech Republic",         "CZE","",               4,18,FALSE,NA)
  ),
  
  # ── NEW YORK SIRENS ──────────────────────────────────────────
  NYR = list(
    P("Kayle Osborne",      82,"G","2002-02-28","Ottawa, Ontario, Canada",      "CAN","Ontario",       NA,26,TRUE, 0.903),
    P("Callie Shanahan",    37,"G","2003-05-26","Commerce, Michigan, USA",      "USA","Michigan",      NA, 4,TRUE, 0.891),
    P("Kaley Doyle",        39,"G","2003-01-01","Duluth, Minnesota, USA",       "USA","Minnesota",     NA, 1,TRUE, 0.889),
    P("Dayle Ross",          2,"D","2003-05-26","Spirit River, Alberta, Canada","CAN","Alberta",        0,13,FALSE,NA),
    P("Maja Nylén Persson",  8,"D","2000-11-20","Avesta, Sweden",               "SWE","",               2,28,FALSE,NA),
    P("Olivia Knowles",      9,"D","1999-01-24","Campbell River, British Columbia, Canada","CAN","British Columbia", 0,10,FALSE,NA),
    P("Nicole Vallario",    11,"D","2001-08-30","Lugano, Switzerland",          "CHE","",               1, 9,FALSE,NA),
    P("Jaime Bourbonnais",  14,"D","1998-09-09","Mississauga, Ontario, Canada", "CAN","Ontario",        1,26,FALSE,NA),
    P("Lauren Bernard",     16,"D","2001-07-15","Madison, Ohio, USA",           "USA","Ohio",            0, 8,FALSE,NA),
    P("Allyson Simpson",    20,"D","2000-12-20","Fort Worth, Texas, USA",       "USA","Texas",          2,28,FALSE,NA),
    P("Micah Zandee-Hart",  28,"D","1997-01-13","Saanichton, British Columbia, Canada","CAN","British Columbia",1,27,FALSE,NA),
    P("Jincy Roese",        33,"D","1997-05-15","O'Fallon, Missouri, USA",      "USA","Missouri",       0,22,FALSE,NA),
    P("Elle Hartje",         4,"F","2001-04-13","Detroit, Michigan, USA",       "USA","Michigan",       0,27,FALSE,NA),
    P("Savannah Norcross",   7,"F","2000-06-10","Lynn, Massachusetts, USA",     "USA","Massachusetts",  1,23,FALSE,NA),
    P("Sarah Fillier",      10,"F","2000-06-09","Georgetown, Ontario, Canada",  "CAN","Ontario",        9,27,FALSE,NA),
    P("Taylor Girard",      17,"F","1998-07-17","Macomb, Michigan, USA",        "USA","Michigan",       7,17,FALSE,NA),
    P("Maddi Wheeler",      18,"F","2002-01-01","Elk River, Minnesota, USA",    "USA","Minnesota",      3,27,FALSE,NA),
    P("Paetyn Levis",       19,"F","1999-11-06","Rogers, Minnesota, USA",       "USA","Minnesota",      5,28,FALSE,NA),
    P("Anna Bargman",       22,"F","2002-08-07","Boxford, Massachusetts, USA",  "USA","Massachusetts",  4,27,FALSE,NA),
    P("Anne Cherkowski",    24,"F","2002-07-06","Coldstream, British Columbia, Canada","CAN","British Columbia",2,26,FALSE,NA),
    P("Casey O'Brien",      26,"F","2001-08-27","Milton, Massachusetts, USA",   "USA","Massachusetts",  7,26,FALSE,NA),
    P("Emmy Fecteau",       29,"F","1999-04-07","Saint-Odilon-de-Cranbourne, Quebec, Canada","CAN","Quebec", 0,26,FALSE,NA),
    P("Clair DeGeorge",     41,"F","1999-06-07","Anchorage, Alaska, USA",       "USA","Alaska",         0,16,FALSE,NA),
    P("Kristin O'Neill",    43,"F","1998-03-30","Mississauga, Ontario, Canada", "CAN","Ontario",        4,28,FALSE,NA),
    P("Denisa Křižová",     44,"F","1994-11-03","Horní Cerekev, Czech Republic","CZE","",               1, 5,FALSE,NA),
    P("Kira Juodikis",      55,"F","2002-01-01","Naperville, Illinois, USA",    "USA","Illinois",       0, 2,FALSE,NA),
    P("Sarah Bujold",       62,"F","1996-02-26","Riverview, New Brunswick, Canada","CAN","New Brunswick",3,21,FALSE,NA),
    P("Kayla Vespa",        81,"F","1997-04-01","Hamilton, Ontario, Canada",    "CAN","Ontario",        2,23,FALSE,NA),
    P("Kristýna Kaltounková",98,"F","2002-04-14","Vlašim, Czech Republic",      "CZE","",              11,21,FALSE,NA)
  ),
  
  # ── OTTAWA CHARGE ────────────────────────────────────────────
  OTT = list(
    P("Sanni Ahola",         1,"G","2000-06-03","Helsinki, Finland",            "FIN","",              NA, 2,TRUE, 0.905),
    P("Kaitlyn Ross",       32,"G","2001-06-12","Red Cliff, Alberta, Canada",   "CAN","Alberta",       NA, 2,TRUE, 0.887),
    P("Gwyneth Philips",    33,"G","2000-09-17","Athens, Ohio, USA",            "USA","Ohio",          NA,26,TRUE, 0.927),
    P("Kendra Woodland",    70,"G","2000-01-01","Ottawa, Ontario, Canada",      "CAN","Ontario",       NA, 1,TRUE, 0.875),
    P("Jocelyne Larocque",   3,"D","1988-05-19","Ste. Anne, Manitoba, Canada",  "CAN","Manitoba",       0,16,FALSE,NA),
    P("Rory Guilday",        5,"D","2002-09-07","Chanhassen, Minnesota, USA",   "USA","Minnesota",      1,16,FALSE,NA),
    P("Stephanie Markowski", 6,"D","2001-08-24","Edmonton, Alberta, Canada",    "CAN","Alberta",        1,16,FALSE,NA),
    P("Kathryn Reilly",      8,"D","2001-01-17","Richmond, British Columbia, Canada","CAN","British Columbia",0,11,FALSE,NA),
    P("Vita Poniatovskaia",  9,"D","2002-01-01","Chelyabinsk, Russia",          "RUS","",               0, 6,FALSE,NA),
    P("Brooke Hobson",      11,"D","1999-05-27","Prince Albert, Saskatchewan, Canada","CAN","Saskatchewan",1,16,FALSE,NA),
    P("Emma Greco",         25,"D","1995-03-06","Burlington, Ontario, Canada",  "CAN","Ontario",        0, 3,FALSE,NA),
    P("Alexie Guay",        42,"D","2001-01-01","Quebec City, Quebec, Canada",  "CAN","Quebec",        NA,NA,FALSE,NA),
    P("Ronja Savolainen",   88,"D","1997-11-29","Helsinki, Finland",            "FIN","",               3,16,FALSE,NA),
    P("Alexa Vasko",        10,"F","1999-02-07","St. Catharines, Ontario, Canada","CAN","Ontario",      0,16,FALSE,NA),
    P("Olivia Wallin",      14,"F","2002-03-09","Oakville, Ontario, Canada",    "CAN","Ontario",        0, 2,FALSE,NA),
    P("Reece Hunt",         15,"F","2002-01-01","Mississauga, Ontario, Canada", "CAN","Ontario",       NA,NA,FALSE,NA),
    P("Kateřina Mrázová",   16,"F","1992-10-19","Kolin, Czech Republic",        "CZE","",               1,16,FALSE,NA),
    P("Gabbie Hughes",      17,"F","1999-10-04","Lino Lakes, Minnesota, USA",   "USA","Minnesota",      2,14,FALSE,NA),
    P("Brianne Jenner",     19,"F","1991-05-04","Oakville, Ontario, Canada",    "CAN","Ontario",        8,16,FALSE,NA),
    P("Taylor House",       22,"F","1998-09-29","Joliet, Illinois, USA",        "USA","Illinois",       0,15,FALSE,NA),
    P("Sarah Wozniewicz",   23,"F","2003-08-25","Cochrane, Alberta, Canada",    "CAN","Alberta",        3,16,FALSE,NA),
    P("Emily Clark",        26,"F","1995-11-28","Saskatoon, Saskatchewan, Canada","CAN","Saskatchewan",  2,16,FALSE,NA),
    P("Brooke McQuigge",    27,"F","2000-06-09","Bowmanville, Ontario, Canada", "CAN","Ontario",        0, 3,FALSE,NA),
    P("Peyton Hemp",        29,"F","2003-05-15","Andover, Minnesota, USA",      "USA","Minnesota",      0,16,FALSE,NA),
    P("Rebecca Leslie",     37,"F","1996-05-08","Ottawa, Ontario, Canada",      "CAN","Ontario",        8,16,FALSE,NA),
    P("Fanuza Kadirova",    71,"F","1998-04-06","Kukmor, Russia",               "RUS","",               4,14,FALSE,NA),
    P("Michela Cava",       86,"F","1994-03-26","Thunder Bay, Ontario, Canada", "CAN","Ontario",        0, 3,FALSE,NA),
    P("Maggy Burbidge",     94,"F","2001-01-01","Ottawa, Ontario, Canada",      "CAN","Ontario",       NA,NA,FALSE,NA)
  ),
  
  # ── TORONTO SCEPTRES ─────────────────────────────────────────
  TOR = list(
    P("Raygan Kirk",         1,"G","2001-03-11","Ste. Anne, Manitoba, Canada",  "CAN","Manitoba",      NA,20,TRUE, 0.930),
    P("Elaine Chuli",       29,"G","1994-05-16","Waterford, Ontario, Canada",   "CAN","Ontario",       NA, 6,TRUE, 0.912),
    P("Jessie McPherson",   91,"G","2001-01-01","Collingwood, Ontario, Canada", "CAN","Ontario",       NA, 2,TRUE, 0.891),
    P("Jessica Kondas",      2,"D","2000-01-03","Calgary, Alberta, Canada",     "CAN","Alberta",        0, 8,FALSE,NA),
    P("Kali Flanagan",       6,"D","1995-09-19","Burlington, Massachusetts, USA","USA","Massachusetts",  2,20,FALSE,NA),
    P("Hannah Baskin",      10,"D","2003-07-10","Minnetonka, Minnesota, USA",   "USA","Minnesota",      0, 6,FALSE,NA),
    P("Allie Munroe",       12,"D","1997-04-20","Yarmouth, Nova Scotia, Canada","CAN","Nova Scotia",    0,14,FALSE,NA),
    P("Renata Fast",        14,"D","1994-10-06","Burlington, Ontario, Canada",  "CAN","Ontario",        1,16,FALSE,NA),
    P("Savannah Harmon",    15,"D","1995-10-27","Downers Grove, Illinois, USA", "USA","Illinois",       1,20,FALSE,NA),
    P("Ella Shelton",       17,"D","1998-01-19","Ingersoll, Ontario, Canada",   "CAN","Ontario",        2,20,FALSE,NA),
    P("Anna Kjellbin",      71,"D","1994-03-16","Göteborg, Sweden",              "SWE","",               1,20,FALSE,NA),
    P("Daryl Watts",         9,"F","1999-05-15","Toronto, Ontario, Canada",     "CAN","Ontario",        7,18,FALSE,NA),
    P("Kiara Zanon",        11,"F","2001-01-01","Peterborough, Ontario, Canada","CAN","Ontario",        1,20,FALSE,NA),
    P("Anneke Rankila",     13,"F","2001-01-01","Lino Lakes, Minnesota, USA",   "USA","Minnesota",      0, 3,FALSE,NA),
    P("Lauren Messier",     16,"F","2003-07-03","Burlington, Ontario, Canada",  "CAN","Ontario",        1, 5,FALSE,NA),
    P("Jesse Compher",      18,"F","1999-07-01","Northbrook, Illinois, USA",    "USA","Illinois",       6,20,FALSE,NA),
    P("Sara Hjalmarsson",   19,"F","1998-02-08","Bankeryd, Sweden",              "SWE","",               2,20,FALSE,NA),
    P("Emma Gentry",        20,"F","2002-10-23","Alpena, Michigan, USA",        "USA","Michigan",       1,17,FALSE,NA),
    P("Kristin Della Rovere",21,"F","2001-01-01","Caledon, Ontario, Canada",    "CAN","Ontario",        0, 3,FALSE,NA),
    P("Maggie Connors",     22,"F","2000-10-22","St. John's, Newfoundland and Labrador, Canada","CAN","Newfoundland and Labrador",2,20,FALSE,NA),
    P("Natalie Spooner",    24,"F","1990-10-17","Scarborough, Ontario, Canada", "CAN","Ontario",        3,20,FALSE,NA),
    P("Clara Van Wieren",   25,"F","2002-01-25","Okemos, Michigan, USA",        "USA","Michigan",       0,20,FALSE,NA),
    P("Emma Maltais",       27,"F","1999-11-04","Burlington, Ontario, Canada",  "CAN","Ontario",        3,20,FALSE,NA),
    P("Blayre Turnbull",    40,"F","1993-07-15","Stellarton, Nova Scotia, Canada","CAN","Nova Scotia",  5,20,FALSE,NA),
    P("Claire Dalton",      42,"F","2000-03-20","Etobicoke, Ontario, Canada",   "CAN","Ontario",        0,20,FALSE,NA),
    P("Emma Woods",         67,"F","1995-12-18","Burford, Ontario, Canada",     "CAN","Ontario",        1,20,FALSE,NA)
  ),
  
  # ── SEATTLE TORRENT ──────────────────────────────────────────
  SEA = list(
    P("Hannah Murphy",      83,"G","2003-08-03","Bradford, Ontario, Canada",    "CAN","Ontario",       NA, 8,TRUE, 0.893),
    P("Corinne Schroeder",  30,"G","1999-08-17","Elm Creek, Manitoba, Canada",  "CAN","Manitoba",      NA,16,TRUE, 0.910),
    P("Carly Jackson",      70,"G","2001-01-01","Guelph, Ontario, Canada",      "CAN","Ontario",       NA, 1,TRUE, 0.889),
    P("Aneta Tejralová",     2,"D","1996-01-04","Prague, Czech Republic",        "CZE","",               1, 9,FALSE,NA),
    P("Cayla Barnes",        3,"D","1999-01-07","Eastvale, California, USA",    "USA","California",     0,16,FALSE,NA),
    P("Anna Wilgren",        5,"D","1999-11-18","Hudson, Wisconsin, USA",        "USA","Wisconsin",      0,16,FALSE,NA),
    P("Emily Zumwinkle",     6,"D","2001-01-01","Lakeville, Minnesota, USA",    "USA","Minnesota",     NA,NA,FALSE,NA),
    P("Emily Brown",        11,"D","1998-12-30","Blaine, Minnesota, USA",       "USA","Minnesota",      0,16,FALSE,NA),
    P("Mariah Keopple",     20,"D","2000-06-27","Menomonie, Wisconsin, USA",    "USA","Wisconsin",      0,16,FALSE,NA),
    P("Lyndie Lobdell",     24,"D","2002-09-01","Aurora, Illinois, USA",        "USA","Illinois",       0,11,FALSE,NA),
    P("Megan Carter",       27,"D","2001-05-23","Milton, Ontario, Canada",      "CAN","Ontario",        1,16,FALSE,NA),
    P("Lily Delianedis",     7,"F","2001-10-07","Edina, Minnesota, USA",        "USA","Minnesota",      0,10,FALSE,NA),
    P("Natalie Snodgrass",   8,"F","1998-12-17","Eagan, Minnesota, USA",        "USA","Minnesota",      1,15,FALSE,NA),
    P("Jenna Buglioni",     10,"F","2002-03-13","Port Moody, British Columbia, Canada","CAN","British Columbia",0,13,FALSE,NA),
    P("Mikyla Grant-Mentis",13,"F","1998-07-15","Brampton, Ontario, Canada",    "CAN","Ontario",        1,16,FALSE,NA),
    P("Jessie Eldridge",    18,"F","1997-12-17","Barrie, Ontario, Canada",      "CAN","Ontario",        7,16,FALSE,NA),
    P("Hannah Bilka",       19,"F","2001-03-24","Coppell, Texas, USA",          "USA","Texas",          4,14,FALSE,NA),
    P("Hilary Knight",      21,"F","1989-07-12","Palo Alto, California, USA",   "USA","California",     3,14,FALSE,NA),
    P("Alex Carpenter",     25,"F","1994-04-13","Cambridge, Massachusetts, USA","USA","Massachusetts",  6,16,FALSE,NA),
    P("Gabrielle David",    28,"F","2001-01-01","Quebec City, Quebec, Canada",  "CAN","Quebec",         0, 7,FALSE,NA),
    P("Theresa Schafzahl",  37,"F","2000-04-12","Weiz, Austria",                "AUT","",               4,28,FALSE,NA),
    P("Jada Habisch",       52,"F","2002-06-25","Buffalo, Minnesota, USA",      "USA","Minnesota",       1, 9,FALSE,NA),
    P("Marah Wagner",       66,"F","2001-09-14","Seattle, Washington, USA",     "USA","Washington",     0,13,FALSE,NA),
    P("Sydney Langseth",    71,"F","2002-01-29","Eden Prairie, Minnesota, USA", "USA","Minnesota",      0, 2,FALSE,NA),
    P("Lexie Adzija",       78,"F","2000-06-30","St. Thomas, Ontario, Canada",  "CAN","Ontario",        3,16,FALSE,NA),
    P("Julia Gosling",      88,"F","2001-02-21","London, Ontario, Canada",      "CAN","Ontario",        6,16,FALSE,NA),
    P("Danielle Serdachny", 92,"F","2001-05-12","Edmonton, Alberta, Canada",    "CAN","Alberta",        2,16,FALSE,NA),
    P("Brooke Bryant",      17,"F","2000-08-22","Linden, California, USA",      "USA","California",     0,16,FALSE,NA)
  ),
  
  # ── VANCOUVER GOLDENEYES ─────────────────────────────────────
  VAN = list(
    P("Kristen Campbell",   50,"G","1997-11-30","Brandon, Manitoba, Canada",    "CAN","Manitoba",      NA,11,TRUE, 0.901),
    P("Emerance Maschmeyer",38,"G","1994-10-05","Bruderheim, Alberta, Canada",  "CAN","Alberta",       NA,18,TRUE, 0.917),
    P("Kimberly Newell",    35,"G","1997-03-15","Listowel, Ontario, Canada",    "CAN","Ontario",       NA, 1,TRUE, 0.889),
    P("Madison Samoskevich", 7,"D","2002-11-15","Sandy Hook, Connecticut, USA", "USA","Connecticut",    1,24,FALSE,NA),
    P("Sydney Bard",        11,"D","2001-01-15","New Hartford, New York, USA",  "USA","New York",       0,27,FALSE,NA),
    P("Sophie Jaques",      16,"D","2000-10-16","Toronto, Ontario, Canada",     "CAN","Ontario",        8,27,FALSE,NA),
    P("Ashton Bell",        21,"D","1999-12-07","Deloraine, Manitoba, Canada",  "CAN","Manitoba",       1,27,FALSE,NA),
    P("Mellissa Channell-Watkins",23,"D","1994-12-16","Oakville, Ontario, Canada","CAN","Ontario",      0,27,FALSE,NA),
    P("Nina Jobst-Smith",   28,"D","2001-08-30","North Vancouver, British Columbia, Canada","DEU","",   0,17,FALSE,NA),
    P("Claire Thompson",    42,"D","1998-01-28","Toronto, Ontario, Canada",     "CAN","Ontario",        3,25,FALSE,NA),
    P("Izzy Daniel",         8,"F","2000-09-29","Minneapolis, Minnesota, USA",  "USA","Minnesota",      7,27,FALSE,NA),
    P("Kaitlin Chan",        9,"F","2003-01-05","Toronto, Ontario, Canada",     "CAN","Ontario",        1,14,FALSE,NA),
    P("Jenn Gardiner",      12,"F","2001-09-18","Surrey, British Columbia, Canada","CAN","British Columbia",4,27,FALSE,NA),
    P("Tereza Vanišová",    13,"F","1996-01-30","Strakonice, Czech Republic",   "CZE","",               3,27,FALSE,NA),
    P("Gabby Rosenthal",    15,"F","1999-09-13","Blaine, Minnesota, USA",       "USA","Minnesota",      1,26,FALSE,NA),
    P("Malia Schneider",    18,"F","1998-10-03","Millarville, Alberta, Canada", "CAN","Alberta",        0, 2,FALSE,NA),
    P("Mannon McMahon",     19,"F","2001-07-29","Maple Grove, Minnesota, USA",  "USA","Minnesota",      3,14,FALSE,NA),
    P("Sarah Nurse",        20,"F","1995-01-04","Hamilton, Ontario, Canada",    "CAN","Ontario",        7,16,FALSE,NA),
    P("Abby Boreen",        22,"F","2000-04-03","Somerset, Wisconsin, USA",     "USA","Wisconsin",      4,27,FALSE,NA),
    P("Michelle Karvinen",  33,"F","1990-03-27","Rodovre, Denmark",              "DNK","",               3,27,FALSE,NA),
    P("Hannah Miller",      34,"F","1996-02-16","North Vancouver, British Columbia, Canada","CAN","British Columbia",4,27,FALSE,NA),
    P("Anna Segedi",        51,"F","2000-12-20","Commerce, Michigan, USA",      "USA","Michigan",       1,24,FALSE,NA),
    P("Darcie Lappan",      72,"F","2001-09-15","Kingston, Ontario, Canada",    "CAN","Ontario",        0,12,FALSE,NA),
    P("Anna Meixner",       94,"F","1994-06-16","Zell am See, Austria",          "AUT","",               1,14,FALSE,NA),
    P("Anna Shokhina",      97,"F","1997-06-23","Novosinkovo, Russia",           "RUS","",               1,14,FALSE,NA)
  )
)

# ================================================================
# CHOROPLETH ORIGIN COUNTS
# ================================================================
PROV_MAP <- c("Alberta"="AB","British Columbia"="BC","Manitoba"="MB","New Brunswick"="NB",
              "Newfoundland and Labrador"="NL","Newfoundland"="NL","Nova Scotia"="NS","Ontario"="ON",
              "Prince Edward Island"="PE","Quebec"="QC","Saskatchewan"="SK",
              "Northwest Territories"="NT","Nunavut"="NU","Yukon"="YT")
STATE_MAP <- c("Alabama"="AL","Alaska"="AK","Arizona"="AZ","Arkansas"="AR","California"="CA",
               "Colorado"="CO","Connecticut"="CT","Delaware"="DE","Florida"="FL","Georgia"="GA",
               "Hawaii"="HI","Idaho"="ID","Illinois"="IL","Indiana"="IN","Iowa"="IA","Kansas"="KS",
               "Kentucky"="KY","Louisiana"="LA","Maine"="ME","Maryland"="MD","Massachusetts"="MA",
               "Michigan"="MI","Minnesota"="MN","Mississippi"="MS","Missouri"="MO","Montana"="MT",
               "Nebraska"="NE","Nevada"="NV","New Hampshire"="NH","New Jersey"="NJ","New Mexico"="NM",
               "New York"="NY","North Carolina"="NC","North Dakota"="ND","Ohio"="OH","Oklahoma"="OK",
               "Oregon"="OR","Pennsylvania"="PA","Rhode Island"="RI","South Carolina"="SC",
               "South Dakota"="SD","Tennessee"="TN","Texas"="TX","Utah"="UT","Vermont"="VT",
               "Virginia"="VA","Washington"="WA","West Virginia"="WV","Wisconsin"="WI","Wyoming"="WY")

norm_region <- function(raw, map) {
  raw <- trimws(raw)
  if (nchar(raw) == 2) return(toupper(raw))
  hit <- map[raw]; if (!is.na(hit)) return(unname(hit))
  idx <- match(tolower(raw), tolower(names(map))); if (!is.na(idx)) return(unname(map[idx]))
  toupper(substr(raw,1,2))
}

aggregate_origins <- function(rosters) {
  us <- list(); ca <- list()
  for (tm_players in rosters) {
    for (p in tm_players) {
      ctry <- toupper(trimws(p$country))
      prov <- trimws(p$province_state)
      if (nchar(prov) == 0) next
      if (ctry %in% c("USA","US","UNITED STATES","U.S.A.","U.S.")) {
        k <- norm_region(prov, STATE_MAP); us[[k]] <- (if(is.null(us[[k]])) 0 else us[[k]]) + 1
      } else if (ctry %in% c("CAN","CANADA","CA")) {
        k <- norm_region(prov, PROV_MAP); ca[[k]] <- (if(is.null(ca[[k]])) 0 else ca[[k]]) + 1
      }
    }
  }
  list(us_states=us, ca_provinces=ca)
}

# ================================================================
# ASSEMBLE OUTPUT
# ================================================================
message("Assembling output...")

is_na_player <- function(p) toupper(p$country) %in% c("USA","US","CAN","CANADA","CA","UNITED STATES","U.S.A.")

teams_output <- list()
for (tm in names(ROSTERS)) {
  pl      <- ROSTERS[[tm]]
  na_pl   <- Filter(is_na_player, pl)
  intl_pl <- Filter(Negate(is_na_player), pl)
  teams_output[[tm]] <- list(
    abbreviation = tm,
    wins         = STANDINGS[[tm]]$wins,
    losses       = STANDINGS[[tm]]$losses,
    otl          = STANDINGS[[tm]]$otl,
    coaches      = COACHES[[tm]],
    players      = na_pl,
    intl_players = intl_pl
  )
  message(sprintf("  %s: %d players (%d NA/CA, %d intl)",
                  tm, length(pl), length(na_pl), length(intl_pl)))
}

origins <- aggregate_origins(ROSTERS)
message("Origins — US states: ", length(origins$us_states),
        ", CA provinces: ", length(origins$ca_provinces))

# ================================================================
# WRITE JSON
# ================================================================
final <- list(
  season       = "2025-26",
  generated_at = as.character(Sys.time()),
  origins      = origins,
  teams        = teams_output
)

write_json(final, "pwhl_map_data.json", auto_unbox=TRUE, pretty=TRUE, na="null")
message("\n✅  Done! Written to: ", normalizePath("pwhl_map_data.json"))
message("    Refresh your browser to see the updated map.")