# ============================================================
# PWHL GIS Map - Data Export Script
# Working directory: ~/Desktop/GEOG456/Final_project
# ============================================================

library(fastRhockey)
library(jsonlite)
library(dplyr)

# ---- helpers ----
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b
safe_str <- function(x) { if (is.null(x)||length(x)==0||is.na(x[1])) "" else trimws(as.character(x[1])) }
safe_int <- function(x) { if (is.null(x)||length(x)==0||is.na(x[1])) NA_integer_ else suppressWarnings(as.integer(x[1])) }
safe_num <- function(x, d=3) { if (is.null(x)||length(x)==0||is.na(x[1])) NA_real_ else round(suppressWarnings(as.numeric(x[1])),d) }
pick_col  <- function(df, cands) intersect(cands, names(df))[1]
get_s <- function(row, cands) { col <- pick_col(row,cands); if(is.na(col)) "" else safe_str(row[[col]]) }
get_i <- function(row, cands) { col <- pick_col(row,cands); if(is.na(col)) NA  else safe_int(row[[col]]) }
get_n <- function(row, cands, d=3) { col <- pick_col(row,cands); if(is.na(col)) NA else safe_num(row[[col]],d) }

# ---- team config ----
TEAMS <- c("BOS","MIN","MTL","NYR","OTT","TOR","SEA","VAN")
TEAM_IDS <- c(BOS=1, MIN=2, MTL=3, NYR=4, OTT=5, TOR=6, SEA=8, VAN=7)
TEAM_NAMES <- c(BOS="Boston Fleet", MIN="Minnesota Frost", MTL="Montreal Victoire",
                NYR="New York Sirens", OTT="Ottawa Charge", TOR="Toronto Sceptres",
                SEA="Seattle Torrent", VAN="Vancouver Goldeneyes")

coaches_manual <- list(
  BOS = list(head="Ryan Perkins",  assistants=c("Erin Ambrose","Kelly Babstock")),
  MIN = list(head="Ken Appleby",   assistants=c("Allie Thunstrom","Samantha Holmes")),
  MTL = list(head="Kori Cheverie", assistants=c("Caroline Ouellette","Emmanuelle Blais")),
  NYR = list(head="Mike Behrens",  assistants=c("Kim St-Pierre","Allyson Clarke")),
  OTT = list(head="Carla MacLeod", assistants=c("Meghann Treacy","Liz Knox")),
  TOR = list(head="Troy Ryan",     assistants=c("Sami Jo Small","Vicky Sunohara")),
  SEA = list(head="Digit Murphy",  assistants=c("Tessa Bonhomme","Lyndsy Fry")),
  VAN = list(head="Perry Pearn",   assistants=c("Jayna Hefford","Tammy Lee Shewchuk"))
)

# ---- region maps ----
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

# ================================================================
# 1. STANDINGS  — pwhl_standings(season)
# ================================================================
message("Fetching standings...")
standings_raw <- tryCatch(
  pwhl_standings(season = 2026),
  error = function(e) { message("  standings error: ", e$message); NULL }
)

if (!is.null(standings_raw) && nrow(standings_raw) > 0) {
  message("  Standings cols: ", paste(names(standings_raw), collapse=", "))
}

parse_standings <- function(df, teams) {
  out <- setNames(
    lapply(teams, function(x) list(wins=NA, losses=NA, otl=NA)),
    teams
  )
  if (is.null(df) || nrow(df) == 0) return(out)
  
  abbr_col <- pick_col(df, c("team_abbreviation","team_abbr","abbreviation","team_code"))
  name_col <- pick_col(df, c("team_name","team","name","franchise_name"))
  
  for (tm in teams) {
    row <- df[0,]
    if (!is.na(abbr_col)) row <- df %>% filter(toupper(.data[[abbr_col]]) == tm)
    if (nrow(row)==0 && !is.na(name_col))
      row <- df %>% filter(grepl(TEAM_NAMES[tm], .data[[name_col]], ignore.case=TRUE))
    if (nrow(row)==0) next
    out[[tm]] <- list(
      wins   = get_i(row[1,], c("wins","w","reg_wins")),
      losses = get_i(row[1,], c("losses","l","reg_losses")),
      otl    = get_i(row[1,], c("ot_losses","otl","overtime_losses","ot_loss"))
    )
  }
  out
}
standings_lookup <- parse_standings(standings_raw, TEAMS)

# ================================================================
# 2. ROSTERS — pwhl_team_roster(team_id, season)
# ================================================================
message("Fetching rosters via pwhl_team_roster()...")

all_rosters <- data.frame()

for (tm in TEAMS) {
  tid <- TEAM_IDS[tm]
  message("  Fetching roster: ", tm, " (id=", tid, ")")
  r <- tryCatch(
    pwhl_team_roster(team_id = tid, season = 2026),
    error = function(e) { message("    error: ", e$message); NULL }
  )
  if (!is.null(r) && nrow(r) > 0) {
    r$team_abbr_key <- tm
    all_rosters <- bind_rows(all_rosters, r)
  }
}

if (nrow(all_rosters) > 0) {
  message("Total roster rows: ", nrow(all_rosters))
  message("Roster cols: ", paste(names(all_rosters), collapse=", "))
} else {
  message("WARNING: No roster data loaded!")
}

# ================================================================
# 3. SKATER STATS — pwhl_stats(position, season)
# ================================================================
message("Fetching skater stats...")
skaters_raw <- tryCatch(
  pwhl_stats(position = "skater", season = 2026, regular = TRUE),
  error = function(e) { message("  skater stats error: ", e$message); NULL }
)
if (!is.null(skaters_raw) && nrow(skaters_raw) > 0)
  message("  Skater cols: ", paste(names(skaters_raw), collapse=", "))

# ================================================================
# 4. GOALIE STATS — pwhl_stats(position, season)
# ================================================================
message("Fetching goalie stats...")
goalies_raw <- tryCatch(
  pwhl_stats(position = "goalie", season = 2026, regular = TRUE),
  error = function(e) { message("  goalie stats error: ", e$message); NULL }
)
if (!is.null(goalies_raw) && nrow(goalies_raw) > 0)
  message("  Goalie cols: ", paste(names(goalies_raw), collapse=", "))

# ================================================================
# 5. BUILD PLAYER LIST PER TEAM
# ================================================================
build_players <- function(tm) {
  players <- list()
  if (nrow(all_rosters) == 0) return(players)
  
  roster_tm <- all_rosters %>% filter(team_abbr_key == tm)
  if (nrow(roster_tm) == 0) return(players)
  
  for (i in seq_len(nrow(roster_tm))) {
    r     <- roster_tm[i,]
    pname <- get_s(r, c("player_name","full_name","name","athlete_display_name","player"))
    pos   <- get_s(r, c("position","position_abbreviation","pos","player_position"))
    dob   <- get_s(r, c("birth_date","date_of_birth","birthdate","dob","birth_year"))
    city  <- get_s(r, c("birth_city","birthCity","hometown","birth_place","city"))
    prov  <- get_s(r, c("birth_state_province","birth_state","province","state","birth_province"))
    ctry  <- get_s(r, c("birth_country","birthCountry","country","nationality","nation"))
    
    hometown_str <- paste(Filter(nchar, c(city, prov, ctry)), collapse=", ")
    is_g <- grepl("^G$|^GK$|goalie|goalkeeper", toupper(pos))
    
    goals <- NA; gp <- NA; sv <- NA
    
    # match stats by player name
    if (!is_g && !is.null(skaters_raw) && nrow(skaters_raw) > 0 && nchar(pname) > 0) {
      nc <- pick_col(skaters_raw, c("player_name","full_name","name","athlete_display_name"))
      if (!is.na(nc)) {
        sk_row <- skaters_raw %>% filter(grepl(pname, .data[[nc]], fixed=TRUE))
        if (nrow(sk_row) > 0) {
          goals <- get_i(sk_row[1,], c("goals","g","goal"))
          gp    <- get_i(sk_row[1,], c("games_played","gp","games","games_played_reg"))
        }
      }
    }
    if (is_g && !is.null(goalies_raw) && nrow(goalies_raw) > 0 && nchar(pname) > 0) {
      nc <- pick_col(goalies_raw, c("player_name","full_name","name","athlete_display_name"))
      if (!is.na(nc)) {
        gk_row <- goalies_raw %>% filter(grepl(pname, .data[[nc]], fixed=TRUE))
        if (nrow(gk_row) > 0) {
          gp <- get_i(gk_row[1,], c("games_played","gp","games"))
          sv <- get_n(gk_row[1,], c("save_percentage","sv_pct","saves_pct","save_pct"), 3)
        }
      }
    }
    
    players <- append(players, list(list(
      name           = pname,
      position       = pos,
      dob            = dob,
      hometown       = hometown_str,
      country        = ctry,
      province_state = prov,
      goals          = goals,
      games_played   = gp,
      is_goalie      = is_g,
      save_pct       = sv
    )))
  }
  players
}

# ================================================================
# 6. CHOROPLETH ORIGIN COUNTS
# ================================================================
aggregate_origins <- function(all_players) {
  us <- list(); ca <- list()
  for (players in all_players) {
    for (p in players) {
      ctry <- toupper(trimws(p$country))
      prov <- trimws(p$province_state)
      if (nchar(prov) == 0) next
      if (ctry %in% c("USA","US","UNITED STATES","U.S.A.","U.S.")) {
        k <- norm_region(prov, STATE_MAP); us[[k]] <- (us[[k]] %||% 0) + 1
      } else if (ctry %in% c("CAN","CANADA","CA")) {
        k <- norm_region(prov, PROV_MAP); ca[[k]] <- (ca[[k]] %||% 0) + 1
      }
    }
  }
  list(us_states=us, ca_provinces=ca)
}

# ================================================================
# 7. ASSEMBLE
# ================================================================
message("Assembling output...")
is_na_player <- function(p) toupper(p$country) %in% c("USA","US","CAN","CANADA","CA","UNITED STATES","U.S.A.")

all_players  <- list()
teams_output <- list()

for (tm in TEAMS) {
  pl      <- build_players(tm)
  all_players[[tm]] <- pl
  na_pl   <- Filter(is_na_player, pl)
  intl_pl <- Filter(Negate(is_na_player), pl)
  
  teams_output[[tm]] <- list(
    abbreviation = tm,
    full_name    = unname(TEAM_NAMES[tm]),
    wins         = standings_lookup[[tm]]$wins,
    losses       = standings_lookup[[tm]]$losses,
    otl          = standings_lookup[[tm]]$otl,
    coaches      = coaches_manual[[tm]],
    players      = na_pl,
    intl_players = intl_pl
  )
  message(sprintf("  %s: %d players (%d NA/CA, %d intl)", tm, length(pl), length(na_pl), length(intl_pl)))
}

origins <- aggregate_origins(all_players)
message("Origin counts — US: ", length(origins$us_states), ", CA: ", length(origins$ca_provinces))

# ================================================================
# 8. WRITE JSON
# ================================================================
final <- list(
  season       = "2025-26",
  generated_at = as.character(Sys.time()),
  origins      = origins,
  teams        = teams_output
)

write_json(final, "pwhl_map_data.json", auto_unbox=TRUE, pretty=TRUE, na="null")
message("\n✅  Done! Written to: ", normalizePath("pwhl_map_data.json"))
message("    Refresh your browser tab to see the updated map.")