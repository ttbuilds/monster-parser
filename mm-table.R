library(jsonlite)
library(tidyverse)
source("../utils/utils.R")

url_stem <- "https://raw.githubusercontent.com/TheGiddyLimit/TheGiddyLimit.github.io/master/data/bestiary/bestiary"
book_abbreviations <- c("mm", "mtf", "vgm", "vrgr")
urls <- paste0(url_stem, "-", book_abbreviations, ".json")
data_list <- urls %>% 
  sapply(fromJSON) %>%
  sapply(pluck, "monster")
names(data_list) <- book_abbreviations

return_heads <- function(list_of_lists, default_value = 0)
{
  result <- list_of_lists
  while(is.list(result)) 
  {
    result <- sapply(
      result,
      function(x){
        ifelse(is.null(x), default_value, pluck(x, 1))})
  }
  return(result)
}

add_prefix <- function(x,prefix)
{
  return(paste0(prefix,x))
}

add_suffix <- function(x,suffix)
{
  return(paste0(x,suffix))
}

flatten_group <- function(parent, groupname)
{
  parent %>% 
    pluck(groupname) %>%
    rename_with(add_prefix, prefix = paste0(groupname, "_"))
}

expand_to_indicators <- function(parent, label, valueset)
{
  sapply(
    valueset, 
      FUN = function(x){
      sapply(
        X = parent %>% pluck(label), 
        FUN = function(t){x %in% t})}) %>%
  as_tibble() %>%
  rename_with(add_prefix, prefix = paste0(label,"_"))
}

get_valueset <- function(frame, label)
{
  valueset <- frame %>%
    pluck(label) %>%
    unlist() %>%
    unique()
  return(valueset)
}

nullsafe_filter <- function(df, condition)
{
  condition <- enquo(condition)
  if(is.null(df)) return(NA)
  else return(filter(df, !!condition))
}

null_to_default <- function(x, default)
{
  replace(x, x == "NULL" | x == "", default)
}

damage_types <- c(
  "fire","cold","lightning","acid","thunder",
  "poison","necrotic","radiant","force","psychic",
  "bludgeoning", "piercing","slashing")

conditional_damage_types <- c(
  "from nonmagical attacks",
  "from nonmagical attacks that aren't silvered",
  "from nonmagical attacks that aren't adamantine")

spellcasting_types <- c("Spellcasting", "Innate Spellcasting")

mm_monsters_raw <- data_list %>%
  pluck("mm") %>%
  as_tibble()

caster_tags <- mm_monsters_raw %>%
  get_valueset("spellcastingTags")
sense_tags <- mm_monsters_raw %>%
  get_valueset("senseTags")
condition_types <- mm_monsters_raw %>%
  get_valueset("conditionImmune")
trait_types <- mm_monsters_raw %>%
  get_valueset("traitTags")

parse_bestiary_frame <- function(frame)
{
parsed_frame <- frame %>%
  as_tibble() %>%
  select(
    name, source, page, srd, cr, 
    size, type,
    ac, passive,
    str, dex, con, int, wis, cha) %>%
  bind_cols(
    frame %>% flatten_group("hp"),
    frame %>% flatten_group("speed"),
    frame %>% flatten_group("skill"),
    frame %>% flatten_group("save"),
    frame %>% expand_to_indicators("resist", valueset = damage_types),
    frame %>% expand_to_indicators("resist", valueset = conditional_damage_types) %>%
      rename(
        resist_nonmagical_attacks = 
          "resist_from nonmagical attacks",
        resist_nonmagical_attacks_not_silvered = 
          "resist_from nonmagical attacks that aren't silvered",
        resist_nonmagical_attacks_not_adamantine = 
          "resist_from nonmagical attacks that aren't adamantine"),
    frame %>% expand_to_indicators("immune", valueset = damage_types),
    frame %>% expand_to_indicators("immune", valueset = conditional_damage_types) %>%
      rename(
        immune_nonmagical_attacks = 
          "immune_from nonmagical attacks",
        immune_nonmagical_attacks_not_silvered = 
          "immune_from nonmagical attacks that aren't silvered",
        immune_nonmagical_attacks_not_adamantine = 
          "immune_from nonmagical attacks that aren't adamantine"),
    frame %>% expand_to_indicators("conditionImmune", valueset = condition_types),
    frame %>% expand_to_indicators("conditionInflict", valueset = condition_types),    
    frame %>% expand_to_indicators("traitTags", valueset = trait_types),        
    frame %>% expand_to_indicators("spellcasting", valueset = spellcasting_types),
    frame %>% expand_to_indicators("spellcastingTags", valueset = caster_tags),    
    frame %>% expand_to_indicators("senseTags", valueset = sense_tags)) %>%
  mutate(
    ac         = return_heads(ac, default_value = 0),
    cr         = return_heads(cr, default_value = NA),
    category   = return_heads(type, default_value = NA)) %>%
  mutate(across(starts_with("speed_"), return_heads, default_value = 0)) %>%
  mutate(across(starts_with("speed_"), replace_na, replace = 0)) %>%  
  mutate(
    can_fly = speed_fly > 0,
    can_burrow = speed_burrow > 0) %>%
  rename(
    can_hover = speed_canHover) %>%    
  mutate(across(starts_with("can_"), replace_na, replace = 0)) %>%  
  mutate(srd = replace_na(srd, replace = FALSE)) %>%
  select(-ends_with("_other")) %>%
  mutate(
    cr = sapply(cr, parse, file = "", n = NULL) %>% sapply(eval)
  ) %>%
  mutate(
    hit_dice = gsub("([0-9]+)d[0-9]+( \\+ [0-9]+)?", "\\1", hp_formula),
    atk1_type = gsub(
      ".*\\{@atk ([a-z]+,?[a-z]*)\\}.*", "\\1",
      frame %>%
        pluck("action") %>%
        lapply(nullsafe_filter, condition = grepl("@atk", entries)) %>%
        lapply(pluck, "entries") %>%
        sapply(pluck, 1)),
    atk1_tohit = gsub(
      ".*\\{@hit ([0-9]+)\\}.*", "\\1",
      frame %>%
        pluck("action") %>%
        lapply(nullsafe_filter, condition = grepl("@atk", entries)) %>%
        lapply(pluck, "entries") %>%
        sapply(pluck, 1)),    
    atk1_avg_dmg = gsub(
      ".*\\{@h\\}([0-9]+).*", "\\1",
      frame %>%
        pluck("action") %>%
        lapply(nullsafe_filter, condition = grepl("@atk", entries)) %>%
        lapply(pluck, "entries") %>%
        sapply(pluck, 1)),        
    atk1_dmg_roll = gsub(
      ".*\\{@h\\}[0-9]+ (\\(\\{@damage ([0-9]+d[0-9]+ ?[\\+\\-]? ?[0-9]*)\\}\\))?.*", "\\2",
      frame %>%
        pluck("action") %>%
        lapply(nullsafe_filter, condition = grepl("@atk", entries)) %>%
        lapply(pluck, "entries") %>%
        sapply(pluck, 1)),            
    atk1_dmg_roll = ifelse(atk1_dmg_roll == "", paste0(atk1_avg_dmg, "d1"), atk1_dmg_roll),
    atk1_reach = gsub(
      ".*to hit( \\(.*\\))?, (reach ([0-9]+))?.*", "\\3",
      frame %>%
        pluck("action") %>%
        lapply(nullsafe_filter, condition = grepl("@atk", entries)) %>%
        lapply(pluck, "entries") %>%
        sapply(pluck, 1)),                
    atk1_range = gsub(
      ".*to hit( \\(.*\\))?,( reach.*or)? (range ([0-9]+)(/[0-9]+)? ft)?.*", "\\4",
      frame %>%
        pluck("action") %>%
        lapply(nullsafe_filter, condition = grepl("@atk", entries)) %>%
        lapply(pluck, "entries") %>%
        sapply(pluck, 1)),                    
    atk2_type = gsub(
      ".*\\{@atk ([a-z]+,?[a-z]*)\\}.*", "\\1",
      frame %>%
        pluck("action") %>%
        lapply(nullsafe_filter, condition = grepl("@atk", entries)) %>%
        lapply(pluck, "entries") %>%
        sapply(pluck, 2)),    
    atk2_tohit = gsub(
      ".*\\{@hit ([0-9]+)\\}.*", "\\1",
      frame %>%
        pluck("action") %>%
        lapply(nullsafe_filter, condition = grepl("@atk", entries)) %>%
        lapply(pluck, "entries") %>%
        sapply(pluck, 2)),    
    atk2_avg_dmg = gsub(
      ".*\\{@h\\}([0-9]+).*", "\\1",
      frame %>%
        pluck("action") %>%
        lapply(nullsafe_filter, condition = grepl("@atk", entries)) %>%
        lapply(pluck, "entries") %>%
        sapply(pluck, 2)),        
    atk2_dmg_roll = gsub(
      ".*\\{@h\\}[0-9]+ (\\(\\{@damage ([0-9]+d[0-9]+ ?[\\+\\-]? ?[0-9]*)\\}\\))?.*", "\\2",
      frame %>%
        pluck("action") %>%
        lapply(nullsafe_filter, condition = grepl("@atk", entries)) %>%
        lapply(pluck, "entries") %>%
        sapply(pluck, 2)),            
    atk2_dmg_roll = ifelse(atk2_dmg_roll == "", paste0(atk2_avg_dmg, "d1"), atk2_dmg_roll),    
    atk2_reach = gsub(
      ".*to hit( \\(.*\\))?, (reach ([0-9]+))?.*", "\\3",
      frame %>%
        pluck("action") %>%
        lapply(nullsafe_filter, condition = grepl("@atk", entries)) %>%
        lapply(pluck, "entries") %>%
        sapply(pluck, 2)),                
    atk2_range = gsub(
      ".*to hit( \\(.*\\))?,( reach.*or)? (range ([0-9]+)(/[0-9]+)? ft)?.*", "\\4",
      frame %>%
        pluck("action") %>%
        lapply(nullsafe_filter, condition = grepl("@atk", entries)) %>%
        lapply(pluck, "entries") %>%
        sapply(pluck, 2)),                        
    atk3_type = gsub(
      ".*\\{@atk ([a-z]+,?[a-z]*)\\}.*", "\\1",
      frame %>%
        pluck("action") %>%
        lapply(nullsafe_filter, condition = grepl("@atk", entries)) %>%
        lapply(pluck, "entries") %>%
        sapply(pluck, 3)),    
    atk3_tohit = gsub(
      ".*\\{@hit ([0-9]+)\\}.*", "\\1",
      frame %>%
        pluck("action") %>%
        lapply(nullsafe_filter, condition = grepl("@atk", entries)) %>%
        lapply(pluck, "entries") %>%
        sapply(pluck, 3)),    
    atk3_avg_dmg = gsub(
      ".*\\{@h\\}([0-9]+).*", "\\1",
      frame %>%
        pluck("action") %>%
        lapply(nullsafe_filter, condition = grepl("@atk", entries)) %>%
        lapply(pluck, "entries") %>%
        sapply(pluck, 3)),        
    atk3_dmg_roll = gsub(
      ".*\\{@h\\}[0-9]+ (\\(\\{@damage ([0-9]+d[0-9]+ ?[\\+\\-]? ?[0-9]*)\\}\\))?.*", "\\2",
      frame %>%
        pluck("action") %>%
        lapply(nullsafe_filter, condition = grepl("@atk", entries)) %>%
        lapply(pluck, "entries") %>%
        sapply(pluck, 3)),                
    atk3_dmg_roll = ifelse(atk3_dmg_roll == "", paste0(atk3_avg_dmg, "d1"), atk3_dmg_roll),        
    atk3_reach = gsub(
      ".*to hit( \\(.*\\))?, (reach ([0-9]+))?.*", "\\3",
      frame %>%
        pluck("action") %>%
        lapply(nullsafe_filter, condition = grepl("@atk", entries)) %>%
        lapply(pluck, "entries") %>%
        sapply(pluck, 3)),                
    atk3_range = gsub(
      ".*to hit( \\(.*\\))?,( reach.*or)? (range ([0-9]+)(/[0-9]+)? ft)?.*", "\\4",
      frame %>%
        pluck("action") %>%
        lapply(nullsafe_filter, condition = grepl("@atk", entries)) %>%
        lapply(pluck, "entries") %>%
        sapply(pluck, 3)),                            
    spellcasting_level = gsub(
      ".*([0-9]+)(st|nd|rd|th)-level spellcaster.*", "\\1",
      frame %>% 
        pluck("spellcasting") %>% 
        lapply(nullsafe_filter, condition = name == "Spellcasting") %>%
        sapply(pluck, "headerEntries")),
    spellcasting_dc = gsub(
      ".*\\{@dc ([0-9]+)\\}.*", "\\1",
      frame %>% 
        pluck("spellcasting") %>% 
        lapply(nullsafe_filter, condition = name == "Spellcasting") %>%
        sapply(pluck, "headerEntries")),
    spellcasting_atk = gsub(
      ".*\\{@hit ([0-9]+)\\}.*", "\\1",
      frame %>% 
        pluck("spellcasting") %>% 
        lapply(nullsafe_filter, condition = name == "Spellcasting") %>%
        sapply(pluck, "headerEntries")),    
    innate_spellcasting_dc = gsub(
      ".*\\{@dc ([0-9]+)\\}.*", "\\1",
      frame %>% 
        pluck("spellcasting") %>% 
        lapply(nullsafe_filter, condition = "Innate Spellcasting" %in% name) %>%
        sapply(pluck, "headerEntries")),
    innate_spellcasting_atk = gsub(
      ".*\\{@hit ([0-9]+)\\}.*", "\\1",
      frame %>% 
        pluck("spellcasting") %>% 
        lapply(nullsafe_filter, condition = "Innate Spellcasting" %in% name) %>%
        sapply(pluck, "headerEntries"))) %>%  
  mutate(
    across(starts_with("spellcasting_"), null_to_default, default = 0),
    across(starts_with("innate_spellcasting_"), null_to_default, default = 0),
    across(starts_with("atk"), null_to_default, default = NA)) %>%  
  mutate(
    across(starts_with("skill_"), parse_number),
    across(starts_with("save_"),  parse_number),
    across(starts_with("resist_"), as.numeric),
    across(starts_with("immune_"), as.numeric),
    across(starts_with("conditionImmune_"), as.numeric),    
    across(starts_with("conditionInflict_"), as.numeric),        
    across(starts_with("traitTags_"), as.numeric),            
    across(starts_with("can_"), as.numeric),
    across(ends_with("_tohit"), as.numeric),    
    across(ends_with("_avg_dmg"), as.numeric),        
    across(starts_with("spellcasting_"), as.numeric),
    across(starts_with("innate_spellcasting_"), as.numeric),    
    across(starts_with("senseTags_"), as.numeric)) %>%  
  mutate(across(any_of(c(
    "save_str", "skill_athletics")), 
    coalesce, mod(str))) %>%      
  mutate(across(any_of(c(
    "save_dex", "skill_sleight of hand", "skill_stealth", "skill_acrobatics")), 
    coalesce, mod(dex))) %>%  
  mutate(across(any_of(c(
    "save_con")), 
    coalesce, mod(con))) %>%  
  mutate(across(any_of(c(
    "save_int", "skill_arcana", "skill_history", "skill_religion", "skill_nature", "skill_investigation")), 
    coalesce, mod(int))) %>%  
  mutate(across(any_of(c(
    "save_wis", "skill_perception", "skill_insight", "skill_survival", "skill_medicine", "skill_animal handling")), 
    coalesce, mod(wis))) %>%
  mutate(across(any_of(c(
    "save_cha", "skill_persuasion", "skill_deception", "skill_intimidation", "skill_performance")), 
    coalesce, mod(cha)))  %>%
  select(
    name, source, page, srd,
    category,
    cr, size, ac, hp_average, hp_formula, hit_dice,
    passive,
    str, dex, con, int, wis, cha,
    starts_with("can"),
    starts_with("speed"),
    starts_with("save"),
    starts_with("resist"),
    starts_with("immune"),
    starts_with("conditionImmune_"),
    starts_with("conditionInflict_"),    
    starts_with("traitTags_"),        
    starts_with("atk"),
    starts_with("spellcasting_"),
    starts_with("innate_spellcasting_"),    
    starts_with("senseTags"),
    starts_with("skill"))
return(parsed_frame)
}

mm <- data_list %>% 
  pluck("mm") %>%
  parse_bestiary_frame()
vgm <- data_list %>%
  pluck("vgm") %>%
  as_tibble() %>%
  mutate(srd = 0) %>%
  parse_bestiary_frame()
mtf <- data_list %>%
  pluck("mtf") %>%
  as_tibble() %>%
  mutate(srd = 0) %>%
  parse_bestiary_frame()

compendium <- bind_rows(mm, vgm, mtf)

compendium %>% write_csv("bestiary.csv")

compendium %>%
  filter(cr > 0) %>%
  mutate(
    cr_bin = cut(cr, breaks = c(0.125, 0.5, 1, 3, 5, 8, 11,16,20,25,30))) %>%
  filter(!is.na(cr_bin)) %>%
  ggplot(aes(x = cr_bin, y = save_int)) +
  geom_boxplot() +
  scale_y_continuous(
    breaks = -3:15) +
  stat_summary(fun = mean, geom = "point", pch = 1, color = "blue")

library(mosaic)
compendium %>%
  filter(cr >= 1, cr <= 14, immune_poison == 0) %>%
  mean(~save_cha, data = .)

