library(R6)

PointTreatment <- R6Class("PointTreatment", list(
  intName = list(
    choices = c('choice 1', 'choice 2', 'choice 3'),
    display = "Intersection Name",
    value = 232),
  intCLNodeID = NA_real_,
  streetName = NA_character_,
  X = NA_real_,
  Y = NA_real_))

sigPhase <- R6Class("sigPhase", inherit = PointTreatment, list(
  display = "Signal Phasing",
  nb = list(display = "N/B", choices = c(NA, "PORT", "PPLT", "POLT", "POLT & PORT"), value = NA_character_),
  sb = list(display = "S/B", choices = c(NA, "PORT", "PPLT", "POLT", "POLT & PORT"), value = NA_character_),
  eb = list(display = "E/B", choices = c(NA, "PORT", "PPLT", "POLT", "POLT & PORT"), value = NA_character_),
  wb = list(display = "W/B", choices = c(NA, "PORT", "PPLT", "POLT", "POLT & PORT"), value = NA_character_),
  tcr = list(display = "TCR", value = NA_character_),
  ewo = list(display = "EWO#", value = NA_character_),
  wo = list(display = "WO#", value = NA_character_),
  status = list(display = "Status", choices = c('In Design', 'Design Completed'), value = NA_character_),
  team = list(display = "Team", choices = c('Scott Brown', 'Adam Driscoll'), value = NA_character_),
  priority = list(display = "Priority", choices = c(1,2,3,4,5,6), value = NA_integer_),
  totalcost = list(display = "Est. Cost (Total)", value = NA_real_),
  designcost = list(display = "Est. Cost (Design)", value = NA_real_),
  matcost = list(display = "Est. Cost (Materials)", value = NA_real_),
  labcost = list(display = "Est. Cost (Labor)", value = NA_real_),
  delivery = list(display = "Construction Delivery Method", value = NA_character_),
  plandate = list(display = "Date Planned", value = NA_integer_),
  installdate = list(display = "Date Installed", value = NA_integer_),
  notes = list(display = "Notes", value = NA_character_)
))

testz <- sigPhase$new()


# Pedestrian-Activated Flashing Beacon
sigPhase <- setClass(
  "sigPhase",
  slots = c(
    display = "Signal Phasing",
    NB = list(
      display = "N/B",
      choices = c(NA, "PORT", "PPLT", "POLT", "POLT & PORT"),
      value = NA_character_
    )
  ),
  contains = "PointTreatment")



