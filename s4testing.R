library(googlesheets)

countermeasureWb <- gs_key('1pss11b0ArdwxXT9jJ9HU6psQ3cRNgBfDVL0gNZcQAM8')

rfgWs <- countermeasureWb %>%
  gs_read('Signal Phasing')

##### S4 Class testing

setClass("Person", representation(name = "character", age = "numeric"))
hadley <- new("Person", name = "Hadley", age = 31)

##### S4 Infrastructure Class testing

PointTreatment <- setClass(
  "PointTreatment",
  slots = c(
    intName = list(
      choices = "vector",
      display = "character",
      value = "numeric"
    ),
    intCLNodeID = "numeric",
    streetName = "character",
    status = "character",
    X = "numeric",
    Y = "numeric"),
  # Set prototype to NA values
  prototype = list(
    intName.choices = c('choice 1', 'choice 2', 'choice 3'),
    intName.display = "Intersection Name",
    intName.value = 232,
    intCLNodeID = NA_real_,
    streetName = NA_character_,
    X = NA_real_,
    Y = NA_real_)
  )

# Pedestrian-Activated Flashing Beacon
sigPhase <- setClass(
  "sigPhase",
  slots = c(
    display = "Signal Phasing",
    treatment = list(
      display = ""
    )
    ),
  contains = "PointTreatment")

tester <- pafb(X = 23, Y = 22.3434)

tester@intName.choices



