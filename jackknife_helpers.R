library(survey)

# TIMSS 2019 jackknife repeated replication (JRR).
# Each sampling zone h gives two replicates: in the first, schools with JKREP = 1
# in zone h have their weight doubled and JKREP = 0 schools are zeroed; the second
# reverses this. All other zones keep their full weight.
# Sampling variance = 1/2 * sum over all 2H replicates of (estimate_r - estimate)^2
# (TIMSS 2019 Methods and Procedures, ch. 14; same 0.5 multiplier EdSurvey uses for TIMSS).

#Attach JKZONE and JKREP from a BSG student file by IDSTUD (within one country)
add_jk <- function(XY, BSG){
  jk <- data.frame(IDSTUD = as.numeric(as.character(BSG$IDSTUD)),
                   JKZONE = as.numeric(as.character(BSG$JKZONE)),
                   JKREP = as.numeric(as.character(BSG$JKREP)))

  XY$IDSTUD <- as.numeric(as.character(XY$IDSTUD))

  return(dplyr::left_join(XY, jk, by = "IDSTUD"))
}

#Replicate weight matrix (n x 2H). zone must be unique across countries when
#countries are pooled, e.g. paste(country, JKZONE).
jk_repweights <- function(zone, rep, weight){
  zones <- sort(unique(zone))
  rw <- matrix(weight, nrow = length(weight), ncol = 2 * length(zones))

  for (h in seq_along(zones)){
    in_zone <- zone == zones[h]
    rw[in_zone, 2*h - 1] <- weight[in_zone] * 2 * rep[in_zone]
    rw[in_zone, 2*h] <- weight[in_zone] * 2 * (1 - rep[in_zone])
  }

  return(rw)
}

#Replicate-weight survey design. Works for student-level data (weight = TOTWGT)
#and for class-level summaries (weight = sum of the class's TOTWGT), because the
#JKREP multiplier is constant within a school.
jk_design <- function(data, zone, rep, weight){
  svrepdesign(data = data,
              repweights = jk_repweights(zone, rep, weight),
              weights = weight,
              type = "other", scale = 0.5, rscales = 1,
              mse = TRUE, combined.weights = TRUE)
}
