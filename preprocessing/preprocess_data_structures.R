require(R6)

Dataset <- R6Class("Dataset",
                   public = list(
                     name = NULL,
                     shortname = NULL,
                     citation = NULL,
                     age = NULL,
                     age_unit = NULL,
                     age_label = NULL,
                     age_sigma = NULL,
                     age_sigma_unit = NULL,
                     data = NULL,
                     uncertainty_type = NULL,
                     data_sigma = NULL, 
                     data_lowerbound = NULL,
                     data_upperbound = NULL,
                     data_sigma_unit = NULL,
                     data_label = NULL,
                     data_unit = NULL,
                     random_agemodel = NULL,
                     random_datamodel = NULL,
                     random_models = NULL,
                     random_models_summary = NULL,
                     n.sigma.data = NULL,
                     n.sigma.ages = NULL,
                     slope.similarity = NULL,
                     
                     initialize = function(name = NA,
                                           shortname = NA,
                                           citation = NA,
                                           age = NA,
                                           age_unit = NA,
                                           age_label = NA,
                                           age_sigma = NA,
                                           age_sigma_unit = NA,
                                           data = NA,
                                           data_sigma = NA,
                                           data_sigma_unit = NA,
                                           data_label= NA,
                                           data_unit = NA,
                                           data_lowerbound = NA,
                                           data_upperbound = NA,
                                           uncertainty_type = "sigma",
                                           random_agemodel =  "yet to be drawn",
                                           random_datamodel = "yet to be drawn",
                                           random_models = "yet to be generated",
                                           random_models_summary = "yet to be generated",
                                           n.sigma.data = 2,
                                           n.sigma.ages = 2,
                                           slope.similarity = 1) {
                       self$name <- name
                       self$shortname <- shortname
                       self$citation <- citation
                       self$age <- age
                       self$age_unit <- age_unit
                       self$age_sigma <- age_sigma
                       self$age_label <- age_label
                       self$age_sigma_unit <- age_sigma_unit
                       self$data <- data
                       self$data_sigma <- data_sigma
                       self$data_lowerbound = data_lowerbound
                       self$data_upperbound = data_upperbound
                       self$data_sigma_unit <- data_sigma_unit
                       self$data_label <- data_label
                       self$data_unit <- data_unit
                       self$uncertainty_type <- uncertainty_type
                       self$random_agemodel <- random_agemodel
                       self$random_datamodel <- random_datamodel
                       self$random_models <- random_models
                       self$random_models_summary <- random_models_summary
                       self$n.sigma.data <- n.sigma.data
                       self$n.sigma.ages <- n.sigma.ages
                       self$slope.similarity <- slope.similarity
                     }
                   )
)
