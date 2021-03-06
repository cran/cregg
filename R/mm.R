#' @rdname mm
#' @title Marginal Means
#' @description Calculate (descriptive) marginal means (MMs) from a conjoint design
#' @param data A data frame containing variables specified in \code{formula}. All RHS variables should be factors.
#' @param formula A formula specifying an outcome (LHS) and conjoint features (RHS) to describe. All variables should be factors; all levels across features should be unique, with constraints specified with an asterisk (*) between features, as in \code{amce}.
#' @template id
#' @template weights
#' @template feature_order
#' @template feature_labels
#' @template level_order
#' @param h0 A numeric value specifying a null hypothesis value to use when generating z-statistics and p-values.
#' @template alpha
#' @param \dots Ignored.
#' @return A data frame of class \dQuote{cj_mm}
#' @details \code{mm} provides descriptive representations of conjoint data as marginal means (MMs), which represent the mean outcome across all appearances of a particular conjoint feature level, averaging across all other features. In forced choice conjoint designs with two profiles per choice task, MMs by definition average 0.5 with values above 0.5 indicating features that increase profile favorability and values below 0.5 indicating features that decrease profile favorability. For continuous outcomes, MMs can take any value in the full range of the outcome.
#' 
#' But note that if feature levels can co-occur, such that both alternatives share a feature level, then the MMs on forced choice outcomes are bounded by the probability of co-occurrence (as a lower bound) and 1 minus that probability as an upper bound.
#' 
#' Plotting functionality is provided in \code{\link{plot.cj_mm}}.
#' 
#' @examples
#' \donttest{
#' data(immigration)
#' # marginal means
#' mm(immigration, ChosenImmigrant ~ Gender + Education + LanguageSkills,
#'    id = ~ CaseID, h0 = 0.5)
#'
#' # higher-order marginal means with feature interactions
#' immigration$language_entry <- 
#'   interaction(immigration$LanguageSkills, immigration$PriorEntry, sep = "_")
#' mm(immigration, ChosenImmigrant ~ language_entry,
#'    id = ~CaseID)
#' }
#' @seealso \code{\link{mm_diffs}} \code{\link{plot.cj_mm}}
#' @import stats
#' @importFrom survey svydesign svyby svymean
#' @export
mm <- 
function(
  data,
  formula,
  id = ~ 0,
  weights = NULL,
  feature_order = NULL,
  feature_labels = NULL,
  level_order = c("ascending", "descending"),
  alpha = 0.05,
  h0 = 0,
  ...
) {
    
    # coerce to "cj_df" to preserve attributes
    if (inherits(data, "survey.design")) {
        stop("mm() does not currently support passing a 'survey.design' object as 'data'")
        # data2 <- cj_df(data[["variables"]])
    } else {
        data2 <- cj_df(data)
    }
    
    # get outcome variable
    outcome <- all.vars(stats::update(formula, . ~ 0))
    if (!length(outcome) || outcome == ".") {
        stop("'formula' is missing a left-hand outcome variable")
    }
    
    # get RHS variables, variable labels, and factor levels
    RHS <- all.vars(stats::update(formula, 0 ~ . ))
    
    # process feature_order argument
    feature_order <- check_feature_order(feature_order, RHS)
    
    # set level_order (within features) to ascending or descending
    level_order <- match.arg(level_order)
    
    # function used in cj and ammplot to produce "fancy" feature labels
    feature_labels <- clean_feature_labels(data = data2, RHS = RHS, feature_labels = feature_labels)
    
    # convert feature labels and levels to data frame
    term_labels_df <- make_term_labels_df(data2, feature_order, level_order = level_order)
    
    # get `weights` as character string
    if (!is.null(weights)) {
        weightsvar <- all.vars(update(weights, 0 ~ . ))[1L]
        data2[["CREGG_WEIGHT"]] <- data2[[weightsvar]]
    } else {
        weights <- ~ 0
        data2[["CREGG_WEIGHT"]] <- 1
    }
    
    # get `id` as character string
    if (length(all.vars(id))) {
        idvar <- all.vars(id)[1L]
    } else {
        id <- ~ 0
        idvar <- NULL
    }
    
    # reshape data so we can just do svyglm(OUTCOME ~ 0 + LEVEL) in a loop
    long <- stats::reshape(data2[c(outcome, RHS, idvar, "CREGG_WEIGHT")], 
                           varying = list(names(data2[RHS])), 
                           v.names = "LEVEL", 
                           timevar = "Feature",
                           times = names(data2[RHS]),
                           idvar = "observation",
                           direction = "long")
    names(long)[names(long) == outcome] <- "OUTCOME"
    
    # calculate MMs, SEs, etc.
    out_list <- lapply(unique(long[["Feature"]]), function(this_feature) {
        dtmp <- long[long[["Feature"]] == this_feature, , drop = FALSE]
        svyd <- survey::svydesign(ids = id, weights = ~ CREGG_WEIGHT, data = dtmp)
        mod <- survey::svyglm(OUTCOME ~ 0 + LEVEL, design = svyd)
        cs <- get_coef_summary(mod = mod, data = data, id = NULL, alpha = alpha)
        names(cs) <- c("estimate", "std.error", "z", "p", "lower", "upper", "level")

        ## correct z test to respect h0
        cs[["z"]] <- (cs[["estimate"]] - h0)/cs[["std.error"]]
        cs[["p"]] <- 2L * stats::pnorm(-abs(cs[["z"]])) 
        
        cs[["level"]] <- factor(sub("^LEVEL", "", cs[["level"]]))
        cs
    })
    out <- do.call("rbind", out_list)
    
    # attach feature labels
    out <- merge(out, make_term_labels_df(data2, RHS), by = c("level"), all = TRUE)
    out[["level"]] <- factor(out[["level"]], levels = term_labels_df[["level"]])
    out[["feature"]] <- factor(out[["feature"]],
                               levels = feature_order,
                               labels = feature_labels[feature_order])
    out[["outcome"]] <- outcome
    
    # return organized data frame
    out[["statistic"]] <- "mm"
    out <- out[c("outcome", "statistic", "feature", "level", "estimate", "std.error", "z", "p", "lower", "upper")]
    out <- out[order(out[["level"]]),]
    rownames(out) <- seq_len(nrow(out))
    return(structure(out, class = c("cj_mm", "data.frame")))
}
