




## Some scripts for calculating correlation within dyadic data.


#' Helper function that computes some basic correlations
#'
#' @description Computes a set of basic correlations between
#' variables, both within a person and between people in the dyads.
#' These are then used to compute more complex correlations and significance levels.
#'
#' @param ax Vector containing a person A's score on variable X
#' @param ay Vector containing a person A's score on variable Y
#' @param bx Vector containing a person B's score on variable X
#' @param by Vector containing a person B's score on variable Y
#' @return A list of correlations (e.g. raxbx = correlation between a person's X value and their partner's Y value)
#' @export
cor.structure <- function(ax,ay,bx,by) {
    r <- function(a, b) {
        return (stats::cor.test(a,b,method='pearson')$estimate[["cor"]])
    }
    return(list(
        N.dyads = length(ax)/2, ## we're assuming that each dyad has two rows in the input table (multiplexed format)
        raxbx = r(ax,bx),
        rayby = r(ay,by),
        raxay = r(ax,ay),
        raxby = r(ax,by)
    ))
}

#' Convert z value into two-tailed p value
#' @param z Z value to be converted into p
#' @return Corresponding p value
#' @export
z.to.p <- function(z) {
    return(2*stats::pnorm(-abs(z)))
}



#' Compute correlation between a person's X value and their Y value,
#' while controlling for dyadic dependence in the data.
#'
#' @description We start from a data table where we have scores of person A and B on variables X and Y. 
#' Data format requirement: all dyad members need to be present in the data
#' once as A and once as B. That is, each dyad corresponds to two, not one, row.
#' The question asked here is whether a person's X value predicts their own Y value.
#' This is within-dyad, but it takes into account potential dependence between
#' values of the same dyad.
#' The question asked here is whether a person's X value predicts their own Y value (ax => ay).
#' This is within-dyad of course, but it takes into account potential dependence between
#' values of the same dyad.
#' Scientific Reference:
#' Griffin, D., & Gonzalez, R. (1995). Correlational analysis of dyad-level data in the exchangeable case. Psychological
#' Bulletin, 118(3), 430–439. https://doi.org/10.1037/0033-2909.118.3.430
#' @param .data Data frame or the like containing the columns below
#' @param ax Column name that contains a person A's score on variable X
#' @param ay Column name that contains a person A's score on variable Y
#' @param bx Column name that contains a person B's score on variable X
#' @param by Column name that contains a person B's score on variable Y
#' @return Correlation, p value, and adjusted sample size (N star)
#' @export
#' 
ax.ay <- function(.data, ax, ay, bx, by) {
    if(!(ax %in% names(.data))) { warning(sprintf("Column ax (%s) not found in data frame.",ax)) }
    if(!(ay %in% names(.data))) { warning(sprintf("Column ay (%s) not found in data frame.",ay)) }
    if(!(bx %in% names(.data))) { warning(sprintf("Column bx (%s) not found in data frame.",bx)) }
    if(!(by %in% names(.data))) { warning(sprintf("Column by (%s) not found in data frame.",by)) }
    if (dplyr::is_grouped_df(.data)) {
      return(dplyr::do(.data, ax.ay(ax=ax,ay=ay,bx=bx,by=by)))
    }
    struct <- cor.structure(.data[[ax]],.data[[ay]],.data[[bx]],.data[[by]])
    N.star.1 <- (2*struct$N.dyads)/(1+ (struct$raxbx*struct$rayby) + struct$raxby**2) ## G&G1995 p.432
    Z <- struct$raxay*sqrt(N.star.1)  # G&G1995 p.432
    p <- z.to.p(Z)
    return(
        tibble::tibble(
            ax=ax,
            ay=ay,
            raxay=struct$raxay,
            N.dyads=struct$N.dyads,
            N.star.1=N.star.1,
            Z=Z,
            p=p
        )
    )
}




#' Compute correlation between a person's X value and their partner's Y value,
#' while controlling for dyadic dependence in the data.
#'
#' @description We start from a data table where we have scores of person A and B on variables X and Y. 
#' Data format requirement: all dyad members need to be present in the data
#' once as A and once as B. That is, each dyad corresponds to two, not one, row.
#' The question asked here is whether a person's X value predicts their own Y value.
#' This is within-dyad, but it takes into account potential dependence between
#' values of the same dyad.
#' Scientific Reference:
#' Griffin, D., & Gonzalez, R. (1995). Correlational analysis of dyad-level data in the exchangeable case. Psychological
#' Bulletin, 118(3), 430–439. https://doi.org/10.1037/0033-2909.118.3.430
#' @param .data Data frame or the like containing the columns below
#' @param ax Column name that contains a person A's score on variable X
#' @param ay Column name that contains a person A's score on variable Y
#' @param bx Column name that contains a person B's score on variable X
#' @param by Column name that contains a person B's score on variable Y
#' @return Correlation, p value, and adjusted sample size (N star)
#' @export
#' 
ax.by <- function(.data, ax, ay, bx, by) {
    if(!(ax %in% names(.data))) { warning(sprintf("Column ax (%s) not found in data frame.",ax)) }
    if(!(ay %in% names(.data))) { warning(sprintf("Column ay (%s) not found in data frame.",ay)) }
    if(!(bx %in% names(.data))) { warning(sprintf("Column bx (%s) not found in data frame.",bx)) }
    if(!(by %in% names(.data))) { warning(sprintf("Column by (%s) not found in data frame.",by)) }
    if (dplyr::is_grouped_df(.data)) {
      return(dplyr::do(.data, ax.by(ax=ax,ay=ay,bx=bx,by=by)))
    }
    struct <- cor.structure(.data[[ax]],.data[[ay]],.data[[bx]],.data[[by]])
    N.star.2 <- (2*struct$N.dyads)/(1+ (struct$raxbx*struct$rayby) + struct$raxay**2) # G&G1995 p.432
    Z <- struct$raxby*sqrt(N.star.2)
    p <- z.to.p(Z)
    return(
        tibble::tibble(
            ax=ax,
            by=by,
            raxby=struct$raxby,
            N.dyads=struct$N.dyads,
            N.star.2=N.star.2,
            Z=Z,
            p=p
        )
    )
}

