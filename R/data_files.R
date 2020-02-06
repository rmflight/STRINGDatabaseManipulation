#' STRING V10 detailed links for human
#'
#' Dataset containing the detailed links from v11 of the STRING database for
#' human. combined_score >= 400, and 10000 randomly chosen links. THIS IS NOT
#' THE COMPLETE DATASET!
#'
#' @format A data.frame with 10000 rows and 10 variables:
#' \describe{
#'   \item{protein1}{first node in edge}
#'   \item{protein2}{second node in edge}
#'   \item{neighborhood}{evidence score for neighborhood}
#'   \item{fusion}{evidence score for fusion}
#'   \item{coocucurence}{evidence score for cooccurence}
#'   \item{coexpression}{evidence score from coexpression}
#'   \item{experimental}{score for experimental evidence}
#'   \item{database}{score for database evidence}
#'   \item{textmining}{score from textmining}
#'   \item{combined_score}{total combined score from all evidences}}
#' @source \url{https://stringdb-static.org/download/protein.links.detailed.v11.0/9606.protein.links.detailed.v11.0.txt.gz}
#' Processed by RMF
"STRING11_9606_links"

#' STRING V10 aliases for human
#'
#' Dataset containing various STRING aliases.
#'
#' @format A data.frame with 2449433 rows and 3 variables:
#' \describe{
#'   \item{string}{the STRING ID}
#'   \item{other}{the other alias}
#'   \item{type}{the source of the alias (space separated for multiple sources)}}
#'
#' @source \url{https://stringdb-static.org/download/protein.aliases.v11.0/9606.protein.aliases.v11.0.txt.gz}
#' Processed by RMF
"STRING11_9606_aliases"
