#' SOLID Study (Anthropometry and Blood Pressure)
#'
#' Data on 2,609 observations on lung function in healthy Indians.
#' Useful data to illustrate multicollinearity.
#' Data distributed under a CC-BY license.
#' Credit to Mohit Aggarwal and Anurag Agrawal for the data.
#'
#' @format A data frame with 2,609 rows and 14 variables:
#' \describe{
#'   \item{sex}{Character variable giving Male or Female}
#'   \item{district}{Character variable}
#'   \item{age_years}{Numeric, age in years}
#'   \item{height_cm}{Height in cm}
#'   \item{weight_kg}{Weight in kg}
#'   \item{bmi}{Body mass index, numeric, three NA values}
#'   \item{waist_circumference_cm}{Waist circumference in cm}
#'   \item{systolic_pressure}{Systolic blood pressure}
#'   \item{waist_height_ratio}{Ratio between waist and heigh}
#'   \item{zhfa_who}{Numeric variable}
#'   \item{zbfa_who}{Numeric variable}
#'   \item{hypertension_nhlbi}{Character variable giving hypertension status}
#'   \item{zbfa_present}{Numeric variable}
#'   \item{zwfa_present}{Numeric variable}
#' }
#' @source \url{https://figshare.com/articles/dataset/SOLID_anthropometry_csv/13150844/4}
"solid"

#' Hypothetical data set from a cohort study examining the association between smoking and lung cancer
#'
#' Data from Kirkwood and Sterne 2nd Edition, p154
#' @format Line list data with 90,000 rows and two variables
#' \describe{
#'  \item{smoker}{Integer; 1 for smoker, 0 for non-smoker}
#'  \item{lung_cancer}{Integer; 1 for lung cancer, 0 for no lung cancer}
#' }
#' @source Kirkwood, Betty R., and Jonathan AC Sterne. Essential medical statistics. John Wiley & Sons, 2010.
"smokers"
