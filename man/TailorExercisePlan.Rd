% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TailorExercisePlan.R
\name{TailorExercisePlan}
\alias{TailorExercisePlan}
\title{Tailor Exercise Plan for T2D Patients}
\usage{
TailorExercisePlan(input_df)
}
\arguments{
\item{input_df}{A data frame containing patient data necessary for
generating a tailored exercise plan. Each column in the dataframe should
be as follows (All data must be numeric.):
\describe{
\item{Age}{Patient's age in years.}
\item{Sex}{Patient's sex, where 0 might denote female and 1 denotes male.}
\item{BMI}{Body Mass Index, a measure of body fat based on height and weight.}
\item{Duration_T2D (year)}{Duration of Type 2 Diabetes Mellitus in years.
It reflects how long the patient has been living with Type 2 diabetes.}
\item{HDL (mmol/L)}{High-Density Lipoprotein cholesterol levels in mmol/L.
HDL is considered the "good" cholesterol, and higher levels are often associated
with a lower risk of heart disease.}
\item{PCS}{Patient's Physical Component Summary (PCS) score,
a metric derived from the SF-12 (Short Form Health Survey) which measures health-related quality of life.
The PCS score is calculated based on responses to questions in the SF-12 survey,
reflecting physical functioning, bodily pain, general health perceptions, and physical roles.
A higher score indicates better physical health. Users can refer to the \code{PCS_Calculation}
function within this package for detailed calculations based on SF-12 survey responses.}
\item{WHtR}{Patient's ratio of waist and height.}
}
Note: It is crucial that the data for each of these columns is correctly formatted
and accurately represents the patient's health information for the exercise
plan to be effectively tailored.}
}
\value{
A list containing two data frames: \verb{$RecommendedExercisePlan}: This data frame includes exercise plans that
are recommended based on the criterion that the intervention leads to a positive reduction in HbA1c levels,
and \verb{$AllExercisePlan} which includes all received plans.
}
\description{
This function generates a tailored exercise plan for T2D (Type 2 Diabetes)
}
\examples{
# Create a demo dataframe
set.seed(16)
df <- data.frame(
Age = sample(39:77, 8, replace = TRUE),
Sex = sample(0:1, 8, replace = TRUE),
BMI = sample(18:31, 8, replace = TRUE),
Duration_T2D = sample(1:26, 8, replace = TRUE),
HDL = sample(1:1.7, 8, replace = TRUE),
PCS = sample(27:54, 8, replace = TRUE),
WHtR = sample(0.4:0.6, 8, replace = TRUE)
)
rownames(df) <- c("xiaoming", "xiaohong", "xiaohua", "xiaogang",
                  "xiaoli", "zhangsan", "lisi", "wangwu")
colnames(df) <- c("Age", "Sex", "BMI", "Duration_T2D (year)",
                  "HDL (mmol/L)", "PCS", "WHtR")
# Run the TailorExercisePlan function
demo_result <- TailorExercisePlan(df)
# View the structure of the returned list
str(demo_result)

}
