#' @title Diet and Condition Data
#' @description This data was derived from NEFSC bottom trawl data. It contains stomach content information and length-weight information.
#' @format A data frame with 1423676 rows and 90 variables:
#' \describe{
#'   \item{\code{obs}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{pynam}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{pyspp}}{double COLUMN_DESCRIPTION}
#'   \item{\code{pyabbr}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{modcat}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{gencat}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{gensci}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{analcat}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{analsci}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{collcat}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{collsci}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{station}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{pdsex}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{pdlen}}{double COLUMN_DESCRIPTION}
#'   \item{\code{pdid}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{fhmat}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{pdmat}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{pdwgt}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{pdsvol}}{double COLUMN_DESCRIPTION}
#'   \item{\code{pdswgt}}{double COLUMN_DESCRIPTION}
#'   \item{\code{fhdat}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{cruise}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{cruise6}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{svspp}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{catsex}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{cf}}{double COLUMN_DESCRIPTION}
#'   \item{\code{sizecat}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{len10cat}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{len5cat}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{pdgutw}}{double COLUMN_DESCRIPTION}
#'   \item{\code{pdgutv}}{double COLUMN_DESCRIPTION}
#'   \item{\code{stratum}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{ttime}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{towdur}}{double COLUMN_DESCRIPTION}
#'   \item{\code{setdepth}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{airtemp}}{double COLUMN_DESCRIPTION}
#'   \item{\code{surftemp}}{double COLUMN_DESCRIPTION}
#'   \item{\code{bottemp}}{double COLUMN_DESCRIPTION}
#'   \item{\code{svgear}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{tow}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{beglat}}{double COLUMN_DESCRIPTION}
#'   \item{\code{beglon}}{double COLUMN_DESCRIPTION}
#'   \item{\code{hour}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{minute}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{month}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{day}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{year}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{haul}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{gearcond}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{area}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{statype}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{towtime}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{purcode}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{status_code}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{season}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{yr2block}}{double COLUMN_DESCRIPTION}
#'   \item{\code{yr3block}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{yr5block}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{decade}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{garea}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{geoarea}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{declat}}{double COLUMN_DESCRIPTION}
#'   \item{\code{declon}}{double COLUMN_DESCRIPTION}
#'   \item{\code{catwgt}}{double COLUMN_DESCRIPTION}
#'   \item{\code{catnum}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{numlen}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{pynum}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{pyperi}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{pywgti}}{double COLUMN_DESCRIPTION}
#'   \item{\code{pyvoli}}{double COLUMN_DESCRIPTION}
#'   \item{\code{pylen}}{logical COLUMN_DESCRIPTION}
#'   \item{\code{pyamtw}}{double COLUMN_DESCRIPTION}
#'   \item{\code{pyamtv}}{double COLUMN_DESCRIPTION}
#'   \item{\code{perpyv}}{double COLUMN_DESCRIPTION}
#'   \item{\code{perpyw}}{double COLUMN_DESCRIPTION}
#'   \item{\code{pdscinam}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{pdcomnam}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{tot_tows_spp_stratum}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{tot_catnum_stratum}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{tot_catwgt_stratum}}{double COLUMN_DESCRIPTION}
#'   \item{\code{stratum_area}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{pycomnam2}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{gencom2}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{analcom3}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{collcom3}}{logical COLUMN_DESCRIPTION}
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION}
#'   \item{\code{spst}}{character COLUMN_DESCRIPTION}
#'   \item{\code{stock_area}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Region}}{character COLUMN_DESCRIPTION}
#'   \item{\code{fish_id}}{character COLUMN_DESCRIPTION} 
#'}
#'
#' @source \url{https://github.com/Laurels1/Condition/blob/master/data/allfh.RData}
#' @details DETAILS
"allfh"

#' @title Stock Assessment Data
#' @description This data set contains stock assessment data on several US stocks.
#' @format A data frame with 35636 rows and 10 variables:
#' \describe{
#'   \item{\code{Species}}{character The species' common name}
#'   \item{\code{Region}}{character The stock region}
#'   \item{\code{Year}}{double The year of the value estimate}
#'   \item{\code{Value}}{double The value of the metric}
#'   \item{\code{Metric}}{character The stock assessment metric (catch, F, abundance, or recruitment)}
#'   \item{\code{Description}}{character Details about the metric}
#'   \item{\code{Units}}{character The units of the metric}
#'   \item{\code{AssessmentYear}}{double The year that the assessment was conducted}
#'   \item{\code{Age}}{character The age of the fish (recruitment/abundance only)}
#'   \item{\code{Category}}{character COLUMN_DESCRIPTION} 
#'}
#' @source \url{https://github.com/NOAA-EDAB/assessmentdata}
#' @details DETAILS
"asmt"

#' @title Stock Assessment Summary Data
#' @description This data set contains summary stock assessment metrics on several US stocks.
#' @format A data frame with 337 rows and 61 variables:
#' \describe{
#'   \item{\code{Stock Name}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Jurisdiction}}{character COLUMN_DESCRIPTION}
#'   \item{\code{FMP}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Science Center}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Regional Ecosystem}}{character COLUMN_DESCRIPTION}
#'   \item{\code{FSSI Stock?}}{character COLUMN_DESCRIPTION}
#'   \item{\code{ITIS Taxon Serial Number}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Scientific Name}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Common Name}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Stock Area}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Assessment Year}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Assessment Month}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Last Data Year}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Update Type}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Review Result}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Assessment Model}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Model Version}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Lead Lab}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Citation}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Final Assessment Report 1}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Final Assessment Report 2}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Point of Contact}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Life History Data}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Abundance Data}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Catch Data}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Assessment Level}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Assessment Frequency}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Assessment Type}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Model Category}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Catch Input Data}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Abundance Input Data}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Biological Input Data}}{logical COLUMN_DESCRIPTION}
#'   \item{\code{Ecosystem Linkage}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Composition Input Data}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{F Year}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Estimated F}}{double COLUMN_DESCRIPTION}
#'   \item{\code{F Unit}}{character COLUMN_DESCRIPTION}
#'   \item{\code{F Basis}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Flimit}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Flimit Basis}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Fmsy}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Fmsy Basis}}{character COLUMN_DESCRIPTION}
#'   \item{\code{F/Flimit}}{double COLUMN_DESCRIPTION}
#'   \item{\code{F/Fmsy}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Ftarget}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Ftarget Basis}}{character COLUMN_DESCRIPTION}
#'   \item{\code{F/Ftarget}}{double COLUMN_DESCRIPTION}
#'   \item{\code{B Year}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Estimated B}}{double COLUMN_DESCRIPTION}
#'   \item{\code{B Unit}}{character COLUMN_DESCRIPTION}
#'   \item{\code{B Basis}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Blimit}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Blimit Basis}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Bmsy}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Bmsy Basis}}{character COLUMN_DESCRIPTION}
#'   \item{\code{B/Blimit}}{double COLUMN_DESCRIPTION}
#'   \item{\code{B/Bmsy}}{double COLUMN_DESCRIPTION}
#'   \item{\code{MSY}}{double COLUMN_DESCRIPTION}
#'   \item{\code{MSY Unit}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Region}}{character COLUMN_DESCRIPTION} 
#'}
#' @source \url{https://github.com/NOAA-EDAB/assessmentdata}
#' @details DETAILS
"asmt_sum"

#' @title NEFSC Bottom Trawl Data with Bio Data
#' @description This data is NEFSC bottom trawl data with bio data.
#' @format A data frame with 1103575 rows and 28 variables:
#' \describe{
#'   \item{\code{CRUISE6}}{character COLUMN_DESCRIPTION}
#'   \item{\code{STATION}}{character COLUMN_DESCRIPTION}
#'   \item{\code{STRATUM}}{character COLUMN_DESCRIPTION}
#'   \item{\code{SVSPP}}{character COLUMN_DESCRIPTION}
#'   \item{\code{CATCHSEX}}{character COLUMN_DESCRIPTION}
#'   \item{\code{LENGTH}}{double COLUMN_DESCRIPTION}
#'   \item{\code{TOW}}{character COLUMN_DESCRIPTION}
#'   \item{\code{SVVESSEL}}{character COLUMN_DESCRIPTION}
#'   \item{\code{YEAR}}{character COLUMN_DESCRIPTION}
#'   \item{\code{SEASON}}{character COLUMN_DESCRIPTION}
#'   \item{\code{LAT}}{double COLUMN_DESCRIPTION}
#'   \item{\code{LON}}{double COLUMN_DESCRIPTION}
#'   \item{\code{EST_TOWDATE}}{double COLUMN_DESCRIPTION}
#'   \item{\code{DEPTH}}{double COLUMN_DESCRIPTION}
#'   \item{\code{SURFTEMP}}{double COLUMN_DESCRIPTION}
#'   \item{\code{SURFSALIN}}{double COLUMN_DESCRIPTION}
#'   \item{\code{BOTTEMP}}{double COLUMN_DESCRIPTION}
#'   \item{\code{BOTSALIN}}{double COLUMN_DESCRIPTION}
#'   \item{\code{ABUNDANCE}}{double COLUMN_DESCRIPTION}
#'   \item{\code{BIOMASS}}{double COLUMN_DESCRIPTION}
#'   \item{\code{NUMLEN}}{double COLUMN_DESCRIPTION}
#'   \item{\code{INDID}}{double COLUMN_DESCRIPTION}
#'   \item{\code{INDWT}}{double COLUMN_DESCRIPTION}
#'   \item{\code{SEX}}{character COLUMN_DESCRIPTION}
#'   \item{\code{MATURITY}}{character COLUMN_DESCRIPTION}
#'   \item{\code{AGE}}{double COLUMN_DESCRIPTION}
#'   \item{\code{STOM_VOLUME}}{double COLUMN_DESCRIPTION}
#'   \item{\code{STOM_WGT}}{double COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"bio_survey"

#' @title Overall Climate Risk
#' @description This is the overall climate risk score data from Hare et al. 2016.
#' @format A data frame with 82 rows and 3 variables:
#' \describe{
#'   \item{\code{Species}}{character Species' common name}
#'   \item{\code{Overall_climate_vulnerability}}{character Overall climate vulnerability score}
#'   \item{\code{Certainty}}{character Certainty score} 
#'}
#' @source \url{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0146756}
#' @details DETAILS
"climate_risk"

#' @title Climate Vulnerability
#' @description This is the climate vulnerability data from Hare et al. 2016.
#' @format A data frame with 1968 rows and 8 variables:
#' \describe{
#'   \item{\code{Species}}{character Species' common name}
#'   \item{\code{Functional.Group}}{character Species guild}
#'   \item{\code{Attribute}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Attribute.Category}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Low}}{integer Number of experts ranking attribute risk as low}
#'   \item{\code{Moderate}}{integer Number of experts ranking attribute risk as moderate}
#'   \item{\code{High}}{integer Number of experts ranking attribute risk as high}
#'   \item{\code{Very.High}}{integer Number of experts ranking attribute risk as very high} 
#'}
#' @source \url{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0146756}
#' @details DETAILS
"climate_vulnerability"

#' @title Commercial Catch and Revenue Data
#' @description DATASET_DESCRIPTION
#' @format A data frame with 31637 rows and 9 variables:
#' \describe{
#'   \item{\code{X}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Year}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{State}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Pounds}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Dollars}}{double COLUMN_DESCRIPTION}
#'   \item{\code{rowname}}{character COLUMN_DESCRIPTION}
#'   \item{\code{CPIAUCSL}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Dollars_adj}}{double COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"com_catch"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 11 rows and 2 variables:
#' \describe{
#'   \item{\code{state_id}}{character COLUMN_DESCRIPTION}
#'   \item{\code{color}}{character COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"com_palette"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 3595 rows and 7 variables:
#' \describe{
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION}
#'   \item{\code{EPU}}{character COLUMN_DESCRIPTION}
#'   \item{\code{sex}}{character COLUMN_DESCRIPTION}
#'   \item{\code{YEAR}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{MeanCond}}{double COLUMN_DESCRIPTION}
#'   \item{\code{nCond}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{n}}{integer COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"cond"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 36 rows and 4 variables:
#' \describe{
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Scientific_name}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Guild}}{character COLUMN_DESCRIPTION}
#'   \item{\code{size}}{character COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"guild_info"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 200 rows and 14 variables:
#' \describe{
#'   \item{\code{Guild}}{character COLUMN_DESCRIPTION}
#'   \item{\code{size}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Indicator}}{character COLUMN_DESCRIPTION}
#'   \item{\code{category}}{character COLUMN_DESCRIPTION}
#'   \item{\code{guild_risk}}{double COLUMN_DESCRIPTION}
#'   \item{\code{n_guild_species}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{n_guild_species_measured}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{avg_guild_risk}}{double COLUMN_DESCRIPTION}
#'   \item{\code{rank}}{double COLUMN_DESCRIPTION}
#'   \item{\code{norm_rank}}{double COLUMN_DESCRIPTION}
#'   \item{\code{total_guild_risk}}{double COLUMN_DESCRIPTION}
#'   \item{\code{sum_ranks}}{double COLUMN_DESCRIPTION}
#'   \item{\code{legend_label}}{character COLUMN_DESCRIPTION}
#'   \item{\code{label_y}}{double COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"guild_risk"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 24 rows and 5 variables:
#' \describe{
#'   \item{\code{Category}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Indicator}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Data_source}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Metric}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Risk}}{character COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"indicator_info"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 3391 rows and 9 variables:
#' \describe{
#'   \item{\code{X}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{stock_season}}{character COLUMN_DESCRIPTION}
#'   \item{\code{sixcode}}{character COLUMN_DESCRIPTION}
#'   \item{\code{stock_area}}{character COLUMN_DESCRIPTION}
#'   \item{\code{strata}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{SVSPP}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{COMNAME}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Region}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"latlong"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 43 rows and 7 variables:
#' \describe{
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Region}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Indicator}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Year}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Value}}{double COLUMN_DESCRIPTION}
#'   \item{\code{rank}}{double COLUMN_DESCRIPTION}
#'   \item{\code{norm_rank}}{double COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"nrcc"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 19 rows and 2 variables:
#' \describe{
#'   \item{\code{prey_id}}{character COLUMN_DESCRIPTION}
#'   \item{\code{color}}{character COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"prey_palette"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 63710 rows and 31 variables:
#' \describe{
#'   \item{\code{X}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{status}}{character COLUMN_DESCRIPTION}
#'   \item{\code{year}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{wave}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{wave_f}}{character COLUMN_DESCRIPTION}
#'   \item{\code{sub_reg}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{sub_reg_f}}{character COLUMN_DESCRIPTION}
#'   \item{\code{st}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{st_f}}{character COLUMN_DESCRIPTION}
#'   \item{\code{sp_code}}{double COLUMN_DESCRIPTION}
#'   \item{\code{common}}{character COLUMN_DESCRIPTION}
#'   \item{\code{mode_fx}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{mode_fx_f}}{character COLUMN_DESCRIPTION}
#'   \item{\code{area_x}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{area_x_f}}{character COLUMN_DESCRIPTION}
#'   \item{\code{estclaim}}{character COLUMN_DESCRIPTION}
#'   \item{\code{estclvar}}{character COLUMN_DESCRIPTION}
#'   \item{\code{estharv}}{character COLUMN_DESCRIPTION}
#'   \item{\code{esthvar}}{character COLUMN_DESCRIPTION}
#'   \item{\code{landing}}{character COLUMN_DESCRIPTION}
#'   \item{\code{land_var}}{character COLUMN_DESCRIPTION}
#'   \item{\code{estrel}}{character COLUMN_DESCRIPTION}
#'   \item{\code{estrlvar}}{character COLUMN_DESCRIPTION}
#'   \item{\code{tot_cat}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{lbs_ab1}}{double COLUMN_DESCRIPTION}
#'   \item{\code{var_lbs}}{character COLUMN_DESCRIPTION}
#'   \item{\code{wgt_ab1}}{character COLUMN_DESCRIPTION}
#'   \item{\code{var_wab1}}{character COLUMN_DESCRIPTION}
#'   \item{\code{miss_fish}}{character COLUMN_DESCRIPTION}
#'   \item{\code{ALT_FLAG}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"rec_catch"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 5 rows and 2 variables:
#' \describe{
#'   \item{\code{rec_mode}}{character COLUMN_DESCRIPTION}
#'   \item{\code{color}}{character COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"rec_palette"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 61 rows and 3 variables:
#' \describe{
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Region}}{character COLUMN_DESCRIPTION}
#'   \item{\code{EPU}}{character COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"regression_species_regions"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 1375 rows and 15 variables:
#' \describe{
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Region}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Indicator}}{character COLUMN_DESCRIPTION}
#'   \item{\code{category}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Year}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Value}}{double COLUMN_DESCRIPTION}
#'   \item{\code{rank}}{character COLUMN_DESCRIPTION}
#'   \item{\code{n_stocks_per_indicator}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{n_stocks_per_region}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{norm_rank}}{double COLUMN_DESCRIPTION}
#'   \item{\code{total_risk}}{double COLUMN_DESCRIPTION}
#'   \item{\code{overall_rank}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{overall_stocks}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{stock}}{character COLUMN_DESCRIPTION}
#'   \item{\code{label}}{character COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"risk"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 50880 rows and 12 variables:
#' \describe{
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Region}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Indicator}}{character COLUMN_DESCRIPTION}
#'   \item{\code{category}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Year}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Value}}{double COLUMN_DESCRIPTION}
#'   \item{\code{rank}}{double COLUMN_DESCRIPTION}
#'   \item{\code{n_years_per_indicator}}{double COLUMN_DESCRIPTION}
#'   \item{\code{n_stocks_per_region}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{norm_rank}}{double COLUMN_DESCRIPTION}
#'   \item{\code{total_risk}}{double COLUMN_DESCRIPTION}
#'   \item{\code{overall_rank}}{integer COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"risk_species"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 41658 rows and 15 variables:
#' \describe{
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Region}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Indicator}}{character COLUMN_DESCRIPTION}
#'   \item{\code{category}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Year}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Value}}{double COLUMN_DESCRIPTION}
#'   \item{\code{rank}}{double COLUMN_DESCRIPTION}
#'   \item{\code{n_stocks_per_indicator}}{double COLUMN_DESCRIPTION}
#'   \item{\code{n_stocks_per_region}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{norm_rank}}{double COLUMN_DESCRIPTION}
#'   \item{\code{total_risk}}{double COLUMN_DESCRIPTION}
#'   \item{\code{overall_rank}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{overall_stocks}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{stock}}{character COLUMN_DESCRIPTION}
#'   \item{\code{label}}{character COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"risk_year_hist"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 42559 rows and 15 variables:
#' \describe{
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Region}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Indicator}}{character COLUMN_DESCRIPTION}
#'   \item{\code{category}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Year}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Value}}{double COLUMN_DESCRIPTION}
#'   \item{\code{rank}}{double COLUMN_DESCRIPTION}
#'   \item{\code{n_stocks_per_indicator}}{double COLUMN_DESCRIPTION}
#'   \item{\code{n_stocks_per_region}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{norm_rank}}{double COLUMN_DESCRIPTION}
#'   \item{\code{total_risk}}{double COLUMN_DESCRIPTION}
#'   \item{\code{overall_rank}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{overall_stocks}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{stock}}{character COLUMN_DESCRIPTION}
#'   \item{\code{label}}{character COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"risk_year_value"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 185 rows and 8 variables:
#' \describe{
#'   \item{\code{FINSTR_ID}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{STRATA}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{A2}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{STR2}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{SET_}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{STRATUMA}}{character COLUMN_DESCRIPTION}
#'   \item{\code{STR3}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{geometry}}{list COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"shape"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 37 rows and 3 variables:
#' \describe{
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Scientific_name}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Guild}}{character COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"species_guilds"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 37 rows and 2 variables:
#' \describe{
#'   \item{\code{SVSPP}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"species_key"

#' @title NEFSC Bottom Trawl Data
#' @description DATASET_DESCRIPTION
#' @format A data frame with 3937953 rows and 27 variables:
#' \describe{
#'   \item{\code{CRUISE6}}{double COLUMN_DESCRIPTION}
#'   \item{\code{STATION}}{double COLUMN_DESCRIPTION}
#'   \item{\code{STRATUM}}{double COLUMN_DESCRIPTION}
#'   \item{\code{TOW}}{double COLUMN_DESCRIPTION}
#'   \item{\code{SVSPP}}{double COLUMN_DESCRIPTION}
#'   \item{\code{CATCHSEX}}{double COLUMN_DESCRIPTION}
#'   \item{\code{SVVESSEL}}{character COLUMN_DESCRIPTION}
#'   \item{\code{YEAR}}{double COLUMN_DESCRIPTION}
#'   \item{\code{SEASON}}{character COLUMN_DESCRIPTION}
#'   \item{\code{LAT}}{double COLUMN_DESCRIPTION}
#'   \item{\code{LON}}{double COLUMN_DESCRIPTION}
#'   \item{\code{EST_TOWDATE}}{double COLUMN_DESCRIPTION}
#'   \item{\code{DEPTH}}{double COLUMN_DESCRIPTION}
#'   \item{\code{SURFTEMP}}{double COLUMN_DESCRIPTION}
#'   \item{\code{SURFSALIN}}{double COLUMN_DESCRIPTION}
#'   \item{\code{BOTTEMP}}{double COLUMN_DESCRIPTION}
#'   \item{\code{BOTSALIN}}{double COLUMN_DESCRIPTION}
#'   \item{\code{ABUNDANCE}}{double COLUMN_DESCRIPTION}
#'   \item{\code{BIOMASS}}{double COLUMN_DESCRIPTION}
#'   \item{\code{LENGTH}}{double COLUMN_DESCRIPTION}
#'   \item{\code{NUMLEN}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION}
#'   \item{\code{spst}}{character COLUMN_DESCRIPTION}
#'   \item{\code{stock_area}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Region}}{character COLUMN_DESCRIPTION}
#'   \item{\code{date}}{character COLUMN_DESCRIPTION}
#'   \item{\code{fish_id}}{character COLUMN_DESCRIPTION} 
#'}
#' @source Oracle query of `svdbs` using the `survdat` package. 
#' @details \itemize{
#'     \item Function call: survdat::get_survdat_data(channel = channel, filterByYear = NA, all.season = TRUE, shg.check = T, conversion.factor = T, use.SAD = F, getBio = FALSE, getLengths = T)
#'     \item Pull date: Wed Mar 03 10:56:23 2021
#' }
"survey"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 4009 rows and 19 variables:
#' \describe{
#'   \item{\code{X.1}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{X}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{YEAR}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{SVSPP}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{N}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{strat.biomass}}{double COLUMN_DESCRIPTION}
#'   \item{\code{biomass.var}}{double COLUMN_DESCRIPTION}
#'   \item{\code{biomass.SE}}{double COLUMN_DESCRIPTION}
#'   \item{\code{strat.abund}}{double COLUMN_DESCRIPTION}
#'   \item{\code{abund.var}}{double COLUMN_DESCRIPTION}
#'   \item{\code{abund.SE}}{double COLUMN_DESCRIPTION}
#'   \item{\code{tot.biomass}}{double COLUMN_DESCRIPTION}
#'   \item{\code{tot.bio.var}}{double COLUMN_DESCRIPTION}
#'   \item{\code{tot.bio.SE}}{double COLUMN_DESCRIPTION}
#'   \item{\code{tot.abundance}}{double COLUMN_DESCRIPTION}
#'   \item{\code{tot.abund.var}}{double COLUMN_DESCRIPTION}
#'   \item{\code{tot.abund.SE}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Season}}{character COLUMN_DESCRIPTION} 
#'}
#' @details DETAILS
"swept"