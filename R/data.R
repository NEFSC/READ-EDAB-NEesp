#' @title Diet and Condition Data
#' @description This data was derived from NEFSC bottom trawl data. It contains stomach content information and length-weight information.
#' @format A data frame with 1423676 rows and 16 variables:
#' \describe{
#'   \item{\code{pynam}}{character Prey scientific name}
#'   \item{\code{gencat}}{character Prey general category (shortened)}
#'   \item{\code{gensci}}{character Prey general category (full)}
#'   \item{\code{analcat}}{character Prey analytical category (shortened)}
#'   \item{\code{analsci}}{character Prey analytical category (full)}
#'   \item{\code{pdlen}}{double Predtaor length}
#'   \item{\code{pdwgt}}{integer Predator weight}
#'   \item{\code{stratum}}{integer Stratum}
#'   \item{\code{surftemp}}{double Surface water temperature}
#'   \item{\code{bottemp}}{double Bottom water temperature}
#'   \item{\code{year}}{integer Year}
#'   \item{\code{season}}{integer Season}
#'   \item{\code{pyamtw}}{double Prey weight}
#'   \item{\code{Species}}{character Predator species}
#'   \item{\code{Region}}{character Predator region}
#'   \item{\code{fish_id}}{character Predator ID number}
#' }
#'
#' @source \url{https://github.com/Laurels1/Condition/blob/master/data/allfh.RData}
#' @details DETAILS
#' "allfh"

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
#' }
#' @source \url{https://github.com/NOAA-EDAB/assessmentdata}
#' @details DETAILS
"asmt"

#' @title Stock Assessment Summary Data
#' @description This data set contains summary stock assessment metrics on several US stocks.
#' @format A data frame with 337 rows and 12 variables:
#' \describe{

#'   \item{\code{FSSI Stock?}}{character Is the stock included in the Fish Stock Sustainability Index?}
#'   \item{\code{Assessment Year}}{integer Assessment year}
#'   \item{\code{Last Data Year}}{integer Last data year used in assessment}
#'   \item{\code{Review Result}}{character Results of assessment review by the Council}
#'   \item{\code{Life History Data}}{integer Biological data rating (before 2019): 0 = no data ; 4 = sufficient data for informed modeling}
#'   \item{\code{Biological Input Data}}{logical Biological data rating (2019 onwards): 0 = no data ; 5 = detailed data with no gaps}
#'   \item{\code{Ecosystem Linkage}}{integer Use of ecosystem linkage data in stock assessment model: 0 = not used ; 5 = model is linked to ecosystem processes}
#'   \item{\code{Composition Input Data}}{integer Availability of size/age data for the stock assessment model: 0 = none ; 5 = complete data with spatial resolution}
#'   \item{\code{F/Fmsy}}{double Fishing pressure relative to the fishing pressure for maximum sustainable yield}
#'   \item{\code{B/Bmsy}}{double Biomass relative to the biomass for maximum sustainable yield}
#'   \item{\code{Species}}{character Species common name}
#'   \item{\code{Region}}{character Stock region}
#' }
#' @source \url{https://github.com/NOAA-EDAB/assessmentdata}
#' @details For more details see \url{https://www.st.nmfs.noaa.gov/stocksmart/StockSMART_DataDictionary.pdf}
#' "asmt_sum"

#' @title NEFSC Bottom Trawl Data with Bio Data
#' @description This data is NEFSC bottom trawl data with bio data.
#' @format A data frame with 1103575 rows and 28 variables:
#' \describe{
#'   \item{\code{CRUISE6}}{character Cruise ID}
#'   \item{\code{STATION}}{character Station ID}
#'   \item{\code{STRATUM}}{character Stratum}
#'   \item{\code{SVSPP}}{character Northeast Resource Surveys Species code}
#'   \item{\code{CATCHSEX}}{character Species-specific numeric sex code}
#'   \item{\code{LENGTH}}{double Length}
#'   \item{\code{TOW}}{character Tow ID}
#'   \item{\code{SVVESSEL}}{character Vessel ID}
#'   \item{\code{YEAR}}{double Year}
#'   \item{\code{SEASON}}{character Season}
#'   \item{\code{LAT}}{double Latitude}
#'   \item{\code{LON}}{double Longitude}
#'   \item{\code{EST_TOWDATE}}{double Date with estimated time}
#'   \item{\code{DEPTH}}{double Depth}
#'   \item{\code{SURFTEMP}}{double Surface water temperature}
#'   \item{\code{SURFSALIN}}{double Surface salinity}
#'   \item{\code{BOTTEMP}}{double Bottom water temperature}
#'   \item{\code{BOTSALIN}}{double Bottom salinity}
#'   \item{\code{ABUNDANCE}}{double Abundance (number of fish caught on a single tow; redundant for rows with the same CRUISE6, STATION, and TOW)}
#'   \item{\code{BIOMASS}}{double Biomass (biomass caught on a single tow; redundant for rows with the same SVSPP, CRUISE6, STATION, and TOW)}
#'   \item{\code{NUMLEN}}{double Number of fish at the specified length (redundant for rows with the same SVSPP, CRUISE6, STATION, TOW, and LENGTH)}
#'   \item{\code{INDID}}{double Individual fish ID; unknown if actually unique}
#'   \item{\code{INDWT}}{double Individual fish weight}
#'   \item{\code{SEX}}{character Sex}
#'   \item{\code{MATURITY}}{character Maturity}
#'   \item{\code{AGE}}{double Age}
#'   \item{\code{STOM_VOLUME}}{double Stomach volume}
#'   \item{\code{STOM_WGT}}{double Stomach weight}
#' }
#' @details \itemize{
#'     \item Function call: survdat::get_survdat_data(channel = channel, filterByYear = NA, all.season = TRUE, shg.check = T, conversion.factor = T, use.SAD = F, getBio = TRUE, getLengths = T)
#'     \item Pull date: Tue Mar 02 16:12:40 2021
#' }
"bio_survey"

#' @title Overall Climate Risk
#' @description This is the overall climate risk score data from Hare et al. 2016.
#' @format A data frame with 82 rows and 3 variables:
#' \describe{
#'   \item{\code{Species}}{character Species' common name}
#'   \item{\code{Overall_climate_vulnerability}}{character Overall climate vulnerability score}
#'   \item{\code{Certainty}}{character Certainty score}
#' }
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
#' }
#' @source \url{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0146756}
#' @details DETAILS
"climate_vulnerability"

#' @title Commercial Catch and Revenue Data
#' @description DATASET_DESCRIPTION
#' @format A data frame with 31637 rows and 9 variables:
#' \describe{
#'   \item{\code{Species}}{character Species}
#'   \item{\code{Year}}{integer Year}
#'   \item{\code{State}}{character State}
#'   \item{\code{Pounds}}{double Pounds landed}
#'   \item{\code{Dollars}}{double Dollars}
#'   \item{\code{Dollars_adj}}{double Dollars (2019 value)}
#' }
#' @details DETAILS
"com_catch"

#' @title Commercial landings data color palette
#' @description A color palette to use with commercial data
#' @format A data frame with 11 rows and 2 variables:
#' \describe{
#'   \item{\code{state_id}}{character State}
#'   \item{\code{color}}{character Color (hex code)}
#' }
#' @details Created from `nmfspalette` package
"com_palette"

#' @title Condition data
#' @description DATASET_DESCRIPTION
#' @format A data frame with 3595 rows and 7 variables:
#' \describe{
#'   \item{\code{Species}}{character Species}
#'   \item{\code{EPU}}{character Environmental Protection Unit}
#'   \item{\code{sex}}{character Sex}
#'   \item{\code{YEAR}}{integer Year}
#'   \item{\code{MeanCond}}{double Mean condition}
#'   \item{\code{nCond}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{n}}{integer COLUMN_DESCRIPTION}
#' }
#' @source 2018 data from: \url{https://github.com/Laurels1/Condition}
#' @details DETAILS
"cond"

#' @title Functional guild information
#' @description Functional guild information
#' @format A data frame with 36 rows and 4 variables:
#' \describe{
#'   \item{\code{Species}}{character Species}
#'   \item{\code{Scientific_name}}{character Scientific name}
#'   \item{\code{Guild}}{character Guild}
#'   \item{\code{size}}{character Size (<40cm = small)}
#' }
#' @details DETAILS
"guild_info"

#' @title Guild risk
#' @description Guild risk, determined by relative rankings
#' @format A data frame with 200 rows and 14 variables:
#' \describe{
#'   \item{\code{Guild}}{character Guild}
#'   \item{\code{size}}{character Size}
#'   \item{\code{Indicator}}{character Indicator}
#'   \item{\code{category}}{character Indicator category}
#'   \item{\code{guild_risk}}{double Sum of risk of all species in the guild for the indicator}
#'   \item{\code{n_guild_species}}{integer Number of species in the guild}
#'   \item{\code{n_guild_species_measured}}{integer Number of species in the guild with indicator measurements}
#'   \item{\code{avg_guild_risk}}{double Average risk of species in the guild}
#'   \item{\code{rank}}{double Rank of the guild, relative to other guilds (ranked by average guild risk)}
#'   \item{\code{norm_rank}}{double Normalized rank}
#'   \item{\code{total_guild_risk}}{double Total guild risk (summed over all indicators)}
#'   \item{\code{sum_ranks}}{double Guild rank summed over all indicators}
#'   \item{\code{legend_label}}{character Legend label (for plotting)}
#'   \item{\code{label_y}}{double Position of y text (for plotting)}
#' }
#' @details DETAILS
"guild_risk"

#' @title Indicator risk key
#' @description Indicator risk key
#' @format A data frame with 24 rows and 5 variables:
#' \describe{
#'   \item{\code{Category}}{character Indicator category}
#'   \item{\code{Indicator}}{character Indicator}
#'   \item{\code{Data_source}}{character Data source}
#'   \item{\code{Metric}}{character How the indicator was processed for risk assessment}
#'   \item{\code{Risk}}{character How risk was assigned}
#' }
#' @details DETAILS
"indicator_info"

#' @title Geographic distribution
#' @description Seasonal strata information
#' @format A data frame with 3391 rows and 5 variables:
#' \describe{
#'   \item{\code{stock_season}}{character Season}
#'   \item{\code{strata}}{integer Stratum}
#'   \item{\code{SVSPP}}{integer Northeast Resource Surveys Species code}
#'   \item{\code{Region}}{character Stock region}
#'   \item{\code{Species}}{character Species common name}
#' }
#' @source \url{https://github.com/NOAA-EDAB/ECSA}
#' @details DETAILS
"latlong"

#' @title NRCC prioritization
#' @description Putative NRCC stock prioritization scores
#' @format A data frame with 43 rows and 7 variables:
#' \describe{
#'   \item{\code{Species}}{character Species common name}
#'   \item{\code{Region}}{character Stock region}
#'   \item{\code{Indicator}}{character Indicator}
#'   \item{\code{Year}}{character Year when indicator was assessed, or descriptive substitute}
#'   \item{\code{Value}}{double Indicator value}
#'   \item{\code{rank}}{double Species rank by indicator value}
#'   \item{\code{norm_rank}}{double Normalized rank}
#' }
#' @source NRCC prioritization data
#' @details DETAILS
"nrcc"

#' @title Diet data color palette
#' @description A color palette to use with diet data
#' @format A data frame with 19 rows and 2 variables:
#' \describe{
#'   \item{\code{prey_id}}{character Prey ID}
#'   \item{\code{color}}{character Color (hex code)}
#' }
#' @details Created from `nmfspalette` package
"prey_palette"

#' @title Recreational landings
#' @description Recreational landings data from MRIP.
#' @format A data frame with 63710 rows and 31 variables:
#' \describe{
#'   \item{\code{X}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{status}}{character COLUMN_DESCRIPTION}
#'   \item{\code{year}}{integer Year}
#'   \item{\code{wave}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{wave_f}}{character COLUMN_DESCRIPTION}
#'   \item{\code{sub_reg}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{sub_reg_f}}{character Ocean region}
#'   \item{\code{st}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{st_f}}{character State}
#'   \item{\code{sp_code}}{double COLUMN_DESCRIPTION}
#'   \item{\code{common}}{character COLUMN_DESCRIPTION}
#'   \item{\code{mode_fx}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{mode_fx_f}}{character Fishing mode}
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
#'   \item{\code{tot_cat}}{integer Total catch (number of fish)}
#'   \item{\code{lbs_ab1}}{double Pounds landed}
#'   \item{\code{var_lbs}}{character COLUMN_DESCRIPTION}
#'   \item{\code{wgt_ab1}}{character COLUMN_DESCRIPTION}
#'   \item{\code{var_wab1}}{character COLUMN_DESCRIPTION}
#'   \item{\code{miss_fish}}{character COLUMN_DESCRIPTION}
#'   \item{\code{ALT_FLAG}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Species}}{character Species common name}
#' }
#' @source \url{https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Survey_Data/CSV/}
#' @details DETAILS
#' "rec_catch"

#' @title Recreational landings data color palette
#' @description A color palette to use with recreational landings dat
#' @format A data frame with 5 rows and 2 variables:
#' \describe{
#'   \item{\code{rec_mode}}{character Fishing mode}
#'   \item{\code{color}}{character Color (hex code)}
#' }
#' @details Created from `nmfspalette` package
"rec_palette"

#' @title Species, stock region, and EPU combinations
#' @description Key to link species, stock regions, and EPUs in all plausible combinations.
#' @format A data frame with 61 rows and 3 variables:
#' \describe{
#'   \item{\code{Species}}{character Species common name}
#'   \item{\code{Region}}{character Stock region}
#'   \item{\code{EPU}}{character Environmental Protection Unit}
#' }
#' @details DETAILS
"regression_species_regions"

#' @title Overall risk assessment
#' @description Species-level risk assessment with a variety of indicator analysis methods
#' @format A data frame with 1375 rows and 15 variables:
#' \describe{
#'   \item{\code{Species}}{character Species common name}
#'   \item{\code{Region}}{character Stock region}
#'   \item{\code{Indicator}}{character Indicator}
#'   \item{\code{category}}{character Indicator category}
#'   \item{\code{Year}}{character Year, time-based description, or other qualitative label if year does not apply}
#'   \item{\code{Value}}{double Value}
#'   \item{\code{rank}}{character Stock's indicator rank relative to all other stocks}
#'   \item{\code{n_stocks_per_indicator}}{integer Number of stocks compared for the indicator}
#'   \item{\code{n_stocks_per_region}}{integer Number of stocks in the stock region}
#'   \item{\code{norm_rank}}{double Normalized rank}
#'   \item{\code{total_risk}}{double Stock's total risk (sum of normalized ranks for all indicators)}
#'   \item{\code{overall_rank}}{integer Stock rank of total risk, compared to all other stocks}
#'   \item{\code{overall_stocks}}{integer Total number of stocks}
#'   \item{\code{stock}}{character Species and region}
#'   \item{\code{label}}{character Label (for plotting)}
#' }
#' @details DETAILS
"risk"

#' @title Risk assessment within stock
#' @description Risk assessment: years ranked within a single stock
#' @format A data frame with 50880 rows and 12 variables:
#' \describe{
#'   \item{\code{Species}}{character Species common name}
#'   \item{\code{Region}}{character Stock region}
#'   \item{\code{Indicator}}{character Indicator}
#'   \item{\code{category}}{character Indicator category}
#'   \item{\code{Year}}{character Window of years}
#'   \item{\code{Value}}{double Value}
#'   \item{\code{rank}}{double Stock's indicator rank for the given year relative to all other years of the same stock}
#'   \item{\code{n_years_per_indicator}}{double Number of years ranked for the stock}
#'   \item{\code{n_stocks_per_region}}{integer NUmber of stocks in the stock region}
#'   \item{\code{norm_rank}}{double Normalized rank}
#'   \item{\code{total_risk}}{double Stock's total risk (sum of normalized ranks for all indicators)}
#'   \item{\code{overall_rank}}{integer Stock's rank of total risk, compared to all other stocks}
#' }
#' @details DETAILS
"risk_species"

#' @title Risk assessment of change compared to historical value
#' @description Risk assessment: present year value compared to historical value compared among stocks
#' @format A data frame with 41658 rows and 15 variables:
#' \describe{
#'   \item{\code{Species}}{character Species common name}
#'   \item{\code{Region}}{character Stock region}
#'   \item{\code{Indicator}}{character Indicator}
#'   \item{\code{category}}{character Indicator category}
#'   \item{\code{Year}}{character Window of years}
#'   \item{\code{Value}}{double Value (change compared to historical)}
#'   \item{\code{rank}}{double Stock's indicator rank for the given year relative to all other stocks in the same year}
#'   \item{\code{n_stocks_per_indicator}}{double Number of stocks ranked for the indicator}
#'   \item{\code{n_stocks_per_region}}{integer NUmber of stocks in the stock region}
#'   \item{\code{norm_rank}}{double Normalized rank}
#'   \item{\code{total_risk}}{double  Stock's total risk (sum of normalized ranks for all indicators)}
#'   \item{\code{overall_rank}}{integer Stock's rank of total risk, compared to all other stocks}
#'   \item{\code{overall_stocks}}{integer Total number of stocks}
#'   \item{\code{stock}}{character Species and region}
#'   \item{\code{label}}{character Label (for plotting)}
#' }
#' @details DETAILS
"risk_year_hist"

#' @title Risk assessment of magnitude of value compared to other stocks
#' @description Risk assessment: present year value compared among stocks
#' @format A data frame with 42559 rows and 15 variables:
#' \describe{
#'   \item{\code{Species}}{character Species common name}
#'   \item{\code{Region}}{character Stock region}
#'   \item{\code{Indicator}}{character Indicator}
#'   \item{\code{category}}{character Indicator category}
#'   \item{\code{Year}}{character Window of years}
#'   \item{\code{Value}}{double Value}
#'   \item{\code{rank}}{double Stock's indicator rank for the given year relative to all other stocks in the same year}
#'   \item{\code{n_stocks_per_indicator}}{double Number of stocks ranked for the indicator}
#'   \item{\code{n_stocks_per_region}}{integer NUmber of stocks in the stock region}
#'   \item{\code{norm_rank}}{double Normalized rank}
#'   \item{\code{total_risk}}{double  Stock's total risk (sum of normalized ranks for all indicators)}
#'   \item{\code{overall_rank}}{integer Stock's rank of total risk, compared to all other stocks}
#'   \item{\code{overall_stocks}}{integer Total number of stocks}
#'   \item{\code{stock}}{character Species and region}
#'   \item{\code{label}}{character Label (for plotting)}
#' }
#' @details DETAILS
"risk_year_value"

#' @title Northeast strata shapefile
#' @description Shapefile containing geometries for NE stock strata
#' @format A data frame with 185 rows and 8 variables:
#' \describe{
#'   \item{\code{FINSTR_ID}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{STRATA}}{integer Strata ID}
#'   \item{\code{A2}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{STR2}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{SET_}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{STRATUMA}}{character COLUMN_DESCRIPTION}
#'   \item{\code{STR3}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{geometry}}{list Geometry}
#' }
#' @details DETAILS
"shape"

#' @title Species guild key
#' @description Species guild key
#' @format A data frame with 37 rows and 3 variables:
#' \describe{
#'   \item{\code{Species}}{character Species common name}
#'   \item{\code{Scientific_name}}{character Species scientific name}
#'   \item{\code{Guild}}{character Guild}
#' }
#' @details Can probably be deprecated in favor of `guild_info`
"species_guilds"

#' @title Species SVSPP key
#' @description A key to connect species common names and SVSPP (Northeast Resource Surveys Species code)
#' @format A data frame with 37 rows and 2 variables:
#' \describe{
#'   \item{\code{SVSPP}}{integer Northeast Resource Surveys Species code}
#'   \item{\code{Species}}{character Species common name}
#' }
#' @details DETAILS
"species_key"

#' @title NEFSC Bottom Trawl Data
#' @description DATASET_DESCRIPTION
#' @format A data frame with 3937953 rows and 27 variables:
#' \describe{
#'
#'   \item{\code{CRUISE6}}{character Cruise ID}
#'   \item{\code{STATION}}{character Station ID}
#'   \item{\code{STRATUM}}{character Stratum}
#'   \item{\code{TOW}}{character Tow ID}
#'   \item{\code{SVSPP}}{character Northeast Resource Surveys Species code}
#'   \item{\code{CATCHSEX}}{character Species-specific numeric sex code}
#'   \item{\code{SVVESSEL}}{character Vessel ID}
#'   \item{\code{YEAR}}{double Year}
#'   \item{\code{SEASON}}{character Season}
#'   \item{\code{LAT}}{double Latitude}
#'   \item{\code{LON}}{double Longitude}
#'   \item{\code{EST_TOWDATE}}{double Date with estimated time}
#'   \item{\code{DEPTH}}{double Depth}
#'   \item{\code{SURFTEMP}}{double Surface water temperature}
#'   \item{\code{SURFSALIN}}{double Surface salinity}
#'   \item{\code{BOTTEMP}}{double Bottom water temperature}
#'   \item{\code{BOTSALIN}}{double Bottom salinity}
#'   \item{\code{ABUNDANCE}}{double Abundance (number of fish caught on a single tow; redundant for rows with the same CRUISE6, STATION, and TOW)}
#'   \item{\code{BIOMASS}}{double Biomass (biomass caught on a single tow; redundant for rows with the same SVSPP, CRUISE6, STATION, and TOW)}
#'   \item{\code{LENGTH}}{double Length}
#'   \item{\code{NUMLEN}}{double Number of fish at the specified length (redundant for rows with the same SVSPP, CRUISE6, STATION, TOW, and LENGTH)}
#'   \item{\code{Species}}{character Species common name}
#'   \item{\code{spst}}{character Species and strata}
#'   \item{\code{stock_area}}{character Stock region}
#'   \item{\code{Region}}{character Stock region}
#'   \item{\code{date}}{character Date}
#'   \item{\code{fish_id}}{character Unique fish ID}
#' }
#' @source Oracle query of `svdbs` using the `survdat` package.
#' @details \itemize{
#'     \item Function call: survdat::get_survdat_data(channel = channel, filterByYear = NA, all.season = TRUE, shg.check = T, conversion.factor = T, use.SAD = F, getBio = FALSE, getLengths = T)
#'     \item Pull date: Wed Mar 03 10:56:23 2021
#' }
"survey"

#' @title Swept area estimates
#' @description Swept area estimates
#' @format A data frame with 4009 rows and 17 variables:
#' \describe{
#'   \item{\code{YEAR}}{integer Year}
#'   \item{\code{SVSPP}}{integer Northeast Resource Surveys Species code}
#'   \item{\code{N}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{strat.biomass}}{double Stratified mean biomass}
#'   \item{\code{biomass.var}}{double Stratified mean biomass variance}
#'   \item{\code{biomass.SE}}{double Stratified mean biomass standard error}
#'   \item{\code{strat.abund}}{double Stratified mean abundance}
#'   \item{\code{abund.var}}{double Stratified mean abundance variance}
#'   \item{\code{abund.SE}}{double Stratified mean abundance standard error}
#'   \item{\code{tot.biomass}}{double Swept area biomass}
#'   \item{\code{tot.bio.var}}{double Biomass variance}
#'   \item{\code{tot.bio.SE}}{double Biomass standard error}
#'   \item{\code{tot.abundance}}{double Swept area abundance}
#'   \item{\code{tot.abund.var}}{double Abundance variance}
#'   \item{\code{tot.abund.SE}}{double Abundance standard error}
#'   \item{\code{Species}}{character Species common name}
#'   \item{\code{Season}}{character Season}
#' }
#' @details See `survdat::swept_area`
"swept"
