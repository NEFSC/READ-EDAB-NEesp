#' @title Diet and Condition Data
#' @description This data was derived from NEFSC bottom trawl data. It contains stomach content information and length-weight information.
#' @format A data frame with 1423676 rows and 90 variables:
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
#'
#'   \item{\code{obs}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{pyspp}}{double COLUMN_DESCRIPTION}
#'   \item{\code{pyabbr}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{modcat}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{collcat}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{collsci}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{station}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{pdsex}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{pdid}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{fhmat}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{pdmat}}{integer COLUMN_DESCRIPTION}
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
#'   \item{\code{ttime}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{towdur}}{double COLUMN_DESCRIPTION}
#'   \item{\code{setdepth}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{airtemp}}{double COLUMN_DESCRIPTION}
#'   \item{\code{svgear}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{tow}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{beglat}}{double COLUMN_DESCRIPTION}
#'   \item{\code{beglon}}{double COLUMN_DESCRIPTION}
#'   \item{\code{hour}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{minute}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{month}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{day}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{haul}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{gearcond}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{area}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{statype}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{towtime}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{purcode}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{status_code}}{integer COLUMN_DESCRIPTION}
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
#'   \item{\code{spst}}{character COLUMN_DESCRIPTION}
#'   \item{\code{stock_area}}{character COLUMN_DESCRIPTION}
#' }
#'
#' @source \url{https://github.com/Laurels1/Condition/blob/master/data/allfh.RData}
#' @details DETAILS
"allfh"

#' @title Diet and Condition Data
#' @description This data was derived from NEFSC bottom trawl data. It contains stomach content information and length-weight information.
#' @format A data frame with 1423676 rows and 16 variables:
#' \describe{
#'    \item{\code{pynam}}{character Prey scientific name}
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
"allfh_small"

#' @title Stock Assessment Summary Data
#' @description This data set contains summary stock assessment metrics on several US stocks.
#' @format A data frame with 337 rows and 61 variables:
#' \describe{
#'
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
#'
#'   \item{\code{Stock Name}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Jurisdiction}}{character COLUMN_DESCRIPTION}
#'   \item{\code{FMP}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Science Center}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Regional Ecosystem}}{character COLUMN_DESCRIPTION}
#'   \item{\code{ITIS Taxon Serial Number}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Scientific Name}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Common Name}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Stock Area}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Assessment Month}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Update Type}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Assessment Model}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Model Version}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Lead Lab}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Citation}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Final Assessment Report 1}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Final Assessment Report 2}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Point of Contact}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Abundance Data}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Catch Data}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Assessment Level}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Assessment Frequency}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Assessment Type}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Model Category}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Catch Input Data}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Abundance Input Data}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{F Year}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Estimated F}}{double COLUMN_DESCRIPTION}
#'   \item{\code{F Unit}}{character COLUMN_DESCRIPTION}
#'   \item{\code{F Basis}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Flimit}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Flimit Basis}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Fmsy}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Fmsy Basis}}{character COLUMN_DESCRIPTION}
#'   \item{\code{F/Flimit}}{double COLUMN_DESCRIPTION}
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
#'   \item{\code{MSY}}{double COLUMN_DESCRIPTION}
#'   \item{\code{MSY Unit}}{character COLUMN_DESCRIPTION}
#' }
#'
#' @source \url{https://github.com/NOAA-EDAB/assessmentdata}
#' @details For more details see \url{https://www.st.nmfs.noaa.gov/stocksmart/StockSMART_DataDictionary.pdf}
"asmt_sum"

#' @title Stock Assessment Summary Data
#' @description This data set contains summary stock assessment metrics on several US stocks.
#' @format A data frame with 337 rows and 12 variables:
#' \describe{
#'
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
#'
#' }
#'
#' @source \url{https://github.com/NOAA-EDAB/assessmentdata}
#' @details For more details see \url{https://www.st.nmfs.noaa.gov/stocksmart/StockSMART_DataDictionary.pdf}S
"asmt_sum_small"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 277963 rows and 10 variables:
#' \describe{
#'   \item{\code{YEAR}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{NESPP3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{QY}}{double COLUMN_DESCRIPTION}
#'   \item{\code{GEAR}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{SIZE}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{EPU}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{UTILCD}}{double COLUMN_DESCRIPTION}
#'   \item{\code{SPPLIVMT}}{double COLUMN_DESCRIPTION}
#'   \item{\code{SPPVALUE}}{double COLUMN_DESCRIPTION}
#'   \item{\code{US}}{logical COLUMN_DESCRIPTION}
#' }
#' @details DETAILS
"com_gear"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 6162875 rows and 19 variables:
#' \describe{
#'   \item{\code{taxa}}{character COLUMN_DESCRIPTION}
#'   \item{\code{year}}{double COLUMN_DESCRIPTION}
#'   \item{\code{season}}{double COLUMN_DESCRIPTION}
#'   \item{\code{strata.x}}{double COLUMN_DESCRIPTION}
#'   \item{\code{lat}}{double COLUMN_DESCRIPTION}
#'   \item{\code{lon}}{double COLUMN_DESCRIPTION}
#'   \item{\code{area}}{double COLUMN_DESCRIPTION}
#'   \item{\code{mean.abund}}{double COLUMN_DESCRIPTION}
#'   \item{\code{rel.proportion}}{double COLUMN_DESCRIPTION}
#'   \item{\code{season.month}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{sci_name}}{character COLUMN_DESCRIPTION}
#'   \item{\code{common_name}}{character COLUMN_DESCRIPTION}
#'   \item{\code{cc_name}}{character COLUMN_DESCRIPTION}
#'   \item{\code{stock_name}}{character COLUMN_DESCRIPTION}
#'   \item{\code{species_code}}{character COLUMN_DESCRIPTION}
#'   \item{\code{svspp}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{stock_season}}{character COLUMN_DESCRIPTION}
#'   \item{\code{strata.y}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{stock_subarea}}{character COLUMN_DESCRIPTION}
#' }
#' @details DETAILS
"ichthyo"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 63710 rows and 7 variables:
#' \describe{
#'   \item{\code{year}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{sub_reg_f}}{character COLUMN_DESCRIPTION}
#'   \item{\code{st_f}}{character COLUMN_DESCRIPTION}
#'   \item{\code{mode_fx_f}}{character COLUMN_DESCRIPTION}
#'   \item{\code{tot_cat}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{lbs_ab1}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION}
#' }
#' @details DETAILS
"rec_catch_small"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 14478 rows and 8 variables:
#' \describe{
#'   \item{\code{Estimate.Status}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Year}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Wave}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{State}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Fishing.Mode}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Fishing.Area}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Angler.Trips}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{PSE}}{integer COLUMN_DESCRIPTION}
#' }
#' @details DETAILS
"rec_effort"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 1023 rows and 14 variables:
#' \describe{
#'   \item{\code{COMNAME}}{character COLUMN_DESCRIPTION}
#'   \item{\code{SVSPP}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{ITISSPP}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{NESPP3}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{SCINAME}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Fed_Managed}}{character COLUMN_DESCRIPTION}
#'   \item{\code{SOE_17}}{character COLUMN_DESCRIPTION}
#'   \item{\code{EMAX}}{character COLUMN_DESCRIPTION}
#'   \item{\code{RPATH}}{character COLUMN_DESCRIPTION}
#'   \item{\code{SOE_18}}{character COLUMN_DESCRIPTION}
#'   \item{\code{SizeCat}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Min_size}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Garrison_Link}}{character COLUMN_DESCRIPTION}
#'   \item{\code{NEIEA}}{character COLUMN_DESCRIPTION}
#' }
#' @details DETAILS
"sp_group"

#' @title DATASET_TITLE
#' @description DATASET_DESCRIPTION
#' @format A data frame with 128 rows and 35 variables:
#' \describe{
#'   \item{\code{ctyp_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{calfin_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{pseudo_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{penilia_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{tlong_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{cham_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{echino_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{larvaceans_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{para_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{gas_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{acarspp_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{mlucens_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{evadnespp_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{salps_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{oithspp_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{chaeto_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{hyper_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{calminor_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{clauso}}{double COLUMN_DESCRIPTION}
#'   \item{\code{dec_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{euph_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{prot_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{poly_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{fish_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{oncaea_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{cory_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{siph_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{coel_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{euph1_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{thecos_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{spirspp_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{cnidar_100m3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Species}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Season}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Time}}{character COLUMN_DESCRIPTION}
#' }
#' @details DETAILS
"zoop_stock"
