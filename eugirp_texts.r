filoutliers<-paste0(invloc,"/checks/countryoutliers/",cursubm,"co")
coutheader<-paste0("# File created by EU-GIRP v.",eugirp.version," on ",figdate)
coutlexp1<-"# List of country outliers. Outliers were identified with the following criteria"
outlmethod2<-paste0("# (1) Country value outside the range of median +/- 1.953 x (median-75percentile/25percentile)")
outlmethod3<-paste0("# (1) Country value outside the range of mean +/- 1.5 standard deviations")
coutlexp2<-paste0(" of the values reported for the variable during the time period and ",
                  "by all countries by which this variable is reported\n",
                  "# (2) To exclude minimim deviations though the country values ",
                  "which are not more different that 10% from the median ",
                  "are NOT considered as outliers\n",
                  "# (3) A wide distribution indicates systematic errors (e.g. wrong unit used by MS(s))")

signexp1<-paste0("# Identification of a 'Potential significant issue' is done only for those issues identified for the year ",signyear,
                 " (See column 'lastyr' 0/1). ",
                 " The significance of the issue depends on the share of the affected emission source category on",signclass,
                 " in the year",signyear," as given in the column 'share' and the potential over- or underestimation of the identified issue. ",
                 "\n# (1) We used the median value as reported over all years and reporting countries for the variable as reference.",
                 " The value in ",signyear," relative to the median is given in the column 'relmedian'.",
                 "\n# (2) The 'effect' is calculated for potential over-estimations (relmedian>1) with the formula '(relmedia - 1)/relmedian*share'",
                 " and for potential under-estimations (relmedian<1) with the formula 'share/relmedian' (column 'effect').",
                 "\n# (3) Significant issues are those where the potential effect is larger than ",signthreshold,"% of total national emissions.",
                 "\n# (4) Finally potential sign issues are only those which are assumed to have an effect on estimated emissions",
                 " including all IEFs and other measures with a direct impact on the estimates (explanation given in column 'note').")
signcatexp<-paste0("# Information for the identification of significant source categories for the year ",signyear,". ",
                   "Criterion: share of emissions on the ",signclass," of at least ",signthreshold,". ",
                   "Significant source categories are indicated in the column 'potsig'",
                   "\n# To identify pot. sign. issues check if the year ",signyear," included in list of outliers.",
                   "\n# If yes calculate RelMedian='Value identified as outlier'/'Median value'. ",
                   "If this value is >1 it could be linked to an overestimation.",
                   "If this value is <1 it could be linked to an underestimation.",
                   "\n#\n# Potential overestimation: only issues from those source categories flagged with '1' in the column potsig can be significant. ",
                   "Whether or not an issue of a listed is potentially significant depends the degree of potential overestimation. ",
                   "\n#   --- Calculate '(RelMedian - 1)/Relmedian*emissionshare'. If this is >",signthreshold," the issue is potentially significant.",
                   "\n#\n# Potential underestimation: all source categories could be a linked to a potential underestimation.",
                   "Whether or not an issue of a listed is potentially significant depends the degree of potential underestimation. ",
                   "\n#   --- Calculate 'emissionshare/RelMedian'. If this is >",format(signthreshold*100),"% the issue is potentially significant.",
                   "\n#\n#")    

colexpl1<-paste0("#\n# Explanation of column names (different from CRF-dimensions)\n",
                 "# min-max: minimum (ignoring reported zeroes) and maximum values")
lulimexpl<-paste0(" # llim-ulim: lower and upper limit of values not considered as outliers.",
                  " # rellim: (mean) value report relative to the lower/upper limit depending if the reported value is low or large.")

whisksexpl<-paste0("# lwhisk-uwhisk: lower and upper 'whisker'. Values below the lower whisker",
                   " or above the upper whisker are considered as outliers.\n",
                   " The whiskers are determined to cover ca. 80% of values when they were normally distributed.")
colexpl2<-paste0("# p25-median-p75: 25percentile-50percentile (median)-75percentile ",
                 "of the distribution of growth rates for the variable in the ",
                 "country over time period ",years[1],"-",years[length(years)],"\n",
                 "# range: max:min ratio - zeros excluded - ",
                 "# value (trend-outlier): list of outlier values identified-space separated - ",
                 "# value (country-outlier): average value of outlier values identified - ",
                 "# years: list of years for which outlier values were identified")
colexpl3<-paste0("# resolved-flag: 0=not resolved - 1=resolved - ",
                 "2=not significant (provide reason) - ",
                 "3=parent (explanation given in more detailed categories) -",
                 "4=follow up of previous unresolved issue - ",
                 "5=postponed (required further evaluation)\n",
                 "# explanation date and source: meta information about the issue: what is the explanation-when has it been solved and what was the source of information")
