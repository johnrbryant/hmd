functions {
  /**
  * Modified from https://github.com/mbjoseph/CARstan, 
  * to account for the cohort matrix not consisting of zeros and ones 
  * Return the log probability of an improper conditional autoregressive (CAR) prior 
  * with a sparse representation for the adjacency matrix
  *
  * @param phi Vector containing the parameters with a CAR prior
  * @param tau Precision parameter for the CAR prior (real)
  * @param W_sparse Sparse representation of adjacency matrix (int array)
  *        including indices for the adjacent pair (i,j) and the value of w_{ij}
  *        (i,j) and (j,i) are condidered as different pairs
  * @param D_sparse \sum_j w_{ij} for each i (vector)
  * @param n Length of phi (int)
  * @param W_n Number of adjacent pairs (int)
  *
  * @return Log probability density of CAR prior up to additive constant
  */
  real sparse_car_lpdf(vector phi, real tau,
    int[,] W_sparse, vector D_sparse, int n, int W_n) {
      row_vector[n] phit_D; // phi' * D
      row_vector[n] phit_W; // phi' * W
    
      phit_D = (phi .* D_sparse)';
      phit_W = rep_row_vector(0, n);
      for (i in 1:W_n) {
        phit_W[W_sparse[i, 2]] = phit_W[W_sparse[i, 2]] + phi[W_sparse[i,1]]*W_sparse[i,3];
      }
    
      return 0.5 * ((n-1) * log(tau)
                    - tau * (phit_D * phi - (phit_W * phi)));
  }
}


data {
  /** Numbers of ages, years and countries **/
  int<lower=0> NAge; //number of ages
  //number of sexes is 2
  int<lower=0> NYear; //number of years under consideration
  int<lower=0> NCohort; //number of cohorts
  //l=first.year+t-age_a, where age_a is the mid-age for age group a
  //  and first.year is the first observation year
  //What is the relationship between NYear, NAge and NCohort???  
  int<lower=0> NCountry; //number of countries
  int<lower=0> NCountry_adj; 
    //number of countries with at least one neighbors, arranged as the first set of countries

  /** Starting and ending years for each country **/
  int<lower=0> StartYear[NCountry]; //Starting year for each country, label starting with 1
  int<lower=0> EndYear[NCountry]; //Ending year for each country, label starting with 1

  /** Numbers of cells **/
  int<lower=0> N_obs; //number of cells with observed data

  /** Observed death counts and exposure **/
  int deaths_obs[N_obs]; //observed death counts, arranged by ages, sex, year and countries
  int exposure_obs[N_obs]; //observed exposure, arranged by ages, sex, year and countries

  /** Used in CAR priors for age intercepts and age slopes **/
  matrix<lower = 0, upper = 1>[NAge,NAge] W_Age; 
    //adjacency matrix for ages, diagonal elements equal to 0

  /** Used in CAR priors for cohort slopes **/
  matrix<lower = -1, upper = 2>[NCohort,NCohort] W_Cohort; 
    //adjacency matrix for cohorts, diagonal elements equal to 0

  /** Used in CAR priors for country intercepts and country slopes **/
  matrix<lower = 0, upper = 1>[NCountry_adj,NCountry_adj] W_Country; 
    //adjacency matrix for countries, diagonal elements equal to 0
}


transformed data {
  int<lower=0> N; //total number of cells
  N=NAge*2*NYear*NCountry;

  /** Indices for give ages, sexes, years and countries **/
  int<lower=0,upper=N> Index[NAge,NSex,NYear,NCountry]; //Indices for give ages, sexes, years and countries
  {
    int a;
    int s;
    int tt;
    int d;
    
    for (a in 1:NAge)
      for (s in 1:2)
        for (tt in 1:NYear)
          for (d in 1:NCountry)
            Index[a,s,tt,d]=(a-1)*2*NYear*NCountry+(s-1)*NYear*NCountry+(tt-1)*NCountry+d;
  }
  

  /** Indicies for cells with observed or missing data **/
  int<lower=0,upper=N> ii_obs[N_obs]; //indices for cells with observed data
  int<lower=0,upper=N> ii_mis[N-N_obs]; //indices for cells with missing data

  {
    int counter_obs;
    int counter_mis;
    int a;
    int s;
    int d;
    int tt;

    counter_obs=1;
    counter_mis=1;

    for (d in 1:NCountry) {
      for (tt in 1:(StartYear[d]-1) {
        for (a in 1:NAge) {
          for (s in 1:2) {
            ii_mis[counter_mis]=Index[a,s,tt,d];
            counter_mis=counter_mis+1;
          }
        }
      }        
      for (tt in StartYear[d]:EndYear[d]) {
        for (a in 1:NAge) {
          for (s in 1:2) {
            ii_obs[counter_obs]=Index[a,s,tt,d];
            counter_obs=counter_obs+1;
          }
        }
      }    
      for (tt in (EndYear[d]+1):NYear) {
        for (a in 1:NAge) {
          for (s in 1:2) {
            ii_mis[counter_mis]=Index[a,s,tt,d];
            counter_mis=counter_mis+1;
          }
        }
      }    
    }
  }

  /** data counts and exposure **/
  int deaths[N]; //death counts for all cells
  int exposure[N]; //exposure for all cells

  deaths[ii_obs]=y_obs;
  deaths[ii_mis]=-1; //not dealing with missing cells

  exposure[ii_obs]=exposure_obs;
  exposure[ii_mis]=-1; //not dealing with missing cells

  /** Cohort-related data **/
  int<lower=0> Cohort.ind[NAge,NYear]; //cohort indices for given age and year.
  //l=first.year+t-age_a, where age_a is the mid-age for age group a
  //  and first.year is the first observation year
  //How to derive the values in cohort.ind???

  int AddNoise[NAge,NCohort];
  // A matrix indicating whether to add noise in order to suppress the random walk part 
  //    for identifiability of the oldest and the youngest cohorts 

  /** Used in CAR priors for age intercepts and age slopes. **/
  int W_n_Age; 
    //number of adjacent pairs of ages, (i,j) and (j,i) are considered different pairs
  {  
    int i;
    int j;

    W_n_Age=0;

    // loop over W to identify pairs of (i,j) with nonzero w_{ij}
    for (i in 1:NAge)) {
      for (j in 1:NAge) {
        if (W_Age[i, j] != 0) {
          W_n_Age=W_n_Age+1;
        }
      }
    }
  }

  int W_Age_sparse[W_n_Age, 3];   
    //adjacency pairs of ages and the corresponding value of w, 
    //(i,j) and (j,i) are considered different pairs
  vector[NAge] D_Age_sparse;     
    //diagonal of D for ages
  
  {  
    int counter;
    int i;
    int j;

    counter = 1;

    // loop over W to identify pairs of (i,j) with nonzero w_{ij}
    for (i in 1:NAge)) {
      for (j in 1:NAge) {
        if (W_Age[i, j] != 0) {
          W_Age_sparse[counter, 1] = i;
          W_Age_sparse[counter, 2] = j;
          W_Age_sparse[counter, 3] = W_Age[i,j];
          counter = counter + 1;
        }
      }
    }
    for (i in 1:NAge) 
      D_Age_sparse[i] = sum(W_Age[i]);
  }

  /** Used in CAR priors for cohort slopes. **/
  int W_n_Cohort; 
    //number of adjacent pairs of cohorts, (i,j) and (j,i) are considered different pairs
  { 
    int i;
    int j;

    W_n_Cohort=0;

    // loop over W to identify pairs of (i,j) with nonzero w_{ij}
    for (i in 1:NCohort)) {
      for (j in 1:NCohort) {
        if (W_Cohort[i, j] != 0) {
          W_n_Cohort=W_n_Cohort+1;
        }
      }
    }
  }

  int W_Cohort_sparse[W_n_Cohort, 3];   
    //adjacency pairs of cohorts and the corresponding value of w, 
    //(i,j) and (j,i) are considered different pairs
  vector[NCohort] D_Cohort_sparse;     
    //diagonal of D for cohorts
  
  { 
    int counter;
    int i;
    int j;

    counter = 1;

    // loop over W to identify pairs of (i,j) with nonzero w_{ij}
    for (i in 1:NCohort)) {
      for (j in 1:NCohort) {
        if (W_Cohort[i, j] != 0) {
          W_Cohort_sparse[counter, 1] = i;
          W_Cohort_sparse[counter, 2] = j;
          W_Cohort_sparse[counter, 3] = W_Cohort[i,j];
          counter = counter + 1;
        }
      }
    }
    for (i in 1:NCohort) 
      D_Cohort_sparse[i] = sum(W_Cohort[i]);
  }

  /** Used in CAR priors for country intercepts and country slopes. **/
  int W_n_Country; 
    //number of adjacent pairs of countries, (i,j) and (j,i) are considered different pairs
  { 
    int i;
    int j;

    W_n_Country=0;
    // loop over upper triangular part of W to identify neighbor pairs
    for (i in 1:NCountry_adj)) {
      for (j in 1:NCountry_adj) {
        if (W_Country[i, j] != 0) {
          W_n_Country=W_n_Country+1;
        }
      }
    }
  }

  int W_Country_sparse[W_n_Country, 3];   
    //adjacency pairs of countries and the corresponding value of w, 
    //(i,j) and (j,i) are considered different pairs
  vector[NCountry_adj] D_Country_sparse;     
    //diagonal of D for countries (number of neigbors for each country with neighbors)
  
  { 
    int counter;
    int i;
    int j;

    counter = 1;

    // loop over upper triangular part of W to identify neighbor pairs
    for (i in 1:NCountry_adj)) {
      for (j in 1:NCountry_adj) {
        if (W_Country[i, j] != 0) {
          W_Country_sparse[counter, 1] = i;
          W_Country_sparse[counter, 2] = j;
          W_Country_sparse[counter, 3] = W_Country[i,j];
          counter = counter + 1;
        }
      }
    }
    for (i in 1:n) 
      D_Country_sparse[i] = sum(W_Country[i]);
  }
}


parameters {
  real Int; //overall intercept
  real Slope; //overall slope

  real IntFemale; //difference between females and males in the intercept
  real SlopeFemale; //difference between females and males in the slope

  vector[NAge] AgeInt_Male_unscaled; //age intercepts for males, first-order random walk
  vector[NAge] AgeInt_Female_unscaled; //age intercepts for females, first-order random walk
  vector[NAge] AgeSlope_Male_unscaled; //age slopes for males, first-order random walk
  vector[NAge] AgeSlope_Female_unscaled; //age slopes for females, first-order random walk
  vector[NAge] CohortSlope_Male_unscaled; //cohort slopes for males, first-order random walk
  vector[NAge] CohortSlope_Female_unscaled; //cohort slopes for females, first-order random walk
  matrix[NAge,NYear] AgeCohort_Male_Year; //age/cohort-year interactions for males
  matrix[NAge,NYear] AgeCohort_Female_Year; //age/cohort-year interactions for females
  //assuming that v_{at}, residual terms in age-year interactions, 
  //follow first-order random walk over time, with v_{a1}=0)

  vector[NCountry_adj] phi_CountryInt_Male_unscaled; //spatial random effects for country intercepts and males
  vector[NCountry_adj] phi_CountryInt_Female_unscaled; //spatial random effects for country intercepts and females
  vector[NCountry] CountryInt_Male; //country intercepts for males, BYM model
  vector[NCountry] CountryInt_Female; //country intercepts for females, BYM model
  vector[NCountry_adj] phi_CountrySlope_Male_unscaled; //spatial random effects for country slopes and males
  vector[NCountry_adj] phi_CountrySlope_Female_unscaled; //spatial random effects for country slopes and females
  vector[NCountry] CountrySlope_Male; //country slopes for males, BYM model
  vector[NCountry] CountrySlope_Female; //country slopes for females, BYM model
  vector[NCountry,NYear] Country_Year_Male; //country-year interactions for males
  vector[NCountry,NYear] Country_Year_Female; //country-year interactions for females
  //assuming that h_{dt}, residual terms in country-year interactions, 
  //follow first-order random walk over time, with h_{d1}=0)  

  vector[NAge,NCountry] Age_Country_Male; //age-country interactions for males, normal
  vector[NAge,NCountry] Age_Country_Female; //age-country interactions for females, normal
   
  vector[N] gamma; //underlying rates for all cells, deaths~poisson(exposure*gamma)

  real<lower = 0> sigma_AgeInt; //precision of age intercepts
  real<lower = 0> sigma_AgeSlope; //precision of age slopes
  real<lower = 0> sigma_CohortSlope; //precision of cohort slopes
  real<lower = 0> sigma_Age_Year; //standard deviation of v_{at}, residual terms in age-year interactions
  real<lower = 0> sigmaSpa_CountryInt; //precision of spatial random effects for country intercepts
  real<lower = 0> sigmaNonspa_CountryInt; //standard deviation of non-spatial random effects for country intercepts
  real<lower = 0> sigmaSpa_CountrySlope; //precision of spatial random effects for country slopes
  real<lower = 0> sigmaNonspa_CountrySlope; //standard deviation of non-spatial random effects for country slopes
  real<lower = 0> sigma_Country_Year; //standard deviation of h_{dt}, residual terms in country-year interactions
  real<lower = 0> sigma_Age_Country; //standard deviation of age-country interactions
  real<lower = 0> sigma_error; //standard deviation for the errors (epsilons)
}


transformed parameters {
  /** Centering of the "spatial" random effects **/
  vector[NAge] AgeInt_Male; //brute force centering of spatial random effects for age intercepts
  AgeInt_Male = AgeInt_Male_unscaled - mean(AgeInt_Male_unscaled);
  vector[NAge] AgeInt_Female; //brute force centering of spatial random effects for age intercepts
  AgeInt_Female = AgeInt_Female_unscaled - mean(AgeInt_Female_unscaled);

  vector[NAge] AgeSlope_Male; //brute force centering of "spatial" random effects for age slopes
  AgeSlope_Male = AgeSlope_Male_unscaled - mean(AgeSlope_Male_unscaled);
  vector[NAge] AgeSlope_Female; //brute force centering of "spatial" random effects for age slopes
  AgeSlope_Female = AgeSlope_Female_unscaled - mean(AgeSlope_Female_unscaled);

  vector[NCohort] CohortSlope_Male; //brute force centering of "spatial" random effects for cohort slopes
  CohortSlope_Male = CohortSlope_Male_unscaled - mean(CohortSlope_Male_unscaled);
  vector[NCohort] CohortSlope_Female; //brute force centering of "spatial" random effects for cohort slopes
  CohortSlope_Female = CohortSlope_Female_unscaled - mean(CohortSlope_Female_unscaled);

  vector[NCountry_adj] phi_CountryInt_Male; //brute force centering of spatial random effects for country intercepts
  phi_CountryInt_Male = phi_CountryInt_Male_unscaled - mean(phi_CountryInt_Male_unscaled);
  vector[NCountry_adj] phi_CountryInt_Female; //brute force centering of spatial random effects for country intercepts
  phi_CountryInt_Female = phi_CountryInt_Female_unscaled - mean(phi_CountryInt_Female_unscaled);

  vector[NCountry_adj] phi_CountrySlope_Male; //brute force centering of spatial random effects for country slopes
  phi_CountrySlope_Male = phi_CountrySlope_Male_unscaled - mean(phi_CountrySlope_Male_unscaled);
  vector[NCountry_adj] phi_CountrySlope_Female; //brute force centering of spatial random effects for country slopes
  phi_CountrySlope_Female = phi_CountrySlope_Female_unscaled - mean(phi_CountrySlope_Female_unscaled);

  /** Transform standard deviations for "spatial" random effects into precisions **/
  real tau_AgeInt;
  tau_AgeInt=sigma_AgeInt^(-2);

  real tau_AgeSlope;
  tau_AgeSlope=sigma_AgeSlope^(-2);

  real tau_CohortSlope;
  tau_CohortSlope=sigma_CohortSlope^(-2);

  real tauSpa_CountryInt;
  tauSpa_CountryInt=sigmaSpa_CountryInt^(-2);

  real tauSpa_CountrySlope;
  tauSpa_CountrySlope=sigmaSpa_CountrySlope^(-2);
}


model {
  /** overall intercept and overall slope **/
  Int ~ normal(0,sqrt(100000));
  Slope ~ normal(0,sqrt(100000));

  /** differences in intercept and slope between females an dm
  IntFemale ~ normal(0,sqrt(100000));
  SlopeFemale ~ normal(0,sqrt(100000));
  
  /** Standard deviations **/
  sigma_AgeInt ~ uniform(0,2);
  sigma_AgeSlope ~ uniform(0,2);
  sigma_CohortSlope ~ uniform(0,2);
  sigma_AgeYear ~ uniform(0,2);
  sigmaSpa_CountryInt ~ uniform(0,2);
  sigmaNonspa_CountryInt ~ uniform(0,2);
  sigmaSpa_CountrySlope ~ uniform(0,2);
  sigmaNonspa_CountrySlope ~ uniform(0,2);
  sigma_Country_Year ~ uniform(0,2);
  sigma_Age_Country ~ uniform(0,2);
  sigma_error ~ uniform(0,2);  

  /** "Spatial" random errors **/
  AgeInt_Male_unscaled ~ sparse_car(tau_AgeInt, W_Age_sparse, D_Age_sparse, NAge, W_n_Age);
  AgeInt_Female_unscaled ~ sparse_car(tau_AgeInt, W_Age_sparse, D_Age_sparse, NAge, W_n_Age);
  AgeSlope_Male_unscaled ~ sparse_car(tau_AgeSlope, W_Age_sparse, D_Age_sparse, NAge, W_n_Age);
  AgeSlope_Female_unscaled ~ sparse_car(tau_AgeSlope, W_Age_sparse, D_Age_sparse, NAge, W_n_Age);
  CohortSlope_Male_unscaled ~ sparse_car(tau_CohortSlope, W_Cohort_sparse, D_Cohort_sparse, NCohort, W_n_Cohort);
  CohortSlope_Female_unscaled ~ sparse_car(tau_CohortSlope, W_Cohort_sparse, D_Cohort_sparse, NCohort, W_n_Cohort);
  phi_CountryInt_Male_unscaled ~ sparse_car(tauSpa_CountryInt, W_Country_sparse, D_Country_sparse, NCountry_adj, W_n_Country);
  phi_CountryInt_Female_unscaled ~ sparse_car(tauSpa_CountryInt, W_Country_sparse, D_Country_sparse, NCountry_adj, W_n_Country);
  phi_CountrySlope_Male_unscaled ~ sparse_car(tauSpa_CountrySlope, W_Country_sparse, D_Country_sparse, NCountry_adj, W_n_Country);
  phi_CountrySlope_Female_unscaled ~ sparse_car(tauSpa_CountrySlope, W_Country_sparse, D_Country_sparse, NCountry_adj, W_n_Country);

  /** Country intercepts and country slopes **/
  CountryInt_Male ~ normal(phi_CountryInt_Male,sigmaNonspa_CountryInt);
  CountryInt_Female ~ normal(phi_CountryInt_Female,sigmaNonspa_CountryInt);
  CountrySlope_Male ~ normal(phi_CountrySlope_Male,sigmaNonspa_CountrySlope);
  CountrySlope_Female ~ normal(phi_CountrySlope_Female,sigmaNonspa_CountrySlope);

  /** Obtain age/cohort-year interactions **/
  {
    matrix[NAge,NYear] temp;
    int a;
    int tt;

    for (a in 1:NAge) {
      temp[a,1]=0;
      AgeCohort_Year_Male[a,1]=0;
      for (tt in 2:NYear) {
        temp[a,tt]=AgeCohort_Year_Male[a,tt-1]+AgeSlope_Male[a]+CohortSlope_Male[Cohort.ind[a,tt]];
        if (AddNoise[a,tt]==1)
          AgeCohort_Year_Male[a,tt] ~ normal(temp[a,tt],sigma_Age_Year);
      }
    }

    for (a in 1:NAge) {
      temp[a,1]=0;
      AgeCohort_Year_Female[a,1]=0;
      for (tt in 2:NYear) {
        temp[a,tt]=AgeCohort_Year_Female[a,tt-1]+AgeSlope_Female[a]+CohortSlope_Female[Cohort.ind[a,tt]];
        if (AddNoise[a,tt]==1)
          AgeCohort_Year_Female[a,tt] ~ normal(temp[a,tt],sigma_Age_Year);
      }
    }
  } 


  /** Obtain country-year interactions **/
  {
     matrix[NCountry,NYear] temp;
     int d;
     int tt;

     for (d in 1:NCountry) {
       temp[d,1]=0;
       Country_Year_Male[d,1]=0;
       for (tt in 2:NYear)  {
         temp[d,tt]=Country_Year_Male[d,tt-1]+CountrySlope_Male[d];
         Country_Year_Male[d,tt] ~ normal(temp[d,tt],sigma_Country_Year);
       }
     }

     for (d in 1:NCountry) {
       temp[d,1]=0;
       Country_Year_Female[d,1]=0;
       for (tt in 2:NYear)  {
         temp[d,tt]=Country_Year_Female[d,tt-1]+CountrySlope_Female[d];
         Country_Year_Female[d,tt] ~ normal(temp[d,tt],sigma_Country_Year);
       }
     }
   }

   /** Obtain age-country interactions **/
   for (a in 1:NAge) {
     Age_Country_Male[a] ~ normal(0,sigma_Age_Country);
     Age_Country_Female[a] ~ normal(0,sigma_Age_Country);
   }

   /** Obtain gamma **/
   gamma[ii_mis]=-1; //not dealing with cells with missing data  
   {
      vector[N] epsilon;
      int a;
      int s;
      int tt;
      int d;

      epsilon ~ normal(0,sigma_Error);
      
      for (d in 1:NCountry) {
        for (tt in StartYear[d]:EndYear[d]) {
          for (a in 1:NAge) {
            for (s in 1:2) {
              if (s==1)
                gamma[Index[a,s,tt,d]]=Int+Slope*tt+AgeInt_Male[a]+AgeCohort_Year_Male[a,tt]+CountryInt_Male[d]+Country_Year_Male[d,tt]+epsilon[Index[a,s,tt,d]];
              else           
                gamma[Index[a,s,tt,d]]=Int+IntFemale+Slope*tt+SlopeFemale*tt+AgeInt_Female[a]+AgeCohort_Year_Female[a,tt]+CountryInt_Female[d]+Country_Year_Female[d,tt]+epsilon[Index[a,s,tt,d]];
            }
          }
        }
      }
   }

   /** The likelihood. **/
   deaths[ii_obs] ~ poisson(exposure[ii_obs].*gamma[ii_obs])
}