/*** How to account for the dimension for sex??? ***/

functions {
  /**
  * Modified from https://github.com/mbjoseph/CARstan, 
  * to account for the cohort matrix not consisting of zeros and ones 
  * Return the log probability of a proper intrinsic autoregressive (IAR) prior 
  * with a sparse representation for the adjacency matrix
  *
  * @param phi Vector containing the parameters with a IAR prior
  * @param tau Precision parameter for the IAR prior (real)
  * @param W_sparse Sparse representation of adjacency matrix (int array)
  * @param n Length of phi (int)
  * @param W_n Number of adjacent pairs (int)
  * @param D_sparse Number of neighbors for each location (vector)
  * @param lambda Eigenvalues of D^{-1/2}*W*D^{-1/2} (vector)
  *
  * @return Log probability density of IAR prior up to additive constant
  */
  real sparse_iar_lpdf(vector phi, real tau,
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
  /** Numbers of cells **/
  int<lower=0> N; //total number of cells
  int<lower=0> N_obs; //number of cells with observed data
  int<lower=0> N_mis; //number of cells with missing data (country-years with no data)
  //N=N_obs+N_mis should hold

  /** Indicies for cells with observed or missing data **/
  int<lower=0,upper=N> ii_obs[N_obs]; //indices for cells with observed data
  int<lower=0,upper=N> ii_mis[N_mis]; //indices for cells with missing data

  /** Observed death counts and exposure **/
  int deaths_obs[N_obs]; //observed death counts
  int exposure_obs[N_obs]; //observed exposure

  /** Numbers of age groups, years and countries **/
  int<lower=0> NAge; //number of age groups
  //number of sexes is 2
  int<lower=0> NYear; //number of years under consideration
  int<lower=0> NCountry; //number of countries
  int<lower=0> NCountry_adj; 
    //number of countries with at least one neighbors, arranged as the first set of countries
  int<lower=0> NCountry_noadj; 
    //number of countries with no neighbors, arranged as the second set of countries
  //NCountry=NCountry_adj+NCountry_noadj should hold
  //N=NAge*2*NYear*NCountry should hold

  /** Starting and ending years for each country **/
  int<lower=0> StartYear[NCountry]; //Starting year for each country, label starting with 1
  int<lower=0> EndYear[NCountry]; //Ending year for each country, label starting with 1

  /** Used in CAR priors for age intercepts and age slopes **/
  matrix<lower = 0, upper = 1>[NAge,NAge] W_Age; 
    //adjacency matrix for age groups, diagonal elements equal to 0
  int W_n_Age; 
    //number of adjacent pairs of age groups, (i,j) and (j,i) are considered different pairs

  /** Used in CAR priors for cohort slopes **/
  matrix<lower = -1, upper = 2>[NCohort,NCohort] W_Cohort; 
    //adjacency matrix for cohorts, diagonal elements equal to 0
  int W_n_Cohort; 
    //number of adjacent pairs of cohorts, (i,j) and (j,i) are considered different pairs

  /** Used in CAR priors for country intercepts and country slopes **/
  matrix<lower = 0, upper = 1>[NCountry_adj,NCountry_adj] W_Country; 
    //adjacency matrix for countries, diagonal elements equal to 0
  int W_n_Country; 
    //number of adjacent pairs of countries, (i,j) and (j,i) are considered different pairs
}


transformed data {
  /** Number of cohorts and cohort indicators **/
  int<lower=0> NCohort; //number of cohorts
  int<lower=0> Cohort.ind[NAge,NYear]; //cohort indicators for given age and year.
  //l=first.year+t-age_a, where age_a is the mid-age for age group a
  //  and first.year is the first observation year
  //What is the relationship between NYear, NAge and NCohort???  
  //How to derive the values in cohort.ind???
  int AddNoise[NAge,NCohort];
  // A matrix indicating whether to add noise in order to suppress the random walk part 
  //    for identifiability of the oldest and the youngest cohorts 
  int Index[NAge,NYear,NCountry];
  // The index for given age, year and country.

  /** data counts and exposure **/
  int deaths[N]; //death counts for all cells
  int exposure[N]; //exposure for all cells

  deaths[ii_obs]=y_obs;
  deaths[ii_mis]=-1; //not dealing with missing cells

  exposure[ii_obs]=exposure_obs;
  exposure[ii_mis]=-1; //not dealing with missing cells

  /** Used in CAR priors for age intercepts and age slopes. **/
  int W_Age_sparse[W_n_Age, 3];   
    //adjacency pairs of age groups and the corresponding value of w, 
    //(i,j) and (j,i) are considered different pairs
  vector[NAge] D_Age_sparse;     
    //diagonal of D for age groups
  
  {  
    int counter;
    int i;
    int j;

    counter = 1;
    // loop over upper triangular part of W to identify neighbor pairs
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
    for (i in 1:n) 
      D_Age_sparse[i] = sum(W_Age[i]);
  }

  /** Used in CAR priors for cohort slopes. **/
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
    // loop over upper triangular part of W to identify neighbor pairs
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
    for (i in 1:n) 
      D_Cohort_sparse[i] = sum(W_Cohort[i]);
  }

  /** Used in CAR priors for country intercepts and country slopes. **/
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

  /** Obtain the index for given age, year and country. **/
  {
    int a;
    int tt;
    int d;
    
    for (a in 1:NAge)
      for (tt in 1:NYear)
        for (d in 1:NCountry)
          Index[a,tt,d]=(a-1)*NYear*NCountry+(tt-1)*NCountry+d;
  }
}


parameters {
  real alpha; //alpha, overall intercept
  real beta0; //beta0, overall slope

  vector[NAge] phi_AgeInt_unscaled; //age intercepts, first-order random walk
  vector[NAge] phi_AgeSlope_unscaled; //age slopes, first-order random walk
  vector[NAge] phi_CohortSlope_unscaled; //cohort slopes, first-order random walk
  matrix[NAge,NYear] AgeCohort_Year; //age/cohort-year interactions
  //residual terms in age-year interactions, v_{at}, follow first-order random walk over time, v_{a1}=0)

  vector[NCountry_adj] phi_CountryInt_unscaled; //spatial random effects for country intercepts
  vector[NCountry] CountryInt; //u_d, country intercepts, BYM model
  vector[NCountry_adj] phi_CountrySlope_unscaled; //spatial random effects for country slopes
  vector[NCountry] CountrySlope; //beta_{2d}, country slopes, BYM model
  vector[NCountry,NYear] Country_Year; //country-year interactions
  //residual terms in country-year interactions, h_{dt}, follow first-order random walk over time, h_{d1}=0)  

  vector[NAge,NCountry] Age_Country; //xi_{ad}, age-country interactions, normal
   
  vector[N] gamma; //underlying rates for all cells, deaths~poisson(exposure*gamma)

  real<lower = 0> tau_AgeInt; //precision of age intercepts
  real<lower = 0> tau_AgeSlope; //precision of age slopes
  real<lower = 0> tau_CohortSlope; //precision of cohort slopes
  real<lower = 0> sigma_AgeYear; //standard deviation of age-year interactions
  real<lower = 0> tauSpa_CountryInt; //precision of spatial random effects for country intercepts
  real<lower = 0> sigmaNonspa_CountryInt; //standard deviation of non-spatial random effects for country intercepts
  real<lower = 0> tauSpa_CountrySlope; //precision of spatial random effects for country slopes
  real<lower = 0> sigmaNonspa_CountrySlope; //standard deviation of non-spatial random effects for country slopes
  real<lower = 0> sigma_Country_Year; //standard deviation of country-year interactions
  real<lower = 0> sigma_Age_Country; //standard deviation of age-country interactions
  real<lower = 0> sigma_error; //standard deviation for the errors (epsilons)
}


transformed parameters {
  /** Centering of the "spatial" random effects **/
  vector[NAge] phi_AgeInt; //brute force centering of spatial random effects for age intercepts
  phi_AgeInt = phi_AgeInt_unscaled - mean(phi_AgeInt_unscaled);

  vector[NAge] phi_AgeSlope; //brute force centering of "spatial" random effects for age slopes
  phi_AgeSlope = phi_AgeSlope_unscaled - mean(phi_AgeSlope_unscaled);

  vector[NCohort] phi_CohortSlope; //brute force centering of "spatial" random effects for cohort slopes
  phi_CohortSlope = phi_CohortSlope_unscaled - mean(phi_CohortSlope_unscaled);

  vector[NCountry_adj] phi_CountryInt; //brute force centering of spatial random effects for country intercepts
  phi_CountryInt = phi_CountryInt_unscaled - mean(phi_CountryInt_unscaled);

  vector[NCountry_adj] phi_CountrySlope; //brute force centering of spatial random effects for country slopes
  phi_CountrySlope = phi_CountrySlope_unscaled - mean(phi_CountrySlope_unscaled);

  /** Transform precisions for "spatial" random effects into standard deviations **/
  real sigma_AgeInt;
  sigma_AgeInt=tau_AgeInt^(-0.5);

  real sigma_AgeSlope;
  sigma_AgeSlope=tau_AgeSlope^(-0.5);

  real sigma_CohortSlope;
  sigma_CohortSlope=tau_CohortSlope^(-0.5);

  real sigmaSpa_CountryInt;
  sigmaSpa_CountryInt=tauNonspa_CountryInt^(-0.5);

  real sigmaSpa_CountrySlope;
  sigmaSpa_CountrySlope=tauNonspa_CountrySlope^(-0.5);
}


model {
  /** Check whether N=N_obs+N_mis holds. **/
  if (N!=N_obs+N_mis)
    reject("N!=N_obs+N_mis");
  /** Check whether NCountry=NCountry_adj+NCountry_noadj holds. **/
  if (NCountry!=NCountry_adj+NCountry_noadj)
    reject("NCountry!=NCountry_adj+NCountry_noadj");
  /** Check whether N=NCountry*NYear*NAge*2 holds. **/
  if (N!=NCountry*NYear*NAge*2)
    reject("N!=NCountry*NYear*NAge*2");

  /** overall intercept and overall slope **/
  alpha ~ normal(0,sqrt(100000));
  beta0 ~ normal(0,sqrt(100000));

  /** "Spatial" random errors **/
  phi_AgeInt_unscaled ~ sparse_iar(tau_AgeInt, W_Age_sparse, D_Age_sparse, NAge, W_n_Age);
  phi_AgeSlope_unscaled ~ sparse_iar(tau_AgeSlope, W_Age_sparse, D_Age_sparse, NAge, W_n_Age);
  phi_CohortSlope_unscaled ~ sparse_iar(tau_CohortSlope, W_Cohort_sparse, D_Cohort_sparse, NCohort, W_n_Cohort);
  phi_CountryInt_unscaled ~ sparse_iar(tauSpa_CountryInt, W_Country_sparse, D_Country_sparse, NCountry_adj, W_n_Country);
  phi_CountrySlope_unscaled ~ sparse_iar(tauSpa_CountrySlope, W_Country_sparse, D_Country_sparse, NCountry_adj, W_n_Country);

  /** Country intercepts and country slopes **/
  CountryInt ~ normal(phi_CountryInt,sigmaNonspa_CountryInt);
  CountrySlope ~ normal(phi_CountrySlope,sigmaNonspa_CountrySlope);

  /** Obtain age/cohort-year interactions **/
  {
    matrix[NAge,NYear] temp;
    int a;
    int tt;

    for (a in 1:NAge) {
      temp[a,1]=0;
      AgeCohort_Year[a,1]=0;
      for (tt in 2:NYear) {
        temp[a,tt]=AgeCohort_Year[a,tt-1]+phi_AgeSlope[a]+phi_CohortSlope[Cohort.ind[a,tt]];
        if (AddNoise[a,tt]==1)
          AgeCohort_Year[a,tt] ~ normal(temp[a,tt],sigma_AgeYear);
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
       Country_Year[d,1]=0;
       for (tt in 2:NYear)  {
         temp[d,tt]=Country_Year[d,tt-1]+CountrySlope[d];
         Country_Year[d,tt] ~ normal(temp[d,tt],sigma_CountryYear);
       }
     }
   }

   /** Obtain age-country interactions **/
   for (a in 1:NAge)
     Age_Country[a] ~ normal(0,sigma_Age_Country);

   /** Obtain gamma **/
   gamma[ii_mis]=-1; //not dealing with cells with missing data  
   {
      vector[N] epsilon;
      int a;
      int tt;
      int d;

      epsilon ~ normal(0,sigma_Error);
      
      for (d in 1:NCountry) 
        for (tt in StartYear[d]:EndYear[d]) 
          for (a in 1:NAge) 
            gamma[Index[a,tt,d]]=alpha+beta0*t+phi_AgeInt[a]+AgeCohort_Year[a,tt]+phi_CountryInt[d]+Country_Year[d,tt]+epsilon[Index[a,tt,d]];           
   }

   /** The likelihood. **/
   deaths[ii_obs] ~ poisson(exposure[ii_obs].*gamma[ii_obs])
}