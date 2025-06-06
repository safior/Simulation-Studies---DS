# Simulation-Studies---DS

The simulations can be found in their respective folders. The iid simulations are located in the same folder because both are performed with the gmjmcmc.ts function.

The two iid simulations and the error ar1 simulation were performed using the nam-shub machines provided by the institute.

The latent ar1 simulation was run locally on my own mac.

## Errata

I have discovered a couple mistakes:
1. The baseline predictions for the iid simulations were incorrectly simulated with 120 datapoints instead of 1020, which was the number of datapoints used in the tables that are given in the thesis. This does, in my opinion, not change the validity of the comments made in the thesis regarding the R^2 score. However, the difference is in reality smaller than what was presented in the thesis. This concerns tables 6.4 and 6.8, and corrected files can be found in the "Corrected_iid_base_preds" subfolder to the "base_preds" folder.
2. The detection numbers in table 6.2 are not completely correct. Corrections below:
    - Noise=5. $F_4: 20$
    - Noise=10. $F_4: 20$
    - Noise=10. $x_2: 12$

