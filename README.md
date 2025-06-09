# Simulation-Studies---DS

The simulations can be found in their respective folders. The iid simulations are located in the same folder because both are performed with the gmjmcmc.ts function.

The two iid simulations and the error ar1 simulation were performed using the nam-shub machines provided by the institute.

The latent ar1 simulation was run locally on my own mac.

## Errata

I have discovered a few mistakes and imprecisions:
1. The baseline predictions for the iid simulations were incorrectly simulated with 120 datapoints instead of 1020, which was the number of datapoints used in the tables that are given in the thesis. This does, in my opinion, not change the validity of the comments made in the thesis regarding the R^2 score. However, the difference is in reality smaller than what was presented in the thesis. This concerns tables 6.4 and 6.8, and corrected files can be found in the "Corrected_iid_base_preds" subfolder to the "base_preds" folder.
2. The detection numbers in table 6.2 are not completely correct. Corrections below:
    - Noise=5. $F_4: 20$
    - Noise=10. $F_4: 20$
    - Noise=10. $x_2: 12$
3. There is also a mistake in section 6.3.1 where firstly, and confusingly, it says "The algorithm can choose from the following latent $AR(1)$ processes". It should rather say "The algorithm can choose from the following error $AR(1)$ processes". Secondly, the align environment specifying the 3 possible error $AR(1)$ processes says 
```math 
a^t = \phi \cdot a^{t-1} + \epsilon_a^t \qquad \epsilon_a^t \sim N(0, 0.1),
```
where $\phi \in \\{0.1, 0.5, 0.9\\}$. This should have been more correctly, and less confusingly, specified as 
```math
\epsilon_y^t = \phi \cdot \epsilon_y^{t-1} + \delta^t \qquad \delta^t \sim N(0, \sigma_{\delta}^2)
```
where $\sigma_{\delta}^2$ are estimated in the model fitting procedure. I also failed to specify that, in the data generating process, $\sigma_{\delta}^2$ is varied with         $\sigma_{\delta}^2 \in \\{0.01, 0.1, 1, 5, 10, 100\\}$. In addition, when describing the results, I should have specified that $\sigma_y^2 := \sigma_{\delta}^2$.

4. In section 6.2.1, 6.2.2, and 6.3.2, when describing the results, I should have specified that $\sigma_y^2 := \sigma_{\epsilon_y}^2$.
