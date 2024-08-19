# Bayesian baseline random effects model - BB-M

 ![BB-8 Source: Wikipedia](https://upload.wikimedia.org/wikipedia/en/3/39/BB-8%2C_Star_Wars_The_Force_Awakens.jpg)

## The BB model

Let $Y_{t,s}$ be the number of dengue cases at time $t$ and region $r$.

$$Y_{t,r} \sim NegBin( \theta_{t,r}, \phi_r), \quad \phi > 0$$ $$ \log(\theta_{t,r}) = \alpha_r + \beta_{S[t],r} + \gamma_{wS[t],r}$$ where

$S = (2015/2016, 2016/2017, 2017/2018,...)$,

$wS = (1,2,3,\ldots)$, wS = 1 \<--\> epiweek 41,

$r = (1,2,\ldots, R)$.

## Random effects and priors

Fixed effects: $\{ \alpha_r \} \sim N(0, 1000 I_R)$

Random effects:

-   $\beta_{S,r}$, the season effect, is a zero-mean Gaussian random effect with variance $\sigma^2_{\beta,r}$
-   $\gamma_{eS,r}$, the week effect, is a cyclic Bayesian spline (rw2) with variance $\sigma^2_{\gamma,r}$
-   Let $\theta_r = (\beta_{.,r}, \gamma_{.,r}, \phi_r, \sigma_{.,r})$. For simplicity, $\theta_r \perp \theta_{r'}$, $\forall r \neq r'$.

Priors:

-   Variances: PC prior (3, 0.01), $P\{(\sigma > 3) = 0.01\}$
-   $\phi$: PC mgamma (7). $-log(\phi) \sim Gamma(1/7, 1/7)$

## Predictions

For a new season, $S^*$, we use the posterior predictive distribution to estimate/forecast the number of new cases for the region $s$ at week $w$, $Y_{t^*, s}$, where $S[t^*] = S^*$ and $wS[t^*] = w$.

$$p(Y_{t^*, s} \mid \mathbf{y}) = \int_\theta p(Y_{t^*, s} | \theta) p (\theta | \mathbf{y}) d\theta$$ Totals of interest:

-   Cases per week by UF: $\sum_{s \in UF} Y_{t^*, s}$
-   Cases by UF: $\sum_{t^*} \sum_{s \in UF} Y_{t^*, s}$
