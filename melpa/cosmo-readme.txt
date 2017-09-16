This package provides a cosmological calculator for Lambda-CDM
models.  Such a framework describes a homogeneous and isotropic
universe containing a cosmological constant (Lambda) and a Cold
Dark Matter (CDM) component, besides ordinary species.  The
model is characterized by the following parameters:

- H_0 :: Hubble parameter (expansion rate) today.
- Omega_m0 :: Matter density parameter today.
- Omega_Lambda :: Cosmological constant density parameter.
- Omega_r0 :: Relativistic species (e.g., photons plus
              neutrinos) density parameter today.
- Omega_k0 :: Curvature density parameter today.  This
              parameter is derived from the others above
              according to Friedmann's equation
              Omega_m0 + Omega_Lambda + Omega_r0 + Omega_k0 = 1.

All cosmological quantities are computed at a given redshift
value:

- redshift :: Gravitational redshift of photons frequency due to the
              expansion of the Universe.

Definitions follow Hogg (1999)
<https://arxiv.org/abs/astro-ph/9905116>.

Names with "--" are for functions and variables that are meant to
be for internal use only.
