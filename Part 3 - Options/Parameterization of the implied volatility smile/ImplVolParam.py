import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import least_squares

# Market implied volatilities (percent)
iv_market_pct = np.array([
24.46,23.96,23.47,23.01,22.57,22.14,21.73,21.34,20.97,
20.61,20.27,19.95,19.65,19.36,19.09,18.84,18.61,18.39,
18.20,18.02,17.94,17.86,17.73,17.62,17.53,17.47,17.45,
17.43,17.41,17.43,17.46,17.48,17.50,17.56,17.62,17.70,
17.78,17.82,17.86
])

# Convert to decimal
iv_market = iv_market_pct / 100.0

F = 9.22     # forward price
T = 0.5      # maturity in years

# Strike prices
K =np.array([7.3,7.4,7.5,7.6,7.7,7.8,7.9,8,8.1,8.2,8.3,
             8.4,8.5,8.6,8.7,8.8,8.9,9,9.1,9.2,9.25,9.3,
             9.4,9.5,9.6,9.7,9.75,9.8,9.9,10,10.1,10.2,10.25,
             10.3,10.4,10.5,10.6,10.7,10.75])

# Log-moneyness
k = np.log(K / F)

# Market total variance
w_market = iv_market**2 * T

# Raw SVI model

def svi_total_variance(k, a, b, rho, m, sigma):
    return a + b * (rho * (k - m) + np.sqrt((k - m)**2 + sigma**2))

# Calibration Objective

def svi_residuals(params, k, w_market):
    a, b, rho, m, sigma = params
    w_model = svi_total_variance(k, a, b, rho, m, sigma)
    if ((a+b*sigma*np.sqrt(1-rho**2))<0):
        return np.inf
    return w_model - w_market

# Initial Guess and Bounds

x0 = np.array([
    0.02,   # a
    0.20,   # b
   -0.30,   # rho
    0.00,   # m
    0.20    # sigma
])

bounds = (
    [-1.0,   1e-6,  -0.999, -1.0,  1e-6],  # lower
    [1.0,   5.0,    0.999,  1.0,  5.0]    # upper
)

# Calibration run

result = least_squares(
    svi_residuals,
    x0,
    bounds=bounds,
    args=(k, w_market)
)

a, b, rho, m, sigma = result.x

# Display Results

print("\nCalibrated SVI parameters:")
print(f"a     = {a:.6f}")
print(f"b     = {b:.6f}")
print(f"rho   = {rho:.6f}")
print(f"m     = {m:.6f}")
print(f"sigma = {sigma:.6f}")

# Model-implied vols
w_fit = svi_total_variance(k, a, b, rho, m, sigma)
iv_fit = np.sqrt(w_fit / T)

rmse = np.sqrt(np.mean((iv_fit - iv_market)**2))

print(f"\nIV RMSE = {rmse:.6%}")

# Plot MKT implied volatility VERSUS SVI vol

plt.figure(figsize=(8, 5))
plt.plot(K, iv_market * 100, "o", label="Market IV")
plt.plot(K, iv_fit * 100, "-", linewidth=2, label="SVI fit")
plt.axvline(F, color="gray", linestyle="--", label="ATM")
plt.xlabel("Strike")
plt.ylabel("Implied Volatility (%)")
plt.title("SVI Calibration")
plt.legend()
plt.grid(True)
plt.show()