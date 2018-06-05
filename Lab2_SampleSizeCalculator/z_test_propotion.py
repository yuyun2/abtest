from scipy.stats import norm, zscore

def sample_calculator_propotion(p1, p2, delta, power, sig):

	""" sample calculator of z test for propotion
		input: pi1, pi2, effect size, power, significant level
		output: sample size
	"""
    z_alpha = norm.isf([sig/2]) # two-sided 
    z_oneMinusPower = norm.isf([1 - power]) 
    s =(p1 * (1-p1) + p2 * (1-p2)) ** 2
    n = s * ((z_alpha - z_oneMinusPower)**2) / (delta**2)
    
    return int(round(n[0]))   