from scipy.stats import norm, zscore

def sample_calculator_proportion(p1, delta, power, sig):

	""" sample calculator of z test for propotion
		input: pi1, pi2, effect size, power, significant level
		output: sample size
	"""
	p2 = delta - p1
	print(p2)
	z_alpha = norm.isf([sig/2]) # two-sided 
	print(z_alpha)
	z_oneMinusPower = norm.isf([1 - power]) 
	print(z_oneMinusPower)
	s =(p1 * (1-p1) + p2 * (1-p2)) ** 2
	print(s)
	n = s * ((z_alpha - z_oneMinusPower)**2) / (delta**2)
	print(n)

	return int(round(n[0]))

print(sample_calculator_proportion(.1, .05, .8, .05))