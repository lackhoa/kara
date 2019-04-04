from math import factorial

def mm1_avg(lam, mu):
    rho = lam/mu
    return rho/(1-rho)

def C(rho, s):
    tmp0 = sum([(((s*rho)**k)/factorial(k)) for k in range(s)])
    tmp1 = tmp0*(factorial(s)/((s*rho)**s))
    tmp2 = tmp1*(1-rho)
    tmp3 = tmp2+1
    return 1/tmp3

def mms_avg(lam, mu, s):
    rho = lam/(mu*s)
    c = C(rho, s)
    return (rho/(1-rho))*c + s*rho

print("m/m/1: {}".format(mm1_avg(5,7)))
print
print("m/m/1 (but with many server formula): {}".format(mms_avg(5,7,1)))
print("m/m/50: {}".format(mms_avg(5,2,50)))
