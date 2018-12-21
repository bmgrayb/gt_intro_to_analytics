from SimPy.Simulation import *
from random import expovariate,seed

## Model components ------------------------

class Source(Process):
    """ Source generates customers randomly"""

    def generate(self,number,interval,resource,mon):
        for i in range(number):
            c = Customer(name = "Customer%02d"%(i,))
            activate(c,c.visit(b=resource,M=mon))
            t = expovariate(1.0/interval)
            yield hold,self,t

class Customer(Process):
    """ Customer arrives, is served and leaves """

    def visit(self,b,M):
        arrive = now()
        yield request,self,b
        wait = now()-arrive
        M.observe(wait)
        tib = expovariate(1.0/timeInBank)
        yield hold,self,tib
        yield release,self,b

## Experiment data -------------------------

maxNumber = 500
maxTime = 200000.0  # minutes
timeInBank = .75   # mean, minutes
ARRint = .2     # mean, minutes
Nc = 3            # number of counters
theSeed = 393939

## Model  ----------------------------------

def model(runSeed=theSeed):
    seed(runSeed)
    k = Resource(capacity=Nc,name="Clerk")
    wM = Monitor()

    initialize()
    s = Source('Source')
    activate(s,s.generate(number=maxNumber,interval=ARRint,
                          resource=k,mon=wM),at=0.0)
    simulate(until=maxTime)
    return (wM.count(),wM.mean())

## Experiment/Result  ----------------------------------

theseeds = [393939,31555999,777999555,319999771]
for Sd in theseeds:
    result = model(Sd)
    print ("Average wait for %3d completions was %6.2f minutes."% result)
