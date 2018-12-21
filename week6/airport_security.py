#import libraries we need 
import simpy
import random 
import csv
import os 
import statistics

dir_path = os.path.dirname(os.path.realpath(__file__))
csvfile = dir_path + "/data/airport_security_data.csv"

arrival_rt = 5 #5 per minute
checkin_rt = 0.75 #time it takes to check in customer
min_scan_time = 0.5
max_scan_time = 1.0

sim_run_time = 1000 #time the system runs for
max_resources = 15

#create the model for the airport
class Airport(object):
  
  def __init__(self, env, c, s):
    self.env = env
    self.checker = simpy.Resource(env,c)
    self.scanner = []
    for i in range(s):
        self.scanner.append(simpy.Resource(env,1))
    self.total_time = 0
    self.num_passengers = 1
    self.time_list = []

  #check in process
  def check(self, passenger):
    yield self.env.timeout(random.expovariate(1/checkin_rt))
    
  #scan process  
  def scan(self, passenger):
    yield self.env.timeout(random.uniform(min_scan_time,max_scan_time))
  

def Passenger(env, name, ap):

  #get arrival time
  arrival_time = env.now
  
  #checker request
  with ap.checker.request() as request:
    yield request
    yield env.process(ap.check(name))
    
  check_time = env.now  
  #now find the shortest checkin queue
  min = 0
  for i in range(len(ap.scanner)):
      if(len(ap.scanner[i].queue) < len(ap.scanner[min].queue)):
        min = i
    
  #scanner request
  with ap.scanner[min].request() as request:
    yield request
    yield env.process(ap.scan(name))
    
  #end time for system
  complete_time = env.now
  
  total_wait = complete_time - arrival_time
  ap.total_time += total_wait
  ap.time_list.append(total_wait)
  ap.num_passengers +=1
  

#create the setup method 
def setup(env, checkers, scanners):
  global airport
  airport = Airport(env,checkers,scanners)
  
  #add people to the security line while the simulation is running
  while True:
        yield env.timeout(random.expovariate(arrival_rt))
        env.process(Passenger(env, 'Passenger %d' % airport.num_passengers, airport))

#list to store the values of each row to output
dt = []

#loop over different values for checkers and scanners to see the best combination
for i in range(1,max_resources):
  for j in range(1,max_resources):
    random.seed(i*j)
    
    # Create an environment and start the setup process
    env = simpy.Environment()
    env.process(setup(env, j, i))
    
    # Execute!
    env.run(until=sim_run_time)
    
    avg_wait_time = airport.total_time/airport.num_passengers

    #build row and then append to data table
    temp = []
    temp.append('%s' % (i))
    temp.append('%s' % (j))
    temp.append('%.2f' % (avg_wait_time))
    temp.append('%.2f' % (statistics.median(airport.time_list)))
    temp.append('%s' % (airport.num_passengers))
    dt.append(temp)
    

#output our data to a csv file for evaluation in R
with open(csvfile, "w") as output:
    writer = csv.writer(output, lineterminator='\n')
    writer.writerows(dt)
         
