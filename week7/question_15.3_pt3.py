#import necessary libraries
import pandas as pd
import pulp as pulp
import locale

global count
count = 0

def rename_column(x,count):
  count+=1
  return str(x) + str(count)


#read in csv using pandas
df = pd.read_csv('data/diet_large.csv')
df.fillna(0)
#df.set_index('Long_Desc')

#df['Long_Desc']=df['Long_Desc'].apply(lambda x: rename_column(x, count))
#df['Long_Desc'].apply(rename_column)

df_constraints = pd.read_csv('data/diet_large_constraints.csv',header=None)
df_constraints_t = df_constraints.T

#convert data frame to dictionary
#food_dict=df.set_index('Long_Desc').T.to_dict('list')
food_dict=df.set_index('Long_Desc').to_dict(orient='index')

#create the problem variable 
prob=pulp.LpProblem("The Diet Problem", pulp.LpMinimize)

#create a dictionary to store problem variables
lp_foods=pulp.LpVariable.dicts("Food",food_dict.keys(),0)

#create the objective function
prob += pulp.lpSum([food_dict[key]['Cholesterol']*lp_foods[key] for key in food_dict.keys()]), "Total Cost of Foods per person"

#add constraints to problem
#prob += pulp.lpSum([food_dict[key][]*lp_foods[key] for key in food_dict.keys()])<= , ""
#prob += pulp.lpSum([food_dict[key][]*lp_foods[key] for key in food_dict.keys()]) >= , ""

for (idx, row) in df_constraints_t.iterrows():
  print(row.loc[1])
  #prob += pulp.lpSum([food_dict[key][idx+1]*lp_foods[key] for key in food_dict.keys()])<= row.loc[1], ""
  #prob += pulp.lpSum([food_dict[key][idx+1]*lp_foods[key] for key in food_dict.keys()]) >=row.loc[0] , ""
  
#export to lp file
prob.writeLP("diet_model_pt3.lp")

pulp.LpSolverDefault.msg = 1
prob.solve()

#print status of the problem, each foods optimum value, and the total cost
print ("Status:", pulp.LpStatus[prob.status])
for v in prob.variables():
    if(v.varValue != 0.0):
      print (v.name, "=", v.varValue)

print ("Total Cost of Foods per person = ", pulp.value(prob.objective))
