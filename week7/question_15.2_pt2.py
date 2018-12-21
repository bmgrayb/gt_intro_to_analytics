#import necessary libraries
import pandas as pd
import pulp as pulp
import locale

#read in csv using pandas
df = pd.read_csv('data/diet.csv')

#strip all $ 
locale.setlocale(locale.LC_ALL,'')
df['Price/ Serving']=df['Price/ Serving'].map(lambda x: locale.atof(x.strip('$')))

#convert data frame to dictionary
food_dict=df.set_index('Foods').T.to_dict('list')

#create the problem variable 
prob=pulp.LpProblem("The Diet Problem", pulp.LpMinimize)

#create a dictionary to store problem variables
lp_foods=pulp.LpVariable.dicts("Food",food_dict.keys(),0)

#create a binary dictionary to track whether a food is selected
lp_selections = pulp.LpVariable.dicts('Select', food_dict.keys(), cat = pulp.LpBinary)

#create the objective function
prob += pulp.lpSum([food_dict[key][0]*lp_foods[key] for key in food_dict.keys()]), "Total Cost of Foods per person"

#add calorie constraints to problem
prob += pulp.lpSum([food_dict[key][2]*lp_foods[key] for key in food_dict.keys()])<= 2500, "Max Calories Requirement"
prob += pulp.lpSum([food_dict[key][2]*lp_foods[key] for key in food_dict.keys()]) >= 1500, "Min Calories Requirement"

#add cholesterol constraints to problem
prob += pulp.lpSum([food_dict[key][3]*lp_foods[key] for key in food_dict.keys()])<= 240, "Max Cholesterol Requirement"
prob += pulp.lpSum([food_dict[key][3]*lp_foods[key] for key in food_dict.keys()]) >= 30, "Min Cholesterol Requirement"

#add fat constraints to problem
prob += pulp.lpSum([food_dict[key][4]*lp_foods[key] for key in food_dict.keys()])<= 70, "Max Fat Requirement"
prob += pulp.lpSum([food_dict[key][4]*lp_foods[key] for key in food_dict.keys()]) >= 20, "Min Fat Requirement"

#add sodium constraints to problem
prob += pulp.lpSum([food_dict[key][5]*lp_foods[key] for key in food_dict.keys()])<= 2000, "Max Sodium Requirement"
prob += pulp.lpSum([food_dict[key][5]*lp_foods[key] for key in food_dict.keys()]) >= 800, "Min Sodium Requirement"

#add carbs constraints to problem
prob += pulp.lpSum([food_dict[key][6]*lp_foods[key] for key in food_dict.keys()])<= 450, "Max Carbs Requirement"
prob += pulp.lpSum([food_dict[key][6]*lp_foods[key] for key in food_dict.keys()]) >= 130, "Min Carbs Requirement"

#add fiber constraints to problem
prob += pulp.lpSum([food_dict[key][7]*lp_foods[key] for key in food_dict.keys()])<= 250, "Max Fiber Requirement"
prob += pulp.lpSum([food_dict[key][7]*lp_foods[key] for key in food_dict.keys()]) >= 125, "Min Fiber Requirement"

#add protein constraints to problem
prob += pulp.lpSum([food_dict[key][8]*lp_foods[key] for key in food_dict.keys()])<= 100, "Max Protein Requirement"
prob += pulp.lpSum([food_dict[key][8]*lp_foods[key] for key in food_dict.keys()]) >= 60, "Min Protein Requirement"

#add vitA constraints to problem
prob += pulp.lpSum([food_dict[key][9]*lp_foods[key] for key in food_dict.keys()])<= 10000, "Max VitA Requirement"
prob += pulp.lpSum([food_dict[key][9]*lp_foods[key] for key in food_dict.keys()]) >= 1000, "Min VitA Requirement"

#add vitC constraints to problem
prob += pulp.lpSum([food_dict[key][10]*lp_foods[key] for key in food_dict.keys()])<= 5000, "Max VitC Requirement"
prob += pulp.lpSum([food_dict[key][10]*lp_foods[key] for key in food_dict.keys()]) >= 400, "Min VitC Requirement"

#add calcium constraints to problem
prob += pulp.lpSum([food_dict[key][11]*lp_foods[key] for key in food_dict.keys()])<= 1500, "Max Calcium Requirement"
prob += pulp.lpSum([food_dict[key][11]*lp_foods[key] for key in food_dict.keys()]) >= 700, "Min Calcium Requirement"

#add iron constraints to problem
prob += pulp.lpSum([food_dict[key][12]*lp_foods[key] for key in food_dict.keys()])<= 40, "Max Iron Requirement"
prob += pulp.lpSum([food_dict[key][12]*lp_foods[key] for key in food_dict.keys()]) >= 10, "Min Iron Requirement"


#add constraint for minimum of 1/10 serving
for key in food_dict.keys():
  prob += lp_foods[key] >= 0.1 * lp_selections[key], ""
  
#frozen broccoli and celery constraint 
prob += lp_selections['Frozen Broccoli'] + lp_selections['Celery, Raw'] <=1, ""

#add varying proteins constraint 
prob += (lp_selections['Roasted Chicken'] + lp_selections['Poached Eggs']  
+ lp_selections['Scrambled Eggs'] + lp_selections['Bologna,Turkey'] 
+ lp_selections['Frankfurter, Beef'] + lp_selections['Ham,Sliced,Extralean']
+ lp_selections['Kielbasa,Prk'] + lp_selections['Pizza W/Pepperoni']
+ lp_selections['Taco'] + lp_selections['Hamburger W/Toppings']
+ lp_selections['Hotdog, Plain'] + lp_selections['Pork'] 
+ lp_selections['Sardines in Oil'] + lp_selections['White Tuna in Water'] )>=3, ""

#export to lp file
prob.writeLP("diet_model_pt2.lp")

prob.solve()

#print status of the problem, each foods optimum value, and the total cost
print ("Status:", pulp.LpStatus[prob.status])
for v in prob.variables():
    if(v.varValue != 0.0):
      print (v.name, "=", v.varValue)

print ("Total Cost of Foods per person = ", pulp.value(prob.objective))

