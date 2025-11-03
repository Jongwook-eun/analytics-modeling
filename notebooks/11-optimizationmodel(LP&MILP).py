### Question 15.2
# In the videos, we saw the “diet problem”. (The diet problem is one of the first large-scale optimization
# problems to be studied in practice. Back in the 1930’s and 40’s, the Army wanted to meet the nutritional
# requirements of its soldiers while minimizing the cost.) In this homework you get to solve a diet problem with real data. The data is given in the file diet.xls. 
# 1. Formulate an optimization model (a linear program) to find the cheapest diet that satisfies the maximum and minimum daily nutrition constraints, and solve it using PuLP.  Turn in your code and the solution. (The optimal solution should be a diet of air-popped popcorn, poached eggs, oranges, raw iceberg lettuce, raw celery, and frozen broccoli. UGH!)
# 2. Please add to your model the following constraints (which might require adding more variables) and solve the new model:
#   a. If a food is selected, then a minimum of 1/10 serving must be chosen. (Hint: now you will need two variables for each food i: whether it is chosen, and how much is part of the diet. You’ll also need to write a constraint to link them.)
#   b. Many people dislike celery and frozen broccoli. So at most one, but not both, can be selected.
#   c. To get day-to-day variety in protein, at least 3 kinds of meat/poultry/fish/eggs must be selected. [If something is ambiguous (e.g., should bean-and-bacon soup be considered meat?), just call it whatever you think is appropriate – I want you to learn how to write this type of constraint, but I don’t really care whether we agree on how to classify foods!]

# If you want to see what a more full-sized problem would look like, try solving your models for the file diet_large.xls, which is a low-cholesterol diet model (rather than minimizing cost, the goal is to minimize cholesterol intake).  I don’t know anyone who’d want to eat this diet – the optimal solution includes dried chrysanthemum garland, raw beluga whale flipper, freeze-dried parsley, etc. – which shows why it’s necessary to add additional constraints beyond the basic ones we saw in the video!
# [Note: there are many optimal solutions, all with zero cholesterol, so you might get a different one.  It probably won’t be much more appetizing than mine.]

### Solution 15.2
import pandas as pd
from pulp import *
# =========================================
# 🥗 1. 데이터 불러오기 및 전처리
# =========================================
# diet.xls 파일 불러오기
df = pd.read_excel("diet.xls")

# “Minimum daily intake”와 “Maximum daily intake” 행의 위치 찾기
imin = df[df['Serving Size'].astype(str).str.contains('Minimum', case=False, na=False)].index[0]
imax = df[df['Serving Size'].astype(str).str.contains('Maximum', case=False, na=False)].index[0]

# 실제 음식 데이터 부분만 추출
foods_df = df.loc[:imin-1].copy()

# 영양소 컬럼명 정의
nutrients = ['Calories','Cholesterol mg','Total_Fat g','Sodium mg',
             'Carbohydrates g','Dietary_Fiber g','Protein g',
             'Vit_A IU','Vit_C IU','Calcium mg','Iron mg']

# --- 가격 컬럼 정리 ---
# $기호, 공백 제거 후 숫자로 변환
foods_df['Price/ Serving'] = (
    foods_df['Price/ Serving'].astype(str)
    .replace('[\$, ]','', regex=True)
    .str.strip()
)
foods_df['Price/ Serving'] = pd.to_numeric(foods_df['Price/ Serving'], errors='coerce').fillna(0)

# --- 영양소 데이터 숫자형 변환 ---
nutr_data = foods_df[nutrients].apply(pd.to_numeric, errors='coerce').fillna(0)
nutr_data.index = foods_df['Foods']

# --- 최소/최대 섭취량 테이블 추출 ---
nutr_min = df.loc[imin, nutrients].apply(pd.to_numeric, errors='coerce').fillna(0)
nutr_max = df.loc[imax, nutrients].apply(pd.to_numeric, errors='coerce').fillna(0)

# --- 음식 리스트 및 가격 딕셔너리 생성 ---
foods = foods_df['Foods']
cost = dict(zip(foods, foods_df['Price/ Serving']))

# =========================================
# ⚙️ 2. Part 1 — Linear Programming (LP)
# =========================================
model = LpProblem("Diet_Problem", LpMinimize)

# 연속형 변수: 각 음식의 섭취량 (servings)
x = LpVariable.dicts("Servings", foods, lowBound=0)

# --- 목적함수: 총비용 최소화 ---
model += lpSum(cost[i] * x[i] for i in foods)

# --- 제약조건: 각 영양소별 최소/최대 섭취량 충족 ---
for n in nutrients:
    model += lpSum(nutr_data.loc[i, n] * x[i] for i in foods) >= nutr_min[n]
    model += lpSum(nutr_data.loc[i, n] * x[i] for i in foods) <= nutr_max[n]

# --- 최적화 실행 ---
model.solve(PULP_CBC_CMD(msg=False))

# --- 결과 출력 ---
print("Status:", LpStatus[model.status])
for i in foods:
    if x[i].value() and x[i].value() > 1e-6:
        print(f"{i:25s}: {x[i].value():7.3f}")
print("\n💰 Total Cost = $", round(value(model.objective), 3))

# =========================================
# ⚙️ 3. Part 2 — Mixed-Integer Linear Programming (MILP)
# =========================================
model2 = LpProblem("Diet_Problem_Extended", LpMinimize)

# 연속 변수 x2 (섭취량), 이진 변수 y (선택 여부)
x2 = LpVariable.dicts("Servings", foods, lowBound=0)
y = LpVariable.dicts("Chosen", foods, cat='Binary')

# --- 목적함수: 총비용 최소화 ---
model2 += lpSum(cost[i] * x2[i] for i in foods)

# --- 영양소 제약 (LP와 동일) ---
for n in nutrients:
    model2 += lpSum(nutr_data.loc[i, n] * x2[i] for i in foods) >= nutr_min[n]
    model2 += lpSum(nutr_data.loc[i, n] * x2[i] for i in foods) <= nutr_max[n]

# --- Big-M 계산: 음식별 최대 섭취 가능량 근사값 ---
Mi = {}
for i in foods:
    ratios = []
    for n in nutrients:
        a = nutr_data.loc[i, n]
        if a > 0:
            ratios.append(nutr_max[n] / a)
    Mi[i] = min(ratios) if ratios else 100.0
    Mi[i] = float(Mi[i] * 1.05)  # 약간의 여유

# --- (a) 음식 선택 시 최소 0.1 serving 포함 ---
for i in foods:
    model2 += x2[i] >= 0.1 * y[i]
    model2 += x2[i] <= Mi[i] * y[i]

# --- (b) 셀러리와 냉동 브로콜리는 동시에 선택 불가 ---
if 'Celery, Raw' in foods.values and 'Frozen Broccoli' in foods.values:
    model2 += y['Celery, Raw'] + y['Frozen Broccoli'] <= 1

# --- (c) 단백질 음식(고기/가금/달걀/생선) 최소 3종 선택 ---
protein_foods = [
    'Poached Eggs','Scrambled Eggs','Roasted Chicken','Hamburger W/Toppings',
    'Hotdog, Plain','Pork','Bologna,Turkey','Frankfurter, Beef',
    'Ham,Sliced,Extralean','Kielbasa,Prk','Sardines in Oil','White Tuna in Water'
]
model2 += lpSum(y[i] for i in protein_foods if i in foods.values) >= 3

# --- 최적화 실행 ---
model2.solve(PULP_CBC_CMD(msg=False))

# --- 결과 출력 ---
print("Status(Part2):", LpStatus[model2.status])
for i in foods:
    if (y[i].value() or 0) > 0.5:
        print(f"{i:25s}: {x2[i].value():7.3f}")
print("\n💰 Total Cost (MILP) = $", round(value(model2.objective), 3))
