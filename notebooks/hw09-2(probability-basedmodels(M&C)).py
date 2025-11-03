### Question 13.2
# In this problem you, can simulate a simplified airport security system at a busy airport. 
# Passengers arrive according to a Poisson distribution with λ1 = 5 per minute (i.e., mean interarrival rate 1 = 0.2 minutes) to the ID/boarding-pass check queue, 
# where there are several servers who each have exponential service time with mean rate 2 = 0.75 minutes. [Hint: model them as one block that has more than one resource.]  
# After that, the passengers are assigned to the shortest of the several personal-check queues, where they go through the personal scanner (time is uniformly distributed between 0.5 minutes and 1 minute). 
# Use the Arena software (PC users) or Python with SimPy (PC or Mac users) to build a simulation of the system, 
# and then vary the number of ID/boarding-pass checkers and personal-check queues to determine how many are needed to keep average wait times below 15 minutes.  
# [If you’re using SimPy, or if you have access to a non-student version of Arena, you can use λ1 = 50 to simulate a busier airport.]

### Solution 13.2
import simpy
import random
import statistics

# ---------------------------
# PARAMETERS
# ---------------------------
LAMBDA_ARRIVAL = 5        # 평균 5명/분 도착
MEAN_ID_TIME = 0.75       # ID체크 평균시간 (지수분포)
PERSONAL_MIN = 0.5        # 개인검색 최소
PERSONAL_MAX = 1.0        # 개인검색 최대
SIM_TIME = 5000           # 시뮬레이션 시간 (분)

# ---------------------------
# DATA COLLECTION
# ---------------------------
wait_times = []  # 전체 시스템 체류시간 저장

# ---------------------------
# FUNCTIONS
# ---------------------------
def passenger(env, name, id_checker, personal_queues):
    """각 승객의 흐름"""
    arrival_time = env.now

    # 1️⃣ ID 체크 단계
    with id_checker.request() as req:
        yield req
        service_time = random.expovariate(1.0 / MEAN_ID_TIME)
        yield env.timeout(service_time)

    # 2️⃣ 개인 검색 단계 (가장 짧은 줄 선택)
    shortest_queue = min(personal_queues, key=lambda q: len(q.queue))
    with shortest_queue.request() as req2:
        yield req2
        service_time2 = random.uniform(PERSONAL_MIN, PERSONAL_MAX)
        yield env.timeout(service_time2)

    # 종료 시점 기록
    total_time = env.now - arrival_time
    wait_times.append(total_time)

def passenger_generator(env, id_checker, personal_queues):
    """포아송 도착"""
    i = 0
    while True:
        i += 1
        yield env.timeout(random.expovariate(LAMBDA_ARRIVAL))
        env.process(passenger(env, f"Passenger {i}", id_checker, personal_queues))

# ---------------------------
# SIMULATION RUNNER
# ---------------------------
def run_simulation(n_id, n_personal, sim_time=SIM_TIME):
    random.seed(20251022)
    env = simpy.Environment()
    id_checker = simpy.Resource(env, capacity=n_id)
    personal_queues = [simpy.Resource(env, capacity=1) for _ in range(n_personal)]

    env.process(passenger_generator(env, id_checker, personal_queues))
    env.run(until=sim_time)

    avg_time = statistics.mean(wait_times)
    return avg_time

# ---------------------------
# EXPERIMENT
# ---------------------------
for id_checkers in [2, 3, 4, 5]:
    for personals in [3, 4, 5, 6]:
        wait_times.clear()
        avg = run_simulation(id_checkers, personals)
        print(f"ID={id_checkers}, Personal={personals} → Avg total time: {avg:.2f} min")
