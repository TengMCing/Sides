import pygame
import color
import random
import time


class cities_cls():

    def __init__(self, num):

        self.num = num
        self.position = [[0, 0] for x in range(num)]
        self.init_position()

    def init_position(self):

        for i in range(self.num):
            self.position[i][0] = random.uniform(0, width)
            # x = self.position[i][0]
            # if random.random() >= 0.5:
            # 	self.position[i][1] = ((width / 2)**2-(x-width / 2)**2)**0.5 + height /2
            # else:
            # 	self.position[i][1] = -(((width / 2)**2-(x-width / 2)**2)**0.5 - height /2)

            # (width/2)^2-(x-width/2)^2=(y-height/2)^2
            self.position[i][1] = random.uniform(0, height)

    def points_list(self, order):

        temp = []
        for i in order:
            temp.append(self.position[i])

        return temp


class order_cls(list):

    def __init__(self, num=None):

        self.extend([x for x in range(num)])
        self.num = num
        self.shuffle()

    def shuffle(self):

        self[:] = random.sample(self, k=self.num)


class population_cls(list):

    def __init__(self, total_population=None, cities_num=None, mutation_rate=0.01):

        self.mutation_rate = mutation_rate
        self.total_population = total_population
        self.cities_num = cities_num
        self.fitness = [-1 for x in range(total_population)]
        self.t_fitness = [-1 for x in range(total_population)]
        self.prob = [0 for x in range(total_population)]
        self.init_population()

    def init_population(self):

        for i in range(self.total_population):
            self.append(order_cls(self.cities_num))

    def dist(self, x, y, x1, y1):

        return ((abs(x - x1))**2 + (abs(y - y1))**2)**0.5

    def calc_fitness(self, cities):

        for i in range(self.total_population):

            sum = 0

            for j in range(self.cities_num - 1):

                pos1 = cities.position[self[i][j]]

                pos2 = cities.position[self[i][j + 1]]

                sum = sum + self.dist(pos1[0], pos1[1], pos2[0], pos2[1])

            self.fitness[i] = sum

        self.calc_prob()

    def calc_prob(self):
        sum = 0

        for i in range(self.total_population):
            mul = 64
            self.t_fitness[i] = 1 / (self.fitness[i]**mul + 1)

        for num in self.t_fitness:
            sum += num

        for i in range(self.total_population):
            self.prob[i] = self.t_fitness[i] / sum

    def select_parent(self):
        num = random.random()
        i = 0
        while True:
            num = num - self.prob[i]
            if num < 0:
                break
            i = i + 1
            if i >= self.total_population - 1:
                break

        return self[i]

    def crossover(self, alist, blist):

        start = random.randrange(self.cities_num)
        end = random.randrange(self.cities_num)
        if end < start:
            temp = start
            start = end
            end = temp

        list1 = alist[start:end + 1]

        for item in blist:
            if item not in list1:
                list1.append(item)

        return list1

    def mutation(self, alist):

        if random.random() <= mutation_rate:
            if random.random() <= 0.2:
                start = random.randrange(cities_num)
                end = random.randrange(cities_num)
                if start > end:
                    temp = start
                    start = end
                    end = temp
                temp = alist[start:end + 1]
                for i in alist:
                    if i not in temp:
                        temp.append(i)
                alist = temp
            else:
                if random.random() <= 0.5:
                    pos1 = random.randrange(self.cities_num)
                    pos2 = pos1 + 1
                    if pos2 >= self.cities_num:
                        pos2 = pos1 - 1
                else:
                    pos1 = random.randrange(self.cities_num)
                    pos2 = random.randrange(self.cities_num)
                temp = alist[pos1]
                alist[pos1] = alist[pos2]
                alist[pos2] = temp

        return alist

    def evolution(self):

        new_population = []

        while True:
            if len(new_population) >= total_population:
                break
            parentA = self.select_parent()
            parentB = self.select_parent()
            child = self.crossover(parentA, parentB)
            child = self.mutation(child)

            new_population.append(child)

        for i in range(total_population):
            self[i][:] = new_population[i]


# init pygame
pygame.init()
pygame.display.set_caption('TSP with Genetic Algorithm')

run = True

# screen parameter
width = 800
height = 600

# cities parameter
cities_num = 100

# gene parameter
total_population = 500
mutation_rate = 0.1
generation = 0
gcount = 0
last_updated = 0

best_dist = 999999999
best_path = [-1 for x in range(cities_num)]

screen = pygame.display.set_mode((width + 400, height + 50))
# clock = pygame.time.Clock()
font_2 = pygame.font.SysFont('Comic Sans MS', 30)

cities = cities_cls(cities_num)

population = population_cls(total_population, cities_num, mutation_rate)


count = 0
screen.fill(color.white)

start_time = time.time()

while run:

    screen.fill(color.white)
    gcount += 1
    generation += 1
    population.calc_fitness(cities)

    for position in cities.position:
        pygame.draw.circle(screen, color.red, [
                           int(position[0]), int(position[1])], 5)

    current_best_dist = 999999999
    current_best_path = [-1 for x in range(cities_num)]

    fsum = 0
    for i in range(total_population):
        fit = population.fitness[i]
        fsum += fit
        if fit < current_best_dist:
            current_best_dist = fit
            current_best_path = population[i]
    fsum = fsum / total_population

    if best_dist > current_best_dist:
        best_dist = current_best_dist
        best_path = current_best_path[:]
        print("upgrade")
        last_updated = time.time()
        gcount = 0
        population.mutation_rate = mutation_rate
    # print(generation)
    population.mutation_rate -= 0.001
    if population.mutation_rate < mutation_rate:
        population.mutation_rate = mutation_rate
    if population.mutation_rate > 1:
        population.mutation_rate = 1
    if gcount > 50:
        population.mutation_rate += 0.1
        gcount = 0

    textsurface = font_2.render(
        "Best: " + str(round(best_dist, 2)), True, color.fuchsia)
    screen.blit(textsurface, (width + 60, 80))

    textsurface = font_2.render(
        "Genration: " + str(generation), True, color.gold)
    screen.blit(textsurface, (width + 60, 20))

    textsurface = font_2.render(
        "Ave: " + str(round(fsum, 2)), True, color.navy)
    screen.blit(textsurface, (width + 60, 200))

    textsurface = font_2.render(
        "Current Best: " + str(round(current_best_dist, 2)), True, color.navy)
    screen.blit(textsurface, (width + 60, 140))

    textsurface = font_2.render(
        "Mutation Rate: " + str(round(population.mutation_rate * 100, 2)) + "%", True, color.dark_green)
    screen.blit(textsurface, (width + 60, 260))

    textsurface = font_2.render(
        "Population : " + str(total_population), True, color.dark_green)
    screen.blit(textsurface, (width + 60, 320))

    textsurface = font_2.render(
        "Last Updated : " + str(round(last_updated - start_time, 1)) + "S", True, color.violet)
    screen.blit(textsurface, (width + 60, 380))

    textsurface = font_2.render(
        "Running Time : " + str(round(time.time() - start_time, 1)) + "S", True, color.violet)
    screen.blit(textsurface, (width + 60, 440))

    # print(best_path,best_dist)

    # points_list = cities.points_list(current_best_path)

    # pygame.draw.lines(screen,color.black,False,points_list,1)

    points_list = cities.points_list(best_path)

    pygame.draw.lines(screen, color.aqua, False, points_list, 4)

    pygame.display.flip()

    population.evolution()

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            pygame.quit()
            run = False
