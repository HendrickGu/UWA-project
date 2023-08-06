# Name: Jiaheng Gu ; Student ID : 23925667
def  main(csvfile,region):
    try:
        with open(csvfile,'r') as file:
            #Initialize variables to store the results
            max_population = 0
            min_population = float('inf')
            max_min_country = []
            stdvavg = []
            specific_country_pop = []
            specific_country_landarea = []
            den_list = []
            
            ## Looping through each line of the CSV file
            for line in file:
                #Splitting each line by commas and removing the newline character
                line = line.rstrip('\n').split(",") 
                # Check if the country is in the specified region
                if region == line[5]:
                    population = int(line[1])
                    net_change = int(line[3])
                    country = line[0]
                    #Task 1：Find the country name which has minimum and maximum population in a specific region which has positive net change in population.
                    if net_change > 0:
                        if population > max_population:
                            max_population = population
                            max_population_country = country
                        elif population == max_population:
                            #If there are countries with the same number of people, they all need to output
                            max_population_country += ", " + line[0]
                        if population < min_population:
                            min_population = population
                            min_population_country = line[0]
                        elif population == min_population:
                            min_population_country += ", " + line[0]

              
                    #Task2：Calculate the average and standard deviation of population for a specific region.    
                    specific_country_pop.append(population)
                    n = len(specific_country_pop)
                    sum_pop = sum(specific_country_pop)
                    if n > 0:
                        #The numerator must be greater than 0, otherwise the equation is meaningless
                        avg_pop = round(sum_pop / n,4)
                    else:
                        avg_pop = 0
                    square_diff_pop = 0
                    for p in specific_country_pop:
                        square_diff_pop += (p - avg_pop) ** 2
                    if n > 1:
                        stdv_pop = round((square_diff_pop / (n - 1)) ** 0.5,4)
                    else:
                        #The numerator must be greater than 0, otherwise the equation is meaningless
                        stdv_pop = 0
                    
                    #Task3：Calculate the density of population for each country in a specific region.                         
                    land_area = int(line[4])
                    den_sity = round(population / land_area,4)
                    den_list.append([country,den_sity])
                    def order(elem):
                        return elem[1]
                    den_list.sort(key=order,reverse=True)               
                    
                    #Task4：Calculate the correlation between population and land area for all the countries in a specific region.
                    specific_country_landarea.append(land_area)
                    sum_landarea = sum(specific_country_landarea)
                    avg_landarea = sum_landarea / n
                    sum_popland = 0
                    sum_pop_square = 0
                    sum_land_square = 0
                    for i in range(n):
                        sum_popland +=  (specific_country_pop[i]-avg_pop) * (specific_country_landarea[i]-avg_landarea)
                        sum_pop_square += (specific_country_pop[i]-avg_pop) ** 2
                        sum_land_square += (specific_country_landarea[i]-avg_landarea) ** 2
                        denominator = (sum_pop_square * sum_land_square) ** 0.5    
                    if denominator > 0:
                        r = round(sum_popland / denominator,4)
                    else:
                        #The numerator must be greater than 0, otherwise the equation is meaningless
                        r = 0
        
            # Add the results of relevant task to their respective lists
            max_min_country.append(max_population_country)
            max_min_country.append(min_population_country)
            stdvavg.append(avg_pop)
            stdvavg.append(stdv_pop)

        return max_min_country,stdvavg,den_list,r
     
    except:
        #if the file or state is error, it should output relevant error message.
        return "The file or state do not exist.","The file or state do not exist.","The file or state do not exist.","The file or state do not exist."

