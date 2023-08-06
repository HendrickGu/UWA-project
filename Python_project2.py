# Name: Jiaheng Gu ,Student ID: 23925667
def main(csvfile):
    try:
        region_dict = {}
        info_dict = {}
        
        with open(csvfile, 'r') as file:
            lines = file.readlines()
            #Find indices of the columns
            header = lines[0].rstrip('\n').lower().split(",")
            country_index = 0
            population_index = 0
            area_index = 0
            net_change_index = 0
            region_index = 0
            
            for i in range(len(header)):
                column = header[i]
                if column == 'country':
                    country_index = i
                elif column == 'population':
                    population_index = i
                elif column == 'land area':
                    area_index = i
                elif column == 'net change':
                    net_change_index = i
                elif column == 'regions':
                    region_index = i
            
            for line in lines[1:]:
                line = line.rstrip('\n').split(",")
                if len(line) != len(header):#skip the rows with differernt number of columns
                    continue
                region = line[region_index].lower()#All string data in the file is case-insensitive
                country = line[country_index].lower()
                population = int(line[population_index])
                area = float(line[area_index])
                net_change=int(line[net_change_index])
                if not region or not country:#region or country should not be empty
                    continue
                
                #for region_dict
                if population > 0 and area > 0:#Population,area should be positive, rows include invalid data should be removed
                    if region in region_dict:
                        if country in region_dict[region]['country']:#There should not be two or more indentical country in a region
                            continue
                        else:
                            region_dict[region]['country'].append(country)
                            region_dict[region]['population'].append(population)
                            region_dict[region]['area'].append(area)
                    else:
                        region_dict[region] = {'country':[country],'population': [population], 'area': [area]}
                
                #for info_dict
                if population > 0 and area > 0:#Population,area should be positive
                    if region in info_dict:
                        if country in info_dict[region]:#There should not be two or more indentical country in a region
                            continue
                        else:
                            info_dict[region][country] = [population,net_change,0,area,0]
                    else:
                        info_dict[region] = {country:[population,net_change,0,area,0]}
                  
            #for loop in region_dict
            for key in region_dict:
                std_error = calculate_std_error(region_dict[key]['population'])
                cosine_sim = calculate_cosine_similarity(region_dict[key]['population'], region_dict[key]['area'])
                # Store results in dictionary
                region_dict[key] = [std_error, cosine_sim]
            
            #for loop in info_dict
            for key in info_dict:
                population_list = []
                for country in info_dict[key]:
                    population_list.append(info_dict[key][country][0])
                sorted_countries = sorted(info_dict[key].items(),key=lambda x:(-x[1][0],-x[1][3],x[0]))
                rank = 1
                for country,country_info in sorted_countries:
                    country_info[2] = round(country_info[0] / sum(population_list) * 100,4)
                    country_info[3] = round(country_info[0] / country_info[3],4)
                    country_info[4] = rank
                    rank += 1
                    
        return region_dict,info_dict
    except:
        return "The file does not exist.","The file does not exist."

def calculate_std_error(population):
    # Calculate standard error of population
    sum_pop = sum(population)
    n = len(population)
    avg_pop = sum_pop / n
    square_diff_pop = 0
    for p in population:
        square_diff_pop += (p - avg_pop) ** 2
    if n > 1:
        stdv_pop = (square_diff_pop / (n - 1)) ** 0.5
    else:
        #The numerator must be greater than 0, otherwise the equation is meaningless
        stdv_pop = 0
    std_error = round(stdv_pop / (n ** 0.5), 4)
    return std_error
    
def calculate_cosine_similarity(population, area):
    # Calculate cosine similarity between population and land area
    sum_product = 0
    sum_pop_squared = 0
    sum_area_squared = 0
    n = len(population)
    for i in range(n):
        sum_product += population[i] * area[i]
        sum_pop_squared += population[i] ** 2
        sum_area_squared += area[i] ** 2
    if sum_pop_squared == 0 or sum_area_squared == 0:
        #The numerator must be greater than 0, otherwise the equation is meaningless
        cosine_sim = 0
    else:
        cosine_sim = round(sum_product / (((sum_pop_squared) ** 0.5) * ((sum_area_squared) ** 0.5)), 4)
    return cosine_sim
    

    
