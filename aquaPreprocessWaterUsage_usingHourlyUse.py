import copy

with open ('water_usage_dataset.csv', 'r') as dat:
    for line in dat:
        header = line.replace('"','')
        header = header.split(sep=',')
        header = header[1:3]
        time = list(range(24))
        time.reverse()
        for i in time:
            header.insert(2,i)
        header = list(map(str,header))
        header = ','.join(header) + '\n'
        with open('water_usage_dataset_new.csv', 'w') as fileNew:
            fileNew.write(header)
        print(header)
        break #header 만들기 각 열의 이름



    dic = {}
    for i in range(24):
        dic['{0}'.format(i)] = '0' #빈 시간 만들기

    trueDic = copy.deepcopy(dic)
    
    for line in dat:
        datNew = line.replace('"','')
        datNew = datNew.replace('\n','')
        datNew = datNew.split(sep=',')
        date = datNew[2]
        print(datNew)
        break

    #unableUsage: 어느정도의 사용량까지가 노이즈가 아닌지
    unableUsage = 10
    
    while True:
    
        oneLine = [datNew[1],datNew[2]]

        #하루에 대한 것    

        for line in dat:
            datNew = line.replace('"','')
            datNew = datNew.replace('\n','')
            datNew = datNew.split(sep=',')
            
            if datNew[2] == 'NA':
                continue
            if datNew[2] != date:
                date = datNew[2]
                break
 
            
            if float(datNew[5]) > unableUsage: #unableUsage
                datNew[5] = '0'
                
            dic[datNew[3]] = datNew[5] # datNew[3]은 시간 datNew[5]는 사용량    
            #print(datNew)

        if datNew[2] == '20161204': ####전체 돌리려면 이거 주석처리해야함
            break
        
        for i in range(24):
            oneLine.append(dic['{0}'.format(i)])

        dic = copy.deepcopy(trueDic)  #다음 것으로 넘어가기 위함. 넘어가면서 0시 사라지면 안 되니까

        if float(datNew[5]) > unableUsage: #unableUsage
                datNew[5] = '0'

        dic[datNew[3]] = datNew[5]

        oneLine = ','.join(oneLine) + '\n'
        
        with open('water_usage_dataset_new.csv', 'a') as fileNew:
            fileNew.write(oneLine)


        
