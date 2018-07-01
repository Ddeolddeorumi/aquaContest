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
        meter_ID = datNew[1]
        tempCul = float(datNew[4])-float(datNew[5])
        print(datNew)
        break

    
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
            if datNew[1] != meter_ID:
                tempCul = float(datNew[4])-float(datNew[5])
            
            dic[datNew[3]] = '{0}'.format(float(datNew[4])-tempCul) # datNew[4]은 현재 누적 tempCul은 전일 누적    
            tempCul = float(datNew[4])

            #if datNew[2] == '20161201' or datNew[2] == '20161130':   #뭐가 잘못된 건지 확인하기 위한 코드 하단 코드도 동일
            #    print(datNew)
            
        
        
        for i in range(24):
            oneLine.append(dic['{0}'.format(i)])

        if datNew[2] == '20161203': ####전체 돌리려면 이거 주석처리해야함
            break

        dic = copy.deepcopy(trueDic)  #다음 것으로 넘어가기 위함. 넘어가면서 0시 사라지면 안 되니까

        #if datNew[2] == '20161201' or datNew[2] == '20161130':
        #    print(datNew)

        dic[datNew[3]] = '{0}'.format(float(datNew[4])-tempCul)
        tempCul = float(datNew[4])

        oneLine = ','.join(oneLine) + '\n'
        
        with open('water_usage_dataset_new.csv', 'a') as fileNew:
            fileNew.write(oneLine)

        if dat.readline() == '': #종료조건
            break

