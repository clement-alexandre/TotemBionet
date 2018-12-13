
import datetime
from pymongo import MongoClient



class save:
    def __init__(self, message=None):
        self.message = message
        self.test = MongoClient('mongo',27017).demo.test


    def saveExperience(self, exp, id):
        self.test.update(
            {"_id" : id},
            {"$push" : {
                "experiences" : {
                    "$each": [{"value" : exp, "date": str(datetime.datetime.now())}],
                    "$sort" : {"date" : -1},
                    "$slice" : 5
                }
            }},
            upsert=True
        )
        print("save sucessfull")

    def getExperience(self,id):
        exp = list(self.test.find({"_id" : id}))
        print(exp)

    def getAllExperiences(self):
        i = 0
        arrayExperiences = list(self.test.find())
        while i < len(arrayExperiences):
            print(arrayExperiences[i]['_id'])
            i += 1


    def saveFileExperience(self,pathExp,id):
        file = open(pathExp,"r")
        value = file.read()
        self.test.update(
            {"_id" : id},
            {"$push" : {
                "experiences" : {
                    "$each": [{"value" : value, "date": str(datetime.datetime.now()), "path":pathExp}],
                    "$sort" : {"date" : -1},
                    "$slice" : 5
                }
            }},
            upsert=True
        )
        print("save sucessfull")

    def downloadExperience(self,id):
        exp = list(self.test.find({"_id" : id}))
        values = exp[0]['experiences'][0]['value']
        file = open(id+".out","w")
        file.write(values)
        file.close()







