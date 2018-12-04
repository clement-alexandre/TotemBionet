package service;

import code.NuSMV;
import code.Smbionet;
import com.mongodb.MongoClient;
import org.jongo.Jongo;
import org.jongo.MongoCollection;
import org.jongo.MongoCursor;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import static code.Out.readFile;


class Handler {

    //@Autowired
    //private static messageProducer producer = new messageProducer();


    static JSONObject expAllModel(JSONObject input) {
        MongoCollection experiences = getExperience();
        Experience data = new Experience(input.getJSONObject("experience"),true);
        experiences.insert(data);
        return new JSONObject().put("inserted", true).put("experience",data.toJson());
    }


    static JSONObject expValidateModel(JSONObject input) {
        MongoCollection experiences = getExperience();
        Experience data = new Experience(input.getJSONObject("experience"),false);
        experiences.insert(data);
        return new JSONObject().put("inserted", true).put("experience",data.toJson());
    }

    static JSONObject consult(JSONObject input) {
        MongoCollection experience = getExperience();
        MongoCursor<Experience> cursor = experience.find().as(Experience.class);
        List array = new ArrayList();
        while(cursor.hasNext()) {
            array.add(cursor.next().toJson());
        }
        return new JSONObject().put("experiences", array);
    }

    static JSONObject purge() {
        MongoCollection experience = getExperience();
        experience.drop();
        return new JSONObject().put("purge", "done");
    }
    
    private static MongoCollection getExperience() {
        MongoClient client = new MongoClient(Network.HOST, Network.PORT);
        return new Jongo(client.getDB(Network.DATABASE)).getCollection(Network.COLLECTION);
    }

    static  JSONObject test(JSONObject input){
        JSONObject exp = input.getJSONObject("experience");
        Smbionet smb = new Smbionet();
        smb.generateInput(exp.getString("input"));
        Process proc;
        String NUSMVPATH="NuSMV";
        String resultests ="";
        try {
            proc = Runtime.getRuntime().exec(NUSMVPATH + " -dynamic result.txt");
            InputStream inputstream2=proc.getInputStream();
            InputStreamReader inputstreamreader2=new InputStreamReader(inputstream2);
            BufferedReader reader2=new BufferedReader(inputstreamreader2);
            String line2 = reader2.readLine();
            resultests = resultests + line2 +"\n";
                while (line2 != null)
                {
                    line2 = reader2.readLine();
                    resultests = resultests + line2+"\n";
                }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return new JSONObject().put("result",resultests).put("input",readFile("result.txt"));
    }
}
