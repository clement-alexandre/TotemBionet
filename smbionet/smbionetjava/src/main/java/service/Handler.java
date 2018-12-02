package service;

import com.mongodb.MongoClient;
import org.jongo.Jongo;
import org.jongo.MongoCollection;
import org.jongo.MongoCursor;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;


class Handler {

    //@Autowired
    //private static messageProducer producer = new messageProducer();


    static JSONObject expAllModel(JSONObject input) {
        MongoCollection experiences = getExperience();
        Experience data = new Experience(input.getJSONObject("experience"));
        experiences.insert(data);
        return new JSONObject().put("inserted", true).put("experience",data.toJson());
    }


    static JSONObject expValidateModel(JSONObject input) {
        MongoCollection experiences = getExperience();
        Experience data = new Experience(input.getJSONObject("experience"));
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

    static JSONObject purge(JSONObject input) {
        MongoCollection experience = getExperience();
        if(input.getString("use_with").equals("caution")) {
            experience.drop();
            return new JSONObject().put("purge", "done");
        }
        throw new RuntimeException("Safe word does not match what is expected!");
    }
    
    private static MongoCollection getExperience() {
        MongoClient client = new MongoClient(Network.HOST, Network.PORT);
        return new Jongo(client.getDB(Network.DATABASE)).getCollection(Network.COLLECTION);
    }
}
