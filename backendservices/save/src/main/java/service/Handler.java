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


    static JSONObject save(JSONObject input) {
        MongoCollection experiences = getExperience();
        Experience data = new Experience(input.getJSONObject("experience"));
        experiences.insert(data);
        return new JSONObject().put("inserted", true).put("experience",data.toJson());
    }


    static JSONObject get(JSONObject input) {
        MongoCollection experiences = getExperience();
        Integer id = input.getInt("id");
        Experience data = experiences.findOne("{id:#}",id).as(Experience.class);
        if (data == null) {
            throw new RuntimeException("No match found for " + id);
        }
        return data.toJson();
    }


    static JSONObject getAll(JSONObject input) {
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
}
