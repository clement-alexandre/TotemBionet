package service;

import org.jongo.marshall.jackson.oid.MongoObjectId;
import org.json.JSONObject;

public class Experience {
    private String experience;
    private int id;


    @MongoObjectId
    String _id;

    public Experience(JSONObject data) {
        this.experience = data.getString("experience");
        this.id = data.getInt("id");
    }

    public Experience() {} // ne pas enlever, c'est pour instancier la class avec findOne(...Experience.class)


    public JSONObject toJson() {
        return new JSONObject()
                .put("experience", this.experience)
                .put("output", this.id);
    }

    @Override
    public String toString(){
        return "id: "+id+", experience: "+experience;
    }

}
