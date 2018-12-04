package service;

import org.jongo.marshall.jackson.oid.MongoObjectId;
import org.json.JSONObject;
import code.Smbionet;

public class Experience {
    private String input;
    private String output;


    @MongoObjectId
    String _id;

    public Experience(JSONObject data,boolean allModel) {
        this.input = data.getString("input");
        this.output = getOutput(data.getString("input"),allModel);
    }

    public Experience() {} // ne pas enlever, c'est pour instancier la class avec findOne(...Experience.class)


    public String getOutput(String input, boolean allModel){
        Smbionet smb = new Smbionet();
        smb.generateInput(input);
        smb.run(allModel);
        return smb.readResult();
    }


    public JSONObject toJson() {
        return new JSONObject()
                .put("input", input)
                .put("output", output);
    }

    @Override
    public String toString(){
        return "input: "+input+", output: "+output;
    }

}
