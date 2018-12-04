package service;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import org.json.JSONObject;

import static service.Handler.*;

@Path("/modeles")
@Produces(MediaType.APPLICATION_JSON)
public class smbionetAPI {

    private static final int INDENT_FACTOR = 2;

    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    public Response process(String input) {
        JSONObject obj = new JSONObject(input);
        try {
            switch (EVENT.valueOf(obj.getString("event"))) {
                case GETALLMODELE:
                    return Response.ok().entity(expAllModel(obj).toString(INDENT_FACTOR)).build();
                case GETVALIDEMODELE:
                    return Response.ok().entity(expValidateModel(obj).toString(INDENT_FACTOR)).build();
                case CONSULT:
                    return Response.ok().entity(consult(obj).toString(INDENT_FACTOR)).build();
                case PURGE:
                    return Response.ok().entity(purge().toString(INDENT_FACTOR)).build();
                case TEST:
                    return Response.ok().entity(test(obj).toString(INDENT_FACTOR)).build();
            }
        }catch(Exception e) {
            JSONObject error = new JSONObject().put("error", e.toString());
            return Response.status(400).entity(error.toString(INDENT_FACTOR)).build();
        }
        return null;
    }

}
