public class Twofer {
    // public String twofer(String name) {
    //     return "One for " + (name == null ? "you" : name) + ", one for me.";
    // }
    
    public String twofer(String name) {
        if (name == null){
            return "One for you, one for me.";
        } else {
            return "One for " + name + ", one for me.";
        }
    }

}