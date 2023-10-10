// package LP;
// package LP;
    
public class Lasagna {
    
    final int PREP_TIME_PER_LAYER = 2;
    
    public int expectedMinutesInOven() {
        return 40;
    }
    
    public int remainingMinutesInOven(int minCooked) {
        return expectedMinutesInOven() - minCooked;
    }
    
    public int preparationTimeInMinutes(int layers) {
        return layers * PREP_TIME_PER_LAYER;
    }
    
    public int totalTimeInMinutes(int layers, int minCooked) {
        return preparationTimeInMinutes(layers) + minCooked;
    }
}
    
