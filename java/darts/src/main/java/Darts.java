import java.lang.Math;
import java.util.HashMap;
import java.util.Map;
import java.util.Iterator;
import java.util.Set;

// private final pointsForDistance =  Map<Integer, Integer> map = Map.of(
//         1, 10,
//         5, 5,
//         1, 10);

// map.entrySet().forEach(System.out::println);

public class Darts {

    private int score;

    Map<Integer, Integer> pointsForDistance = Map.of(1, 10, 5, 5, 1, 10);

    Darts(double x, double y) {

        // float distanceFromCenter = Math.sqrt( Math.pow(x, 2) + Math.pow(x, 2) );

        double distanceFromCenter = Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2));

        // Set set = hmap.entrySet();
        // Iterator iterator = set.iterator();
        // while(iterator.hasNext()) {
        // Map.Entry mentry = (Map.Entry)iterator.next();
        // System.out.print("key is: "+ mentry.getKey() + " & Value is: ");
        // System.out.println(mentry.getValue());
        // }

        Iterator it = pointsForDistance.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry pair = (Map.Entry) it.next();
            System.out.println(pair.getKey() + " = " + pair.getValue());
            
            int comparisonDistance = 5;

            if (comparisonDistance < ( (double) pair.getKey())) {

            };
            

        }

        // this.score = pointsForDistance.get(1);

        this.score = 0;

    }

    int score() {
        return score;
    }

}
