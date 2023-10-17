import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

public class TwoferTest {

    private Twofer twofer;

    @Before
    public void setup() {
        twofer = new Twofer();
    }

    @Test
    public void noNameGiven() {
        
        assertThat(twofer.twofer(null))
            .isEqualTo("One for you, one for me.");
    }

    @Test
    public void aNameGiven() {
        String input = "Alice";
        String expected = "One for Alice, one for me.";

        assertEquals(expected, twofer.twofer(input));
    }

    @Ignore("foo")
    @Test
    public void anotherNameGiven() {
        String input = "Bob";
        String expected = "One for Bob, one for me.";

        assertEquals(expected, twofer.twofer(input));
    }
}
