import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class GreeterTestCopy {

    @Test
    public void testThatGreeterReturnsTheCorrectGreeting() {
        assertEquals("Hello, World!", new Greeter().getGreeting());
    }

}
