package Exercise10;

import java.util.function.Consumer;

public class Callback {

    private final String pattern;
    private final Consumer<String> consumer;
    
    Callback(String pattern, Consumer<String> func) {
        this.pattern = pattern;
        this.consumer = func;
    }

    void call(String input) {
        if (input.matches(pattern)) {
            consumer.accept(input);
        }
    }
}
