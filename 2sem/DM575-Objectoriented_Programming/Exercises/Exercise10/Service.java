package Exercise10;

import java.util.ArrayList;
import java.util.function.Consumer;
import java.util.stream.Stream;

public class Service {
    private final ArrayList<Callback> callbacks = new ArrayList<>();

    public static void main(String[] args) {
        Service service = new Service();

        service.register("INFO:.*", message -> {
            System.out.println(message);
        });
        service.register("WARNING:.*", message -> {
            System.out.println(message);
        });
        service.register("ERROR:.*", message -> {
            System.out.println(message);
        });
        service.register("TEST:.*", message -> {
            System.out.println(message);
        });

        service.event("INFO: This is a info message");
        service.event("WARNING: This is a warning message");
        service.event("ERROR: This is a error message");
        service.event("TEST: This is a test message");
    }

    Service() {
        ArrayList<Callback> callbacks = new ArrayList<>();
    }

    void register(String pattern, Consumer<String> func) {
        Callback callback = new Callback(pattern, func);
        this.callbacks.add(callback);

    }

    void event(String event) {
        for (Callback callbacks : callbacks) {
            callbacks.call(event);
        }
    }

    void events(Stream<String> events) {
        events.forEach(event -> {
            for (Callback callback : callbacks) {
                callback.call(event);
            }
        });
    }
} 
