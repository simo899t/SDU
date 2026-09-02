package Exercise10;

import java.util.Optional;
import java.util.stream.Stream;

public class LambdaAndStreams {
    
    public static void main(String[] args) {
        String[] strings = {"a", "bb", "ccc", "dddd", "eeeee"};
        Integer[] integers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        Stream<String> stringStream = Stream.of(strings);
        Stream<Integer> integerStream = Stream.of(integers);


        System.out.println("Shortest string: " + shortesString(Stream.of(strings)));
        System.out.println("Smallest even: " + smallestEven(integerStream));
        System.out.println("Product of odd lengths: " + prodOddLengths(stringStream));
    }

    static String shortesString(Stream<String> strings) {
        return strings
            .reduce((s1, s2) -> s1.length() < s2.length() ? s1 : s2)
            .orElse(null);
    }

    static Integer smallestEven(Stream<Integer> intergers) {
        return intergers
            .filter(s -> s % 2 == 0)
            .reduce((s1, s2) -> s1 < s2 ? s1 : s2)
            .orElse(null);
    }

    static Integer prodOddLengths(Stream<String> strings) {
        return strings
        .filter(s -> s.length() % 2 == 0)
        .map(s -> s.length())
        .reduce(1, (s1, s2) -> s1 * s2);
    }

    static Optional<String> shortesString2(Stream<String> strings) {
        return strings
            .reduce((s1, s2) -> s1.length() < s2.length() ? s1 : s2);
    }

    static Optional<Integer> smallestEven2(Stream<Integer> intergers) {
        return intergers
            .filter(s -> s % 2 == 0)
            .reduce((s1, s2) -> s1 < s2 ? s1 : s2);
    }
}
