package Exercise1;
public class typevalue {
    public static void main(String[] args) {
        int i = 3;
        double d = 2.19;
        
        System.out.println("a) " + checkType(i + 3) + " : " + (i + 3));
        System.out.println("b) " + checkType((i + 3.0) * i) + " : " + ((i + 3.0) * i));
        System.out.println("c) " + checkType(45 - i + 23) + " : " + (45 - i + 23));
        System.out.println("d) " + checkType(3.24 + i * 3) + " : " + (3.24 + i * 3));
        System.out.println("e) " + checkType(2 * 5.0 / i + 3) + " : " + (2 * 5.0 / i + 3));
        System.out.println("f) " + checkType(2 * 5 / i + 3) + " : " + (2 * 5 / i + 3));
        System.out.println("g) " + checkType(4 - d + i / 2) + " : " + (4 - d + i / 2));
        System.out.println("h) " + checkType((d + 2) / i) + " : " + ((d + 2) / i));
    }

    private static String checkType(Object value) {
        if (value instanceof Integer) {
            return "int";
        } else if (value instanceof Double) {
            return "double";
        } else {
            return "unknown";
        }
    }
}
