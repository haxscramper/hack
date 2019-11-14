public class Main {
    final public static String A = "Value of global method constant";
    final private static String b = "Value of local method constant";

    static int sum(int a, int b) {
        return a + b;
    }

    public static void main(String[] args) {
        final String B = "Local method constant";
        String c = "Local method variable";
        if (args.length == 0 || args[0].equals("test-a")) {
            System.out.println("Hello World");
        } else if (args[0].equals("test-b")) {
            System.out.println(args[1]);
        } else if (args[0].equals("test-c"))  {
            System.out.println(sum(
                Integer.decode(args[1]),
                Integer.decode(args[2])));
        } else if (args[0].equals("test-d")) {
            System.out.println(A);
            System.out.println(b);
            System.out.println(B);
            System.out.println(c);
        }
    }
}
