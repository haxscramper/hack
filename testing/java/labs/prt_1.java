public class HelloWorld {
    public static void main(String[] args) {
        if (args.length < 3) {
            System.out.println("Three arguments required");
        } else {
            Float a = Float.parseFloat(args[0]);
            Float b = Float.parseFloat(args[1]);
            Float c = Float.parseFloat(args[2]);

            Float p = (a + b + c) / 2;

            Double area = Math.sqrt(p * (p - a) * (p - b) * (p - c));
            System.out.print("Area of triangle is ");
            System.out.println(area);
        }
    }
}
