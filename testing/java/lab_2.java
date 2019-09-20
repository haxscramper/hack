import java.util.Scanner;

public class Main {
  final static Float aVar = 7.2f;
  final static Float bVar = 14.3f;
  final static Float cVar = 13.4f;
  final static Float xVar = 5.6f;

  static float compute(Float a, Float b, Float c, Float x) {
    return (float)((a * x + 3.8 * Math.tan(x)) / (Math.sqrt(b * x * x + c)));
  }

  public static void main(String[] args) {
    if (args.length == 0 || args[0].equals("test-a")) {
      System.out.println(compute(aVar, bVar, cVar, xVar));
    } else if (args[0].equals("test-b")) {
      if (args.length >= 5) {
        System.out.println(
            compute(Float.parseFloat(args[1]), Float.parseFloat(args[2]),
                    Float.parseFloat(args[3]), Float.parseFloat(args[4])));
      } else {
        System.out.println("Missing one of the arguments");
      }
    } else if (args[0].equals("test-c")) {
      java.util.Scanner input = new Scanner(System.in);

      System.out.println("Enter A");
      Float a = input.nextFloat();
      System.out.println("Enter B");
      Float b = input.nextFloat();
      System.out.println("Enter C");
      Float c = input.nextFloat();
      System.out.println("Enter X");
      Float x = input.nextFloat();

      System.out.println(compute(a, b, c, x));
    } else {
      System.out.println("Enter 'test-a' to run first test case, 'test "
                         + "-b' and four float  arguments to run second"
                         + "test case, 'test-c' to run third test case");
    }
  }
}
