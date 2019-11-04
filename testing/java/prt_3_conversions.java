import java.math.BigInteger;

undefine(format);

public class IEEE_754_repr {
  public Boolean sign;
  public ArrayList<Boolean> exponent;
  public ArrayList<Boolean> mantissa;
}

public class Convert {
  static void rund(Double inputValue) {
    Long longBits = Double.doubleToLongBits(inputValue);
    String strBits = Long.toBinaryString(longBits);
    System.out.printf("  in:  [%2d] %10.4f -> %s\n", strBits.length(),
                      inputValue, strBits);

    String fixed = strBits + "0".repeat(64 - strBits.length());

    String sign = fixed.substring(0, 0);
    String strExp = fixed.substring(0, 51);
    String strMant = fixed.substring(52, 63);

    // String sign = fixed.substring(63, 63);
    // String strExp = fixed.substring(11, 62);
    // String strMant = fixed.substring(0, 10);

    Long longExp = Long.parseLong(strExp, 2);
    Long longMant = Long.parseLong(strMant, 2);
    Double doblMant = Double.longBitsToDouble(longMant);
    System.out.printf(
        "  sign (%d): %s\n  exp (%d): %10d \n  mant (%d): %4.50f\n\n",
        sign.length(), sign, strExp.length(), longExp, strMant.length(),
        doblMant);

    Long parsed = new BigInteger(strBits, 2).longValue();
    Double revesed = Double.longBitsToDouble(parsed);
    System.out.printf("  out: %3.30f\n", revesed);
    System.out.printf("  (%s)\n", revesed.toString());
  }

  static void runf(Float inputValue) {
    Integer intBits = Float.floatToIntBits(inputValue);
    String strBits = Integer.toBinaryString(intBits);
    System.out.printf("  in:  [%2d] %10.4f -> %s\n", strBits.length(),
                      inputValue, strBits);

    Integer parsed = new BigInteger(strBits, 2).intValue();
    Float revesed = Float.intBitsToFloat(parsed);
    System.out.printf("  out: %3.30f\n", revesed);
    System.out.printf("  (%s)\n", revesed.toString());
  }

  static Double getMantissa(Double d) {
    int exponent = Math.getExponent(d);
    return (Math.abs(d) / Math.pow(2, exponent)) - 1;
  }

  static Integer getExponent(Double d) { return 1021 - Math.getExponent(d); }

  static Double getMantissa(Float f) {
    int exponent = Math.getExponent(f);
    return (Math.abs(f) / Math.pow(2, exponent)) - 1;
  }

  static Integer getExponent(Float f) { return 126 - Math.getExponent(f); }

  static Boolean getSign(Double d) { return d >= 0; }

  static Boolean getSign(Float f) { return f >= 0; }

  static String asString(Double d) { return String.format("%.23f", d); }

  static String asString(Float f) { return String.format("%.23f", f); }

  static Double getConverted(Double inputValue) {
    Long longBits = Double.doubleToLongBits(inputValue);
    String strBits = Long.toBinaryString(longBits);
    Long parsed = new BigInteger(strBits, 2).longValue();
    Double reversed = Double.longBitsToDouble(parsed);
    return reversed;
  }

  static Float getConverted(Float inputValue) {
    Integer intBits = Float.floatToIntBits(inputValue);
    String strBits = Integer.toBinaryString(intBits);
    Integer parsed = new BigInteger(strBits, 2).intValue();
    Float reversed = Float.intBitsToFloat(parsed);
    return reversed;
  }

  static String toBitString(char c) {
    String bin = Integer.toBinaryString((int)c);
    return "0".repeat(16 - bin.length()) + bin;
  }

  static String toHexString(char c) {
    String hex = Integer.toHexString((int)c);
    return "0".repeat(4 - hex.length()) + hex;
  }

  static String toBitString32(Float in) {
    final int maxDigits = 32;
    String res = "";

    var head = in.intValue();
    var decm = in - in.intValue();

    for (int i = 0; i < maxDigits / 2 && decm != 0; ++i) {
      decm *= 2;
      res = res + (decm > 1 ? "1" : "0");
      decm -= (decm > 1 ? 1 : 0);
    }

    res = res + "0".repeat(maxDigits / 2 - res.length());

    if (head == 0) {
      res = "0" + res;
    }

    while (head > 0) {
      res = (head % 2 == 1 ? "1" : "0") + res;
      head = head / 2;
    }

    res = "0".repeat(maxDigits - res.length()) + res;

    return res;
  }

  static String toBits(Integer in) {
    String res = "";
    int maxDigits = 8;

    if (in == 0) {
      res = "0" + res;
    }

    for (int i = 0; i < maxDigits && in > 0; ++i) {
      res = (in % 2 == 1 ? "1" : "0") + res;
      in = in / 2;
    }

    res = "0".repeat(maxDigits - res.length()) + res;

    {
      String tmp = "";
      if (in < 0) {
        for (int i = 0; i < res.length(); ++i) {
          char c = res.charAt(i);
          tmp += (c == '1' ? "0" : "1");
        }
      }
      res = tmp;
    }

    {
      String tmp = "";
      Boolean hasOverflow = false;
      for (int i = 0; i < res.length(); ++i) {
        char c = res.charAt(i);
        if (c == '1' && hasOverflow) {
          tmp += tmp + "0"; // 01 + 01 = 10
        } else if ((c == '0' && hasOverflow) || (c == '1')) {
          hasOverflow = false;
          tmp += "1";
        } else { // c == '0' && !hasOverflow
          tmp += "0";
        }
      }
      res = tmp;
    }

    return res;
  }

  static String shiftLeft(String tmp) {
    return tmp.substring(1, tmp.length()) + "0";
  }

  static String shiftRight(String tmp) {
    return "0" + tmp.substring(0, tmp.length() - 1);
  }

  static void run(Double d) {
    System.out.printf("Representation of %f in IEEE 754\n", d);
    System.out.println("Sign: " + getSign(d));
    System.out.println("Expn: " + getExponent(d));
    System.out.println("Mant: " + asString(getMantissa(d)));
    System.out.printf("Errs: %.3f -> %s\n", d.floatValue(),
                      asString(getConverted(d.floatValue())));
    System.out.println("");
  }

  public static void runTests() {
      System.out.println(shiftLeft("001"));
      System.out.println(shiftRight("001"));
  }
}
