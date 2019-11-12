import java.math.BigInteger;
import java.util.*;
import java.util.stream.*;

// clang-format off
include(utils.m4);
include(misc.m4.java);
// clang-format on

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

  static Integer getExponent(Double d) {
    return 1021 - Math.getExponent(d);
  }

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

  static String add1WithOverflow(String in) {
    String tmp = "";
    Boolean hasOverflow = false;
    for (int i = 0; i < in.length(); ++i) {
      char c = in.charAt(i);
      if (c == '1' && hasOverflow) {
        tmp += tmp + "0"; // 01 + 01 = 10
      } else if ((c == '0' && hasOverflow) || (c == '1')) {
        hasOverflow = false;
        tmp += "1";
      } else { // c == '0' && !hasOverflow
        tmp += "0";
      }
    }
    in = tmp;

    return in;
  }

  static String toBits(Integer in) {
    var bits = Integer.toBinaryString(0xFFFF & in);
    return String.format("%016d", Long.parseLong(bits));
  }

  static int fromPositiveBits(String in) {
    int res = 0;
    int valBits = in.length() - 1;
    for (int i = valBits; i > 0; --i) {
      res += Misc.intpow(2, valBits - i) * (in.charAt(i) == '1' ? 1 : 0);
    }
    return res;
  }

  static short fromBits(String in) { return (short)Long.parseLong(in, 2); }

  static String shiftLeft(String tmp) {
    return tmp.substring(1, tmp.length()) + "0";
  }

  static String shiftRight(String tmp) {
    return "1" + tmp.substring(0, tmp.length() - 1);
  }

  static String toBits(Float in) {
    int maxDigits = 16;
    String res = "";

    {
      String tmp = "";
      int num = in.intValue();

      if (num == 0) {
        tmp = "0";
      } else {
        while (num > 0) {
          tmp += (num % 2 == 1 ? "1" : "0");
          num = num / 2;
        }

        tmp = String.format("%16s", tmp).replace(' ', '0');
      }
    }

    {
      String tmp = "";
      Float flt = in - in.intValue();

      for (int i = 0; i < maxDigits && flt != 0; ++i) {
        flt *= 2;
        tmp += (flt > 1 ? "1" : "0");
        flt -= (flt > 1 ? 1 : 0);
      }

      tmp = String.format("%-16s", tmp).replace(' ', '0');
      res = res + tmp;
    }

    return res;
  }

  static Float floatFromBits(String bits) {
    Float out = 0.0f;

    { String numBits = bits.substring(0, 16); }

    {
      String decBits = bits.substring(16, 33);
      Float res = 0.0f;
      for (int idx = 0; idx < decBits.length(); ++idx) {
        res += (float)Math.pow(2, -idx - 1) *
               (decBits.charAt(idx) == '1' ? 1 : 0);
      }
      out += res;
    }

    return out;
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

  public static String[] tetrades(char c) {
    pprint("--------");
    run(0.5);
    run(0.7);

    String res[] = {"", "", "", ""};

    for (int i = 0; i < res.length; ++i) {
      String bin = Integer.toBinaryString((int)c);
      var start = 4 * i;
      var end = 4 * i + 4;
      bin = "0".repeat(16 - bin.length()) + bin;
      // System.out.println("Binary string is " + bin +
      //                    ", substr range = " + start + " - " + end);

      res[i] = bin.substring(start, end);
    }

    return res;
  }

  public static String[] hexTetrades(char c) {

    String res[] = {"", "", "", ""};
    for (int i = 0; i < res.length; ++i) {
      String hex = Integer.toHexString((int)c);
      hex = "0".repeat(4 - hex.length()) + hex;
      res[i] += hex.charAt(i);
    }
    return res;
  }

  public static void runTests() {
    pprint(toBits(12));

    {
      pprint("Char tetrades test");
      char chars[] = {'c', '→', '2'};
      for (char c : chars) {
        System.out.printf("%s -> %s\n", c, Misc.toString(tetrades(c)));
        System.out.printf("%s -> %s\n", c, Misc.toString(hexTetrades(c)));
      }
    }

    {
      pprint("Shift left test");
      var test = toBits(17);
      for (int i = 0; i < 4; ++i) {
        pprint(test);
        test = shiftLeft(test);
      }

      for (int i = 0; i < 4; ++i) {
        pprint(test);
        test = shiftRight(test);
      }
      pprint("shift test ok");
    }

    pprint(fromBits("01111111111111111"));

    {
      pprint("Bit conversion test");
      int nums[] = {1, -1, 127, 0, -120, 3, -3};
      for (int i = 0; i < nums.length; ++i) {
        var bits = toBits(nums[i]);
        System.out.printf("%5d -> %s -> %5d\n", nums[i], bits,
                          fromBits(bits));
      }
    }

    {
      pprint("float conversion test");
      float flts[] = {1.2f, 12.3f, 0.3f};
      for (int i = 0; i < flts.length; ++i) {
        String bits = toBits(flts[i]);
        Float reverse = floatFromBits(bits);

        System.out.printf("%3.3f -> %s -> %3.24f\n", flts[i], bits,
                          reverse);
      }
    }
  }

  public static void main(String[] args) { runTests(); }
}
