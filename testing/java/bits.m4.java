import java.util.*;
import java.util.stream.*;
import javax.swing.*;

include(utils.m4);

class Bits {
    ArrayList<Boolean> bits;

    public String toString() {
        String res = new String();
        for (int i = bits.size() - 1; i >= 0; --i) {
            res += bits.get(i) ? "1" : "0";
        }
        return res;
    }

    public void debugPrint() {
        Pprint(toNum() + ":");
        for (int i = bits.size() - 1; i >= 0; --i) {
            Pprint(i + " - " + (bits.get(i) ? "1" : "0"));
        }
    }

    public void show() {
        Pprint(toString());
    }

    public Bits() {
        bits = new ArrayList<Boolean>();
    }

    public static Bits fromNum(long num) {
        var res = new Bits();

        if (num == 0) {
            res.bits.add(false);
            return res;
        }

        while (num > 0) {
            res.bits.add(num % 2 == 1);
            num = num / 2;
        }

        return res;
    }

    public static Bits fromDbl(Double flt, Integer maxDigits) {
        var res = new Bits();
        for (int i = 0; i < maxDigits && flt != 0; ++i) {
            flt *= 2;
            res.bits.add(flt > 1);
            flt -= (flt > 1 ? 1 : 0);
        }
        return res;
    }

    public long toNum() {
        long res = 0;
        for (int idx = 0; idx < bits.size(); ++idx) {
            res += (long)Math.pow(2, idx) * (bits.get(idx) ? 1 : 0);
        }
        return res;
    }

    public Double toDbl() {
        Double res = 0.0;
        for (int idx = 0; idx < bits.size(); ++idx) {
            res += Math.pow(2, -idx) * (bits.get(idx) ? 1 : 0);
        }
        return res;
    }

    // TODO return true/false on overflow
    public void doShift(char direction) {
        if (direction == 'r' || direction == 'R') {
            for (int i = 0; i < bits.size() - 1; ++i) {
                bits.set(i, bits.get(i + 1));
            }
            bits.set(bits.size() - 1, false);
        } else {
            for (int i = bits.size() - 1; i >= 1; --i) {
                bits.set(i, bits.get(i - 1));
            }
            bits.set(0, false);
        }
    }

    public void resize(int targetSize) {
        if (bits.size() >= targetSize) {
            bits = new ArrayList<Boolean>(bits.subList(0, targetSize - 1));
        } else {
            bits.addAll(new ArrayList<Boolean>(
                Collections.nCopies(targetSize - bits.size(), false)));
        }
    }

    public int size() {
        return bits.size();
    }

    public Boolean get(Integer index) {
        return bits.get(index);
    }
}

class BitsTest {
    public static void main(String[] args) {
        {
            int[] test = {17};
            for (int i = 0; i < test.length; ++i) {
                Bits bits = Bits.fromNum(test[i]);

                for (int b = 0; b < bits.size() / 2 + 2; ++b) {
                    System.out.print(bits.toString() + " r-> ");
                    bits.doShift('r');
                }

                bits.doShift('l');

                for (int b = 0; b < bits.size() / 2 + 1; ++b) {
                    System.out.print(bits.toString() + " l-> ");

                    bits.doShift('l');
                }

                System.out.println(bits.toString());
            }
        }

        {
            var test = Bits.fromNum(4);
            test.resize(8);
            test.show();
        }
    }
}
