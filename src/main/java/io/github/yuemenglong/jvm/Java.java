/**
 * Created by <yuemenglong@126.com> on 2018/2/8.
 */
package io.github.yuemenglong.jvm;

public class Java {
    public static void main(String args[]) {
        for (int i = 0; i < 3; i++) {
            System.out.print(i);
        }
    }

    public static int test(int i) {
        int ret = 0;
        switch (i) {
            default:
                return ret;
        }
    }

//    public static boolean isSpecialChar(char c) {
//        boolean lAnswer = false;
//        switch (c) {
//            case '"':
//            case '(':
//            case ')':
//            case ',':
//            case '/':
//            case ':':
//            case ';':
//            case '<':
//            case '=':
//            case '>':
//            case '?':
//            case '@':
//            case '[':
//            case '\\':
//            case ']':
//                lAnswer = true;
//            case '#':
//            case '$':
//            case '%':
//            case '&':
//            case '\'':
//            case '*':
//            case '+':
//            case '-':
//            case '.':
//            case '0':
//            case '1':
//            case '2':
//            case '3':
//            case '4':
//            case '5':
//            case '6':
//            case '7':
//            case '8':
//            case '9':
//            case 'A':
//            case 'B':
//            case 'C':
//            case 'D':
//            case 'E':
//            case 'F':
//            case 'G':
//            case 'H':
//            case 'I':
//            case 'J':
//            case 'K':
//            case 'L':
//            case 'M':
//            case 'N':
//            case 'O':
//            case 'P':
//            case 'Q':
//            case 'R':
//            case 'S':
//            case 'T':
//            case 'U':
//            case 'V':
//            case 'W':
//            case 'X':
//            case 'Y':
//            case 'Z':
//            default:
//                return lAnswer;
//        }
//    }


    public static double sub(double a, double b) {
        return a + b;
    }
}
